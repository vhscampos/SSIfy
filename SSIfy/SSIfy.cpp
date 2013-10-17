//===- Hello.cpp - Example code from "Writing an LLVM Pass" ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements two versions of the LLVM "Hello World" pass described
// in docs/WritingAnLLVMPass.html
//
//===----------------------------------------------------------------------===//
#include "SSIfy.h"

using namespace llvm;

// Definition of static members of SSIfy
const std::string SSIfy::phiname = "SSI_phi";
const std::string SSIfy::signame = "SSI_sigma";
const std::string SSIfy::copname = "SSI_copy";

// Command-line options
static cl::opt<bool> Verbose("v", cl::desc("Print details"));
static cl::opt<std::string> ProgramPointOptions("set", cl::desc("Starting program points"), cl::Required);

const char* const flags = strdup(ProgramPointOptions.c_str());
const bool OutConds_Down = flags[0] == '1';
const bool OutConds_Up = flags[1] == '1';
const bool Uses_Down = flags[2] == '1';
const bool Uses_Up = flags[3] == '1';


bool SSIfy::runOnFunction(Function &F) {
	this->F = &F;
	this->DTmap = &getAnalysis<DominatorTree>();
	this->PDTmap = &getAnalysis<PostDominatorTree>();
	this->DFmap = &getAnalysis<DominanceFrontier>();
	this->PDFmap = new PostDominanceFrontier(this->PDTmap);

	if (Verbose) {
		errs() << "Running on function " << F.getName() << "\n";
	}

	// For every instruction in this function, call the SSIfy function
	Function::iterator Fit, Fend;

	for (Fit = F.begin(), Fend = F.end(); Fit != Fend; ++Fit) {
		BasicBlock& BB = *Fit;

		BasicBlock::iterator BBit, BBend;
		for (BBit = BB.begin(), BBend = BB.end(); BBit != BBend; ++BBit) {
			Instruction& I = *BBit;

			dragon(&I);
		}
	}

	clean();

	delete this->PDFmap;
	this->versions.clear();

	return true;
}

/*
 * 	Determines what is the splitting strategy for the variable V
 * 	and calls the SSIfy functions in order
 * 		- split
 * 		- rename
 * 		- clean
 */
void SSIfy::dragon(Instruction* V) {
	std::set<ProgramPoint> Iup;
	std::set<ProgramPoint> Idown;

	// %condition = icmp i32 slt %V 0
	// br i1 %condition BB1 BB2
	// This example above explains this code section below
	// We have to check if a use of a use of V is a branch instruction to assess whether
	// it is a program point of Out(Conds) or not
	for (Value::use_iterator i = V->use_begin(), e = V->use_end(); i != e;
			++i) {
		Instruction* use_inst = dyn_cast<Instruction>(*i);

		// Out(Conds)
		if (CmpInst* possible_cmp = dyn_cast<CmpInst>(use_inst)) {
			for (Value::use_iterator ii = possible_cmp->use_begin(), ee =
					possible_cmp->use_end(); ii != ee; ++ii) {
				if (BranchInst* br_inst = dyn_cast<BranchInst>(*ii)) {
					// (downwards)
					if (OutConds_Down) {
						Idown.insert(ProgramPoint(br_inst, ProgramPoint::Out));
					}

					// (upwards)
					if (OutConds_Up) {
						Iup.insert(ProgramPoint(br_inst, ProgramPoint::Out));
					}
				}
			}
		}
		// Uses
		//
		// EXCEPTIONS
		//  - TerminatorInst
		//  - PHINode
		//
		// These are exceptions because a copy created for them would
		// break the program, or not make sense.
		//
		else if (V->getType()->isIntegerTy()) {
			if (!isa<TerminatorInst>(use_inst) && !isa<PHINode>(use_inst)) {
				// Uses (downwards)	FIXME: only with integer variables
				if (Uses_Down) {
					Idown.insert(ProgramPoint(use_inst, ProgramPoint::Self));
				}

				// Uses (upwards)	FIXME: only with integer variables
				if (Uses_Up) {
					Iup.insert(ProgramPoint(use_inst, ProgramPoint::Self));
				}
			}
		}
	}

	split(V, Iup, Idown);
	rename_initial(V);
//	clean(V);
}

/*
 * 	Splits live range of the variable V according to the splitting strategy
 * 	defined as input.
 */
void SSIfy::split(Instruction* V, std::set<ProgramPoint> Iup,
		std::set<ProgramPoint> Idown) {
	std::set<ProgramPoint> Sup;
	std::set<ProgramPoint> Sdown;

	if (Verbose) {
		errs() << "Splitting " << V->getName() << "\n";
	}

	// Creation of the Sup set. Its logic is defined in the referenced paper.
	for (std::set<ProgramPoint>::iterator sit = Iup.begin(), send = Iup.end();
			sit != send; ++sit) {
		ProgramPoint point = *sit;
		Instruction* I = point.I;
		BasicBlock* BBparent = I->getParent();

		if (point.is_join()) {
			for (pred_iterator PI = pred_begin(BBparent), E = pred_end(
					BBparent); PI != E; ++PI) {
				BasicBlock *BBpred = *PI;

				SmallPtrSet<BasicBlock*, 4> iterated_pdf = get_iterated_pdf(
						BBpred);

				for (SmallPtrSet<BasicBlock*, 4>::iterator sit =
						iterated_pdf.begin(), send = iterated_pdf.end();
						sit != send; ++sit) {
					BasicBlock* BB = *sit;
					Instruction& last = BB->back();
					Sup.insert(ProgramPoint(&last, ProgramPoint::Out));
				}
			}
		} else {
			SmallPtrSet<BasicBlock*, 4> iterated_pdf = get_iterated_pdf(
					BBparent);

			for (SmallPtrSet<BasicBlock*, 4>::iterator sit =
					iterated_pdf.begin(), send = iterated_pdf.end();
					sit != send; ++sit) {
				BasicBlock* BB = *sit;
				Instruction& last = BB->back();
				Sup.insert(ProgramPoint(&last, ProgramPoint::Out));
			}
		}
	}

	// Union of Sup, Idown and V
	std::set<ProgramPoint> NewSet;
	NewSet.insert(Sup.begin(), Sup.end());
	NewSet.insert(Idown.begin(), Idown.end());

	// Creation of Sdown. Logic defined in the paper as well.
	for (std::set<ProgramPoint>::iterator sit = NewSet.begin(), send =
			NewSet.end(); sit != send; ++sit) {
		ProgramPoint point = *sit;
		Instruction* I = point.I;
		BasicBlock* BBparent = I->getParent();

		if (point.is_branch()) {
			for (succ_iterator PI = succ_begin(BBparent), E = succ_end(
					BBparent); PI != E; ++PI) {
				BasicBlock *BBsucc = *PI;

				SmallPtrSet<BasicBlock*, 4> iterated_df = get_iterated_df(
						BBsucc);

				for (SmallPtrSet<BasicBlock*, 4>::iterator sit =
						iterated_df.begin(), send = iterated_df.end();
						sit != send; ++sit) {
					BasicBlock* BB = *sit;
					Instruction& first = BB->front();
					Sdown.insert(ProgramPoint(&first, ProgramPoint::In));
				}
			}
		} else {
			SmallPtrSet<BasicBlock*, 4> iterated_df = get_iterated_df(BBparent);

			for (SmallPtrSet<BasicBlock*, 4>::iterator sit =
					iterated_df.begin(), send = iterated_df.end(); sit != send;
					++sit) {
				BasicBlock* BB = *sit;
				Instruction& first = BB->front();
				Sdown.insert(ProgramPoint(&first, ProgramPoint::In));
			}
		}
	}

//	for (std::set<ProgramPoint>::iterator sit = Sdown.begin(), send =
//			Sdown.end(); sit != send; ++sit) {
//		errs() << (*sit).I->getName() << " " << (*sit).P << "\n";
//	}
//	errs() << "\n";

	// Finally
	std::set<ProgramPoint> S;
	S.insert(Iup.begin(), Iup.end());
	S.insert(Idown.begin(), Idown.end());
	S.insert(Sup.begin(), Sup.end());
	S.insert(Sdown.begin(), Sdown.end());

//	for (std::set<ProgramPoint>::iterator sit = S.begin(), send = S.end();
//			sit != send; ++sit) {
//		errs() << (*sit).I->getName() << " " << (*sit).P << "\n";
//	}

	/*
	 * 	Split live range of v by inserting sigma, phi, and copies
	 */
	for (std::set<ProgramPoint>::iterator sit = S.begin(), send = S.end();
			sit != send; ++sit) {
		ProgramPoint point = *sit;

		if (point.not_definition_of(V)) {
			Instruction* insertion_point = point.I;
			ProgramPoint::Position relative_position = point.P;

			if (point.is_join()) {
				// phi
				unsigned numReservedValues = std::distance(
						pred_begin(insertion_point->getParent()),
						pred_end(insertion_point->getParent()));
				PHINode* new_phi = PHINode::Create(V->getType(),
						numReservedValues, phiname);

				// Add V multiple times as incoming value to the new phi
				for (pred_iterator BBit = pred_begin(
						insertion_point->getParent()), BBend = pred_end(
						insertion_point->getParent()); BBit != BBend; ++BBit) {
					BasicBlock* predBB = *BBit;
					new_phi->addIncoming(V, predBB);
				}

				switch (relative_position) {
				case ProgramPoint::In:
					new_phi->insertBefore(insertion_point);
					break;
				default:
					errs() << "Problem here";
					break;
				}

				if (Verbose) {
					errs() << "Created " << new_phi->getName() << "\n";
				}

				this->versions[V].insert(new_phi);
			} else if (point.is_branch()) {
				// sigma
				// Insert one sigma in each of the successors
				BasicBlock* BBparent = point.I->getParent();
				unsigned numReservedValues = 1;

				for (succ_iterator PI = succ_begin(BBparent), E = succ_end(
						BBparent); PI != E; ++PI) {
					BasicBlock *BBsucc = *PI;

					PHINode* new_sigma = PHINode::Create(V->getType(),
							numReservedValues, signame, &BBsucc->front());
					new_sigma->addIncoming(V, BBparent);

					if (Verbose) {
						errs() << "Created " << new_sigma->getName() << "\n";
					}

					this->versions[V].insert(new_sigma);
				}
			} else {
				// copy
				// FIXME: TEMPORARY SOLUTION!!!
				// If the program point is in fact an SSI_copy
				// we ignore it. Check with Fernando.
				if (is_SSIcopy(point.I)) {
					continue;
				}

				// Zero value
				ConstantInt* zero = ConstantInt::get(
						cast<IntegerType>(V->getType()), 0);

				BinaryOperator* new_copy = BinaryOperator::Create(
						Instruction::Add, V, zero, copname);

				switch (relative_position) {
				case ProgramPoint::Self:
					new_copy->insertAfter(insertion_point);
					break;
				default:
					errs() << "Problem here";
					break;
				}

				if (Verbose) {
					errs() << "Created " << new_copy->getName() << "\n";
				}

				this->versions[V].insert(new_copy);
			}
		}
	}
}

/*
 * 	Renaming function
 * 	Called after the creation of new variables (split function)
 */
void SSIfy::rename_initial(Instruction* V) {
	RenamingStack stack(V);

	BasicBlock* root = V->getParent();

	rename(root, stack);
}

/*
 * 	Passes through all instructions in BB to update uses of the
 * 	variable V to its most recent definition, as well as registering
 * 	new definitions when it takes place
 */
void SSIfy::rename(BasicBlock* BB, RenamingStack& stack) {
	const Value* V = stack.getValue();

	if (Verbose) {
		errs() << "Renaming " << V->getName() << " in " << BB->getName()
				<< "\n";
	}

	// Iterate over all instructions in BB
	for (BasicBlock::iterator iit = BB->begin(), iend = BB->end(); iit != iend;
			++iit) {
		Instruction* I = cast<Instruction>(&*iit);
		PHINode* phi = dyn_cast<PHINode>(I);

		// foreach instruction u in n that uses v
		// We do this renaming only if it is not a SSI_phi
		// because renaming in SSI_phi is done in a step afterwards
		if (!phi || !is_SSIphi(phi)) {
			for (User::op_iterator i = I->op_begin(), e = I->op_end(); i != e;
					++i) {
				Value *used = *i;

				if (used == V) {
					set_use(stack, I);
					break;
				}
			}
		}

		// NEW DEFINITION OF V
		// sigma or phi
		if (phi) {
//			// Check if any of the incoming values is V
//			for (unsigned i = 0, n = phi->getNumIncomingValues(); i < n; ++i) {
//				Value* incoming_value = phi->getIncomingValue(i);
//
//				if (incoming_value == V) {
			set_def(stack, phi);
//					break;
//				}
//			}
		}
		// copy
		else if (is_SSIcopy(I)) {
//			Value* operand = I->getOperand(0);
//
//			if (operand == V) {
			set_def(stack, I);
//			}
		}
	}

	// Searchs for SSI_phis in the successors, in order to rename uses of V in them
	for (succ_iterator sit = succ_begin(BB), send = succ_end(BB); sit != send;
			++sit) {
		BasicBlock* BBsucc = *sit;
		for (BasicBlock::iterator BBit = BBsucc->begin(), BBend =
				BBsucc->getFirstInsertionPt(); BBit != BBend; ++BBit) {
			PHINode* phi = dyn_cast<PHINode>(&*BBit);

			if (phi && is_SSIphi(phi)) {
				set_use(stack, phi, BB);
			}
		}
	}

	// Now call recursively for all children in the dominance tree
	DomTreeNode* domtree = this->DTmap->getNode(BB);
	for (DomTreeNode::iterator begin = domtree->begin(), end = domtree->end();
			begin != end; ++begin) {
		DomTreeNodeBase<BasicBlock> *DTN_children = *begin;
		BasicBlock *BB_children = DTN_children->getBlock();
		rename(BB_children, stack);
	}
}

/*
 * 	Renames uses of the variable V in the instruction inst to its last definition according
 * 	to the stack of definitions. Note that it pops definitions from the stack until it finds
 * 	one that is correct, i.e., dominates the instruction inst.
 *
 * 	from stands for pointer to the predecessor block. It is used when renaming variables
 * 	inside a SSI_phi to tell which incoming value should be renamed.
 *
 * 	Renaming inside a SSI_phi changes the function's logic. It can be tricky. We cannot simply
 * 	check if popped dominates inst anymore.
 */
void SSIfy::set_use(RenamingStack& stack, Instruction* inst, BasicBlock* from) {
	Value* V = stack.getValue();
	Instruction* popped = 0;

	// If from != null, we are dealing with a renaming
	// inside a SSI_phi.
	if (!from) {
		while (!stack.empty()) {
			popped = stack.peek();

			if (!this->DTmap->dominates(popped, inst)) {
				stack.pop();

				if (Verbose) {
					errs() << "set_use: Popping " << popped->getName()
							<< " from the stack of "
							<< stack.getValue()->getName() << "\n";
				}
			} else {
				break;
			}
		}
	} else {
		while (!stack.empty()) {
			popped = stack.peek();

			if (!this->DTmap->dominates(popped, from)) {
				stack.pop();

				if (Verbose) {
					errs() << "set_usephi: Popping " << popped->getName()
							<< " from the stack of "
							<< stack.getValue()->getName() << "\n";
				}
			} else {
				break;
			}
		}
	}

	// If the stack has become empty, it means that the last valid
	// definition is actually V itself, not popped. Otherwise, popped
	// would still be in stack, therefore this wouldn't be empty.
	Instruction* new_name = stack.empty() ? cast<Instruction>(V) : popped;

	// We shouldn't perform renaming in any of the following cases
	if ((new_name != V) && (new_name != inst)) {
		if (!from) {

			if (Verbose) {
				errs() << "set_use: Renaming uses of " << V->getName() << " in "
						<< inst->getName() << " to " << new_name->getName()
						<< "\n";
			}

			inst->replaceUsesOfWith(V, new_name);
		} else {
			PHINode* phi = cast<PHINode>(inst);
			int index = phi->getBasicBlockIndex(from);

			if (phi->getIncomingValue(index) == V) {

				if (Verbose) {
					errs() << "set_usephi: Renaming uses of " << V->getName()
							<< " in " << inst->getName() << " to "
							<< new_name->getName() << "\n";
				}

				phi->setIncomingValue(index, new_name);
			}
		}
	}
}

/*
 * 	Pushes into the stack a new definition of the variable V, that being the
 * 	instruction inst
 */
void SSIfy::set_def(RenamingStack& stack, Instruction* inst) {
	// Check if inst contains an use of V. If not
	// we get out of here.
	if (std::find(inst->op_begin(), inst->op_end(), stack.getValue())
			== inst->op_end()) {
		return;
	}

	if (Verbose) {
		errs() << "set_def: Pushing " << inst->getName() << " to the stack of "
				<< stack.getValue()->getName() << "\n";
	}

	stack.push(inst);

//	this->versions[stack.getValue()].insert(inst);
}

/*
 * 	Based on my new algorithm. Needs extensive testing.
 */
void SSIfy::clean() {

	for (DenseMap<Value*, std::set<Instruction*> >::iterator mit =
			this->versions.begin(), mend = this->versions.end(); mit != mend;
			++mit) {

		Instruction* V = cast<Instruction>(mit->first);
		std::set<Instruction*> created_vars = mit->second;

//		for (std::set<Instruction*>::iterator sit = created_vars.begin(), send =
//				created_vars.end(); sit != send; ++sit) {
//			errs() << "\t" << (*sit)->getName() << "\n";
//		}

		for (std::set<Instruction*>::iterator sit = created_vars.begin(), send =
				created_vars.end(); sit != send; ++sit) {
			Instruction* newvar = *sit;
//			errs() << newvar->getName() << "\n";

			if (is_SSIphi(newvar)) {
				PHINode* ssi_phi = cast<PHINode>(newvar);
				bool any_value_diff_V = false;

				// First case: phis with all incoming values corresponding to
				// the original value.
				for (unsigned i = 0, n = ssi_phi->getNumIncomingValues(); i < n;
						++i) {
					const Value* incoming = ssi_phi->getIncomingValue(i);

					if (incoming != V) {
						any_value_diff_V = true;
						break;
					}
				}

				if (!any_value_diff_V) {

					if (Verbose) {
						errs() << "Erasing " << ssi_phi->getName() << "\n";
					}

					ssi_phi->replaceAllUsesWith(V);
					ssi_phi->eraseFromParent();

					continue;
				}

				// Second case
				// FIXME: may be wrong
				if (!this->DTmap->dominates(V, ssi_phi)) {

					if (Verbose) {
						errs() << "Erasing " << ssi_phi->getName() << "\n";
					}

					ssi_phi->replaceAllUsesWith(V);
					ssi_phi->eraseFromParent();
				}
			} else if (is_SSIsigma(newvar) || is_SSIcopy(newvar)) {
				if (newvar->use_empty()) {
					newvar->eraseFromParent();
				}
			} else {
				errs() << "Problem here5\n";
			}
		}
	}

}

// TODO: remove unnecessary created instructions
/*void SSIfy::clean(Instruction* V) {
 std::set<Instruction*> defined;
 std::set<Instruction*> used;
 std::set<Instruction*> web = this->versions[V];
 std::set<Instruction*> active;
 bool atleastone = true;

 // CREATION OF THE DEFINED SET

 // initialize active with all actual instructions in the function that are a version of V
 for (std::set<Instruction*>::iterator web_it = web.begin(), web_end =
 web.end(); web_it != web_end; ++web_it) {
 Instruction* I = *web_it;
 if (is_actual(I)) {
 active.insert(I);
 }
 }

 while (atleastone) {
 atleastone = false;

 for (std::set<Instruction*>::iterator ait = active.begin(), aend =
 active.end(); ait != aend; ++ait) {

 Instruction* I = *ait;

 // IF I is already in the set Defined, we skip it
 if (defined.find(I) != defined.end()) {
 continue;
 }

 atleastone = true;

 for (Value::use_iterator uit = I->use_begin(), uend = I->use_end();
 uit != uend; ++uit) {
 Instruction* use = cast<Instruction>(*uit);

 active.insert(use);
 }

 defined.insert(I);
 }
 }

 // CREATION OF THE USED SET
 active.clear();

 // Initialize active with all instructions that have any use which is in web
 for (std::set<Instruction*>::iterator wit = web.begin(), wend = web.end();
 wit != wend; ++wit) {
 Instruction* I = *wit;

 for (User::op_iterator oit = I->op_begin(), oend = I->op_end();
 oit != oend; ++oit) {
 Instruction* U = dyn_cast<Instruction>(*oit);
 if (U && is_actual(U)) {
 active.insert(U);
 }
 }
 }

 atleastone = true;
 while (atleastone) {
 atleastone = false;

 for (std::set<Instruction*>::iterator ait = active.begin(), aend =
 active.end(); ait != aend; ++ait) {

 Instruction* I = *ait;

 bool any_use_notin_used = false;

 for (Value::use_iterator uit = I->use_begin(), uend = I->use_end();
 uit != uend; ++uit) {
 Instruction* use = cast<Instruction>(*uit);

 // If not in used
 if (used.find(use) == used.end()) {
 any_use_notin_used = true;
 break;
 }
 }

 // If any use that isn't in used was found, we do not skip
 if (!any_use_notin_used) {
 continue;
 }

 atleastone = true;

 for (std::set<Instruction*>::iterator wit = web.begin(), wend =
 web.end(); wit != wend; ++wit) {

 Instruction* version = *wit;

 for (Value::use_iterator uit = I->use_begin(), uend =
 I->use_end(); uit != uend; ++uit) {
 Instruction* use = cast<Instruction>(*uit);

 if (version == use) {
 if (used.find(version) == used.end()) {
 active.insert(version);
 used.insert(version);
 }
 }
 }
 }
 }
 }

 // NOW, THE REMOVAL STEP
 std::set<Instruction*> live = set_intersection(defined, used);

 // For each non actual instruction 'version' in web
 for (std::set<Instruction*>::iterator wit = web.begin(), wend = web.end();
 wit != wend; ++wit) {
 Instruction* version = *wit;

 if (!is_actual(version)) {
 // For each v_i operand of 'version' so that v_i not in 'live'
 for (User::op_iterator oit = version->op_begin(), oend =
 version->op_end(); oit != oend; ++oit) {
 Instruction* operand = dyn_cast<Instruction>(*oit);

 if (operand && (web.find(operand) != web.end())) {
 // if v_i is not in live:
 // replace v_i with undef
 UndefValue* undef = UndefValue::get(operand->getType());
 version->replaceUsesOfWith(operand, undef);
 }
 }

 // if version == undef or all uses of version are undef
 // the latter basic means that the use set of version is empty
 if (version->use_empty()) {
 version->eraseFromParent();
 }

 }
 }
 }*/

bool SSIfy::is_SSIphi(const Instruction* I) {
	return I->getName().startswith(phiname);
}

bool SSIfy::is_SSIsigma(const Instruction* I) {
	return I->getName().startswith(signame);
}

bool SSIfy::is_SSIcopy(const Instruction* I) {
	return I->getName().startswith(copname);
}

// For a given BasicBlock, return its iterated dominance frontier as a set
SmallPtrSet<BasicBlock*, 4> SSIfy::get_iterated_df(BasicBlock* BB) {
	SmallPtrSet<BasicBlock*, 4> iterated_df;

	SmallVector<BasicBlock*, 4> stack;
	BasicBlock* current = BB;

	// Initialize the stack with the original BasicBlock
	// this stack is further populated with BasicBlocks
	// in the iterated DF of the original BB, until
	// this iterated DF ends.
	stack.push_back(current);

	while (!stack.empty()) {
		current = stack.back();
		stack.pop_back();

		DominanceFrontier::DomSetType frontier =
				this->DFmap->find(current)->second;

		for (DominanceFrontier::DomSetType::iterator fit = frontier.begin(),
				fend = frontier.end(); fit != fend; ++fit) {
			BasicBlock* BB_infrontier = *fit;

			// Only push to stack if this BasicBlock wasn't seen before
			// P.S.: insert returns a pair. The second refers to whether
			// the element was actually inserted or not.
			if ((iterated_df.insert(BB_infrontier))) {
				stack.push_back(BB_infrontier);
			}
		}
	}

	return iterated_df;
}

SmallPtrSet<BasicBlock*, 4> SSIfy::get_iterated_pdf(BasicBlock* BB) {
	SmallPtrSet<BasicBlock*, 4> iterated_pdf;

	SmallVector<BasicBlock*, 4> stack;
	BasicBlock* current = BB;

	// Initialize the stack with the original BasicBlock
	// this stack is further populated with BasicBlocks
	// in the iterated PDF of the original BB, until
	// this iterated PDF ends.
	stack.push_back(current);

	while (!stack.empty()) {
		current = stack.back();
		stack.pop_back();

		PostDominanceFrontier::DomSetType frontier =
				this->PDFmap->find(current)->second;

		for (PostDominanceFrontier::DomSetType::iterator fit = frontier.begin(),
				fend = frontier.end(); fit != fend; ++fit) {
			BasicBlock* BB_infrontier = *fit;

			// Only push to stack if this BasicBlock wasn't seen before
			// P.S.: insert returns a pair. The second refers to whether
			// the element was actually inserted or not.
			if ((iterated_pdf.insert(BB_infrontier))) {
				stack.push_back(BB_infrontier);
			}
		}
	}

	return iterated_pdf;
}

/*

 * 	Performs intersection between two sets

 std::set<Instruction*> SSIfy::set_intersection(const std::set<Instruction*>& s1,
 const std::set<Instruction*>& s2) {
 std::set<Instruction*> result;

 for (std::set<Instruction*>::iterator sit = s1.begin(), send = s2.end();
 sit != send; ++sit) {
 Instruction* value = *sit;
 if (s2.find(value) != s2.end()) {
 result.insert(value);
 }
 }

 return result;
 }

 std::set<Instruction*> SSIfy::set_union(const std::set<Instruction*>& s1,
 const std::set<Instruction*>& s2) {
 std::set<Instruction*> result;

 result.insert(s1.begin(), s1.end());
 result.insert(s2.begin(), s2.end());

 return result;
 }

 std::set<Instruction*> SSIfy::set_difference(const std::set<Instruction*>& s1,
 const std::set<Instruction*>& s2) {
 std::set<Instruction*> result;

 for (std::set<Instruction*>::iterator sit = s1.begin(), send = s2.end();
 sit != send; ++sit) {
 Instruction* value = *sit;
 if (s2.find(value) == s2.end()) {
 result.insert(value);
 }
 }

 return result;
 }
 */

/*
 * 	Checks if I is an actual instruction.
 * 	Actual instruction is defined as not being created by us,
 * 	that is, sigma, artificial phi, and copy.
 */
bool SSIfy::is_actual(const Instruction* I) {
	if (is_SSIphi(I)) {
		return false;
	}
	if (is_SSIsigma(I)) {
		return false;
	}
	if (is_SSIcopy(I)) {
		return false;
	}

	return true;
}

void SSIfy::getAnalysisUsage(AnalysisUsage &AU) const {
	AU.addRequired<DominatorTree>();
	AU.addRequired<PostDominatorTree>();
	AU.addRequired<DominanceFrontier>();
}

char SSIfy::ID = 0;
static RegisterPass<SSIfy> X("ssify", "SSIfy pass");

ProgramPoint::ProgramPoint(Instruction* I, Position P) :
		I(I), P(P) {
}

// Two ProgramPoints are equal iff they are of the same region type and:
//     - if they are Self, their instruction should be the same.
//     - if not, their instructions' parents should be the same.
bool ProgramPoint::operator==(const ProgramPoint& o) const {
	if (this->P != o.P) {
		return false;
	}

	if (this->P == ProgramPoint::Self) {
		return this->I == o.I;
	}

	const BasicBlock* this_I_parent = this->I->getParent();
	const BasicBlock* o_I_parent = o.I->getParent();

	return this_I_parent == o_I_parent;
}

bool ProgramPoint::operator!=(const ProgramPoint& o) const {
	return !(*this == o);
}

bool ProgramPoint::operator<(const ProgramPoint& o) const {
	if (this->P < o.P) {
		return true;
	}
	if (this->P > o.P) {
		return false;
	}

	if (this->P == ProgramPoint::Self) {
		return this->I < o.I;
	}

	const BasicBlock* this_I_parent = this->I->getParent();
	const BasicBlock* o_I_parent = o.I->getParent();

	return this_I_parent < o_I_parent;
}

bool ProgramPoint::operator>(const ProgramPoint& o) const {
	return !(*this == o) && !(*this < o);
}

/*
 * Checks if this program point doesn't have a definition of V already
 * We have three cases: sigma, phi, or copy
 * Each one has a different logic.
 */
bool ProgramPoint::not_definition_of(const Value* V) const {
	const Instruction* I = this->I;
	const BasicBlock* BB = I->getParent();

	if (I == V)
		return false;

	switch (this->P) {
	case ProgramPoint::In:
		// phi case
		for (BasicBlock::const_iterator BBit = BB->begin(), BBend =
				BB->getFirstNonPHI(); BBit != BBend; ++BBit) {

			const PHINode* op = cast<PHINode>(&*BBit);

			if (SSIfy::is_SSIphi(op)) {
				unsigned n = op->getNumIncomingValues();
				unsigned i;
				for (i = 0; i < n; ++i) {
					if (op->getIncomingValue(i) == V) {
						return false;
					}
				}
			}
		}
		break;

	case ProgramPoint::Out:
		// sigma case
		for (succ_const_iterator BBsuccit = succ_begin(BB), BBsuccend =
				succ_end(BB); BBsuccit != BBsuccend; ++BBsuccit) {
			const BasicBlock* BBsucc = *BBsuccit;

			for (BasicBlock::const_iterator BBit = BBsucc->begin(), BBend =
					BBsucc->getFirstNonPHI(); BBit != BBend; ++BBit) {

				const PHINode* op = cast<PHINode>(&*BBit);

				if (SSIfy::is_SSIsigma(op)) {
					unsigned n = op->getNumIncomingValues();
					unsigned i;
					for (i = 0; i < n; ++i) {
						if (op->getIncomingValue(i) == V) {
							return false;
						}
					}
				}
			}
		}
		break;

	case ProgramPoint::Self:
		// copy case
		// we check this case by looking at the instruction AFTER, since
		// because I is actually the instruction for which a copy would have
		// been created. This copy, therefore, is the next instruction.

		// This next line is just a simple way to get the next instruction.
		// Don't panic.
		const Instruction* next = &*(++BasicBlock::const_iterator(*I));

		if (SSIfy::is_SSIcopy(next)) {
			// Check if operand is V
			if (next->getOperand(0) == V) {
				return false;
			}
		}
		break;
	}

	return true;
}

const DominanceFrontier::DomSetType &
PostDominanceFrontier::calculate(const PostDominatorTree &DT,
		const DomTreeNode *Node) {
// Loop over CFG successors to calculate DFlocal[Node]
	BasicBlock *BB = Node->getBlock();
	DomSetType &S = Frontiers[BB];			// The new set to fill in...
	if (getRoots().empty())
		return S;

	if (BB)
		for (pred_iterator SI = pred_begin(BB), SE = pred_end(BB); SI != SE;
				++SI) {
			BasicBlock *P = *SI;
			// Does Node immediately dominate this predecessor?
			DomTreeNode *SINode = DT[P];
			if (SINode && SINode->getIDom() != Node)
				S.insert(P);
		}

// At this point, S is DFlocal.  Now we union in DFup's of our children...
// Loop through and visit the nodes that Node immediately dominates (Node's
// children in the IDomTree)
//
	for (DomTreeNode::const_iterator NI = Node->begin(), NE = Node->end();
			NI != NE; ++NI) {
		DomTreeNode *IDominee = *NI;
		const DomSetType &ChildDF = calculate(DT, IDominee);

		DomSetType::const_iterator CDFI = ChildDF.begin(), CDFE = ChildDF.end();
		for (; CDFI != CDFE; ++CDFI) {
			if (!DT.properlyDominates(Node, DT[*CDFI]))
				S.insert(*CDFI);
		}
	}

	return S;
}

//////////////////////////////////////////////////////////////////

RenamingStack::RenamingStack(Value * V) {
	this->V = V;
}

Value * RenamingStack::getValue() const {
	return this->V;
}

void RenamingStack::push(Instruction* I) {
	this->stack.push_back(I);
}

void RenamingStack::pop() {
	this->stack.pop_back();
}

Instruction * RenamingStack::peek() {
	return this->stack.back();
}

bool RenamingStack::empty() const {
	return this->stack.empty();
}

bool ProgramPoint::is_join() const {
	return !this->I->getParent()->getSinglePredecessor()
			&& (this->P == ProgramPoint::In);
}

bool ProgramPoint::is_branch() const {
	return isa<BranchInst>(this->I) && (this->P == ProgramPoint::Out);
}

bool ProgramPoint::is_copy() const {
	return this->P == ProgramPoint::Self;
}
