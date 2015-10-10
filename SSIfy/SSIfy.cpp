//=====--------------------------------------------------------------------=====
//
// SSIfy: a parameterized tool to convert programs for sparse analyses
//
// This file is part of SSIfy.
//
// SSIfy is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// SSIfy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with SSIfy.  If not, see <http://www.gnu.org/licenses/>.
//
// Copyright (C) 2013, 2015  Victor Hugo Sperle Campos
//
//=====--------------------------------------------------------------------=====

#include "SSIfy.h"
#include "llvm/Analysis/IteratedDominanceFrontier.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

// Definition of static members of SSIfy
const std::string SSIfy::phiname = "SSIfy_phi";
const std::string SSIfy::signame = "SSIfy_sigma";
const std::string SSIfy::copname = "SSIfy_copy";

// Command-line options
static cl::opt<bool> Verbose("v", cl::desc("Print details"));
static cl::opt<bool> eSSA("essa",
                          cl::desc("Extended Static Single Assignment"));
static cl::opt<bool> SSU("ssu", cl::desc("Static Single Use"));
static cl::opt<bool>
    eSSA_up("essa-up", cl::desc("Extended Static Single Assignment (upwards)"));
static cl::opt<bool> SSU_up("ssu-up", cl::desc("Static Single Use (upwards)"));

bool SSIfy::runOnFunction(Function &F) {
  this->F = &F;

  this->DTmap = DominatorTreePointer(new DominatorTree(false));
  this->DTmap->recalculate(F);
  this->PDTmap = DominatorTreePointer(new DominatorTree(true));
  this->PDTmap->recalculate(F);
  this->ForwardIDF = IDFCalculatorPointer(new IDFCalculator(*DTmap));
  this->ReverseIDF = IDFCalculatorPointer(new IDFCalculator(*PDTmap));

  if (eSSA) {
    this->flags[0] = true;
  }
  if (eSSA_up) {
    this->flags[1] = true;
  }
  if (SSU) {
    this->flags[2] = true;
  }
  if (SSU_up) {
    this->flags[3] = true;
  }

  if (Verbose) {
    errs() << "Running on function " << F.getName() << "\n";
  }

  // For every instruction in this function, call the SSIfy function
  Function::iterator Fit, Fend;

  for (Fit = F.begin(), Fend = F.end(); Fit != Fend; ++Fit) {
    BasicBlock &BB = *Fit;

    BasicBlock::iterator BBit, BBend;
    for (BBit = BB.begin(), BBend = BB.end(); BBit != BBend; ++BBit) {
      Instruction &I = *BBit;

      if (is_actual(&I)) {
        run(&I);
      }
    }
  }

  clean();

  this->versions.clear();

  return true;
}

void SSIfy::run(Instruction *V) {
  DenseSet<ProgramPoint> Iup;
  DenseSet<ProgramPoint> Idown;

  // %condition = icmp i32 slt %V 0
  // br i1 %condition BB1 BB2
  // This example explains the code section below
  // We have to check if a use of a use of V is a branch instruction to assess
  // whether it is a program point of Out(Conds) or not
  for (User *U : V->users()) {
    Instruction *use_inst = dyn_cast<Instruction>(U);

    if (!use_inst)
      continue;

    // Out(Conds)
    if (CmpInst *possible_cmp = dyn_cast<CmpInst>(use_inst)) {
      for (User *Uu : possible_cmp->users()) {
        if (BranchInst *br_inst = dyn_cast<BranchInst>(Uu)) {
          // (downwards)
          if (flags[0]) {
            Idown.insert(ProgramPoint(br_inst, ProgramPoint::Out));
          }

          // (upwards)
          if (flags[1]) {
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
    // We only create SSIfy_copy for integer variables. There is no way
    // to clone variables that works across the board for all types.
    // For instance, a variable of custom type (e.g. class A) would have
    // to be copy-constructed. It could lead to problems if such type
    // doesn't have a copy constructor.
    //
    // For integer types, on the other hand, we can simulate cloning by
    // adding it to zero.
    //
    else if (V->getType()->isIntegerTy() && !isa<TerminatorInst>(use_inst) &&
             !isa<PHINode>(use_inst)) {
      // Uses (downwards)
      if (flags[2]) {
        Idown.insert(ProgramPoint(use_inst, ProgramPoint::Self));
      }

      // Uses (upwards)
      if (flags[3]) {
        Iup.insert(ProgramPoint(use_inst, ProgramPoint::Self));
      }
    }
  }

  split(V, Iup, Idown);
  rename_initial(V);
}

void SSIfy::computeDominatorTreeLevels() {
  SmallVector<DomTreeNode *, 32> Worklist;

  DomTreeNode *root = this->DTmap->getRootNode();
  DomLevels[root] = 0u;
  Worklist.push_back(root);

  while (!Worklist.empty()) {
    DomTreeNode *node = Worklist.pop_back_val();
    unsigned child_level = DomLevels[node] + 1u;
    for (DomTreeNode::iterator CI = node->begin(), CEnd = node->end();
         CI != CEnd; ++CI) {
      DomLevels[*CI] = child_level;
      Worklist.push_back(*CI);
    }
  }
}

void SSIfy::computePostDominatorTreeLevels() {
  SmallVector<DomTreeNode *, 32> Worklist;

  DomTreeNode *root = this->PDTmap->getRootNode();
  PDomLevels[root] = 0u;
  Worklist.push_back(root);

  while (!Worklist.empty()) {
    DomTreeNode *node = Worklist.pop_back_val();
    unsigned child_level = PDomLevels[node] + 1u;
    for (DomTreeNode::iterator CI = node->begin(), CEnd = node->end();
         CI != CEnd; ++CI) {
      PDomLevels[*CI] = child_level;
      Worklist.push_back(*CI);
    }
  }
}

const SmallVector<BasicBlock *, 32> &SSIfy::getIDF(BasicBlock *BB) {
  // Was it already computed? If yes, return the result
  const DenseMap<BasicBlock *, SmallVector<BasicBlock *, 32>>::const_iterator
      &mit = this->IDFmap.find(BB);

  if (mit != this->IDFmap.end()) {
    return mit->second;
  }

  SmallPtrSet<BasicBlock *, 1> DefBlocks;
  DefBlocks.insert(BB);
  ForwardIDF->setDefiningBlocks(DefBlocks);

  SmallVector<BasicBlock *, 32> &IDFBlocks = this->IDFmap[BB];
  ForwardIDF->calculate(IDFBlocks);

  return IDFBlocks;
}

const SmallVector<BasicBlock *, 32> &SSIfy::getIPDF(BasicBlock *BB) {
  // Was it already computed? If yes, return the result
  const DenseMap<BasicBlock *, SmallVector<BasicBlock *, 32>>::const_iterator
      &mit = this->IPDFmap.find(BB);

  if (mit != this->IPDFmap.end()) {
    return mit->second;
  }

  SmallPtrSet<BasicBlock *, 1> DefBlocks;
  DefBlocks.insert(BB);
  ReverseIDF->setDefiningBlocks(DefBlocks);

  SmallVector<BasicBlock *, 32> &IPDFBlocks = this->IPDFmap[BB];
  ReverseIDF->calculate(IPDFBlocks);

  return IPDFBlocks;
}

void SSIfy::computeLiveness(Instruction *V) {
  // Clear any previous liveness information from the data structure
  this->liveBlocks.clear();

  // This is a flat out liveness analysis.
  // Backwards, starting from the uses, going up until the definition is found
  SmallVector<BasicBlock *, 32> Worklist;
  for (User *U : V->users()) {
    Value *user = U;
    Instruction *I = dyn_cast<Instruction>(user);
    if (I) {
      Worklist.push_back(I->getParent());
    }
  }

  while (!Worklist.empty()) {
    BasicBlock *BB = Worklist.pop_back_val();

    // If the BasicBlock is already in the set of live blocks, it and all its
    // predecessors have already been inserted into the worklist
    if (!liveBlocks.insert(BB).second) {
      continue;
    }

    for (BasicBlock *pred : predecessors(BB)) {
      // We consider that a variable is not live in the block that defines it
      if (V->getParent() == pred) {
        continue;
      }

      Worklist.push_back(pred);
    }
  }
}

void SSIfy::split(Instruction *V, const DenseSet<ProgramPoint> &Iup,
                  const DenseSet<ProgramPoint> &Idown) {
  DenseSet<ProgramPoint> Sup;
  DenseSet<ProgramPoint> Sdown;

  if (Verbose) {
    errs() << "Splitting " << V->getName() << "\n";
  }

  // Here we run a liveness analysis. This is extremely useful to avoid creating
  // new variables that never get used
  computeLiveness(V);

  for (DenseSet<ProgramPoint>::const_iterator sit = Iup.begin(),
                                              send = Iup.end();
       sit != send; ++sit) {
    const ProgramPoint &point = *sit;
    Instruction *I = point.getInst();
    BasicBlock *BBparent = I->getParent();

    if (point.is_join()) {
      for (BasicBlock *BBpred : predecessors(BBparent)) {
        const SmallVector<BasicBlock *, 32> &iterated_pdf = getIPDF(BBpred);

        for (SmallVector<BasicBlock *, 32>::const_iterator
                 sit = iterated_pdf.begin(),
                 send = iterated_pdf.end();
             sit != send; ++sit) {
          BasicBlock *BB = *sit;
          Instruction &last = BB->back();
          Sup.insert(ProgramPoint(&last, ProgramPoint::Out));
        }
      }
    } else {
      const SmallVector<BasicBlock *, 32> &iterated_pdf = getIPDF(BBparent);

      for (SmallVector<BasicBlock *, 32>::const_iterator
               sit = iterated_pdf.begin(),
               send = iterated_pdf.end();
           sit != send; ++sit) {
        BasicBlock *BB = *sit;
        Instruction &last = BB->back();
        Sup.insert(ProgramPoint(&last, ProgramPoint::Out));
      }
    }
  }

  // Union of Sup, Idown and V
  DenseSet<ProgramPoint> NewSet;
  NewSet.insert(Sup.begin(), Sup.end());
  NewSet.insert(Idown.begin(), Idown.end());

  for (DenseSet<ProgramPoint>::iterator sit = NewSet.begin(),
                                        send = NewSet.end();
       sit != send; ++sit) {
    const ProgramPoint &point = *sit;
    Instruction *I = point.getInst();
    BasicBlock *BBparent = I->getParent();

    if (point.is_branch()) {
      for (BasicBlock *BBsucc : successors(BBparent)) {
        const SmallVector<BasicBlock *, 32> &iterated_df = getIDF(BBsucc);

        for (SmallVector<BasicBlock *, 32>::const_iterator
                 sit = iterated_df.begin(),
                 send = iterated_df.end();
             sit != send; ++sit) {
          BasicBlock *BB = *sit;
          Instruction &first = BB->front();
          Sdown.insert(ProgramPoint(&first, ProgramPoint::In));
        }
      }
    } else {
      const SmallVector<BasicBlock *, 32> &iterated_df = getIDF(BBparent);

      for (SmallVector<BasicBlock *, 32>::const_iterator
               sit = iterated_df.begin(),
               send = iterated_df.end();
           sit != send; ++sit) {
        BasicBlock *BB = *sit;
        Instruction &first = BB->front();
        Sdown.insert(ProgramPoint(&first, ProgramPoint::In));
      }
    }
  }

  DenseSet<ProgramPoint> S;
  S.insert(Iup.begin(), Iup.end());
  S.insert(Idown.begin(), Idown.end());
  S.insert(Sup.begin(), Sup.end());
  S.insert(Sdown.begin(), Sdown.end());

  // Split live range of v by inserting sigma, phi, and copies
  for (DenseSet<ProgramPoint>::iterator sit = S.begin(), send = S.end();
       sit != send; ++sit) {
    const ProgramPoint &point = *sit;

    if (!point.not_definition_of(V)) {
      continue;
    }

    Instruction *insertion_point = point.getInst();

    if (point.is_join()) {
      // Is this Phi actually necessary?
      if (!liveBlocks.count(insertion_point->getParent()) ||
          !this->DTmap->dominates(V, insertion_point)) {
        continue;
      }

      // phi
      const unsigned numReservedValues =
          std::distance(pred_begin(insertion_point->getParent()),
                        pred_end(insertion_point->getParent()));

      PHINode *new_phi = PHINode::Create(V->getType(), numReservedValues,
                                         phiname, insertion_point);

      // Add V multiple times as incoming value to the new phi
      for (BasicBlock *predBB : predecessors(insertion_point->getParent())) {
        new_phi->addIncoming(V, predBB);
      }

      if (Verbose) {
        errs() << "Created " << new_phi->getName() << "\n";
      }

      this->versions[V].insert(new_phi);
      ++NumPHIsCreated;
    } else if (point.is_branch()) {
      // sigma
      // Insert one sigma in each of the successors
      BasicBlock *BBparent = point.getInst()->getParent();

      for (BasicBlock *BBsucc : successors(BBparent)) {
        // Is this Sigma actually necessary?
        if (!dominatesAnyUse(BBsucc->begin(), V)) {
          continue;
        }

        const unsigned numReservedValues =
            std::distance(pred_begin(BBsucc), pred_end(BBsucc));

        PHINode *new_sigma = PHINode::Create(V->getType(), numReservedValues,
                                             signame, &BBsucc->front());

        // Add V multiple times as incoming value to the new phi
        for (BasicBlock *predBB : predecessors(BBsucc)) {
          new_sigma->addIncoming(V, predBB);
        }

        if (Verbose) {
          errs() << "Created " << new_sigma->getName() << "\n";
        }

        this->versions[V].insert(new_sigma);
        ++NumSigmasCreated;
      }
    } else if (point.is_copy()) {
      // copy
      if (!dominatesAnyUse(insertion_point, V))
        continue;

      // Zero value
      ConstantInt *zero = ConstantInt::get(cast<IntegerType>(V->getType()), 0);

      BinaryOperator *new_copy =
          BinaryOperator::Create(Instruction::Add, V, zero, copname);

      new_copy->insertAfter(insertion_point);

      if (Verbose) {
        errs() << "Created " << new_copy->getName() << "\n";
      }

      this->versions[V].insert(new_copy);
      ++NumCopiesCreated;
    }
  }
}

void SSIfy::rename_initial(Instruction *V) {
  RenamingStack stack(V);

  BasicBlock *root = V->getParent();

  rename(root, stack);
}

void SSIfy::rename(BasicBlock *BB, RenamingStack &stack) {
  const Value *V = stack.getValue();

  if (Verbose) {
    errs() << "Renaming " << V->getName() << " in " << BB->getName() << "\n";
  }

  // Iterate over all instructions in BB to perform renaming
  for (BasicBlock::iterator iit = BB->begin(), iend = BB->end(); iit != iend;
       ++iit) {
    Instruction *I = cast<Instruction>(&*iit);
    PHINode *phi = dyn_cast<PHINode>(I);

    bool has_newdef = false;

    for (Value *used : I->operands()) {
      if (used == V) {
        // If it's a regular PHI, SSIfy_phi, SSIfy_sigma, or SSIfy_copy,
        // I is a new definition of V
        if (phi || is_SSIcopy(I)) {
          has_newdef = true;
        }

        if (!phi) {
          set_use(stack, I);
        }

        break;
      }
    }

    // New definition of V
    // sigma, phi or copy
    if (has_newdef) {
      // sigma or phi
      if (phi) {
        set_def(stack, phi);
      }
      // copy
      else if (is_SSIcopy(I)) {
        set_def(stack, I);
      }
    }
  }

  // Search for phis and sigmas in the successors to rename uses of V inside
  // them
  for (BasicBlock *BBsucc : successors(BB)) {
    for (BasicBlock::iterator BBit = BBsucc->begin(),
                              BBend = BBsucc->getFirstInsertionPt();
         BBit != BBend; ++BBit) {
      PHINode *phi = dyn_cast<PHINode>(&*BBit);

      if (phi) {
        set_use(stack, phi, BB);
      }
    }
  }

  // Now call recursively for all children in the dominance tree
  DomTreeNode *domtree = this->DTmap->getNode(BB);
  if (domtree) {
    for (DomTreeNode::iterator begin = domtree->begin(), end = domtree->end();
         begin != end; ++begin) {
      DomTreeNodeBase<BasicBlock> *DTN_children = *begin;
      BasicBlock *BB_children = DTN_children->getBlock();
      rename(BB_children, stack);
    }
  }
}

void SSIfy::set_use(RenamingStack &stack, Instruction *inst, BasicBlock *from) {
  Value *V = stack.getValue();
  Instruction *popped = 0;

  // If the stack is initially empty, renaming hasn't reached the initial
  // definition of V yet, so no point in renaming
  if (stack.empty()) {
    return;
  }

  // If from != null, we are dealing with a renaming inside a PHINode
  if (!from) {
    while (!stack.empty()) {
      popped = stack.peek();

      if (!this->DTmap->dominates(popped, inst)) {
        stack.pop();

        if (Verbose) {
          errs() << "set_use: Popping " << popped->getName()
                 << " from the stack of " << stack.getValue()->getName()
                 << "\n";
        }
      } else {
        break;
      }
    }
  } else {
    while (!stack.empty()) {
      popped = stack.peek();

      if ((popped->getParent() != from) &&
          (!this->DTmap->dominates(popped, from))) {
        stack.pop();

        if (Verbose) {
          errs() << "set_usephi: Popping " << popped->getName()
                 << " from the stack of " << stack.getValue()->getName()
                 << "\n";
        }
      } else {
        break;
      }
    }
  }

  // If the stack has become empty, it means that the last valid definition is
  // actually V itself, not popped. Otherwise, popped would still be in stack
  Instruction *new_name = stack.empty() ? cast<Instruction>(V) : popped;

  if ((new_name != V) && (new_name != inst)) {
    if (!from) {
      if (Verbose) {
        errs() << "set_use: Renaming uses of " << V->getName() << " in "
               << inst->getName() << " to " << new_name->getName() << "\n";
      }

      inst->replaceUsesOfWith(V, new_name);
    } else {
      PHINode *phi = cast<PHINode>(inst);
      int index = phi->getBasicBlockIndex(from);

      if (phi->getIncomingValue(index) == V) {
        if (Verbose) {
          errs() << "set_usephi: Renaming uses of " << V->getName() << " in "
                 << inst->getName() << " to " << new_name->getName() << "\n";
        }

        phi->setIncomingValue(index, new_name);
      }
    }
  }
}

void SSIfy::set_def(RenamingStack &stack, Instruction *inst) {
  // Note that this function *doesn't* check if inst contains an use of
  // stack.Value!
  // Verification has to be done by the user of this function
  if (Verbose) {
    errs() << "set_def: Pushing " << inst->getName() << " to the stack of "
           << stack.getValue()->getName() << "\n";
  }

  stack.push(inst);
}

void SSIfy::eraseInstruction(Instruction *version, Instruction *original,
                             SmallPtrSet<Instruction *, 32> *set) {
  for (Value *V : version->operands()) {
    Instruction *operand = dyn_cast<Instruction>(V);
    if (operand && !is_actual(operand)) {
      set->insert(operand);
    }
  }
  version->replaceAllUsesWith(original);
  version->eraseFromParent();
}

void SSIfy::clean() {
  SmallPtrSet<Instruction *, 32> S;
  DenseMap<Instruction *, Instruction *> maptooldvalues;

  // Put all SSIfy variables into the set S and initialize the map from these
  // variables to their original definition
  for (DenseMap<Value *, SmallPtrSet<Instruction *, 4>>::iterator
           mit = this->versions.begin(),
           mend = this->versions.end();
       mit != mend; ++mit) {
    Instruction *original = cast<Instruction>(mit->first);
    SmallPtrSet<Instruction *, 4> &list = mit->second;

    for (SmallPtrSet<Instruction *, 4>::iterator sit = list.begin(),
                                                 send = list.end();
         sit != send; ++sit) {
      Instruction *version = *sit;
      maptooldvalues[version] = original;

      S.insert(version);
    }
  }

  // Cleaning process
  while (!S.empty()) {
    Instruction *version = *S.begin();
    S.erase(version);

    Instruction *V = maptooldvalues[version];

    // The cleaning criteria for SSIfy_phi has three cases
    // First: phi whose incoming values are all the same.
    // Second: phi that is not dominated by V.
    // Third: phi that is never used.
    if (is_SSIphi(version)) {
      PHINode *ssi_phi = cast<PHINode>(version);
      bool any_value_diff = false;

      // First case: phis with all incoming values corresponding to the original
      // value
      for (unsigned i = 0, n = ssi_phi->getNumIncomingValues(); i < n; ++i) {
        const Value *incoming = ssi_phi->getIncomingValue(i);

        if (incoming != V) {
          any_value_diff = true;
          break;
        }
      }

      if (!any_value_diff) {
        if (Verbose) {
          errs() << "Erasing " << ssi_phi->getName() << " because all \
                                incoming values are equal\n";
        }

        eraseInstruction(version, V, &S);
        ++NumPHIsDeleted;
        continue;
      }

      // Second case
      if (!this->DTmap->dominates(V, ssi_phi)) {
        if (Verbose) {
          errs() << "Erasing " << ssi_phi->getName() << " because the \
                                original definition does not dominate it\n";
        }

        eraseInstruction(version, V, &S);
        ++NumPHIsDeleted;
        continue;
      }

      // Third case
      if (ssi_phi->use_empty()) {
        if (Verbose) {
          errs() << "Erasing " << ssi_phi->getName() << " because it \
                                has no users\n";
        }

        eraseInstruction(version, V, &S);
        ++NumPHIsDeleted;
        continue;
      }
    }
    // SSIfy_sigmas and SSIfy_copies have two cases for cleaning
    // First: they don't have any use
    // Second: they aren't dominated by V.
    else if (is_SSIsigma(version) || is_SSIcopy(version)) {
      if (version->use_empty()) {
        if (Verbose) {
          errs() << "Erasing " << version->getName() << " because it \
                                has no users\n";
        }

        if (is_SSIsigma(version)) {
          ++NumSigmasDeleted;
        } else if (is_SSIcopy(version)) {
          ++NumCopiesDeleted;
        } else {
          llvm_unreachable("Phis could not have fallen into this case.");
        }
        eraseInstruction(version, V, &S);
      } else if (!this->DTmap->dominates(V, version)) {
        if (Verbose) {
          errs() << "Erasing " << version->getName() << " because the \
                                original definition does not dominate it\n";
        }

        if (is_SSIsigma(version)) {
          ++NumSigmasDeleted;
        } else if (is_SSIcopy(version)) {
          ++NumCopiesDeleted;
        } else {
          llvm_unreachable("Phis could not have fallen into this case.");
        }
        eraseInstruction(version, V, &S);
      }
    } else {
      llvm_unreachable(
          "A non-SSIfy value ended up being inserted in the versions "
          "map!");
    }
  }
}

bool SSIfy::is_SSIphi(const Instruction *I) {
  return I->getName().startswith(phiname);
}

bool SSIfy::is_SSIsigma(const Instruction *I) {
  return I->getName().startswith(signame);
}

bool SSIfy::is_SSIcopy(const Instruction *I) {
  return I->getName().startswith(copname);
}

bool SSIfy::is_actual(const Instruction *I) {
  if (I->getName().startswith("SSIfy")) {
    return false;
  }

  return true;
}

bool SSIfy::dominatesAnyUse(const Instruction *insert_point,
                            const Value *V) const {
  for (const User *U : V->users()) {
    const Instruction *use = cast<Instruction>(U);

    if (this->DTmap->dominates(insert_point, use)) {
      return true;
    }
  }

  return false;
}

void SSIfy::getAnalysisUsage(AnalysisUsage &) const {}

// ProgramPoint
ProgramPoint::ProgramPoint(Instruction *I, Position P) : I(I), P(P) {}

template <> struct DenseMapInfo<ProgramPoint> {
  using FirstInfo = DenseMapInfo<Instruction *>;

  static inline ProgramPoint getEmptyKey() {
    return ProgramPoint(
        reinterpret_cast<Instruction *>(static_cast<uintptr_t>(-1)),
        ProgramPoint::Undefined);
  }

  static inline ProgramPoint getTombstoneKey() {
    return ProgramPoint(
        reinterpret_cast<Instruction *>(static_cast<uintptr_t>(-2)),
        ProgramPoint::Undefined);
  }

  static unsigned getHashValue(const ProgramPoint &Val) {
    uint64_t key = (uint64_t)FirstInfo::getHashValue(Val.getInst()) << 3 |
                   (uint64_t)Val.getPosition();
    key += ~(key << 32);
    key ^= (key >> 22);
    key += ~(key << 13);
    key ^= (key >> 8);
    key += (key << 3);
    key ^= (key >> 15);
    key += ~(key << 27);
    key ^= (key >> 31);
    return (unsigned)key;
  }

  static bool isEqual(const ProgramPoint &LHS, const ProgramPoint &RHS) {
    return LHS == RHS;
  }
};

bool ProgramPoint::operator==(const ProgramPoint &o) const {
  if (this->P != o.P) {
    return false;
  }

  switch (this->P) {
  case ProgramPoint::Undefined:
  case ProgramPoint::Self:
    return this->I == o.I;
  case ProgramPoint::Out:
  case ProgramPoint::In: {
    const BasicBlock *this_I_parent = this->I->getParent();
    const BasicBlock *o_I_parent = o.I->getParent();
    return this_I_parent == o_I_parent;
  }
  }
}

bool ProgramPoint::not_definition_of(const Value *V) const {
  const Instruction *I = this->I;
  const BasicBlock *BB = I->getParent();

  if (I == V)
    return false;

  switch (this->P) {
  case ProgramPoint::In:
    // phi case
    for (BasicBlock::const_iterator BBit = BB->begin(),
                                    BBend = BB->getFirstNonPHI();
         BBit != BBend; ++BBit) {
      const PHINode *op = cast<PHINode>(&*BBit);

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
    for (const BasicBlock *BBsucc : successors(BB)) {
      for (BasicBlock::const_iterator BBit = BBsucc->begin(),
                                      BBend = BBsucc->getFirstNonPHI();
           BBit != BBend; ++BBit) {
        const PHINode *op = cast<PHINode>(&*BBit);

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
  case ProgramPoint::Self: {
    // copy case
    //
    // We walk through the instructions that follow I looking for a
    // created
    // SSIfy_copy that redefines V already.
    // If it exists, then there's a definition already.
    const BasicBlock *parent = I->getParent();
    for (BasicBlock::const_iterator bit = BasicBlock::const_iterator(*I),
                                    bend = parent->end();
         bit != bend && SSIfy::is_SSIcopy(&*bit); ++bit) {
      const Instruction *next = &*bit;

      // Check if operand is V
      if (next->getOperand(0) == V) {
        return false;
      }
    }

    break;
  }
  case ProgramPoint::Undefined:
    llvm_unreachable(
        "An Undefined ProgramPoint doesn't contain any definition!");
  }

  return true;
}

bool ProgramPoint::is_join() const {
  return !this->I->getParent()->getSinglePredecessor() &&
         (this->P == ProgramPoint::In);
}

bool ProgramPoint::is_branch() const {
  return isa<BranchInst>(this->I) && (this->P == ProgramPoint::Out);
}

bool ProgramPoint::is_copy() const { return this->P == ProgramPoint::Self; }

// RenamingStack
RenamingStack::RenamingStack(Value *V) { this->V = V; }

Value *RenamingStack::getValue() const { return this->V; }

void RenamingStack::push(Instruction *I) { this->stack.push_back(I); }

void RenamingStack::pop() { this->stack.pop_back(); }

Instruction *RenamingStack::peek() const { return this->stack.back(); }

bool RenamingStack::empty() const { return this->stack.empty(); }

char SSIfy::ID = 0;
static RegisterPass<SSIfy> X("ssify", "SSIfy pass");
