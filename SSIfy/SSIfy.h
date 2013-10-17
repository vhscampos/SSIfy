/*
 * SSIfy.h
 *
 *  Created on: Sep 6, 2013
 *      Author: vhscampos
 */

#ifndef SSIFY_H_
#define SSIFY_H_

#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/CFG.h"
#include "llvm/Analysis/Dominators.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Analysis/DominanceFrontier.h"
#include "llvm/Support/InstIterator.h"
#include "llvm/Support/CommandLine.h"
#include <set>
#include <string>
#include <algorithm>
#include <cstdlib>

#define OUTCONDS_DOWN	1
#define OUTCONDS_UP		0
#define USES_DOWN		1
#define USES_UP			0

namespace llvm {

// Forward declarations
class ProgramPoint;
class RenamingStack;
struct PostDominanceFrontier;

// SSIfy - The second implementation with getAnalysisUsage implemented.
struct SSIfy: public FunctionPass {
	static const std::string phiname;
	static const std::string signame;
	static const std::string copname;

	static char ID; // Pass identification, replacement for typeid
	Function* F;
	DominatorTree* DTmap;
	PostDominatorTree* PDTmap;
	DominanceFrontier* DFmap;
	PostDominanceFrontier* PDFmap;

	// This map associates variables with the set of new variables
	// that have been created for them
	DenseMap<Value*, std::set<Instruction*> > versions;

	SSIfy() :
			FunctionPass(ID) {
		F = 0;
		DTmap = 0;
		PDTmap = 0;
		DFmap = 0;
		PDFmap = 0;
	}

	virtual bool runOnFunction(Function &F);
	void dragon(Instruction* V);
	void split(Instruction* V, std::set<ProgramPoint> Iup,
			std::set<ProgramPoint> Idown);
	void rename_initial(Instruction* V);
	void rename(BasicBlock* BB, RenamingStack& stack);
	void set_def(RenamingStack& stack, Instruction* inst);
	void set_use(RenamingStack& stack, Instruction* inst, BasicBlock* from = 0);
	void clean();

//	std::set<Instruction*> set_intersection(const std::set<Instruction*>& s1,
//			const std::set<Instruction*>& s2);
//	std::set<Instruction*> set_union(const std::set<Instruction*>& s1,
//			const std::set<Instruction*>& s2);
//	std::set<Instruction*> set_difference(const std::set<Instruction*>& s1,
//			const std::set<Instruction*>& s2);

	static bool is_SSIphi(const Instruction* I);
	static bool is_SSIsigma(const Instruction* I);
	static bool is_SSIcopy(const Instruction* I);
	static bool is_actual(const Instruction* I);

	SmallPtrSet<BasicBlock*, 4> get_iterated_df(BasicBlock* BB);
	SmallPtrSet<BasicBlock*, 4> get_iterated_pdf(BasicBlock* BB);
	void getAnalysisUsage(AnalysisUsage &AU) const;
};

class ProgramPoint {
public:
	typedef enum {
		In, Self, Out
	} Position;

public:
	explicit ProgramPoint(Instruction* I, Position P);

	bool not_definition_of(const Value* V) const;

	bool operator==(const ProgramPoint& o) const;
	bool operator!=(const ProgramPoint& o) const;
	bool operator<(const ProgramPoint& o) const;
	bool operator>(const ProgramPoint& o) const;

	inline bool is_join() const;
	inline bool is_branch() const;
	inline bool is_copy() const;

	Instruction* I;
	Position P;
};

/*
 *  Wrapper class for the stack, in order to store the Value for which
 *  this stack exists in the first place
 */
class RenamingStack {
public:
	SmallVector<Instruction*, 4> stack;
	Value* V;

public:
	RenamingStack(Value* V);
	inline Value* getValue() const;
	inline void push(Instruction* I);
	inline void pop();
	inline Instruction* peek();
	inline bool empty() const;
};

/// PostDominanceFrontier Class - Concrete subclass of DominanceFrontier that is
/// used to compute the a post-dominance frontier.
///
struct PostDominanceFrontier {
	static char ID;
public:
	typedef std::set<BasicBlock*> DomSetType;    // Dom set for a bb
	typedef std::map<BasicBlock*, DomSetType> DomSetMapType; // Dom set map

	DomSetMapType Frontiers;
	std::vector<BasicBlock*> Roots;
	const bool IsPostDominators;

	PostDominanceFrontier(PostDominatorTree* DTmap) :
			IsPostDominators(true) {
		calculate_frontiers(DTmap);
	}

	virtual ~PostDominanceFrontier() {
		releaseMemory();
	}

	/// getRoots - Return the root blocks of the current CFG.  This may include
	/// multiple blocks if we are computing post dominators.  For forward
	/// dominators, this will always be a single block (the entry node).
	///
	inline const std::vector<BasicBlock*> &getRoots() const {
		return Roots;
	}

	/// isPostDominator - Returns true if analysis based of postdoms
	///
	bool isPostDominator() const {
		return IsPostDominators;
	}

	virtual void releaseMemory() {
		Frontiers.clear();
	}

	// Accessor interface:
	typedef DomSetMapType::iterator iterator;
	typedef DomSetMapType::const_iterator const_iterator;
	iterator begin() {
		return Frontiers.begin();
	}
	const_iterator begin() const {
		return Frontiers.begin();
	}
	iterator end() {
		return Frontiers.end();
	}
	const_iterator end() const {
		return Frontiers.end();
	}
	iterator find(BasicBlock *B) {
		return Frontiers.find(B);
	}
	const_iterator find(BasicBlock *B) const {
		return Frontiers.find(B);
	}

	iterator addBasicBlock(BasicBlock *BB, const DomSetType &frontier) {
		assert(find(BB) == end() && "Block already in DominanceFrontier!");
		return Frontiers.insert(std::make_pair(BB, frontier)).first;
	}

	/// removeBlock - Remove basic block BB's frontier.
	void removeBlock(BasicBlock *BB) {
		assert(find(BB) != end() && "Block is not in DominanceFrontier!");
		for (iterator I = begin(), E = end(); I != E; ++I)
			I->second.erase(BB);
		Frontiers.erase(BB);
	}

	void addToFrontier(iterator I, BasicBlock *Node) {
		assert(I != end() && "BB is not in DominanceFrontier!");
		I->second.insert(Node);
	}

	void removeFromFrontier(iterator I, BasicBlock *Node) {
		assert(I != end() && "BB is not in DominanceFrontier!");
		assert(
				I->second.count(Node)
						&& "Node is not in DominanceFrontier of BB");
		I->second.erase(Node);
	}

	/// compareDomSet - Return false if two domsets match. Otherwise
	/// return true;
	bool compareDomSet(DomSetType &DS1, const DomSetType &DS2) const {
		std::set<BasicBlock *> tmpSet;
		for (DomSetType::const_iterator I = DS2.begin(), E = DS2.end(); I != E;
				++I)
			tmpSet.insert(*I);

		for (DomSetType::const_iterator I = DS1.begin(), E = DS1.end(); I != E;
				) {
			BasicBlock *Node = *I++;

			if (tmpSet.erase(Node) == 0)
				// Node is in DS1 but not in DS2.
				return true;
		}

		if (!tmpSet.empty())
			// There are nodes that are in DS2 but not in DS1.
			return true;

		// DS1 and DS2 matches.
		return false;
	}

	/// compare - Return true if the other dominance frontier base matches
	/// this dominance frontier base. Otherwise return false.
	bool compare(DominanceFrontierBase &Other) const {
		DomSetMapType tmpFrontiers;
		for (DomSetMapType::const_iterator I = Other.begin(), E = Other.end();
				I != E; ++I)
			tmpFrontiers.insert(std::make_pair(I->first, I->second));

		for (DomSetMapType::iterator I = tmpFrontiers.begin(), E =
				tmpFrontiers.end(); I != E;) {
			BasicBlock *Node = I->first;
			const_iterator DFI = find(Node);
			if (DFI == end())
				return true;

			if (compareDomSet(I->second, DFI->second))
				return true;

			++I;
			tmpFrontiers.erase(Node);
		}

		if (!tmpFrontiers.empty())
			return true;

		return false;
	}

	bool calculate_frontiers(PostDominatorTree* DTmap) {
		Frontiers.clear();
		Roots = DTmap->getRoots();
		if (const DomTreeNode *Root = DTmap->getRootNode())
			calculate(*DTmap, Root);
		return false;
	}

private:
	const DomSetType &calculate(const PostDominatorTree &DT,
			const DomTreeNode *Node);
};

}

#endif /* SSIFY_H_ */
