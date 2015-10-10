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

#ifndef SSIFY_H_
#define SSIFY_H_

#define DEBUG_TYPE "ssify"

#include "llvm/Pass.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Dominators.h"
#include <string>
#include <memory>

namespace llvm {

STATISTIC(NumPHIsCreated, "Number of SSIfy_phis created");
STATISTIC(NumSigmasCreated, "Number of SSIfy_sigmas created");
STATISTIC(NumCopiesCreated, "Number of SSIfy_copies created");
STATISTIC(NumPHIsDeleted, "Number of SSIfy_phis deleted");
STATISTIC(NumSigmasDeleted, "Number of SSIfy_sigmas deleted");
STATISTIC(NumCopiesDeleted, "Number of SSIfy_copies deleted");

// Forward declarations
class Value;
class Function;
class BasicBlock;
class Instruction;
class DominatorTree;
class IDFCalculator;
class ProgramPoint;
class RenamingStack;

// SSIfy
class SSIfy : public FunctionPass {
protected:
  using DominatorTreePointer = std::unique_ptr<DominatorTree>;
  using IDFCalculatorPointer = std::unique_ptr<IDFCalculator>;

  static const std::string phiname;
  static const std::string signame;
  static const std::string copname;

  Function *F;
  DominatorTreePointer DTmap;
  DominatorTreePointer PDTmap;
  IDFCalculatorPointer ForwardIDF;
  IDFCalculatorPointer ReverseIDF;

  // Maps that store, for each dominator tree node, the level at which they are
  // located in the dominator tree
  //
  // The second map refers to postdominator tree
  DenseMap<DomTreeNode *, unsigned> DomLevels;
  DenseMap<DomTreeNode *, unsigned> PDomLevels;

  // These maps store IDFs and IPDFs that have already been computed
  DenseMap<BasicBlock *, SmallVector<BasicBlock *, 32>> IDFmap;
  DenseMap<BasicBlock *, SmallVector<BasicBlock *, 32>> IPDFmap;

  // This is a set of BasicBlocks in which a specific Value is alive. It has to
  // be cleared whenever a new liveness information is going to be computed
  //
  // The function computeLiveness does this clearing
  SmallPtrSet<BasicBlock *, 32> liveBlocks;

  // Command-line options for program points.
  SmallBitVector flags;

  // This map associates variables with the set of new variables that have been
  // created for them
  DenseMap<Value *, SmallPtrSet<Instruction *, 4>> versions;

  virtual bool runOnFunction(Function &F);

  // Determines what is the splitting strategy for the variable V
  // and calls the SSIfy functions in order
  //	- split
  //	- rename
  // 	- clean
  void run(Instruction *V);

  void computeLiveness(Instruction *V);
  void computeDominatorTreeLevels();
  void computePostDominatorTreeLevels();

  // Splits live range of the variable V according to the splitting strategy
  // defined as input
  void split(Instruction *V, const DenseSet<ProgramPoint> &Iup,
             const DenseSet<ProgramPoint> &Idown);

  bool dominatesAnyUse(const Instruction *insert_point, const Value *V) const;

  // Renaming function
  // Called after the creation of new variables (split function)
  void rename_initial(Instruction *V);

  // Passes through all instructions in BB to update uses of the variable V to
  // its most recent definition, as well as registering new definitions when it
  // takes place
  void rename(BasicBlock *BB, RenamingStack &stack);

  // Pushes into the stack a new definition of the variable V, that being the
  // instruction inst
  void set_def(RenamingStack &stack, Instruction *inst);

  // Renames uses of the variable V in the instruction inst to its last
  // definition according to the stack of definitions. Note that it pops
  // definitions from the stack until it finds one that is correct, i.e.,
  // dominates the instruction inst
  //
  // from stands for pointer to the predecessor block. It is used when renaming
  // variables inside a PHINode to tell which incoming value should be renamed
  void set_use(RenamingStack &stack, Instruction *inst, BasicBlock *from = 0);

  // Looks at this->versions, which contains all new variables created and what
  // Value were they created for, and determines which ones should be removed
  // for not being useful or simply wrong
  void clean();

  void eraseInstruction(Instruction *version, Instruction *original,
                        SmallPtrSet<Instruction *, 32> *set);

  // Retrieves the iterated dominance frontier of BB
  const SmallVector<BasicBlock *, 32> &getIDF(BasicBlock *BB);
  const SmallVector<BasicBlock *, 32> &getIPDF(BasicBlock *BB);

  void getAnalysisUsage(AnalysisUsage &AU) const;

public:
  // These functions check whether an instruction is of the custom types that we
  // create in this pass. This check is performed by looking at its name
  static bool is_SSIphi(const Instruction *I);
  static bool is_SSIsigma(const Instruction *I);
  static bool is_SSIcopy(const Instruction *I);

  // Checks if I is an actual instruction.
  // Actual instruction is defined as not being created by us, that is, sigma,
  // artificial phi, and copy
  static bool is_actual(const Instruction *I);

  static char ID; // Pass identification, replacement for typeid

  SSIfy()
      : FunctionPass(ID), F(nullptr), DTmap(nullptr), PDTmap(nullptr),
        ForwardIDF(nullptr), ReverseIDF(nullptr), DomLevels(), PDomLevels(),
        IDFmap(), IPDFmap(), liveBlocks(), flags(4, false) {}
};

//  A program point is a pair of one instruction and one region.
//
//  Region can be:  in (entry of block, join point, phi insertion)
//                  self (middle of block, parallel copy insertion)
//                  out (exit of block, branch point, sigma insertion)
//
//  Instructions have different uses depending on the region associated
//       in:   instruction is used only to determine what BasicBlock the
//             program point refers to.
//       self: instruction is the precise insertion point of the parallel
//             copy. Thus, the future copy will be inserted just after the
//             instruction here.
//       out:  instruction is the branch instruction that is followed by
//             the two outgoing edges.
class ProgramPoint {
public:
  typedef enum { In, Self, Out, Undefined } Position;

private:
  Instruction *I;
  Position P;

public:
  explicit ProgramPoint(Instruction *I, Position P);

  Instruction *getInst() const { return I; }
  Position getPosition() const { return P; }

  // Checks if this program point doesn't have a definition of V already
  // We have three cases: sigma, phi, or copy
  // Each one has a different logic
  bool not_definition_of(const Value *V) const;

  // Two ProgramPoints are equal iff they are of the same region type and:
  //     - if they are Undefined, they have to be of the same type (Empty or
  //     Tombstone)
  //     - if they are Self, their instruction should be the same.
  //     - if not, their instructions' parents should be the same.
  bool operator==(const ProgramPoint &o) const;

  inline bool is_join() const;
  inline bool is_branch() const;
  inline bool is_copy() const;
};

// Wrapper class for the stack, in order to store the Value for which this stack
// exists in the first place
class RenamingStack {
private:
  SmallVector<Instruction *, 4> stack;
  Value *V;

public:
  RenamingStack(Value *V);
  inline Value *getValue() const;
  inline void push(Instruction *I);
  inline void pop();
  inline Instruction *peek() const;
  inline bool empty() const;
};
}

#endif /* SSIFY_H_ */
