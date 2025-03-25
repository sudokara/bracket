#include "llracket/CodeGen/CodeGen.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

namespace {
class ToIRVisitor : public ASTVisitor {
  Module *M;
  IRBuilder<> Builder;
  Type *VoidTy;
  Type *Int32Ty;
  Type *BoolTy;
  PointerType *PtrTy;
  Constant *Int32Zero;
  Value *V;
  StringMap<Value *> nameMap;

public:
  ToIRVisitor(Module *M) : M(M), Builder(M->getContext()) {
    VoidTy = Type::getVoidTy(M->getContext());
    Int32Ty = Type::getInt32Ty(M->getContext());
    BoolTy = Type::getInt1Ty(M->getContext()); // i1 is 1 bit integer
    PtrTy = PointerType::getUnqual(M->getContext());
    Int32Zero = ConstantInt::get(Int32Ty, 0, true);
  }

  // something was exlpained here. you should see. 
  void run(AST *Tree) {
    FunctionType *MainFty = FunctionType::get(Int32Ty, {Int32Ty, PtrTy}, false);
    Function *MainFn =
        Function::Create(MainFty, GlobalValue::ExternalLinkage, "main", M);
    BasicBlock *BB = BasicBlock::Create(M->getContext(), "entry", MainFn); // first basic block, called entry
    Builder.SetInsertPoint(BB); // builder inserts everything that is called like CreateCall at this insertion point
    Tree->accept(*this);

    FunctionType *WriteFnTy = FunctionType::get(VoidTy, {Int32Ty}, false);
    Function *WriteFn = Function::Create(
        WriteFnTy, GlobalValue::ExternalLinkage, "write_int", M);
    Builder.CreateCall(WriteFnTy, WriteFn, {V});
    Builder.CreateRet(Int32Zero);
  }

  virtual void visit(Program &Node) override { Node.getExpr()->accept(*this); };

  virtual void visit(Expr &Node) override {
    if (llvm::isa<Prim>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<Int>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<Var>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<Let>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<Bool>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<If>(Node)) {
      Node.accept(*this);
      return;
    }
  };

  // instead of just checking validity it also generates LLVM IR
  virtual void visit(Prim &Node) override {
    // read
    TokenKind Op = Node.getOp();

    if (Op == tok::read) {
      // creating a custom function: see docs and TB Chapter 4
      Function *ReadFn;
      if ((ReadFn = M->getFunction("read_int")) == nullptr) {
        FunctionType *ReadFty = FunctionType::get(Int32Ty, {PtrTy}, false); // returns int32, takes params as pointer
        ReadFn = Function::Create(ReadFty, GlobalValue::ExternalLinkage, // return type, accessibility, name, module
                                  "read_int", M);
      }
      AllocaInst *ReadInput =
          Builder.CreateAlloca(PtrTy, nullptr, "read_input");
      V = Builder.CreateCall(ReadFn, {ReadInput}); // calls the read_int function
      return;
    }

    // -, +
    if (Op == tok::minus) {
      if (Node.getE1() and !Node.getE2()) { // negation
        Node.getE1()->accept(*this);
        V = Builder.CreateNSWNeg(V); // no signed wrap negation
        return;
      }
    }
    if (Op == tok::plus || Op == tok::minus) {
      Node.getE1()->accept(*this);
      Value *E1 = V;
      Node.getE2()->accept(*this);
      Value *E2 = V;
      if (Op == tok::plus) {
        V = Builder.CreateNSWAdd(E1, E2);
      } else {
        V = Builder.CreateNSWSub(E1, E2);
      }
      return;
    }

    // comparators
    if (Op == tok::lt || Op == tok::le || Op == tok::gt || Op == tok::ge || Op == tok::eq) {
      Node.getE1()->accept(*this);
      Value *E1 = V;
      Node.getE2()->accept(*this);
      Value *E2 = V;

      if (Op == tok::eq) {
        V = Builder.CreateICmpEQ(E1, E2, "eq");
        return;
      }

      switch (Op) {
      case tok::lt:
        V = Builder.CreateICmpSLT(E1, E2, "lt");
        break;
      case tok::le:
        V = Builder.CreateICmpSLE(E1, E2, "le");
        break;
      case tok::gt:
        V = Builder.CreateICmpSGT(E1, E2, "gt");
        break;
      case tok::ge:
        V = Builder.CreateICmpSGE(E1, E2, "ge");
        break;
      default:
        break;
      }
      return;
    }

    // logical operators
    if (Op == tok::logical_not) {
      Node.getE1()->accept(*this);
      V = Builder.CreateNot(V);
      return;
    }

    // TODO: short circuiting
    if (Op == tok::logical_and) {
      Node.getE1()->accept(*this);
      Value *E1 = V;
      Node.getE2()->accept(*this);
      Value *E2 = V;
      V = Builder.CreateAnd(E1, E2);
      return;
    }

    // TODO: short circuiting
    if (Op == tok::logical_or) {
      Node.getE1()->accept(*this);
      Value *E1 = V;
      Node.getE2()->accept(*this);
      Value *E2 = V;
      V = Builder.CreateOr(E1, E2);
      return;
    }
  };

  virtual void visit(Int &Node) override {
    int Intval;
    Node.getValue().getAsInteger(10, Intval);
    V = ConstantInt::get(Int32Ty, Intval, true);
  };

  virtual void visit(Bool &Node) override {
    V = ConstantInt::get(BoolTy, Node.getValue() ? 1 : 0);
  };

  virtual void visit(Var &Node) override {
    StringRef varName = Node.getName();
    if (nameMap.find(varName) == nameMap.end()) {
      // variable not found, sema should have caught
      // but ill just assume the variable to be 0 to continue compilation?
      // TODO: how do i do a blocking error?
      llvm::errs() << "Variable not found: " << varName << "\n";
      V = ConstantInt::get(Int32Ty, 0, true);
      return;
    }

    Value *varPtr = nameMap[varName];
    Type *varType = varPtr->getType();

    if (varType->isIntegerTy(1)) {
      V = Builder.CreateLoad(BoolTy, varPtr, varName + "_val");
    }
    else {
      V = Builder.CreateLoad(Int32Ty, varPtr, varName + "_val");
    }
  };

  virtual void visit(Let &Node) override {
    StringRef varName = Node.getVarName();

    // populate the binding expression first
    // so that the variable can be assigned the value
    Node.getBinding()->accept(*this);
    Value *bindingVal = V; // stored in V from the visit of the expression
    Type *bindingType = bindingVal->getType();

    // checking if a binding exists and we have to backup for shadowing
    Value *oldBinding = nullptr;
    if (nameMap.find(varName) != nameMap.end()) {
      oldBinding = nameMap[varName];
    }

    // assign this variable through an alloca
    AllocaInst *variableAlloc = nullptr;
    if (bindingType->isIntegerTy(1)) {
      variableAlloc = Builder.CreateAlloca(BoolTy, nullptr, varName);
    }
    else {
      variableAlloc = Builder.CreateAlloca(Int32Ty, nullptr, varName);
    }
    Builder.CreateStore(bindingVal, variableAlloc);
    nameMap[varName] = variableAlloc;

    // accept the body
    Node.getBody()->accept(*this);

    // restore shadowed value if present
    // else remove the variable since it is no longer in scope
    if (oldBinding) {
      nameMap[varName] = oldBinding;
    } else {
      nameMap.erase(varName);
    }
  };

  virtual void visit(If &Node) override {
    Function *CurrentFunction = Builder.GetInsertBlock()->getParent();
    BasicBlock *ThenBB = BasicBlock::Create(M->getContext(), "then", CurrentFunction);
    BasicBlock *ElseBB = BasicBlock::Create(M->getContext(), "else", CurrentFunction);
    BasicBlock *MergeBB = BasicBlock::Create(M->getContext(), "ifcont", CurrentFunction);

    // conditional break
    Node.getCondition()->accept(*this);
    Value *Cond = V;
    if (Cond->getType() != BoolTy) {
      Cond = Builder.CreateICmpNE(Cond, ConstantInt::get(Cond->getType(), 0), "ifcond");
    }
    Builder.CreateCondBr(Cond, ThenBB, ElseBB);

    // then block
    Builder.SetInsertPoint(ThenBB);
    Node.getThenExpr()->accept(*this);
    Value *ThenV = V;
    Type *ThenTy = ThenV->getType();
    bool isInt = ThenTy->isIntegerTy(32);
    Builder.CreateBr(MergeBB);

    ThenBB = Builder.GetInsertBlock(); // for phi node
    
    // else block
    Builder.SetInsertPoint(ElseBB);
    Node.getElseExpr()->accept(*this);
    Value *ElseV = V;
    Builder.CreateBr(MergeBB);

    ElseBB = Builder.GetInsertBlock(); // for phi node

    // merge with phi
    Builder.SetInsertPoint(MergeBB);

    PHINode *PN = nullptr;
    if (isInt)
      PN = Builder.CreatePHI(Int32Ty, 2, "iftmp");
    else
      PN = Builder.CreatePHI(BoolTy, 2, "iftmp");

    PN->addIncoming(ThenV, ThenBB);
    PN->addIncoming(ElseV, ElseBB);
    V = PN;
  };
};
}; // namespace

void CodeGen::compile(AST *Tree) {
  ToIRVisitor ToIR(M);
  ToIR.run(Tree);
}
