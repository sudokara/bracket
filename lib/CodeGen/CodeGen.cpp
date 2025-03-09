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
  PointerType *PtrTy;
  Constant *Int32Zero;
  Value *V;
  StringMap<Value *> nameMap;

public:
  ToIRVisitor(Module *M) : M(M), Builder(M->getContext()) {
    VoidTy = Type::getVoidTy(M->getContext());
    Int32Ty = Type::getInt32Ty(M->getContext());
    PtrTy = PointerType::getUnqual(M->getContext());
    Int32Zero = ConstantInt::get(Int32Ty, 0, true);
  }

  void run(AST *Tree) {
    FunctionType *MainFty = FunctionType::get(Int32Ty, {Int32Ty, PtrTy}, false);
    Function *MainFn =
        Function::Create(MainFty, GlobalValue::ExternalLinkage, "main", M);
    BasicBlock *BB = BasicBlock::Create(M->getContext(), "entry", MainFn);
    Builder.SetInsertPoint(BB);
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
  };

  virtual void visit(Prim &Node) override {
    if (Node.getOp() == tok::read) {
      Function *ReadFn;
      if ((ReadFn = M->getFunction("read_int")) == nullptr) {
        FunctionType *ReadFty = FunctionType::get(Int32Ty, {PtrTy}, false);
        ReadFn = Function::Create(ReadFty, GlobalValue::ExternalLinkage,
                                  "read_int", M);
      }
      AllocaInst *ReadInput =
          Builder.CreateAlloca(PtrTy, nullptr, "read_input");
      V = Builder.CreateCall(ReadFn, {ReadInput});
      return;
    }
    if (Node.getOp() == tok::minus) {
      if (Node.getE1() and !Node.getE2()) {
        Node.getE1()->accept(*this);
        V = Builder.CreateNSWNeg(V);
        return;
      }
    }
    if (Node.getOp() == tok::plus || Node.getOp() == tok::minus) {
      Node.getE1()->accept(*this);
      Value *E1 = V;
      Node.getE2()->accept(*this);
      Value *E2 = V;
      if (Node.getOp() == tok::plus) {
        V = Builder.CreateNSWAdd(E1, E2);
      } else {
        V = Builder.CreateNSWSub(E1, E2);
      }
      return;
    }
  };

  virtual void visit(Int &Node) override {
    int Intval;
    Node.getValue().getAsInteger(10, Intval);
    V = ConstantInt::get(Int32Ty, Intval, true);
  };
};
}; // namespace

void CodeGen::compile(AST *Tree) {
  ToIRVisitor ToIR(M);
  ToIR.run(Tree);
}
