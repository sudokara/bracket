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
  };

  // instead of just checking validity it also generates LLVM IR
  virtual void visit(Prim &Node) override {
    if (Node.getOp() == tok::read) {
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
    if (Node.getOp() == tok::minus) {
      if (Node.getE1() and !Node.getE2()) { // negation
        Node.getE1()->accept(*this);
        V = Builder.CreateNSWNeg(V); // no signed wrap negation
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
    V = Builder.CreateLoad(Int32Ty, varPtr, varName + "_val");
  };

  virtual void visit(Let &Node) override {
    StringRef varName = Node.getVarName();

    // populate the binding expression first
    // so that the variable can be assigned the value
    Node.getBinding()->accept(*this);
    Value *bindingVal = V; // stored in V from the visit of the expression

    // checking if a binding exists and we have to backup for shadowing
    Value *oldBinding = nullptr;
    if (nameMap.find(varName) != nameMap.end()) {
      oldBinding = nameMap[varName];
    }

    // assign this variable through an alloca
    AllocaInst *variableAlloc = Builder.CreateAlloca(Int32Ty, nullptr, varName);
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
};
}; // namespace

void CodeGen::compile(AST *Tree) {
  ToIRVisitor ToIR(M);
  ToIR.run(Tree);
}
