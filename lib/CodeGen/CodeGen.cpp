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
  ProgramInfo TypeInfo;
  Program* RootProgram;

public:
  ToIRVisitor(Module *M) : M(M), Builder(M->getContext()), RootProgram(nullptr) {
    VoidTy = Type::getVoidTy(M->getContext());
    Int32Ty = Type::getInt32Ty(M->getContext());
    BoolTy = Type::getInt1Ty(M->getContext()); // i1 is 1 bit integer
    PtrTy = PointerType::getUnqual(M->getContext());
    Int32Zero = ConstantInt::get(Int32Ty, 0, true);
  }

  ExprTypes getExprType(const Expr* E) {
    if (!RootProgram) return ExprTypes::Unknown;
    
    std::string Key = "type_" + std::to_string(reinterpret_cast<uintptr_t>(E));
    
    if (TypeInfo.count(Key)) {
      try {
        int TypeValue = std::any_cast<int>(TypeInfo[Key]);
        return static_cast<ExprTypes>(TypeValue);
      } catch (const std::bad_any_cast&) {
        return ExprTypes::Unknown;
      }
    }
    
    return ExprTypes::Unknown;
  }

  // something was exlpained here. you should see. 
  void run(AST *Tree) {
    if (auto* P = dynamic_cast<Program*>(Tree)) {
      RootProgram = P;
      TypeInfo = P->getInfo();
    }

    FunctionType *MainFty = FunctionType::get(Int32Ty, {Int32Ty, PtrTy}, false);
    Function *MainFn =
        Function::Create(MainFty, GlobalValue::ExternalLinkage, "main", M);
    BasicBlock *BB = BasicBlock::Create(M->getContext(), "entry", MainFn); // first basic block, called entry
    Builder.SetInsertPoint(BB); // builder inserts everything that is called like CreateCall at this insertion point
    Tree->accept(*this);

    // FunctionType *WriteFnTy = FunctionType::get(VoidTy, {Int32Ty}, false);
    // Function *WriteFn = Function::Create(
    //     WriteFnTy, GlobalValue::ExternalLinkage, "write_int", M);
    // Builder.CreateCall(WriteFnTy, WriteFn, {V});

    if (V->getType()->isIntegerTy(1)) {
      // boolean (bitwidth 1)
      FunctionType *WriteBoolFnTy = FunctionType::get(VoidTy, {Int32Ty}, false);
      Function *WriteBoolFn = Function::Create(
          WriteBoolFnTy, GlobalValue::ExternalLinkage, "write_bool", M);
      Value *ExtendedV = Builder.CreateZExt(V, Int32Ty, "ext_bool");
      Builder.CreateCall(WriteBoolFn, {ExtendedV}); // Remove WriteBoolFnTy
    } else {
      // not bool, so int
      FunctionType *WriteIntFnTy = FunctionType::get(VoidTy, {Int32Ty}, false);
      Function *WriteIntFn = Function::Create(
          WriteIntFnTy, GlobalValue::ExternalLinkage, "write_int", M);
      Builder.CreateCall(WriteIntFn, {V}); // Remove WriteIntFnTy
    }

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
      ExprTypes NodeType = getExprType(&Node);

      Function *ReadFn;
      if ((ReadFn = M->getFunction("read_value")) == nullptr) {
        FunctionType *ReadFty = FunctionType::get(Int32Ty, {Int32Ty}, false); // returns int32, takes params as pointer
        ReadFn = Function::Create(ReadFty, GlobalValue::ExternalLinkage, // return type, accessibility, name, module
                                  "read_value", M);
      }
      // Value *IntTypeParam = ConstantInt::get(Int32Ty, 0, true);
      // V = Builder.CreateCall(ReadFn, {IntTypeParam}); // calls read_value(0) for integer reading
      if (NodeType == ExprTypes::Bool) {
        V = Builder.CreateCall(ReadFn, {ConstantInt::get(Int32Ty, 1, true)}); // calls read_value(1) for boolean reading
        // convert int32 to bool
        V = Builder.CreateICmpNE(V, ConstantInt::get(Int32Ty, 0, true), "read_bool");
      } else {
        V = Builder.CreateCall(ReadFn, {ConstantInt::get(Int32Ty, 0, true)}); // calls read_value(0) for integer reading
      }
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
    
      // Get the semantic type of the expression
      ExprTypes E1Type = getExprType(Node.getE1());
      Value *OperandV = V;
      
      // If it's semantically a boolean, ensure the LLVM type is i1
      if (E1Type == ExprTypes::Bool && !OperandV->getType()->isIntegerTy(1)) {
          OperandV = Builder.CreateICmpNE(OperandV, 
                                        ConstantInt::get(OperandV->getType(), 0), 
                                        "to_bool");
      }
      // If it's not a boolean at LLVM level, force conversion
      else if (!OperandV->getType()->isIntegerTy(1)) {
          OperandV = Builder.CreateICmpNE(OperandV, 
                                        ConstantInt::get(OperandV->getType(), 0), 
                                        "to_bool");
      }
      
      V = Builder.CreateNot(OperandV, "logical_not");
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

    ExprTypes bindingExprType = getExprType(Node.getBinding());
    
    // assign this variable through an alloca
    AllocaInst *variableAlloc = nullptr;
    if (bindingExprType == ExprTypes::Bool || bindingType->isIntegerTy(1)) {
        // Always use BoolTy for boolean expressions
        variableAlloc = Builder.CreateAlloca(BoolTy, nullptr, varName);
        
        // If the binding value is not already a boolean, convert it
        if (!bindingType->isIntegerTy(1)) {
            bindingVal = Builder.CreateICmpNE(bindingVal, 
                                             ConstantInt::get(bindingType, 0), 
                                             "to_bool");
        }
    } else {
        variableAlloc = Builder.CreateAlloca(Int32Ty, nullptr, varName);
        
        // If the binding value is a boolean, convert it to integer
        if (bindingType->isIntegerTy(1)) {
            bindingVal = Builder.CreateZExt(bindingVal, Int32Ty, "bool_to_int");
        }
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
    Builder.CreateBr(MergeBB);
    ThenBB = Builder.GetInsertBlock(); // for phi node
    
    // else block
    Builder.SetInsertPoint(ElseBB);
    Node.getElseExpr()->accept(*this);
    Value *ElseV = V;
    Type *ElseTy = ElseV->getType();
    Builder.CreateBr(MergeBB);
    ElseBB = Builder.GetInsertBlock(); // for phi node

    // merge with phi
    Builder.SetInsertPoint(MergeBB);

    ExprTypes NodeType = getExprType(&Node);
    
    // Choose the proper type for the PHI node based on the semantic analysis
    Type *PhiType;
    if (NodeType == ExprTypes::Bool) {
      // If semantic analysis determined this is a Bool expression
      PhiType = BoolTy;
      
      // Convert values if needed
      if (!ThenV->getType()->isIntegerTy(1)) {
        ThenV = Builder.CreateICmpNE(ThenV, ConstantInt::get(ThenV->getType(), 0), "to_bool");
      }
      if (!ElseV->getType()->isIntegerTy(1)) {
        ElseV = Builder.CreateICmpNE(ElseV, ConstantInt::get(ElseV->getType(), 0), "to_bool");
      }
    } else {
      // If semantic analysis determined this is an Integer expression
      PhiType = Int32Ty;
      
      // Convert values if needed
      if (ThenV->getType()->isIntegerTy(1)) {
        ThenV = Builder.CreateZExt(ThenV, Int32Ty, "bool_to_int");
      }
      if (ElseV->getType()->isIntegerTy(1)) {
        ElseV = Builder.CreateZExt(ElseV, Int32Ty, "bool_to_int");
      }
    }
    
    PHINode *PN = Builder.CreatePHI(PhiType, 2, "iftmp");
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
