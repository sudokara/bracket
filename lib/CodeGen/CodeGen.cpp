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
  Type *VectorTy;
  Type *VectorPtrTy;
  PointerType *PtrTy;
  Type *IntPtrTy;
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
    VectorTy = StructType::create(M->getContext(), {Int32Ty, PtrTy}, "Vector"); // length and data
    VectorPtrTy = PointerType::getUnqual(VectorTy); // pointer to vector
    Int32Zero = ConstantInt::get(Int32Ty, 0, true);
    IntPtrTy = M->getDataLayout().getIntPtrType(M->getContext());
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

    // if (V->getType()->isIntegerTy(1)) {
    //   // boolean (bitwidth 1)
    //   FunctionType *WriteBoolFnTy = FunctionType::get(VoidTy, {Int32Ty}, false);
    //   Function *WriteBoolFn = Function::Create(
    //       WriteBoolFnTy, GlobalValue::ExternalLinkage, "write_bool", M);
    //   Value *ExtendedV = Builder.CreateZExt(V, Int32Ty, "ext_bool");
    //   Builder.CreateCall(WriteBoolFn, {ExtendedV}); // Remove WriteBoolFnTy
    // } else {
    //   // not bool, so int
    //   FunctionType *WriteIntFnTy = FunctionType::get(VoidTy, {Int32Ty}, false);
    //   Function *WriteIntFn = Function::Create(
    //       WriteIntFnTy, GlobalValue::ExternalLinkage, "write_int", M);
    //   Builder.CreateCall(WriteIntFn, {V}); // Remove WriteIntFnTy
    // }
    ExprTypes resultType = getExprType(dynamic_cast<Program*>(Tree)->getExpr());

    if (resultType == ExprTypes::Void) {
    } 
    else if (V->getType()->isIntegerTy(1)) {
      // boolean (bitwidth 1)
      FunctionType *WriteBoolFnTy = FunctionType::get(VoidTy, {Int32Ty}, false);
      Function *WriteBoolFn = Function::Create(
          WriteBoolFnTy, GlobalValue::ExternalLinkage, "write_bool", M);
      Value *ExtendedV = Builder.CreateZExt(V, Int32Ty, "ext_bool");
      Builder.CreateCall(WriteBoolFn, {ExtendedV});
    } 
    else if (V->getType()->isIntegerTy(32)) {
      // integer (int32)
      FunctionType *WriteIntFnTy = FunctionType::get(VoidTy, {Int32Ty}, false);
      Function *WriteIntFn = Function::Create(
          WriteIntFnTy, GlobalValue::ExternalLinkage, "write_int", M);
      Builder.CreateCall(WriteIntFn, {V});
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
    if (llvm::isa<Set>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<Begin>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<While>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<Void>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<Vec>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<VecRef>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<VecLen>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<VecSet>(Node)) {
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
        // V = Builder.CreateICmpEQ(E1, E2, "eq");
        // return;

        Node.getE1()->accept(*this);
        Value *E1 = V;
        Node.getE2()->accept(*this);
        Value *E2 = V;
        
        ExprTypes E1Type = getExprType(Node.getE1());
        ExprTypes E2Type = getExprType(Node.getE2());
        
        // Special case for vectors - compare pointers directly
        if (E1Type == ExprTypes::Vector || E2Type == ExprTypes::Vector) {
          // Convert pointers to integers for comparison
          Value *E1Int = Builder.CreatePtrToInt(E1, IntPtrTy, "ptr_to_int");
          Value *E2Int = Builder.CreatePtrToInt(E2, IntPtrTy, "ptr_to_int");
          V = Builder.CreateICmpEQ(E1Int, E2Int, "ptr_eq");
        } else {
          // Regular eq? for non-vector types
          V = Builder.CreateICmpEQ(E1, E2, "eq");
        }
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
    auto name = Node.getName();
    auto it = nameMap.find(name);
    if (it == nameMap.end()) {
      llvm::errs() << "Error: Variable " << name << " is not defined\n";
      
      return;
    }
    Value *varPtr = it->second;      // this is an AllocaInst*

    ExprTypes T = getExprType(&Node);
    if (T == ExprTypes::Vector) {
      // vector variables are already pointers to VectorTy on the stack:
      V = varPtr;
      return;
    }

    // fall back to old behavior for ints/bools
    if (T == ExprTypes::Bool) {
      V = Builder.CreateLoad(BoolTy, varPtr, name + "_val");
    } else {
      V = Builder.CreateLoad(Int32Ty, varPtr, name + "_val");
    }
  }

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
    if (bindingExprType == ExprTypes::Vector) {
      nameMap[varName] = bindingVal;
    } else {
      AllocaInst *variableAlloc = nullptr;
      if (bindingExprType == ExprTypes::Bool || bindingType->isIntegerTy(1)) {
          // Always use BoolTy for boolean expressions
          variableAlloc = Builder.CreateAlloca(BoolTy, nullptr, varName);
          
          // If the binding value is not already a boolean, convert it
          // if (!bindingType->isIntegerTy(1)) {
          //     bindingVal = Builder.CreateICmpNE(bindingVal, 
          //                                      ConstantInt::get(bindingType, 0), 
          //                                      "to_bool");
          // }
      } else {
          variableAlloc = Builder.CreateAlloca(Int32Ty, nullptr, varName);
          
          // If the binding value is a boolean, convert it to integer
          // if (bindingType->isIntegerTy(1)) {
          //     bindingVal = Builder.CreateZExt(bindingVal, Int32Ty, "bool_to_int");
          // }
      }
      Builder.CreateStore(bindingVal, variableAlloc);
      nameMap[varName] = variableAlloc;
    }

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

  virtual void visit(Void &Node) override {
    V = UndefValue::get(Int32Ty);
  }

  virtual void visit(Set &Node) override {
    StringRef varName = Node.getVarName();
    
    // check if variable exists in the scope
    if (nameMap.find(varName) == nameMap.end()) {
      llvm::errs() << "Variable not found during codegen: " << varName << "\n";
      V = UndefValue::get(Int32Ty);
      return;
    }
    
    // Get the allocated location for the variable
    Value *varPtr = nameMap[varName];
    Type *varType = varPtr->getType();
    
    // eval the expression to store
    Node.getValue()->accept(*this);
    Value *valueToStore = V;
    Type *valueType = valueToStore->getType();
    
    // type conversion if necessary
    // if (varType->isIntegerTy(1) && valueType->isIntegerTy(32)) {
    //   // convert int to bool for storing
    //   valueToStore = Builder.CreateICmpNE(
    //       valueToStore, ConstantInt::get(Int32Ty, 0), "int_to_bool");
    // } 
    // else if (varType->isIntegerTy(32) && valueType->isIntegerTy(1)) {
    //   // convert bool to int for storing
    //   valueToStore = Builder.CreateZExt(valueToStore, Int32Ty, "bool_to_int");
    // }
    
    Builder.CreateStore(valueToStore, varPtr);
    
    V = UndefValue::get(Int32Ty);
  }

  virtual void visit(Begin &Node) override {
    const std::vector<Expr*> &Exprs = Node.getExprs();
    
    if (Exprs.empty()) {
      // empty begin should have been caught in semantic analysis
      V = UndefValue::get(Int32Ty);
      return;
    }
    
    // evaluate each expression in sequence
    for (Expr *E : Exprs) {
      if (E) {
        E->accept(*this);
      }
    }
    // the last expression already updated the value of V
  }

  virtual void visit(While &Node) override {
    Function *CurrentFunction = Builder.GetInsertBlock()->getParent();
    
    // basic blocks
    BasicBlock *LoopCondBB = BasicBlock::Create(M->getContext(), "loop.cond", CurrentFunction);
    BasicBlock *LoopBodyBB = BasicBlock::Create(M->getContext(), "loop.body", CurrentFunction);
    BasicBlock *LoopEndBB = BasicBlock::Create(M->getContext(), "loop.end", CurrentFunction);
    
    // branch to loop condition
    Builder.CreateBr(LoopCondBB);
    
    // emit the condition code
    Builder.SetInsertPoint(LoopCondBB);
    Node.getCondition()->accept(*this);
    Value *CondV = V;
    
    // ensure the condition is a boolean
    if (!CondV->getType()->isIntegerTy(1)) {
      CondV = Builder.CreateICmpNE(CondV, ConstantInt::get(CondV->getType(), 0), "loopcond");
    }
    
    // create conditional branch based on the condition
    Builder.CreateCondBr(CondV, LoopBodyBB, LoopEndBB);
    
    // loop body code
    Builder.SetInsertPoint(LoopBodyBB);
    Node.getBody()->accept(*this);
    Builder.CreateBr(LoopCondBB);  // loop back to condition
    
    // continue after the loop
    Builder.SetInsertPoint(LoopEndBB);
    
    V = UndefValue::get(Int32Ty);
  }

  virtual void visit(Vec &Node) override {
    auto &Elems = Node.getElements();
    unsigned N = Elems.size();

    // 1) build constant length
    Value *len32 = ConstantInt::get(Int32Ty, N);
    Value *lenVal = Builder.CreateZExt(len32, IntPtrTy, "len_ptrsz");


    // 2) get or declare malloc
    Function *mallocFn = M->getFunction("malloc");
    if (!mallocFn) {
      FunctionType *mallocTy = FunctionType::get(PtrTy, {IntPtrTy}, false);
      mallocFn = Function::Create(mallocTy,
                                  GlobalValue::ExternalLinkage,
                                  "malloc", M);
    }

    // 3) allocate raw data buffer
    Value *eltSize   = ConstantInt::get(IntPtrTy, sizeof(intptr_t));
    Value *totalSize = Builder.CreateNSWMul(lenVal, eltSize, "vec_bytes");
    Value *rawData   = Builder.CreateCall(mallocFn, { totalSize }, "raw_data");
    Value *dataPtr   = Builder.CreateBitCast(rawData,
                          PointerType::getUnqual(IntPtrTy),
                          "data_ptr");

    // 4) fill in each element
    // for (unsigned i = 0; i < N; ++i) {
    //   Elems[i]->accept(*this);
    //   Value *E = V;
      
    //   // Check if this element is a vector
    //   ExprTypes ElemType = getExprType(Elems[i]);
      
    //   // If it's a vector, we need to store its pointer as an integer
    //   if (ElemType == ExprTypes::Vector) {
    //     // Convert pointer to integer for storage
    //     E = Builder.CreatePtrToInt(E, IntPtrTy, "vec_ptr_to_int");
    //   }
    //   // If it's a bool, convert to i32
    //   else if (E->getType()->isIntegerTy(1)) {
    //     E = Builder.CreateZExt(E, Int32Ty, "bool_to_i32");
    //   }
      
    //   Value *idx = ConstantInt::get(Int32Ty, i);
    //   Value *gep = Builder.CreateGEP(Int32Ty, dataPtr, idx, "elt_ptr");
    //   Builder.CreateStore(E, gep);
    // }
    for (unsigned i = 0; i < N; ++i) {
      // compute slot address
      Value *idx   = ConstantInt::get(IntPtrTy, i, true);
      Value *slot  = Builder.CreateGEP(IntPtrTy, dataPtr, idx, "slot");
      // evaluate element
      Expr *E = Node.getElements()[i];
      E->accept(*this);
      Value *v = V;
      if (getExprType(E) == ExprTypes::Vector) {
        v = Builder.CreatePtrToInt(v, IntPtrTy, "vec2int");
      } else {
        if (v->getType()->isIntegerTy(1) ||
            v->getType()->isIntegerTy(32))
          v = Builder.CreateZExt(v, IntPtrTy, "ext_to_ptrsz");
      }
      Builder.CreateStore(v, slot);
    }

    // 5) allocate the Vector struct and store length + data pointer
    Value *vecPtr = Builder.CreateAlloca(VectorTy, nullptr, "vec");
    Value *lenFld = Builder.CreateStructGEP(VectorTy, vecPtr, 0, "len_addr");
    Builder.CreateStore(lenVal, lenFld);
    Value *dataFld = Builder.CreateStructGEP(VectorTy, vecPtr, 1, "data_addr");
    Builder.CreateStore(dataPtr, dataFld);

    // 6) the result value is the pointer to the vector struct
    V = vecPtr;
  }

  virtual void visit(VecLen &Node) override {
    // 1) evaluate the sub‐expression to get a Vector*
    Node.getVecExpr()->accept(*this);
    Value *vecPtr = V;            // V is already a Vector*
    
    // 2) if it was a Var, grab the alloca from nameMap
    if (auto *VE = dyn_cast<Var>(Node.getVecExpr())) {
      auto it = nameMap.find(VE->getName());
      assert(it != nameMap.end() && "vector var not in map");
      vecPtr = it->second;        // this is a Vector* alloca
    }
    
    // 3) GEP to &vecPtr->length (field 0) and load the i32
    Value *lenAddr = Builder.CreateStructGEP(VectorTy, vecPtr, 0, "len_addr");
    V = Builder.CreateLoad(Int32Ty, lenAddr, "vec_len");
  }

  virtual void visit(VecRef &Node) override {
    // 1) Get the vector expression
    Node.getVecExpr()->accept(*this);
    Value *vecPtr = V;
    
    // // 2) Get the index
    // Node.getIndex()->accept(*this);
    // Value *idx = V;
    // // Ensure index is i32
    // if (!idx->getType()->isIntegerTy(32)) {
    //   idx = Builder.CreateZExt(idx, Int32Ty, "idx_to_i32");
    // }
    
    // // 3) Load the data pointer from the vector
    // Value *dataAddr = Builder.CreateStructGEP(VectorTy, vecPtr, 1, "data_addr");
    // Value *dataPtr = Builder.CreateLoad(PointerType::getUnqual(Int32Ty), dataAddr);
    
    // // 4) Calculate the address of the element
    // Value *elemPtr = Builder.CreateGEP(Int32Ty, dataPtr, idx, "elem_ptr");
    
    // // 5) Load the element value
    // Value *elemVal = Builder.CreateLoad(Int32Ty, elemPtr, "elem_val");
    
    // // 6) Handle the element value based on its type
    // ExprTypes ElementType = getExprType(&Node);
    
    // if (ElementType == ExprTypes::Vector) {
    //   // If this is a vector reference that returns a vector, 
    //   // we need to convert the integer back to a vector pointer
    //   V = Builder.CreateIntToPtr(elemVal, VectorPtrTy, "int_to_vec_ptr");
    // } else if (ElementType == ExprTypes::Bool) {
    //   // For boolean, convert to i1
    //   V = Builder.CreateICmpNE(elemVal, ConstantInt::get(Int32Ty, 0), "to_bool");
    // } else {
    //   // For other types (like integers), use the value directly
    //   V = elemVal;
    // }


    // load dataPtr
    Value *dataFld = Builder.CreateStructGEP(VectorTy, vecPtr, 1, "data_addr");
    Value *dataPtr = Builder.CreateLoad(PointerType::getUnqual(IntPtrTy),
                                        dataFld, "data_ptr");
    // compute index
    Node.getIndex()->accept(*this);
    Value *idx = V;
    if (!idx->getType()->isIntegerTy(IntPtrTy->getIntegerBitWidth()))
      idx = Builder.CreateZExt(idx, IntPtrTy, "idx_ext");
    // element slot pointer
    Value *slot = Builder.CreateGEP(IntPtrTy, dataPtr, idx, "slot");
    // load raw IntPtrTy
    Value *raw = Builder.CreateLoad(IntPtrTy, slot, "raw_elem");
    // dispatch by semantic result type
    if (getExprType(&Node) == ExprTypes::Vector) {
      V = Builder.CreateIntToPtr(raw, VectorPtrTy, "int2vec");
    } else if (getExprType(&Node) == ExprTypes::Bool) {
      V = Builder.CreateTrunc(raw, BoolTy, "raw2bool");
    } else {
      V = Builder.CreateTrunc(raw, Int32Ty, "raw2int");
    }
  }

  virtual void visit(VecSet &Node) override {
    // 1) Evaluate the vector expression to get the vector pointer
    Node.getVecExpr()->accept(*this);
    Value *vecPtr = V;
    
    // 2) Evaluate the index expression to get the index
    Node.getIndex()->accept(*this);
    Value *idx = V;
    // Ensure index is i32
    if (!idx->getType()->isIntegerTy(32)) {
      idx = Builder.CreateZExt(idx, Int32Ty, "idx_to_i32");
    }
    
    // 3) Evaluate the value to be stored
    Node.getValue()->accept(*this);
    Value *valueToStore = V;
    
    // Convert boolean value to i32 if needed (since vectors store i32)
    if (valueToStore->getType()->isIntegerTy(1)) {
      valueToStore = Builder.CreateZExt(valueToStore, Int32Ty, "bool_to_i32");
    }
    
    // 4) Access the data pointer within the vector struct
    Value *dataAddr = Builder.CreateStructGEP(VectorTy, vecPtr, 1, "data_addr");
    Value *dataPtr = Builder.CreateLoad(PointerType::getUnqual(IntPtrTy), dataAddr);
    
    // 5) Calculate the address of the element to modify (pointer‐sized index)
    if (!idx->getType()->isIntegerTy(IntPtrTy->getIntegerBitWidth()))
      idx = Builder.CreateZExt(idx, IntPtrTy, "idx_ext");
    Value *slot = Builder.CreateGEP(IntPtrTy, dataPtr, idx, "slot");
    
    // 6) Extend the stored value to pointer‐size if needed, then store
    if (!valueToStore->getType()->isIntegerTy(IntPtrTy->getIntegerBitWidth()))
      valueToStore = Builder.CreateZExt(valueToStore, IntPtrTy, "ext_to_ptrsz");
    Builder.CreateStore(valueToStore, slot);
    
    // 7) VecSet returns void
    V = UndefValue::get(Int32Ty);
  }
};
}; // namespace

void CodeGen::compile(AST *Tree) {
  ToIRVisitor ToIR(M);
  ToIR.run(Tree);
}
