#include "llracket/CodeGen/CodeGen.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
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
  StringMap<Value*> nameMap;
  StringMap<Function*> functionPrototypes;
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

  // something was explained here. you should see. 
  void run(AST *Tree) {
    if (auto* P = dynamic_cast<Program*>(Tree)) {
      RootProgram = P;
      TypeInfo = P->getInfo();

      for (FunctionDef *FD : P->getFunctionDefs()) {
        // build the LLVM function prototype
        std::vector<Type*> params;
        for (auto &PP : FD->getParams()) {
          ParamType *PT = PP.second;
          switch (PT->getKind()) {
            case ParamType::PK_Integer: params.push_back(Int32Ty);      break;
            case ParamType::PK_Boolean: params.push_back(BoolTy);       break;
            case ParamType::PK_Vector:  params.push_back(VectorPtrTy);  break;
            case ParamType::PK_Function: params.push_back(IntPtrTy);    break;
            case ParamType::PK_Void:                                    break;
          }
        }
        ParamType *RPT = FD->getReturnType();
        Type *retTy = nullptr;
        switch (RPT->getKind()) {
          case ParamType::PK_Integer: retTy = Int32Ty;      break;
          case ParamType::PK_Boolean: retTy = BoolTy;       break;
          case ParamType::PK_Vector:  retTy = VectorPtrTy;  break;
          case ParamType::PK_Void:    retTy = VoidTy;       break;
          case ParamType::PK_Function: retTy = IntPtrTy;    break;
          default: retTy = Int32Ty;                         break;
        }
        FunctionType *FT =
          FunctionType::get(retTy, params, false);
        Function *F =
          Function::Create(FT,
                           GlobalValue::ExternalLinkage,
                           FD->getName(),
                           M);
        functionPrototypes[FD->getName()] = F;
      }

      for (FunctionDef *FD : P->getFunctionDefs()) {
        Function *F = functionPrototypes[FD->getName()];
        auto oldNameMap = nameMap;
        nameMap.clear();

        // entry block
        BasicBlock *BB = BasicBlock::Create(M->getContext(),
                                            FD->getName() + "_entry",
                                            F);
        Builder.SetInsertPoint(BB);

        // emit allocas & stores for arguments
        unsigned idx = 0;
        for (auto &PP : FD->getParams()) {
          StringRef name = PP.first;
          Argument *A = F->getArg(idx++);
          A->setName(name);
          if (A->getType() == VectorPtrTy) {
            // vector argument: keep it directly
            nameMap[name] = A;
          } else {
            // scalar argument: allocate and store
            AllocaInst *All = Builder.CreateAlloca(A->getType(), nullptr, name + ".addr");
            Builder.CreateStore(A, All);
            nameMap[name] = All;
          }
        }

        // codegen the function body
        FD->getBody()->accept(*this);
        Value *retVal = V;

        if (FD->getReturnType()->getKind() == ParamType::PK_Function) {
          retVal = Builder.CreatePtrToInt(retVal,
                                          IntPtrTy,
                                          "func2int");
        }

        // emit return
        if (F->getReturnType()->isVoidTy()) {
          Builder.CreateRetVoid();
        } else {
          Builder.CreateRet(retVal);
        }
        llvm::verifyFunction(*F, &errs());

        // restore
        nameMap = oldNameMap;
      }
    }

    FunctionType *MainFty = FunctionType::get(Int32Ty, {Int32Ty, PtrTy}, false);
    Function *MainFn =
        Function::Create(MainFty, GlobalValue::ExternalLinkage, "main", M);
    BasicBlock *BB = BasicBlock::Create(M->getContext(), "entry", MainFn); // first basic block, called entry
    Builder.SetInsertPoint(BB); // builder inserts everything that is called like CreateCall at this insertion point
    Tree->accept(*this);

    ExprTypes resultType = getExprType(dynamic_cast<Program*>(Tree)->getExpr());

    if (resultType == ExprTypes::Void) {
    } 
    else if (resultType == ExprTypes::Bool || V->getType()->isIntegerTy(1)) {
      // boolean (bitwidth 1)
      FunctionType *WriteBoolFnTy = FunctionType::get(VoidTy, {Int32Ty}, false);
      Function *WriteBoolFn = Function::Create(
          WriteBoolFnTy, GlobalValue::ExternalLinkage, "write_bool", M);
      Value *ExtendedV = Builder.CreateZExt(V, Int32Ty, "ext_bool");
      Builder.CreateCall(WriteBoolFn, {ExtendedV});
    } 
    else if (resultType == ExprTypes::Integer || V->getType()->isIntegerTy(32)) {
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
    if (llvm::isa<Apply>(Node)) {
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
      if (NodeType == ExprTypes::Bool) {
        V = Builder.CreateCall(ReadFn, {ConstantInt::get(Int32Ty, 1, true)});
        V = Builder.CreateICmpNE(V, ConstantInt::get(Int32Ty, 0, true), "read_bool");
      } else {
        V = Builder.CreateCall(ReadFn, {ConstantInt::get(Int32Ty, 0, true)});
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
        Node.getE1()->accept(*this);
        Value *E1 = V;
        Node.getE2()->accept(*this);
        Value *E2 = V;
        
        ExprTypes E1Type = getExprType(Node.getE1());
        ExprTypes E2Type = getExprType(Node.getE2());
        
        // compare pointers directly for vectors
        if (E1Type == ExprTypes::Vector || E2Type == ExprTypes::Vector) {
          Value *E1Int = Builder.CreatePtrToInt(E1, IntPtrTy, "ptr_to_int");
          Value *E2Int = Builder.CreatePtrToInt(E2, IntPtrTy, "ptr_to_int");
          V = Builder.CreateICmpEQ(E1Int, E2Int, "ptr_eq");
        } else {
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
    
      ExprTypes E1Type = getExprType(Node.getE1());
      Value *OperandV = V;
      
      if (E1Type == ExprTypes::Bool && !OperandV->getType()->isIntegerTy(1)) {
          OperandV = Builder.CreateICmpNE(OperandV, 
                                        ConstantInt::get(OperandV->getType(), 0), 
                                        "to_bool");
      }
      else if (!OperandV->getType()->isIntegerTy(1)) {
          OperandV = Builder.CreateICmpNE(OperandV, 
                                        ConstantInt::get(OperandV->getType(), 0), 
                                        "to_bool");
      }
      
      V = Builder.CreateNot(OperandV, "logical_not");
      return;
    }

    if (Op == tok::logical_and) {
      Node.getE1()->accept(*this);
      Value *E1 = V;
      Node.getE2()->accept(*this);
      Value *E2 = V;
      V = Builder.CreateAnd(E1, E2);
      return;
    }

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
    
    // check if it's a global function (directly by name)
    auto funcIt = functionPrototypes.find(name);
    if (funcIt != functionPrototypes.end()) {
        // function reference, use the function directly
        V = funcIt->second;
        return;
    }
    
    // not a global function, check local variables
    auto it = nameMap.find(name);
    if (it == nameMap.end()) {
        llvm::errs() << "Error: Variable " << name << " is not defined\n";
        return;
    }
    
    Value *varPtr = it->second;
    ExprTypes T = getExprType(&Node);
    if (T == ExprTypes::Vector) {
        V = varPtr;
        return;
    } else if (T == ExprTypes::Function) {
        if (auto *ArgVal = dyn_cast<Argument>(varPtr)) {
            V = ArgVal;
        } else if (auto *AllocaVal = dyn_cast<AllocaInst>(varPtr)) {
            V = Builder.CreateLoad(PtrTy, AllocaVal, name + "_func");
        } else {
            V = varPtr;
        }
        return;
    }

    if (T == ExprTypes::Bool) {
        V = Builder.CreateLoad(BoolTy, varPtr, name + "_val");
    } else {
        V = Builder.CreateLoad(Int32Ty, varPtr, name + "_val");
    }
  }

  virtual void visit(Let &Node) override { 
    StringRef varName = Node.getVarName();

    Node.getBinding()->accept(*this);
    Value *bindingVal = V;
    Type *bindingType = bindingVal->getType();

    Value *oldBinding = nullptr;
    if (nameMap.find(varName) != nameMap.end()) {
      oldBinding = nameMap[varName];
    }

    ExprTypes bindingExprType = getExprType(Node.getBinding());
    
    if (bindingExprType == ExprTypes::Vector) {
      nameMap[varName] = bindingVal;
    } else {
      AllocaInst *variableAlloc = nullptr;
      if (bindingExprType == ExprTypes::Bool || bindingType->isIntegerTy(1)) {
          variableAlloc = Builder.CreateAlloca(BoolTy, nullptr, varName);
      } else {
          variableAlloc = Builder.CreateAlloca(Int32Ty, nullptr, varName);
      }
      Builder.CreateStore(bindingVal, variableAlloc);
      nameMap[varName] = variableAlloc;
    }

    Node.getBody()->accept(*this);

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
    
    Type *PhiType;
    if (NodeType == ExprTypes::Bool) {
      PhiType = BoolTy;
      
      if (!ThenV->getType()->isIntegerTy(1)) {
        ThenV = Builder.CreateICmpNE(ThenV, ConstantInt::get(ThenV->getType(), 0), "to_bool");
      }
      if (!ElseV->getType()->isIntegerTy(1)) {
        ElseV = Builder.CreateICmpNE(ElseV, ConstantInt::get(ElseV->getType(), 0), "to_bool");
      }
    } else {
      PhiType = Int32Ty;
      
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
    
    if (nameMap.find(varName) == nameMap.end()) {
      llvm::errs() << "Error: Variable not found during codegen: " << varName << "\n";
      V = UndefValue::get(Int32Ty);
      return;
    }
    
    Value *varPtr = nameMap[varName];
    Type *varType = varPtr->getType();
    
    Node.getValue()->accept(*this);
    Value *valueToStore = V;
    Type *valueType = valueToStore->getType();
    
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
    
    for (Expr *E : Exprs) {
      if (E) {
        E->accept(*this);
      }
    }
  }

  virtual void visit(While &Node) override {
    Function *CurrentFunction = Builder.GetInsertBlock()->getParent();
    
    BasicBlock *LoopCondBB = BasicBlock::Create(M->getContext(), "loop.cond", CurrentFunction);
    BasicBlock *LoopBodyBB = BasicBlock::Create(M->getContext(), "loop.body", CurrentFunction);
    BasicBlock *LoopEndBB = BasicBlock::Create(M->getContext(), "loop.end", CurrentFunction);
    
    // branch to loop condition
    Builder.CreateBr(LoopCondBB);
    
    Builder.SetInsertPoint(LoopCondBB);
    Node.getCondition()->accept(*this);
    Value *CondV = V;
    
    if (!CondV->getType()->isIntegerTy(1)) {
      CondV = Builder.CreateICmpNE(CondV, ConstantInt::get(CondV->getType(), 0), "loopcond");
    }
    
    Builder.CreateCondBr(CondV, LoopBodyBB, LoopEndBB);
    
    Builder.SetInsertPoint(LoopBodyBB);
    Node.getBody()->accept(*this);
    Builder.CreateBr(LoopCondBB);
    
    Builder.SetInsertPoint(LoopEndBB);
    
    V = UndefValue::get(Int32Ty);
  }

  virtual void visit(Vec &Node) override {
    auto &Elems = Node.getElements();
    unsigned N = Elems.size();

    Value *len32 = ConstantInt::get(Int32Ty, N);
    Value *lenVal = Builder.CreateZExt(len32, IntPtrTy, "len_ptrsz");


    Function *mallocFn = M->getFunction("malloc");
    if (!mallocFn) {
        FunctionType *mallocTy = FunctionType::get(PtrTy, {IntPtrTy}, false);
        mallocFn = Function::Create(mallocTy,
                                  GlobalValue::ExternalLinkage,
                                  "malloc", M);
    }

    Value *eltSize   = ConstantInt::get(IntPtrTy, sizeof(intptr_t));
    Value *totalSize = Builder.CreateNSWMul(lenVal, eltSize, "vec_bytes");
    Value *rawData   = Builder.CreateCall(mallocFn, { totalSize }, "raw_data");
    Value *dataPtr   = Builder.CreateBitCast(rawData,
                          PointerType::getUnqual(IntPtrTy),
                          "data_ptr");

    for (unsigned i = 0; i < N; ++i) {
        Value *idx   = ConstantInt::get(IntPtrTy, i, true);
        Value *slot  = Builder.CreateGEP(IntPtrTy, dataPtr, idx, "slot");
        
        Expr *E = Node.getElements()[i];
        E->accept(*this);
        Value *v = V;
        
        if (getExprType(E) == ExprTypes::Vector) {
            v = Builder.CreatePtrToInt(v, IntPtrTy, "vec2int");
        } else if (getExprType(E) == ExprTypes::Function) {
        } else {
            if (v->getType()->isIntegerTy(1) ||
                v->getType()->isIntegerTy(32))
              v = Builder.CreateZExt(v, IntPtrTy, "ext_to_ptrsz");
        }
        Builder.CreateStore(v, slot);
    }

    // allocate the vector struct and store length + data pointer
    Value *vecPtr = Builder.CreateAlloca(VectorTy, nullptr, "vec");
    Value *lenFld = Builder.CreateStructGEP(VectorTy, vecPtr, 0, "len_addr");
    Builder.CreateStore(lenVal, lenFld);
    Value *dataFld = Builder.CreateStructGEP(VectorTy, vecPtr, 1, "data_addr");
    Builder.CreateStore(dataPtr, dataFld);

    V = vecPtr;
  }

  virtual void visit(VecLen &Node) override {
    Node.getVecExpr()->accept(*this);
    Value *vecPtr = V;
    
    // was var, get from namemap
    if (auto *VE = dyn_cast<Var>(Node.getVecExpr())) {
      auto it = nameMap.find(VE->getName());
      assert(it != nameMap.end() && "vector var not in map");
      vecPtr = it->second;
    }
    
    // gep to &vecPtr->length (field 0) and load the i32
    Value *lenAddr = Builder.CreateStructGEP(VectorTy, vecPtr, 0, "len_addr");
    V = Builder.CreateLoad(Int32Ty, lenAddr, "vec_len");
  }

  virtual void visit(VecRef &Node) override {
    Node.getVecExpr()->accept(*this);

    Value *vecPtr;
    if (auto *VE = dyn_cast<Var>(Node.getVecExpr())) {
      // vector variable -> was mapped to a Vector* in nameMap
      vecPtr = nameMap[VE->getName()];
    } else {
      // arbitrary expression -> generate code, V must be VectorPtrTy
      Node.getVecExpr()->accept(*this);
      vecPtr = V;
    }

    // gep on a vecPtr
    Value *dataAddr = Builder.CreateStructGEP(VectorTy, vecPtr, 1, "data_addr");
    Value *dataPtr  = Builder.CreateLoad(PointerType::getUnqual(IntPtrTy),
                                        dataAddr, "data_ptr");

    Node.getIndex()->accept(*this);
    Value *idx = V;
    if (!idx->getType()->isIntegerTy(IntPtrTy->getIntegerBitWidth()))
      idx = Builder.CreateZExt(idx, IntPtrTy, "idx_ext");

    Value *slot = Builder.CreateGEP(IntPtrTy, dataPtr, idx, "slot");
    Value *raw  = Builder.CreateLoad(IntPtrTy, slot, "raw_elem");


    if (getExprType(&Node) == ExprTypes::Vector) {
      V = Builder.CreateIntToPtr(raw, VectorPtrTy, "int2vec");
    } else if (getExprType(&Node) == ExprTypes::Bool) {
      V = Builder.CreateTrunc(raw, BoolTy, "raw2bool");
    } else if (getExprType(&Node) == ExprTypes::Function) {
      V = raw;
    } else {
      V = Builder.CreateTrunc(raw, Int32Ty, "raw2int");
    }
  }

  virtual void visit(VecSet &Node) override {
    Node.getVecExpr()->accept(*this);
    Value *vecPtr = V;
    
    Node.getIndex()->accept(*this);
    Value *idx = V;
    if (!idx->getType()->isIntegerTy(32)) {
      idx = Builder.CreateZExt(idx, Int32Ty, "idx_to_i32");
    }
    
    Node.getValue()->accept(*this);
    Value *valueToStore = V;
    
    if (valueToStore->getType()->isIntegerTy(1)) {
      valueToStore = Builder.CreateZExt(valueToStore, Int32Ty, "bool_to_i32");
    }
    
    // access the data pointer within the vector struct
    Value *dataAddr = Builder.CreateStructGEP(VectorTy, vecPtr, 1, "data_addr");
    Value *dataPtr = Builder.CreateLoad(PointerType::getUnqual(IntPtrTy), dataAddr);
    
    // calculate the address of the element to modify (pointer‐sized index)
    if (!idx->getType()->isIntegerTy(IntPtrTy->getIntegerBitWidth()))
      idx = Builder.CreateZExt(idx, IntPtrTy, "idx_ext");
    Value *slot = Builder.CreateGEP(IntPtrTy, dataPtr, idx, "slot");
    
    // extend the stored value to pointer‐size if needed, then store
    if (!valueToStore->getType()->isIntegerTy(IntPtrTy->getIntegerBitWidth()))
      valueToStore = Builder.CreateZExt(valueToStore, IntPtrTy, "ext_to_ptrsz");
    Builder.CreateStore(valueToStore, slot);
    
    V = UndefValue::get(Int32Ty);
  }

  virtual void visit(Apply &Node) override {
    std::vector<Value*> argsV;
    for (Expr *E : Node.getArguments()) {
        E->accept(*this);
        argsV.push_back(V);
    }

    Node.getFunction()->accept(*this);
    Value *funcValue = V;
    
    if (auto *F = dyn_cast<Function>(funcValue)) {
        // the function is already a Function* 
        V = Builder.CreateCall(F, argsV, "calltmp");
    } else {
        // through a pointer
        std::vector<Type*> argTypes(argsV.size(), Int32Ty);
        FunctionType *FT = FunctionType::get(Int32Ty, argTypes, false);
        
        Value *funcPtr = funcValue;
        if (funcPtr->getType()->isPointerTy()) {
            Type *pointeeType = cast<PointerType>(funcPtr->getType());
            if (llvm::isa<FunctionType>(pointeeType)) {
                funcPtr = Builder.CreateBitCast(funcPtr, FT->getPointerTo(), "func_ptr_cast");
            } else {
                funcPtr = Builder.CreateIntToPtr(funcPtr, FT->getPointerTo(), "int2func");
            }
        } else {
            funcPtr = Builder.CreateIntToPtr(funcPtr, FT->getPointerTo(), "int2func");
        }
        
        V = Builder.CreateCall(FT, funcPtr, argsV, "indirect_call");
    }
  }

  virtual void visit(FunctionDef &Node) override {
    Function *F = functionPrototypes[Node.getName()];
    auto oldNameMap = nameMap;
    nameMap.clear();

    // entry block
    BasicBlock *BB = BasicBlock::Create(M->getContext(),
                                        Node.getName() + "_entry",
                                        F);
    Builder.SetInsertPoint(BB);

    // emit allocas & stores for arguments
    unsigned idx = 0;
    for (auto &PP : Node.getParams()) {
        StringRef name = PP.first;
        ParamType *paramType = PP.second;
        Argument *A = F->getArg(idx++);
        A->setName(name);
        
        if (paramType->getKind() == ParamType::PK_Function) {
            // store the function pointer directly
            nameMap[name] = A;
        } else if (A->getType() == VectorPtrTy) {
            // vector argument: keep it directly
            nameMap[name] = A;
        } else {
            // scalar argument: allocate and store
            AllocaInst *All = Builder.CreateAlloca(A->getType(), nullptr, name + ".addr");
            Builder.CreateStore(A, All);
            nameMap[name] = All;
        }
    }

    // codegen the function body
    Node.getBody()->accept(*this);
    Value *retVal = V;
    
    ParamType *returnPT = Node.getReturnType();
    if (returnPT->getKind() == ParamType::PK_Function) {
      if (auto *FVar = llvm::dyn_cast<Function>(V)) {
        V = Builder.CreatePtrToInt(FVar, IntPtrTy, "func2int");
      } else {
        V = Builder.CreatePtrToInt(V, IntPtrTy, "func2int");
      }
    }
    Builder.CreateRet(V);
    
    llvm::verifyFunction(*F, &errs());
    nameMap = oldNameMap;
  }
};
}; // namespace

void CodeGen::compile(AST *Tree) {
  ToIRVisitor ToIR(M);
  ToIR.run(Tree);
}
