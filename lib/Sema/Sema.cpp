#include "llracket/Sema/Sema.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/raw_ostream.h"

namespace {
struct FunctionInfo {
  std::vector<ExprTypes> paramTypes;
  ExprTypes returnType;
  Vec* returnVec = nullptr;
};
class ProgramCheck : public ASTVisitor {
  bool HasError;
  llvm::StringSet<> ScopedVariables;
  StringMap <ExprTypes> VariableTypes;
  StringMap <Vec*> Vectors;
  DenseMap <const Expr*, ExprTypes> ExpressionTypes;
  DenseMap <const VecRef*, Vec*> VecRefToVec;
  StringMap<FunctionInfo> FunctionTable;
  Program *RootProgram;

  ExprTypes convertParamTypeToExprType(ParamType *T) {
    if (!T) return ExprTypes::Unknown;
    switch (T->getKind()) {
      case ParamType::PK_Integer: return ExprTypes::Integer;
      case ParamType::PK_Boolean: return ExprTypes::Bool;
      case ParamType::PK_Void: return ExprTypes::Void;
      case ParamType::PK_Vector: return ExprTypes::Vector;
      case ParamType::PK_Function: return ExprTypes::Function;
      default: return ExprTypes::Unknown;
    }
  }

public:
  ProgramCheck() : HasError(false), RootProgram(nullptr) {}

  void setRootProgram(Program *Root) {
    RootProgram = Root;
  }

  void saveExpressionTypes() {
    if (!RootProgram) {
      llvm::errs() << "Error: Root program not set when trying to save expression types!\n";
      return;
    }

    ProgramInfo Info = RootProgram->getInfo();
    for (const auto &Pair : ExpressionTypes) {
      const Expr *Node = Pair.first;
      ExprTypes Type = Pair.second;

      std::string Key = "type_" + std::to_string(reinterpret_cast<uintptr_t>(Node));
      Info[Key] = static_cast<int>(Type);
    }

    RootProgram->setInfo(Info);
  }

  bool hasError() { return HasError; }

  void setExprType(const Expr *Node, ExprTypes Type) {
    ExpressionTypes[Node] = Type;
  }

  ExprTypes getExprType(const Expr *Node) {
    if (ExpressionTypes.count(Node))
      return ExpressionTypes[Node];
    return ExprTypes::Unknown;
  }

  void raiseTypeError(const Expr *Node, ExprTypes Expected, ExprTypes Found) {
    llvm::errs()  << "Error: Expected " << getTypeString(Expected)
                  << " but found " << getTypeString(Found)
                  << " in expression\n";
    HasError = true;
  }

  std::string getTypeString(ExprTypes T) {
    switch (T) {
      case ExprTypes::Integer: return "Integer";
      case ExprTypes::Bool: return "Boolean";
      case ExprTypes::Void: return "Void";
      case ExprTypes::Vector: return "Vector";
      case ExprTypes::Function: return "Function";
      default: return "Unknown";
    }
  }

  virtual void visit(Program &Node) override {
    setRootProgram(&Node);

    for (FunctionDef *FD : Node.getFunctionDefs()) {
      if (FunctionTable.count(FD->getName())) {
        llvm::errs() << "Error: Redefinition of function " << FD->getName() << "\n";
        HasError = true;
        return;
      }
      std::vector<ExprTypes> paramTypes;
      for (auto &P : FD->getParams())
        paramTypes.push_back(convertParamTypeToExprType(P.second));
      ExprTypes retTy = convertParamTypeToExprType(FD->getReturnType());
      FunctionTable[FD->getName()] = {paramTypes, retTy};
    }

    for (FunctionDef *Def : Node.getFunctionDefs()) {
      Def->accept(*this);
    }

    if (Node.getExpr()) {
      Node.getExpr()->accept(*this);
      if (auto *Prim = llvm::dyn_cast<::Prim>(Node.getExpr())) {
        if (Prim->getOp() == tok::read) {
          llvm::errs() << "Error: Cannot determine type for standalone read\n";
          HasError = true;
        }
      }
    } else {
      HasError = true;
    }
  }

  virtual void visit(Expr &Node) override {
    // uses the classof method to check the type of the node
    if (llvm::isa<Prim>(Node)) { // expr is a primitive
      Node.accept(*this);
      return;
    }
    if (llvm::isa<Int>(Node)) { // expr is an integer
      Node.accept(*this);
      return;
    }
    if (llvm::isa<Var>(Node)) { // expr is a variable
      Node.accept(*this);
      return;
    }
    if (llvm::isa<Let>(Node)) { // expr is a let expression
      Node.accept(*this);
      return;
    }
    if (llvm::isa<Bool>(Node)) { // expr is a boolean
      Node.accept(*this);
      return;
    }
    if (llvm::isa<If>(Node)) { // expr is an if expression
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
    HasError = true;
  }

  virtual void visit(Prim &Node) override {
    auto Op = Node.getOp();

    if (Op == tok::read) {
      setExprType(&Node, ExprTypes::Unknown);
      return;
    }

    if (Op == tok::minus) {
      if (Node.getE1() and !Node.getE2()) {
        Node.getE1()->accept(*this);
        ExprTypes E1Type = getExprType(Node.getE1());

        auto *E1Prim = llvm::dyn_cast<::Prim>(Node.getE1());
        bool E1IsRead = E1Prim && E1Prim->getOp() == tok::read;
        
        if (E1IsRead) {
          // Since unary minus requires an integer, infer the read as integer
          setExprType(E1Prim, ExprTypes::Integer);
          E1Type = ExprTypes::Integer;
        }

        if (E1Type != ExprTypes::Integer) {
          raiseTypeError(&Node, ExprTypes::Integer, E1Type);
          setExprType(&Node, ExprTypes::Unknown);
        }

        else {
          setExprType(&Node, ExprTypes::Integer);
        }

        return;
      }
    }

    if (Op == tok::plus || (Op == tok::minus && Node.getE2())) {
      Node.getE1()->accept(*this);
      Node.getE2()->accept(*this);

      ExprTypes E1Type = getExprType(Node.getE1()), E2Type = getExprType(Node.getE2());

      auto *E1Prim = llvm::dyn_cast<::Prim>(Node.getE1()), *E2Prim = llvm::dyn_cast<::Prim>(Node.getE2());

      bool E1isread = E1Prim && E1Prim->getOp() == tok::read;
      bool E2isread = E2Prim && E2Prim->getOp() == tok::read;

      if (E1isread && E2isread) {
        setExprType(E1Prim, ExprTypes::Integer);
        setExprType(E2Prim, ExprTypes::Integer);
        E1Type = E2Type = ExprTypes::Integer;
      }
      else if (E1isread) {
        // E1 is read, set its type to E2's type
        setExprType(E1Prim, E2Type);
        E1Type = E2Type;
      }
      else if (E2isread) {
        // E2 is read, set its type to E1's type
        setExprType(E2Prim, E1Type);
        E2Type = E1Type;
      }
      if (E1Type == ExprTypes::Unknown || E2Type == ExprTypes::Unknown) {
        llvm::errs() << "Error: Cannot determine type for read expression\n";
        HasError = true;
        setExprType(&Node, ExprTypes::Unknown);
        return;
      }

      if (E1Type != ExprTypes::Integer) {
        raiseTypeError(&Node, ExprTypes::Integer, E1Type);
        setExprType(&Node, ExprTypes::Unknown);
        return;
      }

      if (E2Type != ExprTypes::Integer) {
        raiseTypeError(&Node, ExprTypes::Integer, E2Type);
        setExprType(&Node, ExprTypes::Unknown);
        return;
      }

      setExprType(&Node, ExprTypes::Integer);
      return;
    }

    if (Op == tok::le || Op == tok::lt || Op == tok::ge || Op == tok::gt) {
      Node.getE1()->accept(*this);
      Node.getE2()->accept(*this);

      ExprTypes E1Type = getExprType(Node.getE1()), E2Type = getExprType(Node.getE2());

      auto *E1Prim = llvm::dyn_cast<::Prim>(Node.getE1()), *E2Prim = llvm::dyn_cast<::Prim>(Node.getE2());

      bool E1isread = E1Prim && E1Prim->getOp() == tok::read;
      bool E2isread = E2Prim && E2Prim->getOp() == tok::read;

      if (E1isread && E2isread) {
        // both are read expressions, we cannot determine the type
        setExprType(E1Prim, ExprTypes::Integer);
        setExprType(E2Prim, ExprTypes::Integer);
        E1Type = E2Type = ExprTypes::Integer;
      }
      else if (E1isread) {
        // E1 is read, set its type to E2's type
        setExprType(E1Prim, E2Type);
        E1Type = E2Type;
      }
      else if (E2isread) {
        // E2 is read, set its type to E1's type
        setExprType(E2Prim, E1Type);
        E2Type = E1Type;
      }
      if (E1Type == ExprTypes::Unknown || E2Type == ExprTypes::Unknown) {
        llvm::errs() << "Error: Cannot determine type for read expression\n";
        HasError = true;
        setExprType(&Node, ExprTypes::Unknown);
        return;
      }

      if (E1Type != ExprTypes::Integer) {
        raiseTypeError(&Node, ExprTypes::Integer, E1Type);
        setExprType(&Node, ExprTypes::Unknown);
        return;
      }

      if (E2Type != ExprTypes::Integer) {
        raiseTypeError(&Node, ExprTypes::Integer, E2Type);
        setExprType(&Node, ExprTypes::Unknown);
        return;
      }

      setExprType(&Node, ExprTypes::Bool);
      return;
    }

    if (Op == tok::eq) {
      Node.getE1()->accept(*this);
      Node.getE2()->accept(*this);

      ExprTypes E1Type = getExprType(Node.getE1()), E2Type = getExprType(Node.getE2());

      auto *E1Prim = llvm::dyn_cast<::Prim>(Node.getE1()), *E2Prim = llvm::dyn_cast<::Prim>(Node.getE2());

      bool E1isread = E1Prim && E1Prim->getOp() == tok::read;
      bool E2isread = E2Prim && E2Prim->getOp() == tok::read;

      if (E1isread && E2isread) {
        // both are read expressions, we cannot determine the type
        llvm::errs() << "Error: Cannot determine type for read expression\n";
        HasError = true;
        setExprType(&Node, ExprTypes::Unknown);
        return;
      }
      else if (E1isread) {
        // E1 is read, set its type to E2's type
        setExprType(E1Prim, E2Type);
        E1Type = E2Type;
      }
      else if (E2isread) {
        // E2 is read, set its type to E1's type
        setExprType(E2Prim, E1Type);
        E2Type = E1Type;
      }

      if (E1Type == ExprTypes::Unknown || E2Type == ExprTypes::Unknown) {
        llvm::errs() << "Error: Cannot determine type for expressions\n";
        HasError = true;
        setExprType(&Node, ExprTypes::Unknown);
        return;
      }

      // vector equality is also handled here
      if (E1Type != E2Type) {
        raiseTypeError(&Node, E1Type, E2Type);
        setExprType(&Node, ExprTypes::Unknown);
        return;
      }

      setExprType(&Node, ExprTypes::Bool);
      return;
    }

    if (Op == tok::logical_or || Op == tok::logical_and) {
      Node.getE1()->accept(*this);
      Node.getE2()->accept(*this);

      ExprTypes E1Type = getExprType(Node.getE1()), E2Type = getExprType(Node.getE2());

      auto *E1Prim = llvm::dyn_cast<::Prim>(Node.getE1()), *E2Prim = llvm::dyn_cast<::Prim>(Node.getE2());

      bool E1isread = E1Prim && E1Prim->getOp() == tok::read;
      bool E2isread = E2Prim && E2Prim->getOp() == tok::read;

      if (E1isread && E2isread) {
        setExprType(E1Prim, ExprTypes::Bool);
        setExprType(E2Prim, ExprTypes::Bool);
        E1Type = E2Type = ExprTypes::Bool;
      }
      else if (E1isread) {
        // E1 is read, set its type to E2's type
        setExprType(E1Prim, E2Type);
        E1Type = E2Type;
      }
      else if (E2isread) {
        // E2 is read, set its type to E1's type
        setExprType(E2Prim, E1Type);
        E2Type = E1Type;
      }
      if (E1Type == ExprTypes::Unknown || E2Type == ExprTypes::Unknown) {
        llvm::errs() << "Error: Cannot determine type for read expression\n";
        HasError = true;
        setExprType(&Node, ExprTypes::Unknown);
        return;
      }

      if (E1Type != ExprTypes::Bool) {
        raiseTypeError(&Node, ExprTypes::Bool, E1Type);
        setExprType(&Node, ExprTypes::Unknown);
        return;
      }

      if (E2Type != ExprTypes::Bool) {
        raiseTypeError(&Node, ExprTypes::Bool, E2Type);
        setExprType(&Node, ExprTypes::Unknown);
        return;
      }

      setExprType(&Node, ExprTypes::Bool);
      return;
    }

    if (Op == tok::logical_not) {
      Node.getE1()->accept(*this);

      ExprTypes E1Type = getExprType(Node.getE1());

      auto *E1Prim = llvm::dyn_cast<::Prim>(Node.getE1());
      bool E1IsRead = E1Prim && E1Prim->getOp() == tok::read;
      
      if (E1IsRead) {
        setExprType(E1Prim, ExprTypes::Bool);
        E1Type = ExprTypes::Bool;
      }

      if (E1Type != ExprTypes::Bool) {
        raiseTypeError(&Node, ExprTypes::Bool, E1Type);
        setExprType(&Node, ExprTypes::Unknown);
        return;
      }

      setExprType(&Node, ExprTypes::Bool);
      return;
    }

    setExprType(&Node, ExprTypes::Unknown);
    HasError = true;
  }

  virtual void visit(Int &Node) override {
    setExprType(&Node, ExprTypes::Integer);
  }

  virtual void visit(Bool &Node) override {
    setExprType(&Node, ExprTypes::Bool);
  }

  virtual void visit(Var &Node) override {
    // if parser attached a ParamType (i.e. function parameter), use that
    if (auto *PT = Node.getType()) {
      setExprType(&Node, convertParamTypeToExprType(PT));
      return;
    }

    // else if it’s a let‐bound local, use your VariableTypes map
    if (ScopedVariables.count(Node.getName())) {
      setExprType(&Node, VariableTypes[Node.getName()]);
      return;
    }

    // else if it's a known function name
    if (FunctionTable.count(Node.getName())) {
      setExprType(&Node, ExprTypes::Function);
      return;
    }

    // otherwise undefined
    llvm::errs() << "Error: Undefined variable " << Node.getName() << "\n";
    HasError = true;
    setExprType(&Node, ExprTypes::Unknown);
  }

  virtual void visit(Let &Node) override {
    if (Node.getBinding()) {
      // direct read
      Node.getBinding()->accept(*this); // accept the binding expression first, no errors in assignment
      
      if (auto *Prim = llvm::dyn_cast<::Prim> (Node.getBinding())) {
        if (Prim->getOp() == tok::read) {
          setExprType(Prim, ExprTypes::Integer);
          setExprType(&Node, ExprTypes::Integer);
        }
      }
    }

    ExprTypes BindingType = getExprType(Node.getBinding()), oldType = ExprTypes::Unknown;
    Vec *oldVector = nullptr;
    bool shadowed = ScopedVariables.count(Node.getVarName()); // true if a variable of same name was defined previously
    if (shadowed) {
      oldType = VariableTypes[Node.getVarName()]; // store the old type if the variable is shadowed
      oldVector = Vectors[Node.getVarName()]; // store the old vector if the variable is shadowed
    }

    ScopedVariables.insert(Node.getVarName()); // add the variable to the scope
    if (BindingType == ExprTypes::Vector) {
      // direct literal
      if (auto *VL = llvm::dyn_cast<Vec>(Node.getBinding())) {
        Vectors[Node.getVarName()] = VL;
      }

      // forwarding a vector‐var
      else if (auto *VV = llvm::dyn_cast<Var>(Node.getBinding())) {
        auto it = Vectors.find(VV->getName());
        if (it != Vectors.end())
          Vectors[Node.getVarName()] = it->second;
      }
    }
    VariableTypes[Node.getVarName()] = BindingType; // set the type of the variable

    if (Node.getBody())
      Node.getBody()->accept(*this); // accept the body expression

    setExprType(&Node, getExprType(Node.getBody())); // set the type of the let expression to the type of the body expression

    // if this variable does not shadow one with the same name
    // remove this from the scope after the body expression is checked
    if (!shadowed) {
      ScopedVariables.erase(Node.getVarName()); // remove the variable from the scope 
      if (BindingType == ExprTypes::Vector) {
        Vectors.erase(Node.getVarName());
      }
      VariableTypes.erase(Node.getVarName()); // remove the type of the variable
    } else {
      VariableTypes[Node.getVarName()] = oldType; // restore the old type if the variable was shadowed
      Vectors[Node.getVarName()] = oldVector; // restore the old vector if the variable was shadowed
    }
  }

  virtual void visit(If &Node) override {
    if (Node.getCondition())
      Node.getCondition()->accept(*this); // accept the condition expression first

    // check if read() in condition
    auto *CondPrim = llvm::dyn_cast<::Prim>(Node.getCondition());
    ExprTypes CondType = getExprType(Node.getCondition());
    
    // if condition is read(), infer it as boolean
    if (CondPrim && CondPrim->getOp() == tok::read) {
      setExprType(CondPrim, ExprTypes::Bool);
      CondType = ExprTypes::Bool;
    }

    CondType = getExprType(Node.getCondition());
    if (CondType == ExprTypes::Unknown) {
      if (auto *VecRefExpr = llvm::dyn_cast<VecRef>(Node.getCondition())) {
        setExprType(VecRefExpr, ExprTypes::Bool);
        CondType = ExprTypes::Bool;
      }
    }
    if (CondType != ExprTypes::Bool) {
      raiseTypeError(&Node, ExprTypes::Bool, CondType);
      return;
    }

    if (Node.getThenExpr())
      Node.getThenExpr()->accept(*this); // accept the then expression

    if (Node.getElseExpr())
      Node.getElseExpr()->accept(*this); // accept the else expression

    ExprTypes ThenType = getExprType(Node.getThenExpr()), ElseType = getExprType(Node.getElseExpr());

    // Handle cases with read expressions in the branches
    if (ThenType == ExprTypes::Unknown && ElseType != ExprTypes::Unknown) {
      // If one branch is read and the other has a known type, infer the read type
      auto *ThenPrim = llvm::dyn_cast<::Prim>(Node.getThenExpr());
      if (ThenPrim && ThenPrim->getOp() == tok::read) {
        setExprType(ThenPrim, ElseType);
        ThenType = ElseType;
      }
    } 
    else if (ThenType != ExprTypes::Unknown && ElseType == ExprTypes::Unknown) {
      auto *ElsePrim = llvm::dyn_cast<::Prim>(Node.getElseExpr());
      if (ElsePrim && ElsePrim->getOp() == tok::read) {
        setExprType(ElsePrim, ThenType);
        ElseType = ThenType;
      }
    }
    else if (ThenType == ExprTypes::Unknown && ElseType == ExprTypes::Unknown) {
      // Both branches are reads - this is ambiguous, so error
      auto *ThenPrim = llvm::dyn_cast<::Prim>(Node.getThenExpr());
      auto *ElsePrim = llvm::dyn_cast<::Prim>(Node.getElseExpr());
      if (ThenPrim && ThenPrim->getOp() == tok::read && 
          ElsePrim && ElsePrim->getOp() == tok::read) {
        llvm::errs() << "Error: Cannot determine type, both unknown\n";
        HasError = true;
        setExprType(&Node, ExprTypes::Unknown);
        return;
      }
    }

    if (ThenType == ExprTypes::Unknown && ElseType == ExprTypes::Bool) {
      if (auto *ThenPrim = llvm::dyn_cast<Prim>(Node.getThenExpr()))
          if (ThenPrim->getOp() == tok::read) {
            setExprType(ThenPrim, ExprTypes::Bool);
            ThenType = ExprTypes::Bool;
          }
    } else if (ElseType == ExprTypes::Unknown && ThenType == ExprTypes::Bool) {
        if (auto *ElsePrim = llvm::dyn_cast<Prim>(Node.getElseExpr()))
            if (ElsePrim->getOp() == tok::read) {
              setExprType(ElsePrim, ExprTypes::Bool);
              ElseType = ExprTypes::Bool;
            }
    }

    if (ThenType != ElseType) {
      raiseTypeError(&Node, ThenType, ElseType);
      setExprType(&Node, ExprTypes::Unknown);
      return;
    }
    
    setExprType(&Node, ThenType); // set the type of the if expression to the type of the then expression
  }

  virtual void visit(Void &Node) override {
    setExprType(&Node, ExprTypes::Void);
  }

  virtual void visit(Set &Node) override {
    // scoping
    if (!ScopedVariables.count(Node.getVarName())) {
      llvm::errs() << "Error: Variable " << Node.getVarName() << " is not defined\n";
      HasError = true;
      setExprType(&Node, ExprTypes::Unknown);
      return;
    }
    
    if (Node.getValue()) {
      Node.getValue()->accept(*this);
      
      ExprTypes ValueType = getExprType(Node.getValue());
      ExprTypes VarType = VariableTypes[Node.getVarName()];
      
      // handling for read()
      auto *ValuePrim = llvm::dyn_cast<::Prim>(Node.getValue());
      if (ValuePrim && ValuePrim->getOp() == tok::read) {
        setExprType(ValuePrim, VarType);
        ValueType = VarType;
      }
      
      // typechecking
      if (ValueType != VarType && ValueType != ExprTypes::Unknown) {
        raiseTypeError(&Node, VarType, ValueType);
        setExprType(&Node, ExprTypes::Unknown);
        return;
      }
    }
    
    // set! always returns Void
    setExprType(&Node, ExprTypes::Void);
  }

  virtual void visit(Begin &Node) override {
    ExprTypes LastType = ExprTypes::Void;  // Default in case of empty list
    
    const std::vector<Expr*> &Exprs = Node.getExprs();
    
    if (Exprs.empty()) {
      // empty begin is technically an error
      llvm::errs() << "Error: Empty begin expression\n";
      HasError = true;
      setExprType(&Node, ExprTypes::Unknown);
      return;
    }
    
    for (Expr *E : Exprs) {
      if (E) {
        E->accept(*this);
        LastType = getExprType(E);
      }
    }
    
    setExprType(&Node, LastType);
  }

  virtual void visit(While &Node) override {
    if (Node.getCondition()) {
      Node.getCondition()->accept(*this);
      
      ExprTypes CondType = getExprType(Node.getCondition());
      
      // read in condition
      auto *CondPrim = llvm::dyn_cast<::Prim>(Node.getCondition());
      if (CondPrim && CondPrim->getOp() == tok::read) {
        setExprType(CondPrim, ExprTypes::Bool);
        CondType = ExprTypes::Bool;
      }
      
      // condition must be boolean
      if (CondType != ExprTypes::Bool) {
        raiseTypeError(Node.getCondition(), ExprTypes::Bool, CondType);
        setExprType(&Node, ExprTypes::Unknown);
        return;
      }
    }
    
    if (Node.getBody()) {
      Node.getBody()->accept(*this);
    }
    
    setExprType(&Node, ExprTypes::Void);
  }

  virtual void visit(Vec &Node) override {
    for (Expr *E : Node.getElements()) {
      if (E) {
        E->accept(*this);
      }
    }

    setExprType(&Node, ExprTypes::Vector);
  }

  virtual void visit(VecLen &Node) override {
    Expr *VecExpr = Node.getVecExpr();

    if (VecExpr) {
      VecExpr->accept(*this);

      // if the sub‐expression was inferred to be a vector (e.g. (empty)), accept it
      if (getExprType(VecExpr) == ExprTypes::Vector) {
        setExprType(&Node, ExprTypes::Integer);
        return;
      }
      
      // case 1: VecExpr is a variable
      // here we can access the vector using the variable name
      if (auto *VarVecExpr = llvm::dyn_cast<Var>(VecExpr)) {
        if (Vectors.count(VarVecExpr->getName())) {
          setExprType(&Node, ExprTypes::Integer);
          return;
        } else {
          llvm::errs() << "Error: The requested vector " << VarVecExpr->getName() << " is not defined\n";
          HasError = true;
          setExprType(&Node, ExprTypes::Unknown);
          return;
        }
      } else if (auto *VecVecExpr = llvm::dyn_cast<Vec>(VecExpr)) {
        // case 2: VecExpr is a direct vector
        // here we just check type validity using dynamic casting
        setExprType(&Node, ExprTypes::Integer);
        return;
      } else {
        llvm::errs() << "Error: Cannot determine type for vector length expression\n";
        HasError = true;
        setExprType(&Node, ExprTypes::Unknown);
        return;
      }
    } else {
      setExprType(&Node, ExprTypes::Unknown);
    }
  }

  virtual void visit(VecRef &Node) override {
    Expr *VecExpr = Node.getVecExpr();
    Expr *IndexExpr = Node.getIndex();
  
    if (!VecExpr) {
      llvm::errs() << "Error: Missing vector expression in vector reference\n";
      HasError = true;
      setExprType(&Node, ExprTypes::Unknown);
      return;
    }
  
    if (!IndexExpr) {
      llvm::errs() << "Error: Missing index in vector reference\n";
      HasError = true;
      setExprType(&Node, ExprTypes::Unknown);
      return;
    }

    if (auto *VarVec = llvm::dyn_cast<Var>(VecExpr)) {
      ParamType *ParamTy = VarVec->getType();
      if (ParamTy && llvm::isa<VectorParamType>(ParamTy)) {
        auto *VecPT = llvm::cast<VectorParamType>(ParamTy);
        // ensure index is a compile-time Int
        IndexExpr->accept(*this);
        if (auto *IdxInt = llvm::dyn_cast<Int>(IndexExpr)) {
          long i;
          if (!IdxInt->getValue().getAsInteger(10, i) &&
              i >= 0 &&
              (size_t)i < VecPT->getElementTypes().size()) {
            // assign the element’s declared type
            ExprTypes et = convertParamTypeToExprType(
                              VecPT->getElementTypes()[i]);
            setExprType(&Node, et);
            return;
          }
        }
        llvm::errs() << "Error: Invalid or out-of-bounds index for vector parameter\n";
        HasError = true;
        setExprType(&Node, ExprTypes::Unknown);
        return;
      }
    }
  
    // vector expression type check
    VecExpr->accept(*this);
    ExprTypes VecExprType = getExprType(VecExpr);
    if (VecExprType != ExprTypes::Vector && VecExprType != ExprTypes::Unknown) {
      raiseTypeError(VecExpr, ExprTypes::Vector, VecExprType);
      setExprType(&Node, ExprTypes::Unknown);
      return;
    }
  
    // ensuring index is an int
    IndexExpr->accept(*this);
    ExprTypes IndexType = getExprType(IndexExpr);
    if (IndexType != ExprTypes::Integer) {
      raiseTypeError(IndexExpr, ExprTypes::Integer, IndexType);
      setExprType(&Node, ExprTypes::Unknown);
      return;
    }
    
    // dynamic typecasting to int literal
    // this is done to make sure that we have actual integers and not read/expressions to be evaluated
    // this is from the clarification by the TA
    auto *IndexInt = llvm::dyn_cast<Int>(IndexExpr);
    if (!IndexInt) {
      llvm::errs() << "Error: Vector index must be a compile-time constant for static type checking\n";
      HasError = true;
      setExprType(&Node, ExprTypes::Unknown);
      return;
    }
    
    int IndexValue = 0;
    if (IndexInt->getValue().getAsInteger(10, IndexValue)) {
      llvm::errs() << "Error: Invalid integer index in vector reference\n";
      HasError = true;
      setExprType(&Node, ExprTypes::Unknown);
      return;
    }

    // resolve the Vec AST that this VecRef indexes
    Vec *ResolvedVector = resolveVectorExpr(VecExpr);
    if (!ResolvedVector) {
      setExprType(&Node, ExprTypes::Unknown);
      return;
    }

    // check index within bounds
    if (IndexValue < 0 || (size_t)IndexValue >= ResolvedVector->getLength()) {
      llvm::errs() << "Error: Index out of bounds for vector reference\n";
      HasError = true;
      setExprType(&Node, ExprTypes::Unknown);
      return;
    }

    // get the element at compile‐time index
    Expr *Element = ResolvedVector->getElements()[IndexValue];
    if (!Element) {
      // shouldn't happen
      setExprType(&Node, ExprTypes::Unknown);
      return;
    }

    // if that element is itself a Vec, map this VecRef -> that Vec AST
    if (auto *ElemVec = llvm::dyn_cast<Vec>(Element))
      VecRefToVec[&Node] = ElemVec;

    // propagate the element's type
    setExprType(&Node, getExprType(Element));
  }

  Vec* resolveVectorExpr(Expr* VecExpr) {
    // first: if it's an Apply of a known function that returns a literal Vec
    if (auto *AP = llvm::dyn_cast<Apply>(VecExpr)) {
      if (auto *FV = llvm::dyn_cast<Var>(AP->getFunction())) {
        auto I = FunctionTable.find(FV->getName());
        if (I != FunctionTable.end() && I->second.returnVec)
          return I->second.returnVec;
      }
    }

    if (auto *VarExpr = llvm::dyn_cast<Var>(VecExpr)) {
      // case 1: VecExpr is a variable
      if (!Vectors.count(VarExpr->getName())) {
        return nullptr;
      }
      return Vectors[VarExpr->getName()];
    } 
    else if (auto *VecNode = llvm::dyn_cast<Vec>(VecExpr)) {
      // case 2: VecExpr is a direct vector
      return VecNode;
    } 
    else if (auto *VecRefExpr = llvm::dyn_cast<VecRef>(VecExpr)) {
      // case 3: VecExpr is a vector reference - recursive case
      if (VecRefToVec.count(VecRefExpr)) {
        return VecRefToVec[VecRefExpr];
      }
      
      // inner vector resolution
      Vec* InnerVector = resolveVectorExpr(VecRefExpr->getVecExpr());
      if (!InnerVector) return nullptr;
      
      // ensuring int literal and not read/expressions to be evaluated
      auto *InnerIndexInt = llvm::dyn_cast<Int>(VecRefExpr->getIndex());
      if (!InnerIndexInt) return nullptr;
      
      int InnerIndexValue;
      if (InnerIndexInt->getValue().getAsInteger(10, InnerIndexValue)) 
        return nullptr;
      
      if (InnerIndexValue < 0 || (size_t)InnerIndexValue >= InnerVector->getLength()) 
        return nullptr;
      
      Expr *Element = InnerVector->getElements()[InnerIndexValue];
      if (!Element) return nullptr;
      
      auto *VecElement = llvm::dyn_cast<Vec>(Element);
      if (!VecElement) return nullptr;
      
      return VecElement;
    } 
    else {
      // not a vector expression
      return nullptr;
    }
  }

  virtual void visit(VecSet &Node) override {
    Expr *VecExpr = Node.getVecExpr();
    Expr *IndexExpr = Node.getIndex();
    Expr *ValueExpr = Node.getValue();
    
    if (!VecExpr) {
      llvm::errs() << "Error: Missing vector expression in vector-set!\n";
      HasError = true;
      setExprType(&Node, ExprTypes::Unknown);
      return;
    }
    
    if (!IndexExpr) {
      llvm::errs() << "Error: Missing index in vector-set!\n";
      HasError = true;
      setExprType(&Node, ExprTypes::Unknown);
      return;
    }
    
    if (!ValueExpr) {
      llvm::errs() << "Error: Missing value in vector-set!\n";
      HasError = true;
      setExprType(&Node, ExprTypes::Unknown);
      return;
    }
    
    VecExpr->accept(*this);
    ExprTypes VecExprType = getExprType(VecExpr);
    if (VecExprType != ExprTypes::Vector && VecExprType != ExprTypes::Unknown) {
      raiseTypeError(VecExpr, ExprTypes::Vector, VecExprType);
      setExprType(&Node, ExprTypes::Unknown);
      return;
    }
    
    IndexExpr->accept(*this);
    ExprTypes IndexType = getExprType(IndexExpr);
    if (IndexType != ExprTypes::Integer) {
      raiseTypeError(IndexExpr, ExprTypes::Integer, IndexType);
      setExprType(&Node, ExprTypes::Unknown);
      return;
    }
    
    ValueExpr->accept(*this);
    ExprTypes ValueType = getExprType(ValueExpr);
    
    Vec *ResolvedVector = resolveVectorExpr(VecExpr);
    
    if (ResolvedVector) {
      // static index value for bounds checking
      auto *IndexInt = llvm::dyn_cast<Int>(IndexExpr);
      if (IndexInt) {
        int IndexValue;
        if (!IndexInt->getValue().getAsInteger(10, IndexValue)) {
          if (IndexValue < 0 || (size_t)IndexValue >= ResolvedVector->getLength()) {
            llvm::errs() << "Error: Index out of bounds for vector-set!\n";
            HasError = true;
            setExprType(&Node, ExprTypes::Unknown);
            return;
          }
          
          // update elem type
          Expr *OldElement = ResolvedVector->getElements()[IndexValue];
          if (OldElement) {
            setExprType(OldElement, ValueType);
          }
        }
      }
    }
    
    // vector-set! always returns Void
    setExprType(&Node, ExprTypes::Void);
  }

  virtual void visit(FunctionDef &Node) override {
    std::vector<ExprTypes> paramTypes;
    for (auto &P : Node.getParams())
      paramTypes.push_back(convertParamTypeToExprType(P.second));
    ExprTypes returnType = convertParamTypeToExprType(Node.getReturnType());

    // stash a stub in the table (returnVec defaults to nullptr)
    FunctionTable[Node.getName()] = { paramTypes, returnType, nullptr };

    // if this function literally returns a Vec AST, record it
    if (returnType == ExprTypes::Vector) {
      if (auto *VB = llvm::dyn_cast<Vec>(Node.getBody()))
        FunctionTable[Node.getName()].returnVec = VB;
    }

    llvm::StringSet<> oldScoped = ScopedVariables;
    StringMap<ExprTypes> oldVariableTypes = VariableTypes;
    ScopedVariables.clear();
    VariableTypes.clear();

    for (auto &Param : Node.getParams()) {
      StringRef name = Param.first;
      ExprTypes type = convertParamTypeToExprType(Param.second);
      ScopedVariables.insert(name);
      VariableTypes[name] = type;
    }

    Node.getBody()->accept(*this);
    ExprTypes bodyType = getExprType(Node.getBody());

    if (bodyType != returnType) {
      llvm::errs() << "Error: Function " << Node.getName() << " return type mismatch\n";
      llvm::errs() << "Expected: " << getTypeString(returnType) << ", got: " << getTypeString(bodyType) << "\n";
      HasError = true;
    }

    ScopedVariables = oldScoped;
    VariableTypes = oldVariableTypes;
  }

  virtual void visit(Apply &Node) override {
    Node.getFunction()->accept(*this);
    ExprTypes funcType = getExprType(Node.getFunction());
    if (funcType != ExprTypes::Function) {
      llvm::errs() << "Error: Applying non-function type " << getTypeString(funcType) << "\n";
      HasError = true;
      setExprType(&Node, ExprTypes::Unknown);
      return;
    }

    if (auto *funcVar = llvm::dyn_cast<Var>(Node.getFunction())) {
      if (auto *PT = funcVar->getType()) {
        if (PT->getKind() == ParamType::PK_Function) {
          auto *FPT = llvm::dyn_cast<FunctionParamType>(PT);
          const auto &expected = FPT->getParamTypes();
          auto *retPT = FPT->getReturnType();

          if (Node.getArguments().size() != expected.size()) {
            llvm::errs() << "Error: Argument count mismatch in function variable "
                         << funcVar->getName() << "\n";
            HasError = true;
            setExprType(&Node, ExprTypes::Unknown);
            return;
          }

          for (size_t i = 0, e = expected.size(); i != e; ++i) {
            Expr *arg = Node.getArguments()[i];
            arg->accept(*this);
            ExprTypes found = getExprType(arg);
            ExprTypes want  = convertParamTypeToExprType(expected[i]);
            if (found != want) {
              raiseTypeError(arg, want, found);
              HasError = true;
            }
          }

          setExprType(&Node, convertParamTypeToExprType(retPT));
          return;
        }
      }
    }

    if (Var *funcVar = llvm::dyn_cast<Var>(Node.getFunction())) {
      StringRef funcName = funcVar->getName();
      auto it = FunctionTable.find(funcName);
      if (it != FunctionTable.end()) {
        const FunctionInfo &info = it->second;
        const std::vector<Expr*> &args = Node.getArguments();
        if (args.size() != info.paramTypes.size()) {
          llvm::errs() << "Error: Function " << funcName << " argument count mismatch\n";
          HasError = true;
          setExprType(&Node, ExprTypes::Unknown);
          return;
        }

        for (size_t i = 0; i < args.size(); ++i) {
          args[i]->accept(*this);
          ExprTypes argType = getExprType(args[i]);
          if (argType != info.paramTypes[i]) {
            llvm::errs() << "Error: Argument " << (i+1) << " type mismatch in function " << funcName << "\n";
            HasError = true;
          }
        }

        setExprType(&Node, info.returnType);
        return;
      }
    }

    setExprType(&Node, ExprTypes::Unknown);
  }

};
} // namespace

bool Sema::semantic(AST *Tree) {
  if (!Tree)
    return false;
  ProgramCheck Check;
  Tree->accept(Check);
  Check.saveExpressionTypes();
  return !Check.hasError();
}
