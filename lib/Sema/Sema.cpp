#include "llracket/Sema/Sema.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/raw_ostream.h"

namespace {
class ProgramCheck : public ASTVisitor {
  bool HasError;
  llvm::StringSet<> ScopedVariables;
  StringMap <ExprTypes> VariableTypes;
  StringMap <Vec*> Vectors;
  DenseMap <const Expr*, ExprTypes> ExpressionTypes;
  DenseMap <const VecRef*, Vec*> VecRefToVec;
  Program *RootProgram;

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
      default: return "Unknown";
    }
  }

  virtual void visit(Program &Node) override {
    setRootProgram(&Node);
    if (Node.getExpr()) {
      Node.getExpr()->accept(*this);

      // just (read) is an error
      if (auto *Prim = llvm::dyn_cast<::Prim>(Node.getExpr())) {
        if (Prim->getOp() == tok::read) {
          llvm::errs() << "Error: Cannot determine type for standalone read expression\n";
          HasError = true;
        }
      }
    }
    else
      HasError = true;
  };

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
    if (!ScopedVariables.count(Node.getName())) { // check that the variable is in scope
      llvm::errs() << "Error: Variable " << Node.getName() << " is not defined\n";
      HasError = true;
      setExprType(&Node, ExprTypes::Unknown);
      return;
    }
    setExprType(&Node, VariableTypes[Node.getName()]);
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
      Vectors[Node.getVarName()] = llvm::dyn_cast<Vec>(Node.getBinding());
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
      
      if (auto *VarVecExpr = llvm::dyn_cast<Var>(VecExpr)) {
        if (Vectors.count(VarVecExpr->getName())) {
          setExprType(&Node, ExprTypes::Integer);
          return;
        } else {
          llvm::errs() << "The requested vector " << VarVecExpr->getName() << " is not defined\n";
          HasError = true;
          setExprType(&Node, ExprTypes::Unknown);
          return;
        }
      } else if (auto *VecVecExpr = llvm::dyn_cast<Vec>(VecExpr)) {
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

  // virtual void visit(VecRef &Node) override {
  //   Expr *VecExpr = Node.getVecExpr();

  //   if (VecExpr) {
  //     VecExpr->accept(*this);

  //     Vec *VectorReferenced = nullptr;
  //     if (auto *VarVecExpr = llvm::dyn_cast<Var>(VecExpr)) {
  //       // this is a var node, get the corresponding var name
  //       if (Vectors.count(VarVecExpr->getName())) {
  //         VectorReferenced = Vectors[VarVecExpr->getName()];
  //       } else {
  //         llvm::errs() << "The requested vector " << VarVecExpr->getName() << " is not defined\n";
  //         HasError = true;
  //         setExprType(&Node, ExprTypes::Unknown);
  //         return;
  //       }
  //     } else if (auto *VecVecExpr = llvm::dyn_cast<Vec>(VecExpr)) {
  //       VectorReferenced = VecVecExpr;
  //     } else if (auto *VecRefVecExpr = llvm::dyn_cast<VecRef>(VecExpr)) {
        
  //     }
  //     } else {
  //       llvm::errs() << "Error: Cannot determine type for vector reference expression\n";
  //       HasError = true;
  //       setExprType(&Node, ExprTypes::Unknown);
  //       return;
  //     }

  //     // check if the index is an integer
  //     Node.getIndex()->accept(*this);
  //     ExprTypes IndexType = getExprType(Node.getIndex());
  //     if (IndexType != ExprTypes::Integer) {
  //       raiseTypeError(Node.getIndex(), ExprTypes::Integer, IndexType);
  //       setExprType(&Node, ExprTypes::Unknown);
  //       return;
  //     }
  //     // check if the index is an Int node
  //     llvm::errs() << "Index type checking successful\n";
  //     auto *IndexInt = llvm::dyn_cast<Int>(Node.getIndex());
  //     int IndexValue = 0;
  //     if (IndexInt) {
  //       IndexInt->getValue().getAsInteger(10, IndexValue);
  //       if (IndexValue < 0 || (size_t) IndexValue >= VectorReferenced->getLength()) {
  //         llvm::errs() << "Error: Index out of bounds for vector reference\n";
  //         HasError = true;
  //         setExprType(&Node, ExprTypes::Unknown);
  //         return;
  //       }
  //     }

  //     Expr *IndexedElem = VectorReferenced->getElements()[IndexValue];
  //     if (IndexedElem) {
  //       setExprType(&Node, getExprType(IndexedElem));
  //     } else {
  //       llvm::errs() << "Error: Cannot determine type for vector reference expression\n";
  //       HasError = true;
  //       setExprType(&Node, ExprTypes::Unknown);
  //       return;
  //     }
  //   }

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
  
    // Check vector expression type
    VecExpr->accept(*this);
    ExprTypes VecExprType = getExprType(VecExpr);
    if (VecExprType != ExprTypes::Vector && VecExprType != ExprTypes::Unknown) {
      raiseTypeError(VecExpr, ExprTypes::Vector, VecExprType);
      setExprType(&Node, ExprTypes::Unknown);
      return;
    }
  
    // Check that index is an integer
    IndexExpr->accept(*this);
    ExprTypes IndexType = getExprType(IndexExpr);
    if (IndexType != ExprTypes::Integer) {
      raiseTypeError(IndexExpr, ExprTypes::Integer, IndexType);
      setExprType(&Node, ExprTypes::Unknown);
      return;
    }
    
    // We require literal indices for static type checking
    auto *IndexInt = llvm::dyn_cast<Int>(IndexExpr);
    if (!IndexInt) {
      llvm::errs() << "Error: Vector index must be a compile-time constant for static type checking\n";
      HasError = true;
      setExprType(&Node, ExprTypes::Unknown);
      return;
    }
    
    // Get the actual index value
    int IndexValue = 0;
    if (IndexInt->getValue().getAsInteger(10, IndexValue)) {
      llvm::errs() << "Error: Invalid integer index in vector reference\n";
      HasError = true;
      setExprType(&Node, ExprTypes::Unknown);
      return;
    }
  
    // Resolve the vector reference
    Vec *ResolvedVector = resolveVectorExpr(VecExpr);
    if (!ResolvedVector) {
      // Error already reported in resolveVectorExpr
      setExprType(&Node, ExprTypes::Unknown);
      return;
    } else {
      // Store the mapping for VecRef to Vec
      VecRefToVec[&Node] = ResolvedVector;
    }
  
    // Check bounds
    if (IndexValue < 0 || (size_t)IndexValue >= ResolvedVector->getLength()) {
      llvm::errs() << "Error: Vector index " << IndexValue << " out of bounds [0," 
                  << ResolvedVector->getLength() - 1 << "]\n";
      HasError = true;
      setExprType(&Node, ExprTypes::Unknown);
      return;
    }
  
    // Get the element and its type
    Expr *Element = ResolvedVector->getElements()[IndexValue];
    if (!Element) {
      llvm::errs() << "Error: Null element in vector\n";
      HasError = true;
      setExprType(&Node, ExprTypes::Unknown);
      return;
    }
  
    // Element->accept(*this); // Ensure element has a type
    ExprTypes ElementType = getExprType(Element);
    setExprType(&Node, ElementType);
  }
  
  // Helper method to resolve a vector expression to a Vec node
  /* Vec* resolveVectorExpr(Expr* VecExpr) {
    if (auto *VarExpr = llvm::dyn_cast<Var>(VecExpr)) {
      if (!Vectors.count(VarExpr->getName())) {
        llvm::errs() << "Error: The requested vector " << VarExpr->getName() 
                    << " is not defined\n";
        HasError = true;
        return nullptr;
      }
      return Vectors[VarExpr->getName()];
    } 
    else if (auto *VecNode = llvm::dyn_cast<Vec>(VecExpr)) {
      return VecNode;
    } 
    else if (auto *VecRefExpr = llvm::dyn_cast<VecRef>(VecExpr)) {
      // Handle nested vector reference
      Expr *InnerVecExpr = VecRefExpr->getVecExpr();
      Expr *InnerIndexExpr = VecRefExpr->getIndex();
      
      // We need the inner vector and index to both be resolvable
      Vec *InnerVector = resolveVectorExpr(InnerVecExpr);
      if (!InnerVector) {
        return nullptr; // Error already reported
      }
      
      auto *InnerIndexInt = llvm::dyn_cast<Int>(InnerIndexExpr);
      if (!InnerIndexInt) {
        llvm::errs() << "Error: Nested vector index must be a compile-time constant\n";
        HasError = true;
        return nullptr;
      }
      
      int InnerIndexValue;
      if (InnerIndexInt->getValue().getAsInteger(10, InnerIndexValue)) {
        llvm::errs() << "Error: Invalid integer index in nested vector reference\n";
        HasError = true;
        return nullptr;
      }
      
      if (InnerIndexValue < 0 || (size_t)InnerIndexValue >= InnerVector->getLength()) {
        llvm::errs() << "Error: Vector index " << InnerIndexValue << " out of bounds\n";
        HasError = true;
        return nullptr;
      }
      
      Expr *Element = InnerVector->getElements()[InnerIndexValue];
      if (!Element) {
        llvm::errs() << "Error: Null element in vector\n";
        HasError = true;
        return nullptr;
      }
      
      // The element must itself be a vector
      auto *VecElement = llvm::dyn_cast<Vec>(Element);
      if (!VecElement) {
        llvm::errs() << "Error: Element at index " << InnerIndexValue 
                    << " is not a vector\n";
        HasError = true;
        return nullptr;
      }
      
      return VecElement;
    } 
    else {
      llvm::errs() << "Error: Cannot determine vector for reference expression\n";
      HasError = true;
      return nullptr;
    }
  } */

  Vec* resolveVectorExpr(Expr* VecExpr) {
    if (auto *VarExpr = llvm::dyn_cast<Var>(VecExpr)) {
      // Case 1: VecExpr is a variable
      if (!Vectors.count(VarExpr->getName())) {
        return nullptr; // Variable not found or not a vector
      }
      return Vectors[VarExpr->getName()];
    } 
    else if (auto *VecNode = llvm::dyn_cast<Vec>(VecExpr)) {
      // Case 2: VecExpr is a direct vector
      return VecNode;
    } 
    else if (auto *VecRefExpr = llvm::dyn_cast<VecRef>(VecExpr)) {
      // Case 3: VecExpr is a vector reference - recursive case
      if (VecRefToVec.count(VecRefExpr)) {
        return VecRefToVec[VecRefExpr]; // Return the mapped Vec
      }
      
      // Resolve inner vector
      Vec* InnerVector = resolveVectorExpr(VecRefExpr->getVecExpr());
      if (!InnerVector) return nullptr;
      
      // Handle constant index only
      auto *InnerIndexInt = llvm::dyn_cast<Int>(VecRefExpr->getIndex());
      if (!InnerIndexInt) return nullptr;
      
      // Get the integer value
      int InnerIndexValue;
      if (InnerIndexInt->getValue().getAsInteger(10, InnerIndexValue)) 
        return nullptr;
      
      // Check bounds
      if (InnerIndexValue < 0 || (size_t)InnerIndexValue >= InnerVector->getLength()) 
        return nullptr;
      
      // Get the element at the index
      Expr *Element = InnerVector->getElements()[InnerIndexValue];
      if (!Element) return nullptr;
      
      // The element must be a vector itself
      auto *VecElement = llvm::dyn_cast<Vec>(Element);
      if (!VecElement) return nullptr;
      
      return VecElement;
    } 
    else {
      // Not a vector expression
      return nullptr;
    }
  }

  virtual void visit(VecSet &Node) override {
    Expr *VecExpr = Node.getVecExpr();
    Expr *IndexExpr = Node.getIndex();
    Expr *ValueExpr = Node.getValue();
    
    // Check if all required expressions are present
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
    
    // Check vector expression type
    VecExpr->accept(*this);
    ExprTypes VecExprType = getExprType(VecExpr);
    if (VecExprType != ExprTypes::Vector && VecExprType != ExprTypes::Unknown) {
      raiseTypeError(VecExpr, ExprTypes::Vector, VecExprType);
      setExprType(&Node, ExprTypes::Unknown);
      return;
    }
    
    // Check index type
    IndexExpr->accept(*this);
    ExprTypes IndexType = getExprType(IndexExpr);
    if (IndexType != ExprTypes::Integer) {
      raiseTypeError(IndexExpr, ExprTypes::Integer, IndexType);
      setExprType(&Node, ExprTypes::Unknown);
      return;
    }
    
    // Process the value expression
    ValueExpr->accept(*this);
    ExprTypes ValueType = getExprType(ValueExpr);
    
    // Try to resolve the vector and check bounds
    Vec *ResolvedVector = resolveVectorExpr(VecExpr);
    
    if (ResolvedVector) {
      // Try to get numeric index value for static bounds checking
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
          
          // Update the type of the element in our tracking data
          Expr *OldElement = ResolvedVector->getElements()[IndexValue];
          if (OldElement) {
            // This is key for correct aliasing: updating the element type directly
            setExprType(OldElement, ValueType);
          }
        }
      }
    }
    
    // vector-set! always returns Void
    setExprType(&Node, ExprTypes::Void);
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
