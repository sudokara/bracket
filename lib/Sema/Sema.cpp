#include "llracket/Sema/Sema.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/raw_ostream.h"

namespace {
class ProgramCheck : public ASTVisitor {
  bool HasError;
  llvm::StringSet<> ScopedVariables;
  StringMap <ExprTypes> VariableTypes;
  DenseMap <const Expr*, ExprTypes> ExpressionTypes;

public:
  ProgramCheck() : HasError(false) {}

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
      default: return "Unknown";
    }
  }

  virtual void visit(Program &Node) override {
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
        llvm::errs() << "Error: Cannot determine type for read expression\n";
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
    bool shadowed = ScopedVariables.count(Node.getVarName()); // true if a variable of same name was defined previously
    if (shadowed)
      oldType = VariableTypes[Node.getVarName()]; // store the old type if the variable is shadowed

    ScopedVariables.insert(Node.getVarName()); // add the variable to the scope
    VariableTypes[Node.getVarName()] = BindingType; // set the type of the variable

    if (Node.getBody())
      Node.getBody()->accept(*this); // accept the body expression

    setExprType(&Node, getExprType(Node.getBody())); // set the type of the let expression to the type of the body expression

    // if this variable does not shadow one with the same name
    // remove this from the scope after the body expression is checked
    if (!shadowed) {
      ScopedVariables.erase(Node.getVarName()); // remove the variable from the scope 
      VariableTypes.erase(Node.getVarName()); // remove the type of the variable
    } else {
      VariableTypes[Node.getVarName()] = oldType; // restore the old type if the variable was shadowed
    }
  }

  virtual void visit(If &Node) override {
    if (Node.getCondition())
      Node.getCondition()->accept(*this); // accept the condition expression first

    ExprTypes CondType = getExprType(Node.getCondition());
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
        llvm::errs() << "Error: Cannot determine type for read expression\n";
        HasError = true;
        setExprType(&Node, ExprTypes::Unknown);
        return;
      }
    }

    if (ThenType != ElseType) {
      raiseTypeError(&Node, ThenType, ElseType);
      setExprType(&Node, ExprTypes::Unknown);
      return;
    }
    
    setExprType(&Node, ThenType); // set the type of the if expression to the type of the then expression
  }
};
} // namespace

bool Sema::semantic(AST *Tree) {
  if (!Tree)
    return false;
  ProgramCheck Check;
  Tree->accept(Check);
  return !Check.hasError();
}
