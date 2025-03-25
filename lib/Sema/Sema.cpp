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
    llvm::errs() << "Error: Expected " ;
    printType(Expected);
    llvm::errs() << " but found ";
    printType(Found);
    llvm::errs() << " in expression\n";
    HasError = true;
  }

  void printType(ExprTypes T) {
    switch (T) {
      case ExprTypes::Integer: llvm::errs() << "Integer"; break;
      case ExprTypes::Bool: llvm::errs() << "Boolean"; break;
      default: llvm::errs() << "Unknown"; break;
    }
  }

  virtual void visit(Program &Node) override {
    if (Node.getExpr())
      Node.getExpr()->accept(*this);
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
    auto &PrimNode = llvm::cast<Prim>(Node);
    if (PrimNode.getOp() == tok::read) {
      return;
    }
    if (PrimNode.getOp() == tok::minus) {
      if (PrimNode.getE1() and !PrimNode.getE2()) {
        PrimNode.getE1()->accept(*this);
        return;
      }
    }
    if (PrimNode.getOp() == tok::plus || PrimNode.getOp() == tok::minus) {
      PrimNode.getE1()->accept(*this);
      PrimNode.getE2()->accept(*this);
      return;
    }
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
    if (Node.getBinding())
      Node.getBinding()->accept(*this); // accept the binding expression first, no errors in assignment

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
