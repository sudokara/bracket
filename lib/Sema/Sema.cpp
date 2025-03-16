#include "llracket/Sema/Sema.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/raw_ostream.h"

namespace {
class ProgramCheck : public ASTVisitor {
  bool HasError;
  llvm::StringSet<> ScopedVariables;

public:
  ProgramCheck() : HasError(false) {}

  bool hasError() { return HasError; }

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

  virtual void visit(Int &Node) override { return; } // no semantic check for integer

  virtual void visit(Var &Node) override {
    if (!ScopedVariables.count(Node.getName())) { // check that the variable is in scope
      llvm::errs() << "Error: Variable " << Node.getName() << " is not defined\n";
      HasError = true;
    }
  }

  virtual void visit(Let &Node) override {
    if (Node.getBinding())
      Node.getBinding()->accept(*this); // accept the binding expression first, no errors in assignment

    bool shadowed = ScopedVariables.count(Node.getVarName()); // true if a variable of same name was defined previously

    ScopedVariables.insert(Node.getVarName()); // add the variable to the scope

    if (Node.getBody())
      Node.getBody()->accept(*this); // accept the body expression

    // if this variable does not shadow one with the same name
    // remove this from the scope after the body expression is checked
    if (!shadowed)
      ScopedVariables.erase(Node.getVarName()); // remove the variable from the scope 
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
