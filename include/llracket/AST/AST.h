#ifndef LLRACKET_AST_AST_H
#define LLRACKET_AST_AST_H

#include "llracket/Lexer/Token.h"
#include <any>
#include <llvm/ADT/StringMap.h>
#include <llvm/ADT/StringRef.h>
#include <vector>

enum class ExprTypes {
  Unknown,
  Integer,
  Bool,
  Void,
  Vector
};

class AST;
class Program;
typedef llvm::StringMap<std::any> ProgramInfo;
class Expr;
class Prim;
class Int;
class Var;
class Let;
class Bool;
class If;
class Set;
class Begin;
class While;
class Void;
class Vec;
class VecRef;
class VecLen;
class VecSet;


class ASTVisitor {
public:
  virtual ~ASTVisitor() {}
  virtual void visit(Program &) {};
  virtual void visit(Expr &) {};
  virtual void visit(Prim &) {};
  virtual void visit(Int &) = 0;
  virtual void visit(Var &) {};
  virtual void visit(Let &) {};
  virtual void visit(Bool &) {};
  virtual void visit(If &) {};
  virtual void visit(Set &) {};
  virtual void visit(Begin &) {};
  virtual void visit(While &) {};
  virtual void visit(Void &) {};
  virtual void visit(Vec&) {};
  virtual void visit(VecRef &) {};
  virtual void visit(VecLen &) {};
  virtual void visit(VecSet &) {};
};

class AST {
public:
  virtual ~AST() {}
  virtual void accept(ASTVisitor &V) = 0;
};

class Program : public AST {
  Expr *E;
  ProgramInfo Info;

public:
  Program(Expr *E) : E(E) {};
  Program(Expr *E, ProgramInfo Info) : E(E), Info(Info) {};

  Expr *getExpr() const { return E; };
  ProgramInfo getInfo() const { return Info; };
  void setInfo(ProgramInfo Info) { this->Info = Info; };

  virtual void accept(ASTVisitor &V) override { V.visit(*this); }
};

class Expr : public AST {
public:
  enum ExprKind { ExprPrim, ExprInt, ExprVar, ExprLet, ExprBool, ExprIf, ExprSet, ExprBegin, ExprWhile, ExprVoid, ExprVec, ExprVecRef, ExprVecLen, ExprVecSet };

private:
  const ExprKind Kind;

public:
  Expr(ExprKind Kind) : Kind(Kind) {}

  ExprKind getKind() const { return Kind; }
  virtual void accept(ASTVisitor &V) override { V.visit(*this); }
};

class Prim : public Expr {
  TokenKind Op;
  Expr *E1 = NULL;
  Expr *E2 = NULL;

public:
  Prim(TokenKind Op) : Expr(ExprPrim), Op(Op) {};
  Prim(TokenKind Op, Expr *E1) : Expr(ExprPrim), Op(Op), E1(E1) {};
  Prim(TokenKind Op, Expr *E1, Expr *E2)
      : Expr(ExprPrim), Op(Op), E1(E1), E2(E2) {};

  TokenKind getOp() const { return Op; };
  Expr *getE1() const { return E1; };
  Expr *getE2() const { return E2; };

  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprPrim; }
};

class Int : public Expr {
  StringRef Value;

public:
  Int(StringRef Value) : Expr(ExprInt), Value(Value) {};
  StringRef getValue() const { return Value; };
  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return true; }
};

class Bool : public Expr {
  bool Value;

public:
  Bool(bool Value) : Expr(ExprBool), Value(Value) {};
  Bool(StringRef Value): Expr(ExprBool), Value(Value == "#t") {};
  bool getValue() const { return Value; };
  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprBool; }
};

class Var: public Expr {
  StringRef Name;

  public:
  Var(StringRef Name) : Expr(ExprVar), Name(Name) {};
  StringRef getName() const { return Name; };
  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprVar; }
};

class Let: public Expr {
  StringRef VarName;
  Expr *Binding; // what will be stored to the variable
  Expr *Body; // the body of the let expression where the variable is in scope

  public:
  Let(StringRef VarName, Expr *Binding, Expr *Body) : Expr(ExprLet), VarName(VarName), Binding(Binding), Body(Body) {};

  StringRef getVarName() const { return VarName; };
  Expr *getBinding() const { return Binding; };
  Expr *getBody() const { return Body; };

  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprLet; }
};

class If: public Expr {
  Expr *Condition;
  Expr *ThenExpr;
  Expr *ElseExpr;

  public:
  If(Expr *Condition, Expr *ThenExpr, Expr *ElseExpr) : Expr(ExprIf), Condition(Condition), ThenExpr(ThenExpr), ElseExpr(ElseExpr) {};

  Expr *getCondition() const { return Condition; };
  Expr *getThenExpr() const { return ThenExpr; };
  Expr *getElseExpr() const { return ElseExpr; };

  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprIf; }
};

class Set : public Expr {
  StringRef VarName;
  Expr *Value;

public:
  Set(StringRef VarName, Expr *Value) 
    : Expr(ExprSet), VarName(VarName), Value(Value) {};

  StringRef getVarName() const { return VarName; };
  Expr *getValue() const { return Value; };

  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprSet; }
};

class Begin : public Expr {
  std::vector<Expr*> Exprs;

public:
  Begin(const std::vector<Expr*> &Exprs) 
    : Expr(ExprBegin), Exprs(Exprs) {};

  const std::vector<Expr*> &getExprs() const { return Exprs; };

  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprBegin; }
};

class While : public Expr {
  Expr *Condition;
  Expr *Body;

public:
  While(Expr *Condition, Expr *Body) 
    : Expr(ExprWhile), Condition(Condition), Body(Body) {};

  Expr *getCondition() const { return Condition; };
  Expr *getBody() const { return Body; };

  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprWhile; }
};

class Void : public Expr {
  public:
    Void() : Expr(ExprVoid) {};
  
    virtual void accept(ASTVisitor &V) override { V.visit(*this); }
  
    static bool classof(const Expr *E) { return E->getKind() == ExprVoid; }
};

class Vec : public Expr {
  std::vector<Expr*> Elements;
  size_t Length;

public:
  Vec(const std::vector<Expr*> &Elements) 
    : Expr(ExprVec), Elements(Elements), Length(Elements.size()) {};
  
  const std::vector<Expr*> &getElements() const { return Elements; };
  size_t getLength() const { return Length; };

  virtual void accept(ASTVisitor &V) override { V.visit(*this); }
  static bool classof(const Expr *E) { return E->getKind() == ExprVec; }
};

class VecRef : public Expr {
  Expr *VecExpr;
  Expr *Index;

public:
  VecRef(Expr *VecExpr, Expr *Index) 
    : Expr(ExprVecRef), VecExpr(VecExpr), Index(Index) {};

  Expr *getVecExpr() const { return VecExpr; };
  Expr *getIndex() const { return Index; };
  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprVecRef; }
};

class VecLen : public Expr {
  Expr *VecExpr;

public:
  VecLen(Expr *VecExpr) : Expr(ExprVecLen), VecExpr(VecExpr) {};

  Expr *getVecExpr() const { return VecExpr; };
  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprVecLen; }
};

class VecSet : public Expr {
  Expr *VecExpr;
  Expr *Index;
  Expr *Value;

public:
  VecSet(Expr *VecExpr, Expr *Index, Expr *Value) 
    : Expr(ExprVecSet), VecExpr(VecExpr), Index(Index), Value(Value) {};

  Expr *getVecExpr() const { return VecExpr; };
  Expr *getIndex() const { return Index; };
  Expr *getValue() const { return Value; };
  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprVecSet; }
};

#endif
