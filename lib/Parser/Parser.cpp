#include "llracket/Parser/Parser.h"
#include "llracket/AST/AST.h"
#include "llracket/Lexer/Token.h"
#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>

using namespace llracket;
using tok::TokenKind;

AST *Parser::parse() {
  Program *P = new Program(parseExpr());
  AST *Res = llvm::dyn_cast<AST>(P); // program is a subclass of AST so we can do this
  expect(TokenKind::eof);
  return Res;
}

Expr *Parser::parseExpr() {
  auto ErrorHandler = [this]() {
    Diags.report(Tok.getLocation(), diag::err_unexpected_token, Tok.getText());
    skipUntil(tok::r_paren);
    return nullptr;
  };

  // variable
  if (Tok.is(TokenKind::identifier)) {
    Var *VarExpr = new Var(Tok.getText());
    advance();
    return VarExpr;
  }

  if (Tok.is(TokenKind::integer_literal)) {
    Int *Ret = new Int(Tok.getText());
    advance();
    return Ret;
  }

  if (!consume(TokenKind::l_paren))
    return ErrorHandler();

  // we have seen a left parenthesis so far for all 
  // tokens below this point

  // let expression
  if (Tok.is(TokenKind::kw_LET)) {
    advance();

    // check for the second opening parenthesis in (let (
    if (!consume(TokenKind::l_paren))
      return ErrorHandler();

    // check for the opening square bracket in (let ([
    if (!consume(TokenKind::l_square))
      return ErrorHandler();

    // read the variable name in (let ([ varname
    if (!Tok.is(TokenKind::identifier)) {
      Diags.report(Tok.getLocation(), diag::err_expected_identifier, Tok.getText());
      return ErrorHandler();
    }

    StringRef VarName = Tok.getText();
    advance();

    // (let ([ varname bindingexpr
    // read the expression which will evaluate
    // tp the value to be stored in the variable
    Expr *Binding = parseExpr();
    if (!Binding)
      return ErrorHandler();

    // (let ([ varname bindingexpr bodyexpr
    // read the body expression
    // where the variable is in scope
    Expr *Body = parseExpr();
    if (!Body)
      return ErrorHandler();

    // (let ([ varname bindingexpr bodyexpr ]
    // check for the closing square bracket
    if (!consume(TokenKind::r_square))
      return ErrorHandler();

    // (let ([ varname bindingexpr bodyexpr ])
    // check for the closing parenthesis
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();

    return new Let(VarName, Binding, Body);
  }

  if (Tok.is(TokenKind::read)) {
    advance();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new Prim(TokenKind::read);
  }

  if (Tok.is(TokenKind::plus)) {
    advance();
    Expr *E1 = parseExpr();
    Expr *E2 = parseExpr();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new Prim(TokenKind::plus, E1, E2); // addition
  }
  if (Tok.is(TokenKind::minus)) { // negation or subtraction
    advance();

    Expr *E1 = parseExpr();

    if (Tok.is(TokenKind::r_paren)) {
      advance();
      return new Prim(TokenKind::minus, E1);
    }

    Expr *E2 = parseExpr();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new Prim(TokenKind::minus, E1, E2); // subtraction
  }
  return ErrorHandler();
}
