#include "llracket/Parser/Parser.h"
#include "llracket/AST/AST.h"
#include "llracket/Lexer/Token.h"
#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>
#include <vector>
#include <utility>

using namespace llracket;
using tok::TokenKind;

AST *Parser::parse() {
  std::vector<FunctionDef*> defs;
  while (Tok.is(TokenKind::l_paren) && peekExpect(TokenKind::define, 1)) {
      FunctionDef* def = parseFunctionDef();
      if (def) {
          defs.push_back(def);
      } else {
          if (!Tok.is(TokenKind::eof)) advance();
      }
  }

  Expr* E = parseExpr();
  expect(TokenKind::eof);

  return new Program(defs, E);
}

FunctionDef* Parser::parseFunctionDef() {
  llvm::SMLoc DefStartLoc = Tok.getLocation(); // Location of the starting '('

  if (!consume(TokenKind::l_paren)) {
    Diags.report(Tok.getLocation(), diag::err_no_lparen, Tok.getText());
    return nullptr;
  }
  if (!consume(TokenKind::define)) {
    Diags.report(Tok.getLocation(), diag::err_expected_define, Tok.getText());
    skipUntil(tok::r_paren);
    if (Tok.is(tok::r_paren)) advance();
    return nullptr;
  }

  // Expect '( name [params...] )' for the function signature
  if (!consume(TokenKind::l_paren)) {
    Diags.report(Tok.getLocation(), diag::err_no_lparen, Tok.getText());
    skipUntil(tok::r_paren);
    if (Tok.is(tok::r_paren)) advance();
    return nullptr;
  }

  // Function name
  if (!Tok.is(TokenKind::identifier)) {
    Diags.report(Tok.getLocation(), diag::err_expected_identifier, Tok.getText());
    skipUntil(tok::r_paren);
    if (Tok.is(tok::r_paren)) advance();
    skipUntil(tok::r_paren);
    if (Tok.is(tok::r_paren)) advance();
    return nullptr;
  }
  StringRef fnName = Tok.getText();
  advance();

  // Parse parameters: zero or more [param : type]
  std::vector<std::pair<StringRef, ParamType*>> params;
  while (Tok.is(TokenKind::l_square)) {
    advance();
    if (!Tok.is(TokenKind::identifier)) {
      Diags.report(Tok.getLocation(), diag::err_expected_identifier, Tok.getText());
      skipUntil(tok::r_square);
      if (Tok.is(tok::r_square)) advance();
      continue;
    }
    StringRef pName = Tok.getText();
    advance();

    if (!consume(TokenKind::colon)) {
      Diags.report(Tok.getLocation(), diag::err_no_colon, Tok.getText());
      skipUntil(tok::r_square);
      if (Tok.is(tok::r_square)) advance();
      continue;
    }

    ParamType* pType = parseType();
    if (!pType) { 
        skipUntil(tok::r_square);
        if (Tok.is(tok::r_square)) advance();
        continue;
    }

    if (!consume(TokenKind::r_square)) {
      Diags.report(Tok.getLocation(), diag::err_no_rsquare, Tok.getText());
    }
    params.emplace_back(pName, pType);
  }

  if (!consume(TokenKind::r_paren)) {
    Diags.report(Tok.getLocation(), diag::err_no_rparen, Tok.getText());
    skipUntil(tok::r_paren);
    if (Tok.is(tok::r_paren)) advance();
    return nullptr;
  }

  if (!consume(TokenKind::colon)) {
    Diags.report(Tok.getLocation(), diag::err_no_colon, Tok.getText());
    skipUntil(tok::r_paren);
    if (Tok.is(tok::r_paren)) advance();
    return nullptr;
  }
  ParamType* retTy = parseType();
  if (!retTy) {
      skipUntil(tok::r_paren);
      if (Tok.is(tok::r_paren)) advance();
        return nullptr; 
  }

  Expr* body = parseExpr();
  if (!body) {
      skipUntil(tok::r_paren);
      if (Tok.is(tok::r_paren)) advance();
      return nullptr;
  }


  if (!consume(TokenKind::r_paren)) {
    Diags.report(Tok.getLocation(), diag::err_no_rparen, Tok.getText());
  }

  return new FunctionDef(fnName, params, retTy, body);
}

ParamType* Parser::parseType() {
  llvm::SMLoc TypeStartLoc = Tok.getLocation();

  if (Tok.is(TokenKind::kw_INTEGERTYPE)) {
    advance();
    return new BasicParamType(ParamType::PK_Integer, "Integer");
  }
  if (Tok.is(TokenKind::kw_BOOLEANTYPE)) {
    advance();
    return new BasicParamType(ParamType::PK_Boolean, "Boolean");
  }
  if (Tok.is(TokenKind::kw_VOIDTYPE)) {
    advance();
    return new BasicParamType(ParamType::PK_Void, "Void");
  }

  if (Tok.is(TokenKind::l_paren)) {
    advance();

    if (Tok.is(TokenKind::kw_VECTORTYPE)) {
      advance();
      std::vector<ParamType*> elems;
      while (!Tok.is(TokenKind::r_paren) && !Tok.is(TokenKind::eof)) {
        ParamType* elemType = parseType();
        if (!elemType) {
            skipUntil(tok::r_paren);
            if(Tok.is(tok::r_paren)) advance();
            return nullptr;
        }
        elems.push_back(elemType);
      }
      if (!consume(TokenKind::r_paren)) {
        Diags.report(Tok.getLocation(), diag::err_no_rparen, Tok.getText());
        return nullptr;
      }
      return new VectorParamType(elems);

    } else {
      std::vector<ParamType*> args;
      while (!Tok.is(TokenKind::arrow) && !Tok.is(TokenKind::r_paren) && !Tok.is(TokenKind::eof)) {
          ParamType* argType = parseType();
          if (!argType) {
              skipUntil(tok::r_paren);
              if(Tok.is(tok::r_paren)) advance();
              return nullptr;
          }
          args.push_back(argType);
      }

      if (!consume(TokenKind::arrow)) {
        Diags.report(Tok.getLocation(), diag::err_unexpected_token, "Expected '->' in function type");
        skipUntil(tok::r_paren);
        if(Tok.is(tok::r_paren)) advance();
        return nullptr;
      }

      ParamType* ret = parseType();
       if (!ret) {
          skipUntil(tok::r_paren);
          if(Tok.is(tok::r_paren)) advance();
          return nullptr;
       }

      if (!consume(TokenKind::r_paren)) {
        Diags.report(Tok.getLocation(), diag::err_no_rparen, Tok.getText());
        return nullptr;
      }
      return new FunctionParamType(args, ret);
    }
  }

  Diags.report(TypeStartLoc, diag::err_unknown_param_type, Tok.getText());
  advance();
  return nullptr;
}

Expr *Parser::parseExpr() {
  auto ErrorHandler = [this](int error=diag::err_unexpected_token) {
    Diags.report(Tok.getLocation(), error, Tok.getText());
    skipUntil(tok::r_paren);
    return nullptr;
  };

  // boolean
  if (Tok.is(TokenKind::boolean_literal)) {
    Bool *BoolExpr = new Bool(Tok.getText());
    advance();
    return BoolExpr;
  }

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
    return ErrorHandler(diag::err_no_lparen);

  // we have seen a left parenthesis so far for all 
  // tokens below this point

  // Handle void expression
  if (Tok.is(TokenKind::kw_VOID)) {
    advance();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler(diag::err_no_rparen);
    return new Void();
  }

  // Handle begin expression
  if (Tok.is(TokenKind::begin)) {
    advance();
    
    std::vector<Expr*> Exprs;
    
    // Parse expressions until we see a right parenthesis
    while (!Tok.is(TokenKind::r_paren)) {
      Expr *E = parseExpr();
      if (!E)
        return ErrorHandler();
      Exprs.push_back(E);
    }
    
    // Make sure we have at least one expression
    if (Exprs.empty()) {
      Diags.report(Tok.getLocation(), diag::err_unexpected_token, Tok.getText());
      return ErrorHandler();
    }
    
    if (!consume(TokenKind::r_paren))
      return ErrorHandler(diag::err_no_rparen);
    
    return new Begin(Exprs);
  }
  

  // Handle set! expression
  if (Tok.is(TokenKind::setbang)) {
    advance();
    
    if (!Tok.is(TokenKind::identifier)) {
      Diags.report(Tok.getLocation(), diag::err_expected_identifier, Tok.getText());
      return ErrorHandler();
    }
    
    StringRef VarName = Tok.getText();
    advance();
    
    Expr *Value = parseExpr();
    if (!Value)
      return ErrorHandler();
    
    if (!consume(TokenKind::r_paren))
      return ErrorHandler(diag::err_no_rparen);
    
    return new Set(VarName, Value);
  }

  // Handle while expression
  if (Tok.is(TokenKind::whileloop)) {
    advance();
    
    Expr *Condition = parseExpr();
    if (!Condition)
      return ErrorHandler();
    
    Expr *Body = parseExpr();
    if (!Body)
      return ErrorHandler();
    
    if (!consume(TokenKind::r_paren))
      return ErrorHandler(diag::err_no_rparen);
    
    return new While(Condition, Body);
  }

  // case 1:
  // if condition
  if (Tok.is(TokenKind::kw_IF)) {
    advance ();

    Expr *Cond = parseExpr();
    if (!Cond)
      return ErrorHandler();

    Expr *Then = parseExpr();
    if (!Then)
      return ErrorHandler();

    Expr *Else = parseExpr();
    if (!Else)
      return ErrorHandler();

    if (!consume(TokenKind::r_paren))
      return ErrorHandler(diag::err_no_rparen);

    return new If(Cond, Then, Else);
  }

  // case 2:
  // logical and relational operations
  llvm::SmallVector <TokenKind, 8> LogRelOps = {
    TokenKind::logical_and, TokenKind::logical_or,
    TokenKind::eq, TokenKind::lt, TokenKind::le, TokenKind::gt, TokenKind::ge
  };

  if (std::find(LogRelOps.begin(), LogRelOps.end(), Tok.getKind()) != LogRelOps.end()) {
    TokenKind Op = Tok.getKind();
    advance();

    Expr *E1 = parseExpr();
    if (!E1)
      return ErrorHandler();

    Expr *E2 = parseExpr();
    if (!E2)
      return ErrorHandler();

    if (!consume(TokenKind::r_paren))
      return ErrorHandler(diag::err_no_rparen);

    return new Prim(Op, E1, E2);
  }

  if (Tok.is(TokenKind::logical_not)) {
    advance();
    Expr *E1 = parseExpr();
    if (!E1)
      return ErrorHandler();

    if (!consume(TokenKind::r_paren))
      return ErrorHandler(diag::err_no_rparen);

    return new Prim(TokenKind::logical_not, E1);
  }

  // case 3:
  // let expression
  if (Tok.is(TokenKind::kw_LET)) {
    advance();

    // check for the second opening parenthesis in (let (
    if (!consume(TokenKind::l_paren))
      return ErrorHandler(diag::err_no_lparen);

    // check for the opening square bracket in (let ([
    if (!consume(TokenKind::l_square))
      return ErrorHandler(diag::err_no_lsquare);

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

    // (let ([ varname bindingexpr ]
    // check for the closing square bracket
    if (!consume(TokenKind::r_square))
      return ErrorHandler(diag::err_no_rsquare);

    if (!consume(TokenKind::r_paren))
      return ErrorHandler(diag::err_no_rparen);

    // (let ([ varname bindingexpr ]) bodyexpr
    // read the body expression
    // where the variable is in scope
    Expr *Body = parseExpr();
    if (!Body)
      return ErrorHandler();

    // (let ([ varname bindingexpr ]) bodyexpr)
    // check for the closing parenthesis
    if (!consume(TokenKind::r_paren))
      return ErrorHandler(diag::err_no_rparen);

    return new Let(VarName, Binding, Body);
  }

  if (Tok.is(TokenKind::read)) {
    advance();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler(diag::err_no_rparen);
    return new Prim(TokenKind::read);
  }

  if (Tok.is(TokenKind::plus)) {
    advance();
    Expr *E1 = parseExpr();
    Expr *E2 = parseExpr();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler(diag::err_no_rparen);
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
      return ErrorHandler(diag::err_no_rparen);
    return new Prim(TokenKind::minus, E1, E2); // subtraction
  }

  // vector creation
  if (Tok.is(TokenKind::vector)) {
    advance();
    
    std::vector<Expr*> Elements;
    
    // keep parsing for elements till r paren
    while (!Tok.is(TokenKind::r_paren)) {
      Expr *E = parseExpr();
      if (!E)
        return ErrorHandler();
      Elements.push_back(E);
    }
    
    // consume r paren once found
    if (!consume(TokenKind::r_paren))
      return ErrorHandler(diag::err_no_rparen);
    
    return new Vec(Elements);
  }

  // vector reference
  if (Tok.is(TokenKind::vector_ref)) {
    advance();
    
    Expr *VecExpr = parseExpr();
    if (!VecExpr)
      return ErrorHandler();
    
    Expr *IndexExpr = parseExpr();
    if (!IndexExpr)
      return ErrorHandler();
    
    if (!consume(TokenKind::r_paren))
      return ErrorHandler(diag::err_no_rparen);
    
    return new VecRef(VecExpr, IndexExpr);
  }

  // vector length
  if (Tok.is(TokenKind::vector_length)) {
    advance();
    
    Expr *VecExpr = parseExpr();
    if (!VecExpr)
      return ErrorHandler();
    
    if (!consume(TokenKind::r_paren))
      return ErrorHandler(diag::err_no_rparen);
    
    return new VecLen(VecExpr);
  }

  // vector set
  if (Tok.is(TokenKind::vector_set)) {
    advance();
    
    Expr *VecExpr = parseExpr();
    if (!VecExpr)
      return ErrorHandler();
    
    Expr *IndexExpr = parseExpr();
    if (!IndexExpr)
      return ErrorHandler();
    
    Expr *ValueExpr = parseExpr();
    if (!ValueExpr)
      return ErrorHandler();
    
    if (!consume(TokenKind::r_paren))
      return ErrorHandler(diag::err_no_rparen);
    
    return new VecSet(VecExpr, IndexExpr, ValueExpr);
  }

  Expr* funcExpr = parseExpr();
  if (!funcExpr) {
      skipUntil(tok::r_paren); if(Tok.is(tok::r_paren)) advance();
      return nullptr;
  }


  std::vector<Expr*> args;
  while (!Tok.is(TokenKind::r_paren) && !Tok.is(TokenKind::eof)) {
    Expr* argExpr = parseExpr();
    if (!argExpr) {
      skipUntil(tok::r_paren); if(Tok.is(tok::r_paren)) advance();
      return nullptr;
    }
    args.push_back(argExpr);
  }

  if (!consume(TokenKind::r_paren)) {
    return ErrorHandler(diag::err_no_rparen);
  }

  return new Apply(funcExpr, args);
}
