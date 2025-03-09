#ifndef LLRACKET_PARSER_PARSER_H
#define LLRACKET_PARSER_PARSER_H

#include "llracket/AST/AST.h"
#include "llracket/Lexer/Lexer.h"
#include "llvm/Support/raw_ostream.h"

class Parser {
  Lexer &Lex;
  Token Tok;
  bool HasError;

  void error() {
    llvm::errs() << "Error: unexpected token: " << Tok.getText() << '\n';
    HasError = true;
  }

  void advance() { Lex.next(Tok); }
  void advance(unsigned N) {
    for (unsigned I = 0; I < N; I++)
      advance();
  }

  bool expect(TokenKind Kind) {
    if (Tok.getKind() != Kind) {
      error();
      return false;
    }
    return true;
  }

  bool consume(TokenKind Kind) {
    if (!expect(Kind))
      return false;
    advance();
    return true;
  }

  Expr *parseExpr();

public:
  Parser(Lexer &Lex) : Lex(Lex), HasError(false) { advance(); }

  bool hasError() { return HasError; }

  AST *parse();
};
#endif
