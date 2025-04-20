#ifndef LLRACKET_PARSER_PARSER_H
#define LLRACKET_PARSER_PARSER_H

#include "llracket/AST/AST.h"
#include "llracket/Basic/Diagnostic.h"
#include "llracket/Lexer/Lexer.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"
#include <vector>

class Parser {
  Lexer &Lex;
  Token Tok;
  DiagnosticsEngine &Diags;

  llvm::SmallVector<llvm::StringMap<ParamType*>, 8> TypeEnv;
  void pushScope()        { TypeEnv.emplace_back(); }
  void popScope()         { TypeEnv.pop_back(); }
  ParamType *lookupType(StringRef Name) {
    for (auto &Env : llvm::reverse(TypeEnv))
      if (auto *T = Env.lookup(Name))
        return T;
    return nullptr;
  }

  // {Received Token, Expected Token}
  std::vector<std::pair<TokenKind, TokenKind>> UnexpectedTokens;

  void error(TokenKind Kind) {
    UnexpectedTokens.push_back({Tok.getKind(), Kind});
  }

  // move through the tokens using next of lexer
  void advance() { Lex.next(Tok); }
  void advance(unsigned N) {
    for (unsigned I = 0; I < N; I++) {
      if (Tok.getKind() == tok::eof) {
        break;
      }
      advance();
    }
  }

  // check if character is of expected kind, else error
  bool expect(TokenKind Kind) {
    if (Tok.getKind() != Kind) {
      error(Kind);
      return false;
    }
    return true;
  }

  bool peekExpect(TokenKind Kind, unsigned N = 1) {
    Token peekedTok = Lex.peek(N);
    if (peekedTok.getKind() != Kind) {
      error(Kind);
      return false;
    }
    return true;
  }

  // check if character is of expected kind and move forward, else error
  bool consume(TokenKind Kind) {
    if (!expect(Kind))
      return false;
    advance();
    return true;
  }

  ParamType* parseType();
  FunctionDef* parseFunctionDef();
  Expr *parseExpr();

public:
  Parser(Lexer &Lex, DiagnosticsEngine &Diags) : Lex(Lex), Diags(Diags) {
    advance();
  }

  AST *parse();

  template <class... Tokens> void skipUntil(Tokens... Toks) {
    std::unordered_set<tok::TokenKind> Skipset = {tok::eof, Toks...};
    while (true) {
      if (Skipset.count(Tok.getKind())) {
        break;
      }
      advance();
    }
  }
};
#endif
