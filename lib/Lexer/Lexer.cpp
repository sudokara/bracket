#include "llracket/Lexer/Lexer.h"

namespace charinfo {
LLVM_READNONE inline static bool isWhitespace(char c) {
  return c == ' ' || c == '\t' || c == '\f' || c == '\v' || c == '\r' ||
         c == '\n';
}
LLVM_READNONE inline static bool isDigit(char c) {
  return c >= '0' && c <= '9';
}
LLVM_READNONE inline static bool isLetter(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

LLVM_READNONE inline static bool isAlphanumeric(char c) {
  return isLetter(c) || isDigit(c);
}

LLVM_READNONE inline static bool isAlphanumeric_(char c) {
  return isAlphanumeric(c) || c == '_';
}
} // namespace charinfo

void Lexer::next(Token &token) {
  // parses one token at a time
  while (*BufferPtr && charinfo::isWhitespace(*BufferPtr))
    ++BufferPtr;

  if (!*BufferPtr) {
    token.Kind = TokenKind::eof;
    return;
  }

  if (*BufferPtr == '#') {
    if (*(BufferPtr + 1) && *(BufferPtr + 1) == 't') {
      formToken(token, BufferPtr + 2, TokenKind::boolean_literal);
    }
    else if (*(BufferPtr + 1) && *(BufferPtr + 1) == 'f') {
      formToken(token, BufferPtr + 2, TokenKind::boolean_literal);
    }
    else {
      Diags.report(getLoc(), diag::err_unknown_token, *BufferPtr); // return an unknown token error with diag
      if (*(BufferPtr + 1))
        formToken(token, BufferPtr + 2, TokenKind::unknown); // form an unknown token
      else
        formToken(token, BufferPtr + 1, TokenKind::unknown); // form an unknown token
    }
    return;
  }

  if (charinfo::isDigit(*BufferPtr)) { // read until all digits
    const char *End = BufferPtr + 1;
    while (charinfo::isDigit(*End))
      ++End;
    formToken(token, End, TokenKind::integer_literal); // construct the integer literal from the string
    return;
  }
  if (charinfo::isLetter(*BufferPtr)) {
    const char *End = BufferPtr + 1;
    while (charinfo::isAlphanumeric_(*End))
      ++End;

    llvm::StringRef Text(BufferPtr, End - BufferPtr);
    // TODO: becomes worse if more keywords, can have a map for construction of tokens for keywords
    // StringMap <int> keywords_map;

    if (Text == "if") {
      formToken(token, End, TokenKind::kw_IF);
      return;
    }
    if (Text == "read") { // override for keyword read, can do similar for let
      formToken(token, End, TokenKind::read);
      return;
    }
    if (Text == "let") {
      // form a let token and return
      formToken(token, End, TokenKind::kw_LET);
      return;
    }
    if (Text == "and") {
      formToken(token, End, TokenKind::logical_and);
      return;
    }
    if (Text == "or") {
      formToken(token, End, TokenKind::logical_or);
      return;
    }
    if (Text == "not") {
      formToken(token, End, TokenKind::logical_not);
      return;
    }
    if (Text == "eq?") {
      formToken(token, End, TokenKind::eq);
      return;
    }
    // formToken(token, End, TokenKind::unknown);
    
    // ! I'm assuming that this token is going to be an identifer. Syntax errors should be caught later.
    formToken(token, End, TokenKind::identifier);
    return;
  }

  switch (*BufferPtr) {
#define CASE(ch, tok)                                                          \
  case ch:                                                                     \
    formToken(token, BufferPtr + 1, TokenKind::tok);                           \
    break;

    CASE('+', plus);
    CASE('-', minus);
    CASE('(', l_paren);
    CASE(')', r_paren);
    CASE('[', l_square);
    CASE(']', r_square);
#undef CASE

  case '<':
    if (*(BufferPtr + 1) && *(BufferPtr + 1) == '=') {
      formToken(token, BufferPtr + 2, TokenKind::le);
    } else {
      formToken(token, BufferPtr + 1, TokenKind::lt);
    }
    break;

  case '>':
    if (*(BufferPtr + 1) && *(BufferPtr + 1) == '=') {
      formToken(token, BufferPtr + 2, TokenKind::ge);
    } else {
      formToken(token, BufferPtr + 1, TokenKind::gt);
    }
    break;

  default:
    Diags.report(getLoc(), diag::err_unknown_token, *BufferPtr); // return an unknown token error with diag
    formToken(token, BufferPtr + 1, TokenKind::unknown); // form an unknown token
    break;
  }
  return;
}

void Lexer::formToken(Token &Tok, const char *TokEnd, TokenKind Kind) {
  Tok.Kind = Kind;
  Tok.Text = StringRef(BufferPtr, TokEnd - BufferPtr);
  BufferPtr = TokEnd;
}
