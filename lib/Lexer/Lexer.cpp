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

LLVM_READNONE inline static bool isSpecialChar(char c) {
  return c == '!' || c == '?' || c == '_' || c == '-';
}

LLVM_READNONE inline static bool isAlphanumeric_special(char c) {
  return isAlphanumeric(c) || isSpecialChar(c);
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

  if (*BufferPtr == '-' && *(BufferPtr + 1) &&  *(BufferPtr + 1) == '>') {
    formToken(token, BufferPtr + 2, TokenKind::arrow);
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
  if (charinfo::isLetter(*BufferPtr) || *BufferPtr == '_') {
    const char *End = BufferPtr + 1;
    while (charinfo::isAlphanumeric_special(*End))
      ++End;

    llvm::StringRef Text(BufferPtr, End - BufferPtr);
    StringMap <TokenKind> text_map({
      {"if", TokenKind::kw_IF},
      {"read", TokenKind::read},
      {"let", TokenKind::kw_LET},
      {"and", TokenKind::logical_and},
      {"or", TokenKind::logical_or},
      {"not", TokenKind::logical_not},
      {"eq?", TokenKind::eq},
      {"set!", TokenKind::setbang},
      {"begin", TokenKind::begin},
      {"while", TokenKind::whileloop},
      {"void", TokenKind::kw_VOID},
      {"vector", TokenKind::vector},
      {"vector-ref", TokenKind::vector_ref},
      {"vector-length", TokenKind::vector_length},
      {"vector-set!", TokenKind::vector_set},
      {"define", TokenKind::define},
      {"Integer", TokenKind::kw_INTEGERTYPE},
      {"Boolean", TokenKind::kw_BOOLEANTYPE},
      {"Void", TokenKind::kw_VOIDTYPE},
      {"Vector", TokenKind::kw_VECTORTYPE},
    });

    if (text_map.find(Text) != text_map.end()) {
      formToken(token, End, text_map[Text]);
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

  case ':':
    formToken(token, BufferPtr + 1, TokenKind::colon);
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
