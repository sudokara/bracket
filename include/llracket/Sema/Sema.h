#ifndef LLRACKET_SEMA_SEMA_H
#define LLRACKET_SEMA_SEMA_H

#include "llracket/AST/AST.h"
#include "llracket/Lexer/Lexer.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/DenseMap.h"

using llvm::StringMap;
using llvm::DenseMap;

class Sema {
public:
  bool semantic(AST *Tree);
};

enum class ExprTypes {
  Unknown,
  Integer,
  Bool
};

#endif
