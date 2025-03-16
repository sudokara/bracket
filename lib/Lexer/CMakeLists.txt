add_library(llracketLexer Lexer.cpp)

target_link_libraries(llracketLexer PRIVATE LLVMCore LLVMSupport llracketBasic)

target_include_directories(llracketLexer PRIVATE ${LLVM_INCLUDE_DIRS})
