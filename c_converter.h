#pragma once

#include <string>
#include "ast.h"

// ==================================================
// C Converter
// ==================================================
// Converts an AST (your program) into valid C code.
//
// Example usage:
//   std::string code = convertToC(ast);
//   // write code to file, compile, etc.
//
// Note: Generated code is wrapped in:
//   void generated_main() { ... }
//   int main() { generated_main(); return 0; }
// ==================================================

void generate_cpp_code(const char* filename, Ast_Block* program);