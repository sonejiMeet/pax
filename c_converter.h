#pragma once

#include <string>
#include "ast.h"

void generate_cpp_code(const char* filename, Ast_Block* program);