#pragma once

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdarg.h> // for variadic function
#include <math.h>
#include <string>
#include <stdint.h>

#include "pool.h"
#include "tools.h"
#include "token.h"
#include "lexer.h"
#include "ast.h"
#include "parser.h"
#include "code_manager.h"
#include "c_converter.h"
#include "interp.h"
#include "lexer_printer.h"

#include "tools.cpp"
#include "interp.cpp"
#include "lexer.cpp"
#include "parser.cpp"
#include "code_manager.cpp"
#include "c_converter.cpp"
