#pragma once
#include "ast.h"
#include <iostream>
#include <iomanip>
#include <string>

inline void printAST(const ASTNode* node, std::string indent = "", bool isLast = true) {
    if (!node) return;

    std::cout << indent;

    if (!indent.empty()) {
        if (isLast)
            std::cout << "`-- ";  // last child
        else
            std::cout << "|-- ";  // not last
    }

    // Print node type
    std::cout << astNodeTypeToString(node->type);

    // Print extra info only if relevant
    switch (node->type) {
        case AST_IDENTIFIER:
            std::cout << " (" << node->token.value << ")";
            break;
        case AST_NUMBER_LITERAL:
            std::cout << " (" << node->token.int_value << ")";
            break;
        case AST_STRING_LITERAL:
            std::cout << " (\""
                      << std::string((char*)node->token.string_value.data,
                                     node->token.string_value.count)
                      << "\")";
            break;
        default:
            if (node->token.value) {
                std::cout << " (\"" << node->token.value << "\")";
            }
            break;
    }

    std::cout << std::endl;

    // Recurse for children
    for (size_t i = 0; i < node->children.size(); i++) {
        printAST(node->children[i],
                 indent + (isLast ? "    " : "|   "),
                 i == node->children.size() - 1);
    }
}



// // a:= 4 + (5*2);
// printf   ("something");

// if(asdasd){

// }

// functionName (asda) : int {

// }

// struct structName{

// }

// ,

// /*
// some random stuff
//  */