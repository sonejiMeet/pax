#include "parser.h"
#include <iostream>
//CAC6B3FF
Parser::Parser(Lexer* l) : lexer(l) {
    current = lexer->nextToken();
}

void Parser::advance() {
    current = lexer->nextToken();
}



// ASTNode* Parser::parseFactor() {
//     if (current.type == TOK_NUMBER) {
//         ASTNode* node = new ASTNode(AST_NUMBER_LITERAL, current);
//         advance();
//         return node;
//     } else if(current.type == TOK_IDENTIFIER){
//         ASTNode* node = new ASTNode(AST_IDENTIFIER, current);
//         advance();
//         return node;

//     }

//     // fallback
//     return new ASTNode(AST_UNKNOWN, current);
// }

// ASTNode* Parser::parseTerm() {
//     ASTNode* left = parseFactor();
//     while (current.type == TOK_STAR) {
//         Token op = current;
//         advance();
//         ASTNode* right = parseFactor();
//         ASTNode* node = new ASTNode(AST_BINARY_EXPR, op);
//         node->children.push_back(left);
//         node->children.push_back(right);
//         left = node;
//     }
//     return left;
// }

// ASTNode* Parser::parseExpression() {
//     ASTNode* left = parseTerm();
//     while (current.type == TOK_PLUS) {
//         Token op = current;
//         advance();
//         ASTNode* right = parseTerm();
//         ASTNode* node = new ASTNode(AST_BINARY_EXPR, op);
//         node->children.push_back(left);
//         node->children.push_back(right);
//         left = node;
//     }
//     return left;
// }

// ASTNode* Parser::parse


// Reports parsing errors and terminates execution.
void Parser::parseError(const std::string& message) {
    std::cerr << "Parsing Error: " << message
              << " at token '" << current.value << "' (Type: "
              << tokenTypeToString(current.type) << ")" << std::endl;
    throw std::runtime_error("Parsing failed due to syntax error.");
}

// Consumes an expected token, or reports an error if mismatch.
void Parser::expect(TokenType expectedType, const std::string& errorMessage) {
    if (current.type != expectedType) {
        parseError(errorMessage);
    }
    advance();
}

// Parses a factor in an expression (numbers, identifiers, parenthesized expressions).
ASTNode* Parser::parseFactor() {
    if (current.type == TOK_NUMBER) {
        ASTNode* node = new ASTNode(AST_NUMBER_LITERAL, current);
        advance();
        return node;
    } else if (current.type == TOK_IDENTIFIER) {
        ASTNode* node = new ASTNode(AST_IDENTIFIER, current);
        advance();
        return node;
    } else if (current.type == TOK_LPAREN) {
        expect(TOK_LPAREN, "Expected '(' for parenthesized expression.");
        ASTNode* expr = parseExpression();
        expect(TOK_RPAREN, "Expected ')' after expression in parentheses.");
        return expr;
    }
    parseError("Expected a number, identifier, or '(' for expression factor.");
    return nullptr; // Unreachable if parseError throws
}

// Parses a term (multiplication and division operations).
ASTNode* Parser::parseTerm() {
    ASTNode* left = parseFactor();

    while (current.type == TOK_STAR || current.type == TOK_SLASH) {
        Token op = current;
        advance();
        ASTNode* right = parseFactor();

        ASTNode* node = new ASTNode(AST_BINARY_EXPR, op);
        node->children.push_back(left);
        node->children.push_back(right);

        left = node;
    }
    return left;
}

// Parses an expression (addition, subtraction, and comparison operations).
ASTNode* Parser::parseExpression() {
    ASTNode* left = parseTerm();

    while (current.type == TOK_PLUS || current.type == TOK_MINUS ||
           current.type == TOK_EQUAL || current.type == TOK_NOT_EQUAL ||
           current.type == TOK_LESS || current.type == TOK_GREATER ||
           current.type == TOK_LESS_EQUAL || current.type == TOK_GREATER_EQUAL) {

        Token op = current;
        advance();
        ASTNode* right = parseTerm();

        ASTNode* node = new ASTNode(AST_BINARY_EXPR, op);
        node->children.push_back(left);
        node->children.push_back(right);

        left = node;
    }
    return left;
}

// Parses a variable declaration statement (e.g., `identifier = expression;`).
ASTNode* Parser::parseVarDeclaration() {
    if (current.type != TOK_IDENTIFIER) {
        parseError("Expected identifier for variable declaration.");
    }
    Token varNameToken = current;
    advance();

    expect(TOK_ASSIGN, "Expected '=' after variable name in declaration.");

    ASTNode* initializerExpr = parseExpression();
    if (!initializerExpr) {
        parseError("Failed to parse initializer expression for variable declaration.");
    }

    expect(TOK_SEMICOLON, "Expected ';' after variable declaration.");

    ASTNode* varDeclNode = new ASTNode(AST_VAR_DECL, varNameToken);
    varDeclNode->children.push_back(initializerExpr);

    return varDeclNode;
}

// Parses a single statement, dispatching to specific parsing functions.
ASTNode* Parser::parseStatement() {
    // Check for variable declaration/assignment pattern: identifier followed by '='
    if (current.type == TOK_IDENTIFIER) {
        Token next = lexer->peekNextToken();
        if (next.type == TOK_ASSIGN) {
            return parseVarDeclaration();
        }
    }

    // Handle 'print' statement: print EXPRESSION ;
    if (current.type == TOK_PRINT) {
        Token printToken = current;
        advance();
        ASTNode* expr = parseExpression();
        expect(TOK_SEMICOLON, "Expected ';' after print statement.");

        ASTNode* printStatementNode = new ASTNode(AST_PRINT_STATEMENT, printToken);
        printStatementNode->children.push_back(expr);
        return printStatementNode;
    }

    // Default: Assume it's an expression statement (e.g., `5 + 3;`).
    ASTNode* expr = parseExpression();
    expect(TOK_SEMICOLON, "Expected ';' after expression statement.");

    ASTNode* exprStatementNode = new ASTNode(AST_EXPR_STATEMENT);
    exprStatementNode->children.push_back(expr);
    return exprStatementNode;

    // TODO: Extend this function for 'if', 'while', 'for', 'return', etc.
}

// Parses the entire program, which is a list of statements.
ASTNode* Parser::parseProgram() {
    ASTNode* programNode = new ASTNode(AST_PROGRAM);
    ASTNode* statementList = new ASTNode(AST_STATEMENT_LIST);

    while (current.type != TOK_END_OF_FILE) {
        ASTNode* statement = parseStatement();
        if (statement) {
            statementList->children.push_back(statement);
        } else {
            // Should be caught by parseError throwing in `parseStatement`.
            parseError("Failed to parse a statement.");
            break;
        }
    }
    programNode->children.push_back(statementList);
    return programNode;
}
