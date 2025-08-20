#include "parser.h"
#include "token.h"
#include <iostream>
#include <vector>
//CAC6B3FF
Parser::Parser(Lexer* l) : lexer(l) {
    current = lexer->nextToken();
    tempTokens.push_back(current);

}

void Parser::advance() {

    current = lexer->nextToken();
    tempTokens.push_back(current);
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

// Reports parsing errors and terminates execution.
void Parser::parseError(const std::string& message) {
    std::cerr << "Parsing Error: " <<  "Line[" << current.row << ":" << current.col << "] "  << message
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
        ASTNode* node = new ASTNode(AST_IDENTIFIER, current); // Use AST_IDENTIFIER for expression identifiers
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

ASTNode* Parser::parseTypeSpecifier() {
    // Expect a type keyword
    if (!(current.type == TOK_TYPE_INT || current.type == TOK_TYPE_FLOAT ||
          current.type == TOK_TYPE_STRING || current.type == TOK_TYPE_BOOL)) {
        parseError("Expected a type keyword (e.g., 'int', 'float', 'string', 'bool').");
    }
    Token typeToken = current; // Capture the type token (e.g., TOK_TYPE_INT)
    advance(); // Consume the type keyword

    // Create an AST_TYPE_SPECIFIER node, associating it with the type token
    return new ASTNode(AST_TYPE_SPECIFIER, typeToken);
}

// Parses a variable declaration statement (e.g., `identifier = expression;`).
ASTNode* Parser::parseVarDeclaration() {
    if (current.type != TOK_IDENTIFIER) {
        parseError("Expected identifier for variable declaration.");
    }
    Token varNameToken = current;
    advance();

    expect(TOK_COLON, "Expected ':' after variable name for type declaration.");

    // 3. Parse the type specifier (e.g., 'int', 'float')
    ASTNode* typeSpecifierNode = parseTypeSpecifier();
    // advance();

    if (!typeSpecifierNode) {
        parseError("Failed to parse type specifier for variable declaration.");
    }

    ASTNode* initializerExpr = nullptr;
    if (current.type == TOK_ASSIGN) {
        advance(); // Consume the '='
        initializerExpr = parseExpression(); // Parse the expression that provides the initial value
        if (!initializerExpr) {
            parseError("Failed to parse initializer expression for variable declaration.");
        }
    }

    expect(TOK_SEMICOLON, "Expected ';' after variable declaration.");

    ASTNode* varDeclNode = new ASTNode(AST_VAR_DECL, varNameToken); // Using AST_VAR_DECL
    varDeclNode->children.push_back(initializerExpr);

    if (initializerExpr) {
        varDeclNode->children.push_back(initializerExpr); // Child 1 (optional): Initializer Expression
    }

    return varDeclNode;
}

void Parser::logDebug(const std::string& message, const Token* token = nullptr) const {
        std::cout << "DEBUG: " << message;
        if (token) {
            std::cout << " Token: " << tokenTypeToString(token->type)
                      << " ('" << token->value << "')"
                      << " Line: " << token->row << ", Col: " << token->col;
        }
        std::cout << std::endl;
    }

ASTNode* Parser::parseStatement() {
    std::cout << "\n--- DEBUG: Entering parseStatement() ---" << std::endl; // Keep this one for high-level entry
    logDebug("Current token at start of parseStatement", &current);

    switch (current.type) {
        case TOK_IDENTIFIER: {
            logDebug("current.type is TOK_IDENTIFIER.");
            Token next = lexer->peekNextToken();
            logDebug("Peeked token (next)", &next);

            if (next.type == TOK_COLON) {
                logDebug("Next token is TOK_COLON. Calling parseVarDeclaration().");
                return parseVarDeclaration();
            }
            // Fall-through if not a typed variable declaration
            logDebug("Identifier not followed by colon. Assuming expression statement.");
            ASTNode* expr = parseExpression();
            logDebug("parseExpression completed. Current token before semicolon check", &current);
            expect(TOK_SEMICOLON, "Expected ';' after expression statement.");
            ASTNode* exprStatementNode = new ASTNode(AST_EXPR_STMT);
            exprStatementNode->children.push_back(expr);
            return exprStatementNode;
        }
        case TOK_PRINT: {
            logDebug("current.type is TOK_PRINT. Calling print statement logic.");
            Token printToken = current;
            advance();
            ASTNode* expr = parseExpression();
            expect(TOK_SEMICOLON, "Expected ';' after print statement.");

            ASTNode* printStatementNode = new ASTNode(AST_PRINT_STMT, printToken);
            printStatementNode->children.push_back(expr);
            return printStatementNode;
        }
        // Add similar debug prints to other cases as they get implemented
        case TOK_NUMBER:
            // Token intToken = current;
            // advance();
            // ASTNode* expr
        case TOK_STRING:
        case TOK_LPAREN: {
            logDebug("current.type is an expression start. Calling parseExpression().");
            ASTNode* expr = parseExpression();
            logDebug("parseExpression completed. Current token before semicolon check", &current);
            expect(TOK_SEMICOLON, "Expected ';' after expression statement.");
            ASTNode* exprStatementNode = new ASTNode(AST_EXPR_STMT);
            exprStatementNode->children.push_back(expr);
            return exprStatementNode;
        }
        default:
            logDebug("Unexpected token at start of statement in default case", &current);
            parseError("Unexpected token at start of statement: " );
            return nullptr;
    }
}

// Parses the entire program, which is a list of statements.
// This is the top-level parsing function you'd call from main.
ASTNode* Parser::parseProgram() {
    // Create the root node for the entire program
    ASTNode* programNode = new ASTNode(AST_PROGRAM);

    // Create a node to hold all statements within the program
    ASTNode* statementList = new ASTNode(AST_STATEMENT_LIST);

    // Continuously parse statements until the end of the input file is reached.
    while (current.type != TOK_END_OF_FILE) {
        ASTNode* statement = parseStatement(); // Parse a single statement
        if (statement) {
            // Add the parsed statement as a child to the statement list
            statementList->children.push_back(statement);
        } else {
            // This 'else' block should ideally not be reached if parseError throws an exception.
            // It acts as a safeguard.
            parseError("Failed to parse a statement within the program.");
            break; // Exit loop on unrecoverable error
        }
    }
    // Add the list of statements as a child of the program node
    programNode->children.push_back(statementList);
    return programNode; // Return the root of the AST
}
