#include "parser.h"
#include "token.h"
#include <iostream>
#include <vector>

//CAC6B3FF
// F6F42EFF

Parser::Parser(Lexer* l) : lexer(l) {
    current = lexer->nextToken();

    // (TEMP) for debugging
    // tempTokens.push_back(current);
}

void Parser::advance() {

    current = lexer->nextToken();

    // (TEMP) for debugging
    // tempTokens.push_back(current);
}

// Reports parsing errors and terminates execution.
void Parser::parseError(const std::string& message) {
    // Token prevToken =
    std::cerr << "Parsing Error: " <<  "Line[" << current.row << ":" << current.col << "] "  << message
              << " at token '" << current.value << "' (Type: "
              << tokenTypeToString(current.type) << ")" << std::endl;
    // throw std::runtime_error("Parsing failed due to syntax error.");
}

// Consumes an expected token, or reports an error if mismatch.
void Parser::expect(TokenType expectedType, const std::string& errorMessage)
{
    if (current.type != expectedType) {
        // fix semicolon checker, current token is wrong.
        parseError(errorMessage);
    }
    advance();
}

// Parses a factor in an expression (numbers, identifiers, parenthesized expressions).
ASTNode* Parser::parseFactor()
{
    if (current.type == TOK_NUMBER){
        ASTNode* node = new ASTNode(AST_NUMBER_LITERAL, new Token(current));
        advance();
        return node;
    } else if (current.type == TOK_FLOAT) {
        ASTNode* node = new ASTNode(AST_FLOAT_LITERAL, new Token(current));
        advance();
        return node;
    }else if (current.type == TOK_IDENTIFIER){
        ASTNode* node = new ASTNode(AST_IDENTIFIER, new Token(current)); // Use AST_IDENTIFIER for expression identifiers
        advance();
        return node;
    } else if (current.type == TOK_LPAREN){
        expect(TOK_LPAREN, "Expected '(' for parenthesized expression.");
        ASTNode* expr = parseExpression();
        expect(TOK_RPAREN, "Expected ')' after expression in parentheses.");
        // Create a new AST node to explicitly represent the parentheses
        ASTNode* paren_node = new ASTNode(AST_PARENTHESIZED_EXPR); // No associated token needed, it's structural
        paren_node->children.push_back(expr); // The expression is its child
        return paren_node;
    } else if (current.type == TOK_STRING ){
        ASTNode* node = new ASTNode(AST_STRING_LITERAL, new Token(current));
        advance();
        return node;
    }

    parseError("Expected a number, identifier, or '(' for expression factor.");
    return nullptr; // Unreachable if parseError throws
}

// Parses a term (multiplication and division operations).
ASTNode* Parser::parseTerm()
{
    ASTNode* left = parseFactor();

    while (current.type == TOK_STAR || current.type == TOK_SLASH) {
        Token op = current;
        advance();
        ASTNode* right = parseFactor();

        ASTNode* node = new ASTNode(AST_BINARY_EXPR, new Token(op));
        node->children.push_back(left);
        node->children.push_back(right);

        left = node;
    }
    return left;
}

// Parses an expression (addition, subtraction, and comparison operations).
ASTNode* Parser::parseExpression()
{
    ASTNode* left = parseTerm();

    while (current.type == TOK_PLUS || current.type == TOK_MINUS ||
           current.type == TOK_EQUAL || current.type == TOK_NOT_EQUAL ||
           current.type == TOK_LESS || current.type == TOK_GREATER ||
           current.type == TOK_LESS_EQUAL || current.type == TOK_GREATER_EQUAL) {

        Token op = current;
        advance();
        ASTNode* right = parseTerm();

        ASTNode* node = new ASTNode(AST_BINARY_EXPR, new Token(op));
        node->children.push_back(left);
        node->children.push_back(right);

        left = node;
    }
    return left;
}

ASTNode* Parser::parseTypeSpecifier()
{
    // Expect a type keyword
    if (!(current.type == TOK_TYPE_INT || current.type == TOK_TYPE_FLOAT ||
          current.type == TOK_TYPE_STRING || current.type == TOK_TYPE_BOOL)) {
        parseError("Expected a type keyword (e.g., 'int', 'float', 'string', 'bool').");
    }
    Token typeToken = current; // Capture the type token (e.g., TOK_TYPE_INT)
    advance(); // Consume the type keyword

    // Create an AST_TYPE_SPECIFIER node, associating it with the type token
    return new ASTNode(AST_TYPE_SPECIFIER, new Token(typeToken));
}

// Parses a variable declaration statement (e.g., `identifier = expression;`).
ASTNode* Parser::parseVarDeclaration()
{
    if (current.type != TOK_IDENTIFIER) {
        parseError("Expected identifier for variable declaration.");
    }
    Token varNameToken = current;
    advance();

    expect(TOK_COLON, "Expected ':' after variable name for type declaration.");

    // 3. Parse the type specifier (e.g., 'int', 'float')
    ASTNode* typeSpecifierNode = parseTypeSpecifier();

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

    ASTNode* varDeclNode = new ASTNode(AST_VAR_DECL, new Token(varNameToken)); // Using AST_VAR_DECL
    varDeclNode->children.push_back(typeSpecifierNode);

    if (initializerExpr) {
        varDeclNode->children.push_back(initializerExpr); // Child 1 (optional): Initializer Expression
    }

    return varDeclNode;
}

ASTNode* Parser::parseIfStatement(){

    Token ifToken = current;
    advance();
    expect(TOK_LPAREN, "Expected '(' before if statement.");
    ASTNode* condition = parseExpression();
    expect(TOK_RPAREN, "Expected ')' after if statement.");

    ASTNode* thenBranch = parseStatement();

    ASTNode* ifStatementNode = new ASTNode(AST_IF_STMT, new Token(ifToken));
    ifStatementNode->children.push_back(condition);
    ifStatementNode->children.push_back(thenBranch);


    return ifStatementNode;

}

// Parses a block of statements enclosed in curly braces: '{ StatementList }'
ASTNode* Parser::parseBlockStatement() {
    logDebug("Entering parseBlockStatement(). Current token", &current);
    expect(TOK_LCURLY_PAREN, "Expected '{' to start a block statement.");

    ASTNode* blockNode = new ASTNode(AST_BLOCK_STMT);
    ASTNode* statementList = new ASTNode(AST_STATEMENT_LIST); // Block contains a list of statements

    // Parse statements until a closing curly brace or EOF
    while (current.type != TOK_RCURLY_PAREN && current.type != TOK_END_OF_FILE) {
        ASTNode* statement = parseStatement(); // Recursively parse statements within the block
        if (statement) {
            statementList->children.push_back(statement);
        } else {
            // Error or unexpected token within the block
            parseError("Failed to parse statement within block.");
            break;
        }
    }
    expect(TOK_RCURLY_PAREN, "Expected '}' to close a block statement.");
    logDebug("Successfully parsed block statement. Current token after '}'", &current);

    blockNode->children.push_back(statementList);
    return blockNode;
}


void Parser::logDebug(const std::string& message, const Token* token = nullptr) const
{
    std::cout << "DEBUG: " << message;
    if (token) {
        std::cout << " Token: " << tokenTypeToString(token->type)
                  << " ('" << token->value << "')"
                  << " Line: " << token->row << ", Col: " << token->col;
    }
    std::cout << std::endl;
}

ASTNode* Parser::parseStatement()
{

    switch (current.type) {
        case TOK_IDENTIFIER: {
            Token next = lexer->peekNextToken();

            if (next.type == TOK_COLON) {
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
            Token printToken = current;
            advance();
            ASTNode* expr = parseExpression();
            expect(TOK_SEMICOLON, "Expected ';' after print statement.");

            ASTNode* printStatementNode = new ASTNode(AST_PRINT_STMT, new Token(printToken));
            printStatementNode->children.push_back(expr);

            return printStatementNode;
        }
        // Add similar debug prints to other cases as they get implemented
        case TOK_NUMBER:
        case TOK_STRING:
        case TOK_IF:
            return parseIfStatement();
        case TOK_LCURLY_PAREN:
            return parseBlockStatement();
        case TOK_LPAREN: {
            ASTNode* expr = parseExpression();
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


ASTNode* Parser::parseProgram()
{
    ASTNode* programNode = new ASTNode(AST_PROGRAM);

    ASTNode* statementList = new ASTNode(AST_STATEMENT_LIST);

    while (current.type != TOK_END_OF_FILE)
    {
        ASTNode* statement = parseStatement();
        if (statement){
            statementList->children.push_back(statement);
        } else {
            parseError("Failed to parse a statement within the program.");
            break;
        }
    }
    programNode->children.push_back(statementList);
    return programNode;
}


