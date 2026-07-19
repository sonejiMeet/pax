#pragma once


inline void printLex(FileBuffer buf, Pool *pool){

    Lexer lexer((const char*)buf.data, buf.size, pool);

    while (true) {
        Token *tok = lexer.nextToken();
        printf("[%-2d:%-2d] %-15s\t", tok->row, tok->col, tokenTypeToString(tok->type));

        switch (tok->type) {
            case TOK_NUMBER:
                printf("Value: \"%llu\"\n", tok->int_value);
                break;
            case TOK_FLOAT:
                printf("Value: \"%.17g\"\n", tok->float64_value);
                break;

                // printf("Value: \"%c\"\n",(char)tok.value);
            case TOK_STRING:
                printf("Value: \"%.*s\"\n", (int)tok->string_value.count, tok->string_value.data);
                break;
            case TOK_IMPORT:
                printf("Value: \"import\" \n");
                break;
            case TOK_MAIN_ENTRY_POINT:
                printf("Value: \"main\"\n");
                break;
            case TOK_IDENTIFIER:
            default:
                // For simple tokens (operators, etc.)
                if (tok->value) {
                    printf("Value: \"%s\"\n", tok->value);
                } else {
                    printf("\n");
                }
                break;
        }

        if (tok->type == TOK_END_OF_FILE) {
            break;
        }
    }

    printf("\n");

}
