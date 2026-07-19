
struct CLI {
    const char *input_file;
    bool        print_lex;
    bool        time;
    bool        memory_profiler;
    bool        help;
};

static void cli_usage(const char *prog) {
    printf("\nUsage :  pax file.pax [option] \n"
    	   "Option: \n"
           "        -time,     show how long each step takes\n"
           "        -profile,  enable memory tracer (outputs a text file)\n"
           "        -lex,      print lexer tokens\n"
           "        -h,        print help\n\n");
}

static bool cli_parse(CLI *cli, int argc, char **argv) {
    cli->input_file      = nullptr;
    cli->print_lex       = false;
    cli->time            = false;
    cli->memory_profiler = false;
    cli->help            = false;

    for (int i = 1; i < argc; i++) {
        const char *a = argv[i];

        if (strcmp(a, "-lex") == 0) {
            cli->print_lex = true;
        } else if (strcmp(a, "-time") == 0) {
            cli->time = true;
        } else if (strcmp(a, "-profile") == 0) {
            cli->memory_profiler = true;
        } else if (strcmp(a, "-h") == 0 || strcmp(a, "--help") == 0) {
            cli->help = true;
        } else if (a[0] == '-') {
            printf("Unknown option: %s\n", a);
            return false;
        } else if (cli->input_file) {
            printf("Unexpected argument: %s\n", a);
            cli_usage(argv[0]);
            return false;
        } else {
            cli->input_file = a;
        }
    }

    if (cli->help) {
        cli_usage(argv[0]);
        return false;
    }
    if (!cli->input_file) {
        cli_usage(argv[0]);
        return false;
    }
    return true;
}