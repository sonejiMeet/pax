#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#define NULL_DEVICE "nul"
#define BIN_PATH    ".\\src\\pax.exe"
#define TESTS_DIR   ".\\tests\\"
#define RUN_PREFIX  ".\\"
#else
#define NULL_DEVICE "/dev/null"
#define BIN_PATH    "./src/pax.exe"
#define TESTS_DIR   "./tests/"
#define RUN_PREFIX  "./"
#endif

#define RED   "\x1b[91m"
#define GREEN "\x1b[92m"
#define RESET "\x1b[0m"

typedef struct {
    const char *name;
    int compile_should_succeed;
    int run_executable;
} Test;

#define PASS(name)         { #name, 1, 1 }
#define COMPILE_ONLY(name) { #name, 1, 0 }
#define FAIL(name)         { #name, 0, 0 }

static const Test tests[] = {
    PASS(demo),
    PASS(demo_struct),
    PASS(demo_nested_structs),
    PASS(demo_recursion),
    PASS(demo_struct_order),
    PASS(demo_memory),
    PASS(demo_static_array),
    PASS(demo_if_and_while),
    PASS(demo_lists),
    PASS(demo_struct_init_from_global),
    PASS(demo_array_subscript_dot),
    PASS(demo_dereference_assigned_value),
    PASS(demo_pass_initialized_ptr_member_passed_by_value),
    PASS(demo_pass_is_declaration_passed_through_function),
    PASS(demo_struct_members_should_get_inferred),
    PASS(demo_pointer_get_same_instance),
    PASS(demo_pointer_get_new_instance),
    PASS(demo_copy_pointer),
    PASS(demo_nested_init),
    PASS(demo_pass_nested_structs_with_members_as_pointers),
    PASS(demo_simple_nested_structs),
    PASS(demo_string),
    PASS(demo_pointer_cast_byte),

    COMPILE_ONLY(demo_runtime_crash_handler),
    PASS(demo_named_defaults_parameter),
    PASS(demo_multi_return),
    PASS(demo_defer),
    PASS(demo_for),
    PASS(demo_sort),
    PASS(demo_binary_search),
    PASS(demo_compound_assignment),

    PASS(a),
    PASS(demo_multi_variable),
    PASS(c),
    PASS(d),
    PASS(e),
    PASS(f),

    FAIL(demo_fail_uninitialized_ptr_member_passed_by_value),
    FAIL(demo_fail_is_declaration_passed_through_function),
    FAIL(demo_fail_nested_init),
    FAIL(demo_fail_nested_structs_with_members_as_pointers),
    FAIL(demo_fail_named_defaults_parameter),
    FAIL(demo_fail_named_defaults_parameter_2),
};

static void show_output(const char *path) {
    char buffer[4096];
    size_t count;
    FILE *file = fopen(path, "rb");

    if (!file)
        return;

    while ((count = fread(buffer, 1, sizeof(buffer), file)) != 0)
        fwrite(buffer, 1, count, stderr);

    fclose(file);
    printf("\n");
}

/* Pre-create the log so a shell-redirect failure can't kill the test
   silently. */
static int prepare_log(const char *path) {
    for (int attempt = 0; attempt < 3; attempt++) {
        FILE *f = fopen(path, "wb");
        if (f) {
            fclose(f);
            return 1;
        }
    }
    return 0;
}

int main(int argc, char **argv) {
    char command[2048];
    char log[256];
    char exe[256];

    int total = 0;
    int passed = 0;
    int failed = 0;

    for (int i = 0; i < (int)(sizeof(tests) / sizeof(*tests)); i++) {
        const Test *test = &tests[i];
        int compile_ok;
        int run_ok;

        if (argc > 1 && strcmp(argv[1], test->name) != 0)
            continue;

        total++;

        snprintf(log, sizeof(log), "tmp_run_tests_%s.log", test->name);
        snprintf(exe, sizeof(exe), "%s.exe", test->name);
        remove(exe);

        printf("[Compiling] src/pax.exe tests/%s.pax -mem -debug -verbose", test->name);
        fflush(stdout);

        if (!prepare_log(log)) {
            printf(RED " FAIL" RESET " (cannot create log)\n");
            failed++;
            continue;
        }

        snprintf(
            command,
            sizeof(command),
            "%s -verbose -debug -mem %s%s.pax > \"%s\" 2>&1",
            BIN_PATH,
            TESTS_DIR,
            test->name,
            log
        );

        compile_ok = system(command) == 0;

        if (test->compile_should_succeed) {
            FILE *probe = fopen(exe, "rb");
            compile_ok = compile_ok && probe != NULL;
            if (probe)
                fclose(probe);
        } else
            compile_ok = !compile_ok;

        printf(compile_ok ? GREEN " OK" RESET "\n" : RED " FAIL" RESET "\n");

        if (!compile_ok && test->compile_should_succeed)
            show_output(log);

        remove(log);

        if (!compile_ok) {
            failed++;
            continue;
        }

        if (!test->run_executable) {
            passed++;
            continue;
        }

        printf("[Running]   tests/%s", test->name);
        fflush(stdout);

        snprintf(command, sizeof(command), "%s%s.exe > %s 2>&1", RUN_PREFIX, test->name, NULL_DEVICE);
        run_ok = system(command) == 0;

        printf(run_ok ? GREEN " OK" RESET "\n" : RED " FAIL" RESET "\n");

        if (run_ok)
            passed++;
        else
            failed++;
    }

    printf(
        "\nTotal test count: %d\n"
        "Passed: %d\n"
        "Failed: %d\n",
        total,
        passed,
        failed
    );

    return failed != 0;
}
