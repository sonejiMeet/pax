@echo off
setlocal enabledelayedexpansion

rem escape code for coloring
for /f "tokens=*" %%e in ('echo prompt $E^| cmd') do set "ESC=%%e"

set FAIL=0
set FAIL_LIST=
set TOTAL_TEST=0

for %%f in (
    demo
    demo_struct
    demo_nested_structs
    demo_recursion
    demo_struct_order

    demo_memory
    demo_static_array
    demo_if_and_while
    demo_lists
    demo_struct_init_from_global
    demo_array_subscript_dot

    demo_dereference_assigned_value

    demo_pass_initialized_ptr_member_passed_by_value

    demo_pass_is_declaration_passed_through_function

    demo_struct_members_should_get_inferred
    demo_pointer_get_same_instance
    demo_pointer_get_new_instance
    demo_copy_pointer
    demo_nested_init

    demo_pass_nested_structs_with_members_as_pointers

    demo_simple_nested_structs

    demo_string
    demo_pointer_cast_byte

    demo_runtime_crash_handler

    demo_named_defaults_parameter

    demo_multi_return
    demo_defer
    demo_for

    demo_sort
    demo_binary_search
) do (
    set /a TOTAL_TEST+=1
    set LOG=tmp_%%~nf.log

    .\%%f.exe
    echo Testing %%f... !errorlevel!

    if !errorlevel! NEQ 0 (
        echo %ESC%[91m    Failed%ESC%[0m

        set FAIL=1
        set FAIL_LIST=!FAIL_LIST! %%f
    ) else (
        del "!LOG!" >nul 2>&1
    )
)

echo.
if %FAIL%==0 (
    echo ALL TESTS PASSED
    echo Total test count %TOTAL_TEST%
) else (
    echo %ESC%[91mFAILED TESTS%ESC%[0m
    for %%f in (%FAIL_LIST%) do type "tmp_%%~nf.log"
)
exit /b %FAIL%
