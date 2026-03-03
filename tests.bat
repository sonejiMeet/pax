@echo off
setlocal enabledelayedexpansion

set FAIL=0
set FAIL_LIST=
set TOTAL_TEST=0

for %%f in (
    demo.pax
    demo_struct.pax
    demo_nested_structs.pax
    demo_recursion.pax
    demo_struct_order.pax

    demo_memory.pax
    demo_static_array.pax
    demo_if_and_while.pax
    demo_lists.pax
    demo_struct_init_from_global.pax
    demo_array_subscript_dot.pax

    demo_dereference_assigned_value.pax

    demo_pass_initialized_ptr_member_passed_by_value.pax

    demo_pass_is_declaration_passed_through_function.pax

    demo_struct_members_should_get_inferred.pax
    demo_pointer_get_same_instance.pax
    demo_pointer_get_new_instance.pax
    demo_copy_pointer.pax
    demo_nested_init.pax

    demo_pass_nested_structs_with_members_as_pointers.pax

    demo_simple_nested_structs.pax
    demo_string.pax
) do (
    set /a TOTAL_TEST+=1
    echo Testing %%f...
    set LOG=tmp_%%~nf.log

    .\src\pax.exe .\tests\%%f > "!LOG!" 2>&1

    if errorlevel 3 (
        echo   FAILED
        set FAIL=1
        set FAIL_LIST=!FAIL_LIST! %%f
    ) else (
        del "!LOG!" >nul 2>&1
    )
)

echo.
for %%f in (
    demo_fail_uninitialized_ptr_member_passed_by_value.pax

    demo_fail_is_declaration_passed_through_function.pax

    demo_fail_nested_init.pax
    demo_fail_nested_structs_with_members_as_pointers.pax
) do (
    set /a TOTAL_TEST+=1
    echo Testing %%f...
    set LOG=tmp_%%~nf.log

    .\src\pax.exe .\tests\%%f > "!LOG!" 2>&1

    if not errorlevel 1 (
        echo   FAILED ^(demo_fail passed but should have failed^)
        set FAIL=1
        set FAIL_LIST=!FAIL_LIST! %%f
    )
)

echo.
if %FAIL%==0 (
    echo ALL TESTS PASSED
    echo Total test count %TOTAL_TEST%
) else (
    for %%f in (%FAIL_LIST%) do type "tmp_%%~nf.log"
)
exit /b %FAIL%
