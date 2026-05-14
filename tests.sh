#!/usr/bin/env bash

FAIL=0
FAIL_LIST=()
TOTAL_TEST=0

RED='\033[91m'
RESET='\033[0m'

pass_tests=(
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
  demo_pointer_cast_byte.pax

  demo_runtime_crash_handler.pax
)

fail_tests=(
  demo_fail_uninitialized_ptr_member_passed_by_value.pax
  demo_fail_is_declaration_passed_through_function.pax
  demo_fail_nested_init.pax
  demo_fail_nested_structs_with_members_as_pointers.pax
)

for f in "${pass_tests[@]}"; do
  TOTAL_TEST=$((TOTAL_TEST + 1))
  log="tmp_${f%.pax}.log"

  ./src/pax.exe ./tests/"$f" >"$log" 2>&1
  status=$?

  echo "Testing $f... $status"

  if [ "$status" -ne 0 ]; then
    printf "${RED}    Failed${RESET}\n"
    FAIL=1
    FAIL_LIST+=("$f")
  else
    rm -f "$log"
  fi
done

echo

for f in "${fail_tests[@]}"; do
  TOTAL_TEST=$((TOTAL_TEST + 1))
  log="tmp_${f%.pax}.log"

  ./src/pax.exe ./tests/"$f" >"$log" 2>&1
  status=$?

  echo "Testing $f... $status"

  if [ "$status" -eq 0 ]; then
    printf "${RED}   FAILED (demo_fail passed but should have failed) ${RESET}\n"
    FAIL=1
    FAIL_LIST+=("$f")
  fi
done

echo
if [ "$FAIL" -eq 0 ]; then
  echo "ALL TESTS PASSED"
  echo "Total test count $TOTAL_TEST"
else
  printf "${RED}FAILED TESTS${RESET}\n"
  for f in "${FAIL_LIST[@]}"; do
    cat "tmp_${f%.pax}.log"
  done
fi

exit "$FAIL"
