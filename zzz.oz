call proc_main
    halt
proc_main:
    push_stack_frame 2
    int_const r0, 0
    store 0, r0
    int_const r0, 0
    store 1, r0
    string_const r0, "Type integer n: "
    call_builtin print_string
    call_builtin read_int
    store 0, r0
    load r0, 0
    int_const r1, 0
    load r2, 1
    call proc_bell
    string_const r0, "Bell(n) is: "
    call_builtin print_string
    load r0, 1
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
    pop_stack_frame 2
    return
proc_bell:
    push_stack_frame 5
    int_const r0, 0
    store 3, r0
    int_const r0, 0
    store 4, r0
    load r0, 1
    int_const r1, 0
    cmp_eq_int r0, r0, r1
    branch_on_false r0, bell_label_0
    load r0, 0
    int_const r1, 0
    cmp_eq_int r0, r0, r1
    branch_on_false r0, bell_label_2
    int_const r0, 1
    load_address r1,2
    store_indirect r1, r0
    branch_uncond bell_label_3
bell_label_2:
    int_const r0, 1
    load_address r1,2
    store_indirect r1, r0
bell_label_3:
    branch_uncond bell_label_1
bell_label_0:
    load r0, 0
    int_const r1, 0
    cmp_eq_int r0, r0, r1
    branch_on_false r0, bell_label_4
    int_const r0, 1
    load_address r1,2
    store_indirect r1, r0
    branch_uncond bell_label_5
bell_label_4:
    int_const r0, 1
    load_address r1,2
    store_indirect r1, r0
bell_label_5:
bell_label_1:
    pop_stack_frame 5
    return