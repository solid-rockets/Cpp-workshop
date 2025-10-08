section .data
    msg db "%d", 0xA, 0x0  ; Newline and 0-ended.

section .text
    extern printf
    global main
    main:
        ; Looks like when I use GCC to link stuff...
        ; GCC links my asm object code with another library that already
        ; contains a '_start' symbol, so I need to use the 'main' symbol
        ; instead.

        ; Prepare the stack.
        push rbp
        mov rbp, rsp

        ; Prepare the arguments.
        mov rsi, 123 ; THIS IS WHERE THE INTEGER VALUE IS ENTERED.
        mov rdi, msg

        ; Make the call.
        call printf

        mov eax, 0x0
        pop rbp

        ; Finish the program.
        ret
