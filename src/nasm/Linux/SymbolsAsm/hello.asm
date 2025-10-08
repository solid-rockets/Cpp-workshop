section .data
    extern msg
    extern msgLen

section .text
    global main
    main:
        ; Write message.
        mov eax, 0x04 ; Write.
        mov ebx, 0
        mov ecx, msg
        mov edx, msgLen
        int 0x80

        ; Exit program.
        mov eax, 0x01 ; Exit.
        int 0x80
