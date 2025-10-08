section .data
    ; Well, then only explanation I have is that both 'msg' and 'msgLen'
    ; both represent numeric values of different types.
    ; 'msg' holds the address of the string that follows its definition.
    ; 'msgLen' holds the integer length of the string defined after 'msg'.

    global msg 
    msg     db "Hello, world!", 0xA

    global msgLen
    msgLen  equ $-msg
    
