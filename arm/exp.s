.data
string: .asciz "The number is: %d\n"
.text
.global main
.extern printf
main:
       push {ip, lr}
       ldr r0, =string
       mov r1, #1029
       bl printf
       pop {ip, pc}
