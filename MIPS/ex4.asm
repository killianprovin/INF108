.text
main:
	ori $v0, $zero, 5
    ori $a0, $zero, 35
    syscall
    addu $t1, $zero, $v0
	ori $v0, $zero, 11
    loop:
        syscall
        add $t1, $t1, -1
        bgtz $t1 loop
    endloop:
end:
    ori $a0, $zero, 10
    ori $v0, $zero, 11
	syscall
	li $v0, 10
	syscall