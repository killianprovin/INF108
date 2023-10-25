.text
main:
    ori $v0, $zero, 5
    syscall
    addi $t0, $v0, -1
    add $t1, $zero, 1
    add $t2, $zero, 1

    loop:
        add $t3, $t1, $t2
        add $t1, $zero, $t2
        add $t2, $zero, $t3
        add $t0, $t0, -1
        bgtz $t0 loop
    endloop:
    add $a0, $zero, $t1
    ori $v0, $zero, 1
    syscall
end:
    ori $a0, $zero, 10
    ori $v0, $zero, 11
	syscall
	li $v0, 10
	syscall