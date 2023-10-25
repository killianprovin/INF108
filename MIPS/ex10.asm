.text
fibo:

    beq $a0, 1, fin
    beq $a0, 0, fin

    addi $sp, $sp, -8
    sw $ra, 0($sp)

    add $a0, $a0, -1
    jal fibo
    sw $v0, 4($sp)

    add $a0, $a0, -1
    jal fibo
    lw $t1, 4($sp) 
    add $v0, $v0, $t1
    add $a0, $a0, 2

    lw $ra, 0($sp)
    addi $sp, $sp, 8
    jr $ra

    fin:
        add $v0, $zero, 1
        jr $ra

main:
    ori $v0, $zero, 5
    syscall
    move $a0 $v0

    jal fibo

    move $a0, $v0
    li $v0 1
    syscall

end:
    ori $a0, $zero, 10
    ori $v0, $zero, 11
	syscall
	li $v0, 10
	syscall