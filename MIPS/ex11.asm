.text

exp:
    
    beq $s1 0 casbase

    addi $sp $sp -4
    sw $ra 0($sp)

    and $t0 $s1 1
    beq $t0 1 impaire

    paire:
        div $s1 $s1 2
        jal exp
        mul $v0 $v0 $v0
        j finexp
    impaire:
        add $s1 $s1 -1
        div $s1 $s1 2
        jal exp
        mul $v0 $v0 $v0
        mul $v0 $v0 $s0

    finexp:
        lw $ra 0($sp)
        addi $sp $sp 4
        jr $ra

    casbase:
        li $v0 1
        jr $ra

main:
    li $v0 5
    syscall
    move $s0 $v0
    li $v0 5
    syscall
    move $s1 $v0
    li $v0 5
    syscall
    move $s2 $v0


    jal exp

    move $a0 $v0
    li $v0 1
    syscall
end:
    ori $a0 $zero 10
    ori $v0 $zero 11
	syscall
	li $v0 10
	syscall