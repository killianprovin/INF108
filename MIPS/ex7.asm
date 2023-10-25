.text
syracuse:
    move $a0 $t0
    ori $v0, $zero, 1
	syscall
    ori $a0, $zero, 10
    ori $v0, $zero, 11
	syscall
    beq $t0 1 end
    and $t1 $t0 1
    beq $t1 1 impaire 
    beq $t1 0 paire
    
    impaire:
        mul $t0, $t0, 3
        add $t0, $t0, 1
        j syracuse
    paire:
        div $t0, $t0, 2
        j syracuse
main:
    ori $v0, $zero, 5
    syscall
    add $t0, $zero, $v0
    j syracuse
end:
    ori $a0, $zero, 10
    ori $v0, $zero, 11
	syscall
	li $v0, 10
	syscall