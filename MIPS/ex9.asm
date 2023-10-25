.text
fact:
    ble $t0 1 end
    mul $t1, $t1, $t0
    add $t0, $t0, -1
    jal fact
    jr $ra
main:
    ori $v0, $zero, 5
    syscall
    add $t0, $zero, $v0
    ori $t1, $zero, 1
    jal fact
end:
    ori $v0, $zero, 1
    move $a0, $t1
    syscall
    ori $a0, $zero, 10
    ori $v0, $zero, 11
	syscall
	li $v0, 10
	syscall