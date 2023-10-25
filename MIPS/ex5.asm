.text
main:
	ori $v0, $zero, 5
    syscall
    add $t0, $zero, $v0
	ori $v0, $zero, 11
    loop:
        add $t1, $zero, $t0
        loopline:
            ori $a0, $zero, 35
            syscall
            add $t1, $t1, -1
            bgtz $t1 loopline
        endloopline:
        ori $a0, $zero, 10
        syscall
        add $t0, $t0, -1
        bgtz $t0 loop
    endloop:
end:
    ori $a0, $zero, 10
    ori $v0, $zero, 11
	syscall
	li $v0, 10
	syscall