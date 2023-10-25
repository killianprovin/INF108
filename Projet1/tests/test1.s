	.text
main:
	li	$v0, 5
	syscall
	sw	$v0,Var_x
	lw	$t0,Var_x
	sw	$t0,0($sp)
	lw	$a0,0($sp)
	li	$v0, 1
	syscall
	li	$a0, 10
	li	$v0, 11
	syscall
end:
	li	$v0, 10
	syscall
	.data
Var_x: 	.word 0
