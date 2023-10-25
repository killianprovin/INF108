	.text
f1:
	sw	$ra,4($sp)
	li	$t0, 4
	sw	$t0,-4($sp)
	lw	$t0,0($sp)
	sw	$t0,-12($sp)
	lw	$t0,-4($sp)
	sw	$t0,-16($sp)
	lw	$t0,-12($sp)
	lw	$t1,-16($sp)
	mul	$t0,$t0,$t1
	sw	$t0,-8($sp)
	lw	$t0,-8($sp)
	sw	$t0,-4($sp)
	lw	$ra,4($sp)
	jr	$ra
f2:
	sw	$ra,4($sp)
	lw	$t0,0($sp)
	sw	$t0,-8($sp)
	li	$t0, 1
	sw	$t0,-12($sp)
	lw	$t0,-8($sp)
	lw	$t1,-12($sp)
	add	$t0,$t0,$t1
	sw	$t0,-4($sp)
	lw	$t0,Var_z
	sw	$t0,-16($sp)
	lw	$t0,-4($sp)
	sw	$t0,-20($sp)
	lw	$t0,-16($sp)
	lw	$t1,-20($sp)
	mul	$t0,$t0,$t1
	sw	$t0,-12($sp)
	lw	$t0,-4($sp)
	sw	$t0,-16($sp)
	li	$t0, 3
	sw	$t0,-20($sp)
	lw	$t0,-20($sp)
	sw	$t0,-16($sp)
	lw	$t0,-12($sp)
	lw	$t1,-16($sp)
	add	$t0,$t0,$t1
	sw	$t0,-8($sp)
	lw	$t0,-8($sp)
	sw	$t0,-4($sp)
	lw	$ra,4($sp)
	jr	$ra
main:
	li	$v0, 5
	syscall
	sw	$v0,Var_x
	li	$v0, 5
	syscall
	sw	$v0,Var_y
	li	$v0, 5
	syscall
	sw	$v0,Var_z
	sub	$sp,$sp,4
	sub	$sp,$sp,4
	lw	$t0,Var_z
	sw	$t0,0($sp)
	jal	f2
	lw	$t0,-4($sp)
	add	$sp,$sp,4
	sw	$t0,0($sp)
	jal	f1
	lw	$t0,-4($sp)
	add	$sp,$sp,4
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
Var_y: 	.word 0
Var_z: 	.word 0
