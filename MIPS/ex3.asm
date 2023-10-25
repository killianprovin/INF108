.text
main:
	ori $v0, $zero, 5
    syscall
    addu $a0, $v0, $v0
	ori $v0, $zero, 1
    syscall
end:
    ori $a0, $zero, 10
    ori $v0, $zero, 11
	syscall
	li $v0, 10
	syscall