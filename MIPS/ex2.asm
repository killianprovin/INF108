.text
main:
	ori $v0, $zero, 11
    ori $a0, $zero, 72
	syscall
    ori $a0, $zero, 101
	syscall
    ori $a0, $zero, 108
	syscall
    ori $a0, $zero, 108
	syscall
    ori $a0, $zero, 111
	syscall
end:
    ori $a0, $zero, 10
    ori $v0, $zero, 11
	syscall
	li $v0, 10
	syscall