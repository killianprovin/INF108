.data
myStr: .asciiz "Hello World!"
.text
main:
	la $a0, myStr
	ori $v0, $zero, 4
	syscall
end:
    ori $a0, $zero, 10
    ori $v0, $zero, 11
	syscall
	li $v0, 10
	syscall