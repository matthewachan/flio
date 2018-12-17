	.text
	.file	"flio"
	.globl	addLineNumbers          # -- Begin function addLineNumbers
	.p2align	4, 0x90
	.type	addLineNumbers,@function
addLineNumbers:                         # @addLineNumbers
	.cfi_startproc
# %bb.0:                                # %entry
	subq	$56, %rsp
	.cfi_def_cfa_offset 64
	movq	%rdi, 24(%rsp)
	movl	$.Lmode, %esi
	xorl	%eax, %eax
	callq	fopen
	movq	%rax, 16(%rsp)
	movq	24(%rsp), %rsi
	movl	$.Lstrptr.1, %edi
	callq	concat
	movq	%rax, 48(%rsp)
	movq	24(%rsp), %rdi
	movq	%rax, %rsi
	callq	copy
	movq	48(%rsp), %rdi
	movl	$.Lmode.2, %esi
	xorl	%eax, %eax
	callq	fopen
	movq	%rax, 40(%rsp)
	movq	16(%rsp), %rdi
	callq	readLine
	movq	%rax, 8(%rsp)
	movl	$0, 4(%rsp)
	jmp	.LBB0_1
	.p2align	4, 0x90
.LBB0_2:                                # %for_body
                                        #   in Loop: Header=BB0_1 Depth=1
	movl	4(%rsp), %edi
	callq	intToStr
	movl	$.Lstrptr.3, %edi
	movq	%rax, %rsi
	callq	concat
	movq	%rax, 32(%rsp)
	movl	$.Lstrptr.4, %esi
	movq	%rax, %rdi
	callq	concat
	movq	%rax, 32(%rsp)
	movq	8(%rsp), %rsi
	movq	%rax, %rdi
	callq	concat
	movq	40(%rsp), %rdi
	movq	%rax, %rsi
	callq	bwrite
	movq	16(%rsp), %rdi
	callq	readLine
	movq	%rax, 8(%rsp)
	incl	4(%rsp)
.LBB0_1:                                # %for
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rsp), %rdi
	movl	$.Lstrptr.5, %esi
	callq	strcmp
	testl	%eax, %eax
	jne	.LBB0_2
# %bb.3:                                # %merge
	addq	$56, %rsp
	retq
.Lfunc_end0:
	.size	addLineNumbers, .Lfunc_end0-addLineNumbers
	.cfi_endproc
                                        # -- End function
	.globl	main                    # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$.Lstrptr, %edi
	callq	addLineNumbers
	xorl	%eax, %eax
	popq	%rcx
	retq
.Lfunc_end1:
	.size	main, .Lfunc_end1-main
	.cfi_endproc
                                        # -- End function
	.type	.Lstrptr,@object        # @strptr
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lstrptr:
	.asciz	"sample.txt"
	.size	.Lstrptr, 11

	.type	.Lmode,@object          # @mode
.Lmode:
	.asciz	"r+"
	.size	.Lmode, 3

	.type	.Lstrptr.1,@object      # @strptr.1
.Lstrptr.1:
	.asciz	"lined_"
	.size	.Lstrptr.1, 7

	.type	.Lmode.2,@object        # @mode.2
.Lmode.2:
	.asciz	"r+"
	.size	.Lmode.2, 3

	.type	.Lstrptr.3,@object      # @strptr.3
.Lstrptr.3:
	.asciz	"["
	.size	.Lstrptr.3, 2

	.type	.Lstrptr.4,@object      # @strptr.4
.Lstrptr.4:
	.asciz	"] "
	.size	.Lstrptr.4, 3

	.type	.Lstrptr.5,@object      # @strptr.5
.Lstrptr.5:
	.zero	1
	.size	.Lstrptr.5, 1


	.section	".note.GNU-stack","",@progbits
