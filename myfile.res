Symbol table level 0
 35430416        real  BASIC  basicdt   1          siz     8
 35430512     integer  BASIC  basicdt   0          siz     4
 35430608        char  BASIC  basicdt   2          siz     1
 35430704     boolean  BASIC  basicdt   3          siz     4
 35430800         exp  knd 5 0  typ 35430896  lvl  0  siz     0  off     0
(FUNCTION real
          real)
 35431088       trexp  knd 5 0  typ 35431184  lvl  0  siz     0  off     0
(FUNCTION real
          real)
 35431376         sin  knd 5 0  typ 35431472  lvl  0  siz     0  off     0
(FUNCTION real
          real)
 35431664         cos  knd 5 0  typ 35431760  lvl  0  siz     0  off     0
(FUNCTION real
          real)
 35431952       trsin  knd 5 0  typ 35432048  lvl  0  siz     0  off     0
(FUNCTION real
          real)
 35432240        sqrt  knd 5 0  typ 35432336  lvl  0  siz     0  off     0
(FUNCTION real
          real)
 35432528       round  knd 5 0  typ 35432624  lvl  0  siz     0  off     0
(FUNCTION real
          real)
 35432816      iround  knd 5 0  typ 35432912  lvl  0  siz     0  off     0
(FUNCTION integer
          real)
 35433104         ord  knd 5 0  typ 35433200  lvl  0  siz     0  off     0
(FUNCTION integer
          integer)
 35433392         new  knd 5 0  typ 35433488  lvl  0  siz     0  off     0
(FUNCTION integer
          integer)
 35433680       trnew  knd 5 0  typ 35433776  lvl  0  siz     0  off     0
(FUNCTION integer
          integer)
 35433968       write  knd 5 0  typ 35434064  lvl  0  siz     0  off     0
(FUNCTION NULL
          char)
 35434256     writeln  knd 5 0  typ 35434352  lvl  0  siz     0  off     0
(FUNCTION NULL
          char)
 35434544      writef  knd 5 0  typ 35434640  lvl  0  siz     0  off     0
(FUNCTION NULL
          real)
 35434832    writelnf  knd 5 0  typ 35434928  lvl  0  siz     0  off     0
(FUNCTION NULL
          real)
 35435120      writei  knd 5 0  typ 35435216  lvl  0  siz     0  off     0
(FUNCTION NULL
          integer)
 35435408    writelni  knd 5 0  typ 35435504  lvl  0  siz     0  off     0
(FUNCTION NULL
          integer)
 35435696        read  knd 5 0  typ 35435792  lvl  0  siz     0  off     0
(FUNCTION NULL
          NULL)
 35435984      readln  knd 5 0  typ 35436080  lvl  0  siz     0  off     0
(FUNCTION NULL
          NULL)
 35436272         eof  knd 5 0  typ 35436368  lvl  0  siz     0  off     0
(FUNCTION boolean
          NULL)
Symbol table level 1
 35519456           i  VAR    0 typ integer  lvl  1  siz     4  off     0
 35519552         lim  VAR    0 typ integer  lvl  1  siz     4  off     4
yyparse result =        0
(program graph1 (progn output)
                (progn (:= lim 7)
                       (progn (:= i 0)
                              (label 0)
                              (if (<= i lim)
                                  (progn (funcall writeln '*')
                                         (:= i (+ i 1))
                                         (goto 0))))))
# ---------------- Beginning of Generated Code --------------------
        .file   "foo"
        .text
.globl graph1
        .type   graph1, @function
graph1:
.LFB0:
	.cfi_startproc
	pushq	%rbp              # save base pointer on stack
	.cfi_def_cfa_offset 16
	movq	%rsp, %rbp        # move stack pointer to base pointer
	.cfi_offset 6, -16
	.cfi_def_cfa_register 6
        subq	$32, %rsp 	  # make space for this stack frame
	movq	%rbx, %r9        # save %rbx (callee-saved) in %r9
# ------------------------- begin Your code -----------------------------
	movl	$7,%eax         	#  7 -> %eax
	movl	%eax,-28(%rbp)     	#  %eax -> lim
	movl	$0,%eax         	#  0 -> %eax
	movl	%eax,-32(%rbp)     	#  %eax -> i
.L0:
	movl	-32(%rbp),%eax     	#  i -> %eax
	movl	-28(%rbp),%ecx     	#  lim -> %ecx
	cmpl	%ecx,%eax           	#  compare %eax - %ecx
	jle	.L2 			#  jump if     <=
	jmp	.L3 			#  jump 
.L2:
	movl	$.LC4,%eax       	#  addr of literal .LC4
	call	writeln              	#  writeln()
	movl	-32(%rbp),%eax     	#  i -> %eax
	movl	$1,%ecx         	#  1 -> %ecx
	addl	%ecx,%eax         	#  %eax + %ecx -> %eax
	movl	%eax,-32(%rbp)     	#  %eax -> i
	jmp	.L0 			#  jump 
.L3:
# ----------------------- begin Epilogue code ---------------------------
	movq	%r9, %rbx        # restore %rbx (callee-saved) from %r9
        leave
        ret
        .cfi_endproc
.LFE0:
        .size   graph1, .-graph1
# ----------------- end Epilogue; Literal data follows ------------------
        .section        .rodata
	.align  4
.LC4:
	.string	"*"

        .ident  "CS 375 Compiler - Spring 2017"
