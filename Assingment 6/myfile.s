Symbol table level 0
 26218512        real  BASIC  basicdt   1          siz     8
 26218608     integer  BASIC  basicdt   0          siz     4
 26218704        char  BASIC  basicdt   2          siz     1
 26218800     boolean  BASIC  basicdt   3          siz     4
 26218896         exp  knd 5 0  typ 26218992  lvl  0  siz     0  off     0
(FUNCTION real
          real)
 26219184       trexp  knd 5 0  typ 26219280  lvl  0  siz     0  off     0
(FUNCTION real
          real)
 26219472         sin  knd 5 0  typ 26219568  lvl  0  siz     0  off     0
(FUNCTION real
          real)
 26219760         cos  knd 5 0  typ 26219856  lvl  0  siz     0  off     0
(FUNCTION real
          real)
 26220048       trsin  knd 5 0  typ 26220144  lvl  0  siz     0  off     0
(FUNCTION real
          real)
 26220336        sqrt  knd 5 0  typ 26220432  lvl  0  siz     0  off     0
(FUNCTION real
          real)
 26220624       round  knd 5 0  typ 26220720  lvl  0  siz     0  off     0
(FUNCTION real
          real)
 26220912      iround  knd 5 0  typ 26221008  lvl  0  siz     0  off     0
(FUNCTION integer
          real)
 26221200         ord  knd 5 0  typ 26221296  lvl  0  siz     0  off     0
(FUNCTION integer
          integer)
 26221488         new  knd 5 0  typ 26221584  lvl  0  siz     0  off     0
(FUNCTION integer
          integer)
 26221776       trnew  knd 5 0  typ 26221872  lvl  0  siz     0  off     0
(FUNCTION integer
          integer)
 26222064       write  knd 5 0  typ 26222160  lvl  0  siz     0  off     0
(FUNCTION NULL
          char)
 26222352     writeln  knd 5 0  typ 26222448  lvl  0  siz     0  off     0
(FUNCTION NULL
          char)
 26222640      writef  knd 5 0  typ 26222736  lvl  0  siz     0  off     0
(FUNCTION NULL
          real)
 26222928    writelnf  knd 5 0  typ 26223024  lvl  0  siz     0  off     0
(FUNCTION NULL
          real)
 26223216      writei  knd 5 0  typ 26223312  lvl  0  siz     0  off     0
(FUNCTION NULL
          integer)
 26223504    writelni  knd 5 0  typ 26223600  lvl  0  siz     0  off     0
(FUNCTION NULL
          integer)
 26223792        read  knd 5 0  typ 26223888  lvl  0  siz     0  off     0
(FUNCTION NULL
          NULL)
 26224080      readln  knd 5 0  typ 26224176  lvl  0  siz     0  off     0
(FUNCTION NULL
          NULL)
 26224368         eof  knd 5 0  typ 26224464  lvl  0  siz     0  off     0
(FUNCTION boolean
          NULL)
Symbol table level 1
 26308352     complex  TYPE   typ 26308064  lvl  1  siz    16  off     0
(RECORD (re real)
        (im real))
 26309216         red  CONST  typ INTEGER  val  0
 26309376       white  CONST  typ INTEGER  val  1
 26309536        blue  CONST  typ INTEGER  val  2
 26309728       color  TYPE   typ 26309632  lvl  1  siz     4  off     0
  0 ..   2
 26310144      person  TYPE   typ 26311968  lvl  1  siz    48  off     0
(RECORD (age integer)
        (friend (^ person))
        (location (RECORD (re real)
                          (im real)))
        (favorite   0 ..   2)
        (salary real))
 26310336          pp  TYPE   typ 26310240  lvl  1  siz     8  off     0
(^ person)
 26313056           c  VAR    0 typ 26308064  lvl  1  siz    16  off     0
(RECORD (re real)
        (im real))
 26313152           d  VAR    0 typ 26308064  lvl  1  siz    16  off    16
(RECORD (re real)
        (im real))
 26313632           i  VAR    0 typ integer  lvl  1  siz     4  off    32
 26313728         sum  VAR    0 typ integer  lvl  1  siz     4  off    36
 26314720          ac  VAR    0 typ 26314624  lvl  1  siz   160  off    48
(ARRAY   1 ..  10 (RECORD (re real)
                          (im real)))
 26315936         aco  VAR    0 typ 26315840  lvl  1  siz   120  off   208
(ARRAY   1 ..  10 (ARRAY   0 ..   2   0 ..   2))
 26316672        john  VAR    4 typ 26310240  lvl  1  siz     8  off   328
(^ person)
 26316768        mary  VAR    4 typ 26310240  lvl  1  siz     8  off   336
(^ person)
 26316864        fred  VAR    4 typ 26310240  lvl  1  siz     8  off   344
(^ person)
 26316960         ptr  VAR    4 typ 26310240  lvl  1  siz     8  off   352
(^ person)
 26317952      people  VAR    0 typ 26317856  lvl  1  siz   960  off   368
(ARRAY   1 ..  20 (RECORD (age integer)
                          (friend (^ person))
                          (location (RECORD (re real)
                                            (im real)))
                          (favorite   0 ..   2)
                          (salary real)))
yyparse result =        0
(program graph1 (progn output)
                (progn (:= john (funcall new 48))
                       (:= mary (funcall new 48))
                       (:= fred (funcall new 48))
                       (:= (aref (^ john)
                                 32)
                           2)
                       (:= (aref (^ john)
                                 0)
                           19)
                       (progn (label 1)
                              (:= (aref (^ john)
                                        8)
                                  mary))
                       (:= (aref (^ john)
                                 40)
                           4.000000e+04)
                       (:= (aref (^ john)
                                 (+ 16
                                    0))
                           3)
                       (:= (aref (^ mary)
                                 0)
                           21)
                       (:= (aref (^ mary)
                                 8)
                           fred)
                       (progn (label 0)
                              (:= (aref (^ fred)
                                        0)
                                  20))
                       (:= (aref (^ fred)
                                 8)
                           0)
                       (:= (aref (^ (aref (^ (aref (^ john)
                                                   8))
                                          8))
                                 (+ 16
                                    8))
                           4.500000e+00)
                       (:= (aref ac (+ (+ -16
                                          (* 16
                                             7))
                                       0))
                           (aref (^ john)
                                 40))
                       (:= ptr john)
                       (:= sum 0)
                       (:= i 1)
                       (progn (label 2)
                              (if (<> ptr 0)
                                  (progn (progn (:= sum (+ sum (aref (^ ptr)
                                                                     0)))
                                                (:= (aref people (+ (+ -48
                                                                       (* 48
                                                                          i))
                                                                    0))
                                                    (aref (^ ptr)
                                                          0))
                                                (:= (aref aco (+ (+ -12
                                                                    (* 12
                                                                       i))
                                                                 (+ 0
                                                                    (* 4
                                                                       1))))
                                                    (aref (^ john)
                                                          32))
                                                (:= ptr (aref (^ ptr)
                                                              8))
                                                (:= i (+ i 1)))
                                         (goto 2))))
                       (funcall write 'i = ')
                       (funcall writelni i)
                       (funcall write 'Sum of ages = ')
                       (funcall writelni sum)
                       (funcall write 'Fred loc im = ')
                       (funcall writelni (aref (^ fred)
                                               (+ 16
                                                  8)))
                       (if (< sum 3)
                           (goto 0))))
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
        subq	$1344, %rsp 	  # make space for this stack frame
	movq	%rbx, %r9        # save %rbx (callee-saved) in %r9
# ------------------------- begin Your code -----------------------------
	movl	$48,%eax         	#  48 -> %eax
	call	new              	#  new()
	movl	%eax,-1016(%rbp)     	#  %eax -> john
	movl	$48,%eax         	#  48 -> %eax
	call	new              	#  new()
	movl	%eax,-1008(%rbp)     	#  %eax -> mary
	movl	$48,%eax         	#  48 -> %eax
	call	new              	#  new()
	movl	%eax,-1000(%rbp)     	#  %eax -> fred
	movl	$2,%eax         	#  2 -> %eax
	movl	%eax,-1312(%rbp)     	#  %eax -> 
	movl	$19,%eax         	#  19 -> %eax
	movl	%eax,-1344(%rbp)     	#  %eax -> 
.L1:
	movq	-1008(%rbp),%rax     	#  mary -> %rax
	movl	%eax,-1336(%rbp)     	#  %eax -> 
	movsd	.LC4(%rip),%xmm0   	#  40000.000000 -> %xmm0
	movl	%xmm0,-1304(%rbp)     	#  %xmm0 -> 
	movl	$3,%eax         	#  3 -> %eax
	movl	%eax,-1344(%rbp)     	#  %eax -> 
	movl	$21,%eax         	#  21 -> %eax
	movl	%eax,-1344(%rbp)     	#  %eax -> 
	movq	-1000(%rbp),%rax     	#  fred -> %rax
	movl	%eax,-1336(%rbp)     	#  %eax -> 
.L0:
	movl	$20,%eax         	#  20 -> %eax
	movl	%eax,-1344(%rbp)     	#  %eax -> 
	movl	$0,%eax         	#  0 -> %eax
	movl	%eax,-1336(%rbp)     	#  %eax -> 
	movsd	.LC5(%rip),%xmm0   	#  4.500000 -> %xmm0
	movl	%xmm0,-1336(%rbp)     	#  %xmm0 -> 
	movl	$40,%eax         	#  40 -> %eax
	cltq	                  	#  sign-extend
	movl	%r8,-1296(%rbp)     	#  %r8 -> 
	movq	-1016(%rbp),%rax     	#  john -> %rax
	movl	%eax,-992(%rbp)     	#  %eax -> ptr
	movl	$0,%eax         	#  0 -> %eax
	movl	%eax,-1308(%rbp)     	#  %eax -> sum
	movl	$1,%eax         	#  1 -> %eax
	movl	%eax,-1312(%rbp)     	#  %eax -> i
.L2:
	movq	-992(%rbp),%rax     	#  ptr -> %rax
	movl	$0,%ecx         	#  0 -> %ecx
	cmpl	%ecx,%eax           	#  compare %eax - %ecx
	jne	.L6 			#  jump if     !=
	jmp	.L7 			#  jump 
.L6:
	movl	-1308(%rbp),%eax     	#  sum -> %eax
	movl	$0,%ecx         	#  0 -> %ecx
	cltq	                  	#  sign-extend
	addl	%r8,%eax         	#  %eax + %r8 -> %eax
	movl	%eax,-1308(%rbp)     	#  %eax -> sum
	movl	$0,%eax         	#  0 -> %eax
	cltq	                  	#  sign-extend
	movl	%r8,-976(%rbp)     	#  %r8 -> 
	movl	$32,%eax         	#  32 -> %eax
	cltq	                  	#  sign-extend
	movl	%r8,-1136(%rbp)     	#  %r8 -> 
	movl	$8,%eax         	#  8 -> %eax
	cltq	                  	#  sign-extend
	movl	%r8,-992(%rbp)     	#  %r8 -> ptr
	movl	-1312(%rbp),%eax     	#  i -> %eax
	movl	$1,%ecx         	#  1 -> %ecx
	addl	%ecx,%eax         	#  %eax + %ecx -> %eax
	movl	%eax,-1312(%rbp)     	#  %eax -> i
	jmp	.L2 			#  jump 
.L7:
	movl	$.LC8,%edi       	#  addr of literal .LC8
	call	write              	#  write()
	movl	-1312(%rbp),%eax     	#  i -> %eax
	call	writelni              	#  writelni()
	movl	$.LC9,%edi       	#  addr of literal .LC9
	call	write              	#  write()
	movl	-1308(%rbp),%eax     	#  sum -> %eax
	call	writelni              	#  writelni()
	movl	$.LC10,%edi       	#  addr of literal .LC10
	call	write              	#  write()
	movl	$16,%eax         	#  16 -> %eax
	movl	$8,%ecx         	#  8 -> %ecx
	addl	%ecx,%eax         	#  %eax + %ecx -> %eax
	cltq	                  	#  sign-extend
	call	writelni              	#  writelni()
	movl	-1308(%rbp),%eax     	#  sum -> %eax
	movl	$3,%ecx         	#  3 -> %ecx
	movl	%ecx,%eax         	#  %ecx -> %eax
	jl	.L11 			#  jump if     <
	jmp	.L12 			#  jump 
.L11:
	jmp	.L0 			#  jump 
.L12:
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
.LC8:
	.string	"i = "
	.align  4
.LC9:
	.string	"Sum of ages = "
	.align  4
.LC10:
	.string	"Fred loc im = "
	.align  8
.LC4:
	.long	0   	#  40000.000000
	.long	1088653312
	.align  8
.LC5:
	.long	0   	#  4.500000
	.long	1074921472

        .ident  "CS 375 Compiler - Spring 2017"
