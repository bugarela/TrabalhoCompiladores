.class public Test
.super java/lang/Object

.method public <init>()V
	aload_0

	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static f(II)I
	.limit stack 10
	.limit locals 10

	iload 0
	iload 1
	iadd
	istore 0

	iload 0
	ireturn 

.end method

.method public static main(Ljava/lang/String;)V
	.limit stack 10
	.limit locals 10

	iconst_1
	istore 1

L3:
	iload 1
	bipush 10
	if_icmple L2
	goto L1
L2:
	iload 1
	bipush 7
	if_icmpne L0
	goto L1
L0:
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 1
	invokevirtual java/io/PrintStream/println(I)V

	iload 1
	iconst_1
	iadd
	istore 1

	goto L3
L1:

	return
.end method

