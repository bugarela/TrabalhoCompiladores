.class public Test
.super java/lang/Object

.method public <init>()V
	aload_0

	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static fat(I)I
	.limit stack 10
	.limit locals 10

	iload 0
	iconst_1
	if_icmpeq L0
	goto L2
L2:
	iload 0
	iconst_0
	if_icmpeq L0
	goto L1
L0:
	iconst_1
	ireturn

L1:

	iload 0
	iload 0
	iconst_1
	isub

	invokestatic Test.fat(I)I
	imul
	ireturn

.end method

.method public static imprime(Ljava/lang/String;)V
	.limit stack 10
	.limit locals 10

	getstatic java/lang/System/out Ljava/io/PrintStream;
	aload 0
	invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V

	getstatic java/lang/System/out Ljava/io/PrintStream;
	ldc "\n"
	invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V

	return

.end method

.method public static maior(II)I
	.limit stack 10
	.limit locals 10

	iload 0
	iload 1
	if_icmpgt L3
	goto L4
L3:
	iload 0
	ireturn

L4:

	iload 1
	ireturn

.end method

.method public static main([Ljava/lang/String;)V
	.limit stack 10
	.limit locals 10

	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_1
	iconst_2

	invokestatic Test.maior(II)I
	invokevirtual java/io/PrintStream/println(I)V

	return

.end method

