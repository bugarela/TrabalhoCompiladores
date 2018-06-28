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

	return

.end method

.method public static cast(I)F
	.limit stack 10
	.limit locals 10

	iconst_1
	istore 1

	iload 1
	i2f
	fstore 2

	fload 2
	fstore 3

	iload 0
	iload 1
	imul
	i2f
	fload 2
	fadd
	fload 3
	fsub
	freturn

.end method

.method public static main([Ljava/lang/String;)V
	.limit stack 10
	.limit locals 10

	iconst_5

	invokestatic Test.fat(I)I
	istore 1

	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 1
	invokevirtual java/io/PrintStream/println(I)V

	getstatic java/lang/System/out Ljava/io/PrintStream;
	iconst_1
	iconst_1
	iadd

	invokestatic Test.fat(I)I
	iconst_1
	iadd
	invokevirtual java/io/PrintStream/println(I)V

	iload 1
	bipush 120
	if_icmpeq L5
	goto L3
L5:
	iconst_1
	iconst_1
	iadd
	iconst_3
	if_icmpgt L3
	goto L4
L3:
	getstatic java/lang/System/out Ljava/io/PrintStream;
	ldc "Deu ruim"
	invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V

	goto L6
L4:
	iconst_0

	invokestatic Test.fat(I)I
	iconst_1
	if_icmpeq L9
	goto L8
L9:
	iconst_1

	invokestatic Test.fat(I)I
	iconst_1
	if_icmpeq L7
	goto L8
L7:
	getstatic java/lang/System/out Ljava/io/PrintStream;
	ldc "Deu boa"
	invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V

L8:

L6:

	ldc "ALGO"

invokestatic Test.imprime(Ljava/lang/String;)V

	iload 1

	invokestatic Test.cast(I)F
	fstore 2

	return

.end method

