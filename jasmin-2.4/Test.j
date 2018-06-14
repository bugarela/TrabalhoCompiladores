.class public Test
.super java/lang/Object

.method public <init>()V
	aload_0

	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static main([Ljava/lang/String;)V
	.limit stack 10
	.limit locals 10

	iconst_1
	iconst_2
	iadd
	istore 1

	iconst_2
	istore 2

	iconst_1
	iconst_3
	iadd
	i2f
	fstore 4

	ldc 1.0
	fload 4
	fsub
	fstore 4

	iload 1
	iload 2
	iadd
	ineg
	i2f
	ldc 1.0
	fmul
	fstore 4

	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 1
	invokevirtual java/io/PrintStream/println(I)V

	return
.end method
