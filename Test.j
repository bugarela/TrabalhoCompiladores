.class public Test
.super java/lang/Object

.method public <init>()V
	aload_0

	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static f(II)F
	.limit stack 10
	.limit locals 10

	iload 0
	iload 1
	iadd
	istore 0

	i2f
	freturn
.end method

.method public static main([Ljava/lang/String;)V
	.limit stack 10
	.limit locals 10

	iconst_1
	i2f
	fstore 1

	iconst_2
	i2f
	fstore 2

	iconst_1


iconst_2



	invokestatic Test.f(II)F
	fstore 1

	getstatic java/lang/System/out Ljava/io/PrintStream;
	fload 1
	invokevirtual java/io/PrintStream/println(F)V

	return
.end method

