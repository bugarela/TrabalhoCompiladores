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

.method public static main([Ljava/lang/String;)V
	.limit stack 10
	.limit locals 10

	iconst_1
	istore 1

	iconst_2
	istore 2

	iload 1


iload 2



	invokestatic Test.f(II)I
	istore 1

	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 1
	invokevirtual java/io/PrintStream/println(I)V

	return
.end method

