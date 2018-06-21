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
	istore 1

	bipush 50
	i2f
	fstore 4

	iconst_4
	istore 2

L2:
	fload 4
	bipush 100
	i2f
	fcmpl
	iflt L0
	goto L1
L0:
	fload 4
	iconst_1
	i2f
	fadd
	fstore 4

	goto L2
L1:

	getstatic java/lang/System/out Ljava/io/PrintStream;
	fload 4
	invokevirtual java/io/PrintStream/println(F)V

	return
.end method
