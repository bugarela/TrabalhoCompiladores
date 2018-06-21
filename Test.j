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

	iload 1
	iconst_5
	if_icmpgt L0
	goto L1
L0:
	iconst_1
	istore 2

	goto L2
L1:
	iconst_2
	istore 2

L2:

	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 2
	invokevirtual java/io/PrintStream/println(I)V

	return
.end method
