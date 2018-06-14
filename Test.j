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

	bipush 10
	istore 1

	bipush 20
	istore 2

	ldc 3.3
	fstore 4

	ldc "batata"
	astore 5

	getstatic java/lang/System/out Ljava/io/PrintStream;
	ldc "a"
	invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V

	getstatic java/lang/System/out Ljava/io/PrintStream;
	aload 5
	invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V

	return
.end method
