.class public Teste1
.super java/lang/Object

.method public <init>()V
   aload_0
 
   invokenonvirtual java/lang/Object/<init>()V
   return
.end method

.method public static main([Ljava/lang/String;)V
   .limit stack 2 
   .limit locals 4
   iconst_1
   istore_2
 l1:
   iload_2
   bipush 10
   if_icmpge l2
   iload_2
   iconst_2
   imul
   istore_1 
   getstatic java/lang/System/out Ljava/io/PrintStream;
   iload_1
   invokevirtual java/io/PrintStream/println(I)V
   iinc 2 1 
   goto l1
  l2:
   return
.end method
