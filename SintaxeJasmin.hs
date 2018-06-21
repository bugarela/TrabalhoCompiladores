module SintaxeJasmin where

import Head

cabecalho a = ".class public " ++ nomeClasse a ++"\n.super java/lang/Object\n\n.method public <init>()V\n\taload_0\n\n\tinvokenonvirtual java/lang/Object/<init>()V\n\treturn\n.end method\n\n.method public static main([Ljava/lang/String;)V\n\t.limit stack 10\n\t.limit locals 10\n\n"

rodape = ".end method\n"

nomeClasse ('.':ss) = []
nomeClasse (s:ss) = s:nomeClasse ss

identa = map (\s -> '\t':s)

getstatic Print = "getstatic java/lang/System/out Ljava/io/PrintStream;"

invokevirtual Print t = "invokevirtual java/io/PrintStream/println(" ++ letra t ++ ")V"

letra TInt = "I"
letra TFloat = "F"
letra TString = "Ljava/lang/String;"

goto l = ["goto " ++ l]
