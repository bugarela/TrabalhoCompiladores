module SintaxeJasmin where

import Head

cabecalho a = ".class public " ++ nomeClasse a ++"\n.super java/lang/Object\n\n.method public <init>()V\n\taload_0\n\n\tinvokenonvirtual java/lang/Object/<init>()V\n\treturn\n.end method\n\n"

cabecalhoFuncao nome ls r = ".method public static " ++ nome ++ assinaturaFuncao ls r ++ "\n\t.limit stack 10\n\t.limit locals 10\n\n"
cabecalhoMain = ".method public static main([Ljava/lang/String;)V\n\t.limit stack 10\n\t.limit locals 10\n\n"

rodape = ".end method\n\n"

assinaturaFuncao ls r = "(" ++ letras ls ++ ")" ++ letraR r

nomeClasse ('.':ss) = []
nomeClasse (s:ss) = s:nomeClasse ss

identa = map (\s -> '\t':s)

getstatic Print = "getstatic java/lang/System/out Ljava/io/PrintStream;"

invokevirtual Print t = "invokevirtual java/io/PrintStream/println(" ++ letra t ++ ")V"

letra TInt = "I"
letra TFloat = "F"
letra TString = "Ljava/lang/String;"

letras ls = foldr1 (++) (map letra ls)

letraR (R l) = letra l
letraR (Void) = "V"

goto l = ["goto " ++ l]
