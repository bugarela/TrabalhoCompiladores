module Aux where

import Head
import Tabelas
import RBTree
import SintaxeJasmin

--------------------------
instance Functor LX where
   fmap f (LX m) = LX (\e -> let (a, e') = m e in (f a, e'))

instance Applicative LX where
    pure a = LX (\e -> (a, e))
    LX fs <*> LX vs = LX (\e -> let (f, e') = fs e; (a, e'') = vs e' in (f a, e''))

instance Monad LX where
    return x = LX (\e -> (x, e))
    LX m >>= f  = LX (\e -> let (a, e') = m e; LX fa = f a in fa e')

novoLabel :: LX String
novoLabel = LX (\e -> let v = "L"++show e in (v, e+1))

runLX (LX m) = let (t, _) = m 0 in t

----------------------------

tipoEA _ (Numero (Inteiro _)) = TInt
tipoEA _ (Numero (Flutuante _)) = TFloat
tipoEA ts (Var i) = tipoVariavel i ts

store i TString ts = if (tipoVariavel i ts == TString) then ["astore " ++ posicao i ts] else
                         error ("Atribuição de algo tipo string para a variavel " ++ i ++ " do tipo " ++ show (tipoVariavel i ts))
store i TInt ts = if (tipoVariavel i ts == TFloat) then ["i2f","fstore " ++ posicao i ts] else
                  if (tipoVariavel i ts == TInt) then ["istore " ++ posicao i ts] else
                      error ("Atribuição de algo tipo int para a variavel " ++ i ++ " do tipo string")
store i TFloat ts = if (tipoVariavel i ts == TFloat) then ["fstore " ++ posicao i ts] else
                       error ("Atribuição de algo tipo float para a variavel " ++ i ++ " do tipo " ++ show (tipoVariavel i ts))

loadConst (Numb a) = toConst a
loadConst (Str s) = "ldc " ++ show s

tipoConst (Numb a) = tipoEA emptyRB (Numero a)
tipoConst (Str s) = TString

pre TInt = "i"
pre TFloat = "f"
pre TString = "a"

toConst (Inteiro 0) = "iconst_0"
toConst (Inteiro 1) = "iconst_1"
toConst (Inteiro 2) = "iconst_2"
toConst (Inteiro 3) = "iconst_3"
toConst (Inteiro 4) = "iconst_4"
toConst (Inteiro 5) = "iconst_5"
toConst (Inteiro n) = if (n >= -128 && n <= 127)
                         then "bipush " ++ show n
                         else "ldc " ++ show n
toConst (Flutuante n) = "ldc " ++ show n

tipoRet (R t) = t
tipoRet (Void) = error("Operacao com funcao que retorna void")

fold [] = []
fold (f:fs) = f ++ fold fs
