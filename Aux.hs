module Aux where

import Parser
import Head
import RBTree

--------------------------
instance Functor LX where
   fmap f (LX m) = LX (\e -> let (a, e') = m e in (f a, e'))

instance Applicative LX where
    pure a = LX (\e -> (a, e))
    LX fs <*> LX vs = LX (\e -> let (f, e') = fs e; (a, e'') = vs e' in (f a, e''))

instance Monad LX where
    return x = LX (\e -> (x, e))
    LX m >>= f  = LX (\e -> let (a, e') = m e; LX fa = f a in fa e')

freshLabel :: LX String
freshLabel = LX (\e -> let v = "L"++show e in (v, e+1))

runLX (LX m) = let (t, _) = m 0 in t

----------------------------

busca = searchFast compVar
insere = insert compVar
compVar ((a :>: _),_) ((b :>: _),_) = compare a b

insereTabelaSimbolos :: [Declaracao] -> TabelaDeSimbolos -> Integer -> TabelaDeSimbolos
insereTabelaSimbolos [] ts p = ts
insereTabelaSimbolos ((Decl t []):ds) ts p = insereTabelaSimbolos ds ts p
insereTabelaSimbolos ((Decl t (i:is)):ds) ts p = if (busca ts ((i :>: t), p) /= Nothing)
                                                     then error ("Variavel " ++ show i ++ " duplamente declarada")
                                                     else insereTabelaSimbolos ((Decl t is):ds) (insere ts ((i :>: t),p)) (p+1)


tipoEA _ (Numero (Inteiro _)) = TInt
tipoEA _ (Numero (Flutuante _)) = TFloat
tipoEA ts (Var i) = tipoVariavel i ts

posicao i ts = case busca ts ((i :>: TInt), 0) of
                    Nothing -> error("Variavel " ++ show i ++ " indefinida")
                    Just ((_ :>: _),p) -> show p

tipoVariavel i ts = case busca ts ((i :>: TInt), 0) of
                         Nothing -> error("Variavel " ++ show i ++ " indefinida")
                         Just ((_ :>: t),_) -> t

store i TString ts = if (tipoVariavel i ts == TString) then ["nseicomo " ++ posicao i ts] else
                         error ("Atribuição de algo tipo string para a variavel " ++ i ++ " do tipo " ++ show (tipoVariavel i ts))
store i TInt ts = if (tipoVariavel i ts == TFloat) then ["i2f","fstore " ++ posicao i ts] else
                  if (tipoVariavel i ts == TInt) then ["istore " ++ posicao i ts] else
                      error ("Atribuição de algo tipo int para a variavel " ++ i ++ " do tipo string")
store i TFloat ts = if (tipoVariavel i ts == TFloat) then ["fstore " ++ posicao i ts] else
                       error ("Atribuição de algo tipo float para a variavel " ++ i ++ " do tipo " ++ show (tipoVariavel i ts))

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

encontraCoercoes :: TabelaDeSimbolos -> ExpressaoAritmetica -> ([[Char]], Tipo)
encontraCoercoes ts (Multiplicacao a b) = let (sa,sb,t) = coercaoExpr ts (encontraCoercoes ts a) (encontraCoercoes ts b) in (sa ++ sb ++ [pre t ++ "mul"], t)
encontraCoercoes ts (Divisao a b) =       let (sa,sb,t) = coercaoExpr ts (encontraCoercoes ts a) (encontraCoercoes ts b) in (sa ++ sb ++ [pre t ++ "div"], t)
encontraCoercoes ts (Adicao a b) =        let (sa,sb,t) = coercaoExpr ts (encontraCoercoes ts a) (encontraCoercoes ts b) in (sa ++ sb ++ [pre t ++ "add"], t)
encontraCoercoes ts (Subtracao a b) =     let (sa,sb,t) = coercaoExpr ts (encontraCoercoes ts a) (encontraCoercoes ts b) in (sa ++ sb ++ [pre t ++ "sub"], t)
encontraCoercoes ts (Neg a) =             let (sa,t) = encontraCoercoes ts a in (sa ++ [pre t ++ "neg"],t)
encontraCoercoes ts (Numero a) =          ([toConst a], tipoEA ts (Numero a))
encontraCoercoes ts (Var i) =             ([pre t ++ "load " ++ posicao i ts], t) where t = tipoVariavel i ts

coercaoExpr ts (a,TInt) (b,TInt) = (a,b,TInt)
coercaoExpr ts (a,TFloat) (b,TFloat) = (a,b,TFloat)
coercaoExpr ts (a,TInt) (b,TFloat) = (a ++ ["i2F"],b,TFloat)
coercaoExpr ts (a,TFloat) (b,TInt) = (a,b ++ ["i2F"],TFloat)

traduzComparacao (Maior a b) ts =      let (sa,sb,t) = coercaoExpr ts (encontraCoercoes ts a) (encontraCoercoes ts b) in (sa ++ sb ++ [pre t ++ "cmpgt"])
traduzComparacao (Menor a b) ts =      let (sa,sb,t) = coercaoExpr ts (encontraCoercoes ts a) (encontraCoercoes ts b) in (sa ++ sb ++ [pre t ++ "cmplt"])
traduzComparacao (MaiorIgual a b) ts = let (sa,sb,t) = coercaoExpr ts (encontraCoercoes ts a) (encontraCoercoes ts b) in (sa ++ sb ++ [pre t ++ "cmpge"])
traduzComparacao (MenorIgual a b) ts = let (sa,sb,t) = coercaoExpr ts (encontraCoercoes ts a) (encontraCoercoes ts b) in (sa ++ sb ++ [pre t ++ "cmple"])
traduzComparacao (Igual a b) ts =      let (sa,sb,t) = coercaoExpr ts (encontraCoercoes ts a) (encontraCoercoes ts b) in (sa ++ sb ++ [pre t ++ "cmpeq"])
traduzComparacao (Diferente a b) ts =  let (sa,sb,t) = coercaoExpr ts (encontraCoercoes ts a) (encontraCoercoes ts b) in (sa ++ sb ++ [pre t ++ "cmpne"])
