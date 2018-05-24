module Aux where

import Parser
import Head
import RBTree

busca = searchFast compVar
insere = insert compVar
compVar (a :>: _) (b :>: _) = compare a b

insereTabelaSimbolos :: [Declaracao] -> TabelaDeSimbolos -> TabelaDeSimbolos
insereTabelaSimbolos [] ts = ts
insereTabelaSimbolos ((Decl t []):ds) ts = insereTabelaSimbolos ds ts
insereTabelaSimbolos ((Decl t (i:is)):ds) ts =  if (busca ts (i :>: t) /= Nothing)
                                                   then error ("Variavel " ++ show i ++ " duplamente declarada")
                                                   else insereTabelaSimbolos ((Decl t is):ds) (insere ts (i :>: t))


tipoEA _ (Numero (Inteiro _)) = TInt
tipoEA _ (Numero (Flutuante _)) = TFloat
tipoEA ts (Var i) = case busca ts (i :>: TInt) of
                         Nothing -> error("Variavel " ++ show i ++ " indefinida")
                         Just (_ :>: t) -> t

encontraCoercoes :: TabelaDeSimbolos -> ExpressaoAritmetica -> ([Char], Tipo)
encontraCoercoes ts (Multiplicacao a b) = let (sa,sb,t) = coercaoExpr ts (encontraCoercoes ts a) (encontraCoercoes ts b) in (sa ++ " " ++ sb ++ " *", t)
encontraCoercoes ts (Divisao a b) =       let (sa,sb,t) = coercaoExpr ts (encontraCoercoes ts a) (encontraCoercoes ts b) in (sa ++ " " ++ sb ++ " /", t)
encontraCoercoes ts (Adicao a b) =        let (sa,sb,t) = coercaoExpr ts (encontraCoercoes ts a) (encontraCoercoes ts b) in (sa ++ " " ++ sb ++ " +", t)
encontraCoercoes ts (Subtracao a b) =     let (sa,sb,t) = coercaoExpr ts (encontraCoercoes ts a) (encontraCoercoes ts b) in (sa ++ " " ++ sb ++ " -", t)
encontraCoercoes ts (Neg a) =             let (sa,t) = encontraCoercoes ts a in (sa ++  " /-/ ",t)
encontraCoercoes ts (Numero a) =          (show a, tipoEA ts (Numero a))
encontraCoercoes ts (Var i) =             (i, tipoEA ts (Var i))

coercaoExpr ts (a,TInt) (b,TInt) = (a,b,TInt)
coercaoExpr ts (a,TFloat) (b,TFloat) = (a,b,TFloat)
coercaoExpr ts (a,TInt) (b,TFloat) = (a ++ " i2F", b,TFloat)
coercaoExpr ts (a,TFloat) (b,TInt) = (a, b ++ " i2F",TFloat)
