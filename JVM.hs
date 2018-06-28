module JVM where

import Head
import Tabelas
import Aux
import SintaxeJasmin

coercaoParam TInt TFloat = "\n\ti2f"
coercaoParam tp t = if tp == t then "" else error ("Um parametro deveria ser do tipo " ++ show t)


coercaoExpr ts (a,TInt) (b,TInt) = (a,b,TInt)
coercaoExpr ts (a,TFloat) (b,TFloat) = (a,b,TFloat)
coercaoExpr ts (a,TInt) (b,TFloat) = (a ++ ["i2f"],b,TFloat)
coercaoExpr ts (a,TFloat) (b,TInt) = (a,b ++ ["i2f"],TFloat)

coercaoEAs ts tf a b = coercaoExpr ts (traduzExpr ts tf a) (traduzExpr ts tf b)

empilha ts tf (ParametroLiteral l) = ([loadConst l], tipoConst l)
empilha ts tf (ParametroExpressao e) = (traduzExpr ts tf e)

traduzExpr :: TabelaDeSimbolos -> (TabelaDeFuncoes,String) -> ExpressaoAritmetica -> ([[Char]], Tipo)
traduzExpr ts tf (Multiplicacao a b) = let (sa,sb,t) = coercaoEAs ts tf a b in (sa ++ sb ++ [pre t ++ "mul"], t)
traduzExpr ts tf (Divisao a b) =       let (sa,sb,t) = coercaoEAs ts tf a b in (sa ++ sb ++ [pre t ++ "div"], t)
traduzExpr ts tf (Adicao a b) =        let (sa,sb,t) = coercaoEAs ts tf a b in (sa ++ sb ++ [pre t ++ "add"], t)
traduzExpr ts tf (Subtracao a b) =     let (sa,sb,t) = coercaoEAs ts tf a b in (sa ++ sb ++ [pre t ++ "sub"], t)
traduzExpr ts tf (Neg a) =             let (sa,t) = traduzExpr ts tf a in (sa ++ [pre t ++ "neg"],t)
traduzExpr ts tf (Numero a) =          ([toConst a], tipoEA ts (Numero a))
traduzExpr ts tf (Var i) =             ([pre t ++ "load " ++ posicao i ts], t) where t = tipoVariavel i ts
traduzExpr ts tf (Fun f) =             traduzChamadaFuncao ts tf f

geraIf TInt s = "if_icmp" ++ s
geraIf TFloat s = "fcmpl\n\tif" ++ s

traduzComparacao ts tf (Maior a b) =      let (sa,sb,t) = coercaoEAs ts tf a b in (sa ++ sb, geraIf t "gt")
traduzComparacao ts tf (Menor a b) =      let (sa,sb,t) = coercaoEAs ts tf a b in (sa ++ sb, geraIf t "lt")
traduzComparacao ts tf (MaiorIgual a b) = let (sa,sb,t) = coercaoEAs ts tf a b in (sa ++ sb, geraIf t "ge")
traduzComparacao ts tf (MenorIgual a b) = let (sa,sb,t) = coercaoEAs ts tf a b in (sa ++ sb, geraIf t "le")
traduzComparacao ts tf (Igual a b) =      let (sa,sb,t) = coercaoEAs ts tf a b in (sa ++ sb, geraIf t "eq")
traduzComparacao ts tf (Diferente a b) =  let (sa,sb,t) = coercaoEAs ts tf a b in (sa ++ sb, geraIf t "ne")

desvios ts tf (E a b) lv lf = do laux <- novoLabel
                                 da <- desvios ts tf a laux lf
                                 db <- desvios ts tf b lv lf
                                 return (da ++ [laux ++ ":"] ++ db)

desvios ts tf (Ou a b) lv lf = do laux <- novoLabel
                                  da <- desvios ts tf a lv laux
                                  db <- desvios ts tf b lv lf
                                  return (da ++ [laux ++ ":"] ++ db)

desvios ts tf (Nao e) lv lf = desvios ts tf e lf lv

desvios ts tf (ER e) lv lf = do let (se,c) = traduzComparacao ts tf e
                                return (identa (se ++ [c ++ " " ++ lv] ++ goto lf))

assinatura (ParamFormal t i) = t

organizaParametros [] _ = []
organizaParametros ((ParamFormal t i):ps) e = (i:>:t,e):organizaParametros ps (e+1)

declaraParametro (ParamFormal t i) = Decl t [i]

casaParametros _ _ [] [] = []
casaParametros _ _ [] _ = error ("Poucos parametros")
casaParametros _ _ _ [] = error ("Muitos parametros")
casaParametros ts tf (p:ps) (f:fs) = let (ls,t) = empilha ts tf p
                                         c = coercaoParam t f
                                     in (((unlines (identa ls)) ++ c) ++ casaParametros ts tf ps fs)

traduzChamadaFuncao ts tf (Chamada i ps) = let (i',r,pf) = buscaTipos i tf
                                               ls = casaParametros ts tf ps pf
                                               a = "invokestatic " ++ i' ++ assinaturaFuncao pf r
                                           in ([ls] ++ [a], tipoRet r)
