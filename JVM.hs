module JVM where

import Head
import Tabelas
import Aux
import SintaxeJasmin

coercaoExpr ts (a,TInt) (b,TInt) = (a,b,TInt)
coercaoExpr ts (a,TFloat) (b,TFloat) = (a,b,TFloat)
coercaoExpr ts (a,TInt) (b,TFloat) = (a ++ ["i2f"],b,TFloat)
coercaoExpr ts (a,TFloat) (b,TInt) = (a,b ++ ["i2f"],TFloat)

coercao ts a b = coercaoExpr ts (traduzExpr ts a) (traduzExpr ts b)

traduzExpr :: TabelaDeSimbolos -> ExpressaoAritmetica -> ([[Char]], Tipo)
traduzExpr ts (Multiplicacao a b) = let (sa,sb,t) = coercao ts a b in (sa ++ sb ++ [pre t ++ "mul"], t)
traduzExpr ts (Divisao a b) =       let (sa,sb,t) = coercao ts a b in (sa ++ sb ++ [pre t ++ "div"], t)
traduzExpr ts (Adicao a b) =        let (sa,sb,t) = coercao ts a b in (sa ++ sb ++ [pre t ++ "add"], t)
traduzExpr ts (Subtracao a b) =     let (sa,sb,t) = coercao ts a b in (sa ++ sb ++ [pre t ++ "sub"], t)
traduzExpr ts (Neg a) =             let (sa,t) = traduzExpr ts a in (sa ++ [pre t ++ "neg"],t)
traduzExpr ts (Numero a) =          ([toConst a], tipoEA ts (Numero a))
traduzExpr ts (Var i) =             ([pre t ++ "load " ++ posicao i ts], t) where t = tipoVariavel i ts

geraIf TInt s = "if_icmp" ++ s
geraIf TFloat s = "fcmpl\n\tif" ++ s

traduzComparacao ts (Maior a b) =      let (sa,sb,t) = coercao ts a b in (sa ++ sb, geraIf t "gt")
traduzComparacao ts (Menor a b) =      let (sa,sb,t) = coercao ts a b in (sa ++ sb, geraIf t "lt")
traduzComparacao ts (MaiorIgual a b) = let (sa,sb,t) = coercao ts a b in (sa ++ sb, geraIf t "ge")
traduzComparacao ts (MenorIgual a b) = let (sa,sb,t) = coercao ts a b in (sa ++ sb, geraIf t "le")
traduzComparacao ts (Igual a b) =      let (sa,sb,t) = coercao ts a b in (sa ++ sb, geraIf t "eq")
traduzComparacao ts (Diferente a b) =  let (sa,sb,t) = coercao ts a b in (sa ++ sb, geraIf t "ne")

desvios ts (E a b) lv lf = do laux <- novoLabel
                              da <- desvios ts a laux lf
                              db <- desvios ts b lv lf
                              return (da ++ [laux ++ ":"] ++ db)

desvios ts (Ou a b) lv lf = do laux <- novoLabel
                               da <- desvios ts a lv laux
                               db <- desvios ts b lv lf
                               return (da ++ [laux ++ ":"] ++ db)

desvios ts (Nao e) lv lf = desvios ts e lf lv

desvios ts (ER e) lv lf = do let (se,c) = traduzComparacao ts e
                             return (identa (se ++ [c ++ " " ++ lv] ++ goto lf))

assinatura (ParamFormal t i) = t

organizaParametros [] _ = []
organizaParametros ((ParamFormal t i):ps) e = (i:>:t,e):organizaParametros ps (e+1)

declaraParametro (ParamFormal t i) = Decl t [i]
