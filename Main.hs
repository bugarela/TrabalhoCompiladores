import Aux
import Head
import Parser
import RBTree
import SintaxeJasmin

complila a = do ls <- parseFile a
                case ls of
                  Left e -> print e
                  Right ls -> putStr (cabecalho a ++ (unlines ((snd (semanticaPrograma ls)))) ++ rodape)
                return ()

semanticaPrograma (Prog fs b) = semanticaBlocoPrincipal b-- e semanticaFuncoes de alguma forma

semanticaFuncoes = undefined

semanticaBlocoPrincipal (Main ds b) = let ts = semanticaDeclaracoes ds in (ts,semanticaBloco ts b)

semanticaBloco ts cs = map (semanticaComando ts) cs ++ (identa ["return"])

semanticaComando ts (Atribui i (ParametroExpressao p)) = let (sea,t) = semanticaExpressaoAritmetica ts p in
                                                            unlines (identa (sea ++ store i t ts))
semanticaComando ts (If e b1 b2) = unlines (init se ++ ["if_" ++ (last se) ++ " LX"]) where
                                     se = semanticaExpressaoLogica ts e

semanticaComando ts (Escreve a) = unlines (identa ([getstatic Print] ++ empilha ts a ++ [invokevirtual Print]))
semanticaComando _ _ = ""

semanticaDeclaracoes ds = insereTabelaSimbolos ds emptyRB 1

semanticaExpressaoLogica ts (ER e) = semanticaExpressaoRelacional ts e

semanticaExpressaoRelacional ts e = traduzComparacao e ts

semanticaExpressaoAritmetica ts e = encontraCoercoes ts e

empilha ts (ParametroLiteral l) = [loadConst l]
empilha ts (ParametroExpressao e) = fst (semanticaExpressaoAritmetica ts e)

testeTabelaDeSimbolos a = do ls <- parseFile a
                             case ls of
                               Left e -> print e
                               Right ls -> print (fst (semanticaPrograma ls))
                             return ()
