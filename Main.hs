import Aux
import Head
import Parser
import RBTree

testeTabelaDeSimbolos a = do ls <- parseFile a
                             case ls of
                               Left e -> print e
                               Right ls -> print (fst (semanticaPrograma ls))
                             return ()

testeArvoreSintatica a = do ls <- parseFile a
                            case ls of
                              Left e -> print e
                              Right ls -> putStr (unlines (snd (semanticaPrograma ls)))
                            return ()

semanticaPrograma (Prog fs b) = semanticaBlocoPrincipal b-- e semanticaFuncoes de alguma forma

semanticaFuncoes = undefined

semanticaBlocoPrincipal (Main ds b) = let ts = semanticaDeclaracoes ds in (ts,semanticaBloco ts b)

semanticaBloco ts cs = map (semanticaComando ts) cs

semanticaComando ts (Atribui i (ParametroExpressao p)) = let (sea,t) = semanticaExpressaoAritmetica ts p in
                                                            unlines (sea ++ store i t ts)
semanticaComando ts (If e b1 b2) = "if_" ++ semanticaExpressaoRelacional ts e ++ " LX"
semanticaComando _ _ = ""

semanticaDeclaracoes ds = insereTabelaSimbolos ds emptyRB 1

semanticaExpressaoAritmetica ts e = encontraCoercoes ts e

semanticaExpressaoRelacional ts e =undefined
