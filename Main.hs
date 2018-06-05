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

semanticaComando ts (Atribui i (ParametroExpressao p)) = i ++ " = " ++ (unlines (semanticaExpressaoAritmetica ts p))
semanticaComando _ _ = ""

semanticaDeclaracoes ds = insereTabelaSimbolos ds emptyRB 1

semanticaExpressaoAritmetica ts e = let (sea,_) = encontraCoercoes ts e in sea
