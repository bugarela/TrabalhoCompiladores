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
                            let s = case ls of
                                     Left e -> ""
                                     Right ls -> show (snd (semanticaPrograma ls))
                            return s

semanticaPrograma (Prog fs b) = semanticaBlocoPrincipal b-- e semanticaFuncoes de alguma forma

semanticaFuncoes = undefined

semanticaBlocoPrincipal (Main ds b) = let ts = semanticaDeclaracoes ds in (ts,semanticaBloco ts b)

semanticaBloco ts cs = map (semanticaComando ts) cs

semanticaComando ts (Atribui i (ParametroExpressao p)) = i ++ " = " ++ (showLista (semanticaExpressaoAritmetica ts p))
semanticaComando _ _ = ""

semanticaDeclaracoes ds = insereTabelaSimbolos ds emptyRB 1

semanticaExpressaoAritmetica ts e = let (sea,_) = encontraCoercoes ts e in sea
