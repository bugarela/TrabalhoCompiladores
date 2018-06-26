import Aux
import Head
import Tabelas
import JVM
import Parser
import RBTree
import SintaxeJasmin

compila a = do ls <- parseFile a
               case ls of
                  Left e -> print e
                  Right ls -> runLX (compila' a ls)
               return ()

compila' a ls = do (_,_,bs) <- semanticaPrograma ls
                   return (writeFile (nomeClasse a ++ ".j") (cabecalho a ++ (unlines bs) ++ rodape))

semanticaPrograma :: Programa -> LX (TabelaDeSimbolos, TabelaDeFuncoes, [String])
semanticaPrograma (Prog fs b) = let tf = semanticaDeclFuncoes fs in semanticaBlocoPrincipal tf b

semanticaFuncoes = undefined

semanticaBlocoPrincipal tf (Main ds b) = do let ts = semanticaDeclaracoes ds
                                            bs <- semanticaBloco ts tf b
                                            let r = identa ["return"]
                                            return (ts,tf,bs ++ r)

semanticaBloco ts tf cs = do bs <- mapM (semanticaComando ts tf) cs
                             return (bs)

semanticaComando ts tf (Atribui i (ParametroExpressao p)) = do let (sea,t) = semanticaExpressaoAritmetica ts p
                                                               return (unlines (identa (sea ++ store i t ts)))

semanticaComando ts tf (Atribui i (ParametroLiteral l)) = return (unlines (identa ((fst (empilha ts (ParametroLiteral l))) ++ (store i (tipoConst l) ts))))

semanticaComando ts tf (Atribui i (ParametroFuncao f)) = undefined

semanticaComando ts tf (If e b1 []) = do (se,lf) <- semanticaExpressaoLogica ts e
                                         s1 <- semanticaBloco ts tf b1
                                         return (unlines (se ++ s1 ++ [lf ++ ":"]))

semanticaComando ts tf (If e b1 b2) = do (se,lf) <- semanticaExpressaoLogica ts e
                                         laux <- novoLabel
                                         s1 <- semanticaBloco ts tf b1
                                         s2 <- semanticaBloco ts tf b2
                                         let r = se ++ s1 ++ identa (goto laux) ++ [lf ++ ":"] ++ s2 ++ [laux ++ ":"]
                                         return (unlines r)

semanticaComando ts tf (While e b) = do (se,lf) <- semanticaExpressaoLogica ts e
                                        lw <- novoLabel
                                        lb <- novoLabel
                                        s <- semanticaBloco ts tf b
                                        let r = [lw ++ ":"] ++ se ++ s ++ identa (goto lw) ++ [lf ++ ":"]
                                        return (unlines r)

semanticaComando ts tf (Escreve a) = do let p = empilha ts a
                                        return (unlines (identa ([getstatic Print] ++ fst p ++ [invokevirtual Print (snd p)])))

semanticaComando ts tf (ChamadaProc a) = undefined
semanticaComando _ _ _ = return ""

semanticaDeclaracoes ds = insereTabelaSimbolos ds emptyRB 1
semanticaDeclFuncoes df = insereTabelaFuncoes df emptyRB

semanticaExpressaoLogica ts e = do lv <- novoLabel
                                   lf <- novoLabel
                                   se <- desvios ts e lv lf
                                   return (se ++ [lv ++ ":"],lf)

semanticaExpressaoAritmetica ts e = traduzExpr ts e

empilha ts (ParametroLiteral l) = ([loadConst l], tipoConst l)
empilha ts (ParametroExpressao e) = (semanticaExpressaoAritmetica ts e)

testeTabelaDeSimbolos a = do ls <- parseFile a
                             case ls of
                               Left e -> print e
                               Right ls -> runLX (testeTabelaDeSimbolos' ls)
                             return ()
testeTabelaDeSimbolos' ls = do (p,_,_) <- semanticaPrograma ls
                               return (print p)

testeTabelaDeFuncoes a = do ls <- parseFile a
                            case ls of
                              Left e -> print e
                              Right ls -> runLX (testeTabelaDeFuncoes' ls)
                            return ()
testeTabelaDeFuncoes' ls = do (_,p,_) <- semanticaPrograma ls
                              return (print p)
