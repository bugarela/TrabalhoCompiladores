import Aux
import Head
import Parser
import RBTree
import SintaxeJasmin

compila a = do ls <- parseFile a
               case ls of
                  Left e -> print e
                  Right ls -> runLX (compila' a ls)
               return ()

compila' a ls = do (_,bs) <- semanticaPrograma ls
                   return (writeFile (nomeClasse a ++ ".j") (cabecalho a ++ (unlines bs) ++ rodape))

semanticaPrograma :: Programa -> LX (TabelaDeSimbolos, [String])
semanticaPrograma (Prog fs b) = semanticaBlocoPrincipal b

semanticaFuncoes = undefined

semanticaBlocoPrincipal (Main ds b) = do let ts = semanticaDeclaracoes ds
                                         bs <- semanticaBloco ts b
                                         let r = identa ["return"]
                                         return (ts,bs ++ r)

semanticaBloco ts cs = do bs <- mapM (semanticaComando ts) cs
                          return (bs)

semanticaComando ts (Atribui i (ParametroExpressao p)) = do let (sea,t) = semanticaExpressaoAritmetica ts p
                                                            return (unlines (identa (sea ++ store i t ts)))

semanticaComando ts (Atribui i (ParametroLiteral l)) = return (unlines (identa ((fst (empilha ts (ParametroLiteral l))) ++ (store i (tipoConst l) ts))))

semanticaComando ts (If e b1 b2) = do (se,lf) <- semanticaExpressaoLogica ts e
                                      laux <- novoLabel
                                      s1 <- semanticaBloco ts b1
                                      s2 <- semanticaBloco ts b2
                                      return (unlines (se ++ s1 ++ identa (goto laux) ++ [lf ++ ":"] ++ s2 ++ [laux ++ ":"]))

semanticaComando ts (Escreve a) = do let p = empilha ts a
                                     return (unlines (identa ([getstatic Print] ++ fst p ++ [invokevirtual Print (snd p)])))
semanticaComando _ _ = return ""

semanticaDeclaracoes ds = insereTabelaSimbolos ds emptyRB 1

semanticaExpressaoLogica ts e = do lv <- novoLabel
                                   lf <- novoLabel
                                   se <- desvios ts e lv lf
                                   return (se ++ [lv ++ ":"],lf)

semanticaExpressaoAritmetica ts e = encontraCoercoes ts e

empilha ts (ParametroLiteral l) = ([loadConst l], tipoConst l)
empilha ts (ParametroExpressao e) = (semanticaExpressaoAritmetica ts e)

testeTabelaDeSimbolos a = do ls <- parseFile a
                             case ls of
                               Left e -> print e
                               Right ls -> runLX (testeTabelaDeSimbolos' ls)
                             return ()
testeTabelaDeSimbolos' ls = do (p,_) <- semanticaPrograma ls
                               return (print p)
