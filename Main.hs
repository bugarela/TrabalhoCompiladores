import Aux
import Head
import Tabelas
import JVM
import Parser
import RBTree
import SintaxeJasmin
import Data.List

compila a = do ls <- parseFile a
               case ls of
                  Left e -> print e
                  Right ls -> runLX (compila' a ls)
               return ()

compila' a ls = do (_,_,bs) <- semanticaPrograma ls
                   return (writeFile (nomeClasse a ++ ".j") (cabecalho a ++ bs))

semanticaPrograma :: Programa -> LX (TabelaDeSimbolos, TabelaDeFuncoes, String)
semanticaPrograma (Prog fs b) = do (tf,fs) <- semanticaDeclFuncoes fs
                                   (ts,tf,bs) <- semanticaBlocoPrincipal tf b
                                   let cabecalhoMain = cabecalhoFuncao "main" [TString] Void
                                   return (ts,tf,fs ++ (cabecalhoMain ++ (unlines bs) ++ rodape))

semanticaBlocoFuncao ts tf (Main ds b) n = do let ts' = insereTabelaSimbolos ds ts n
                                              bs <- semanticaBloco ts' tf b
                                              let r = adicionaRetorno bs
                                              return r

semanticaBlocoPrincipal tf (Main ds b) = do let ts = semanticaDeclaracoes ds
                                            bs <- semanticaBloco ts tf b
                                            let r = adicionaRetorno bs
                                            return (ts,tf,r)

semanticaBloco ts tf cs = do bs <- mapM (semanticaComando ts tf) cs
                             return bs

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

semanticaComando ts tf (Ret a) = do let (p,t) = empilha ts a
                                        r = pre t ++ "return "
                                    return (unlines (identa (p ++ [r])))

semanticaComando _ _ _ = return ""

semanticaDeclaracoes ds = insereTabelaSimbolos ds emptyRB 1
semanticaDeclFuncoes df = do let tf = insereTabelaFuncoes df emptyRB
                             fs <- mapM (traduzFuncao tf) df
                             return (tf, fold fs)

traduzFuncao tf (Funcao r i ps b) = do let c = cabecalhoFuncao i (map assinatura ps) r
                                           ds = map declaraParametro ps
                                           ts = insereTabelaSimbolos ds emptyRB 0
                                       sb <- semanticaBlocoFuncao ts tf b (toInteger(length ds))
                                       return (c ++ (unlines sb) ++ rodape)

semanticaExpressaoLogica ts e = do lv <- novoLabel
                                   lf <- novoLabel
                                   se <- desvios ts e lv lf
                                   return (se ++ [lv ++ ":"],lf)

semanticaExpressaoAritmetica ts e = traduzExpr ts e

empilha ts (ParametroLiteral l) = ([loadConst l], tipoConst l)
empilha ts (ParametroExpressao e) = (semanticaExpressaoAritmetica ts e)

adicionaRetorno bs = if ("return" `isInfixOf` last bs) then bs else (bs ++ identa ["return"])


--------- Testes --------

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
