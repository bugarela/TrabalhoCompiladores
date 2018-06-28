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

compila' a ls = do let s = nomeClasse a
                   (_,_,bs) <- semanticaPrograma s ls
                   return (writeFile (s ++ ".j") (cabecalho a ++ bs))

semanticaPrograma :: String -> Programa -> LX (TabelaDeSimbolos, TabelaDeFuncoes, String)
semanticaPrograma a (Prog fs b) = do (tf,fs) <- semanticaDeclFuncoes fs a
                                     (ts,bs) <- semanticaBlocoPrincipal tf b
                                     return (ts,fst tf,fs ++ (cabecalhoMain ++ (unlines bs) ++ rodape))

semanticaBlocoFuncao ts tf (Main ds b) n = do let ts' = insereTabelaSimbolos ds ts n
                                              bs <- semanticaBloco ts' tf b
                                              return bs

semanticaBlocoPrincipal tf (Main ds b) = do let ts = semanticaDeclaracoes ds
                                            bs <- semanticaBloco ts tf b
                                            let r = adicionaRetorno bs
                                            return (ts,r)

semanticaBloco ts tf cs = do bs <- mapM (semanticaComando ts tf) cs
                             return bs

semanticaComando ts tf (Atribui i (ParametroExpressao p)) = do let (sea,t) = semanticaExpressaoAritmetica ts tf p
                                                               return (unlines (identa (sea ++ store i t ts)))

semanticaComando ts tf (Atribui i (ParametroLiteral l)) = return (unlines ((fst (empilha ts tf (ParametroLiteral l))) ++ identa (store i (tipoConst l) ts)))

semanticaComando ts tf (If e b1 []) = do (se,lf) <- semanticaExpressaoLogica ts tf e
                                         s1 <- semanticaBloco ts tf b1
                                         return (unlines (se ++ s1 ++ [lf ++ ":"]))

semanticaComando ts tf (If e b1 b2) = do (se,lf) <- semanticaExpressaoLogica ts tf e
                                         laux <- novoLabel
                                         s1 <- semanticaBloco ts tf b1
                                         s2 <- semanticaBloco ts tf b2
                                         let r = se ++ s1 ++ identa (goto laux) ++ [lf ++ ":"] ++ s2 ++ [laux ++ ":"]
                                         return (unlines r)

semanticaComando ts tf (While e b) = do (se,lf) <- semanticaExpressaoLogica ts tf e
                                        lw <- novoLabel
                                        lb <- novoLabel
                                        s <- semanticaBloco ts tf b
                                        let r = [lw ++ ":"] ++ se ++ s ++ identa (goto lw) ++ [lf ++ ":"]
                                        return (unlines r)

semanticaComando ts tf (Escreve a) = do let p = empilha ts tf a
                                        return (unlines (identa ([getstatic Print] ++ fst p ++ [invokevirtual Print (snd p)])))

semanticaComando ts tf (ChamadaProc c) = do let (s,_) = (traduzChamadaFuncao ts tf c)
                                            return (unlines s)

semanticaComando ts tf (Ret a) = do let (p,t) = empilha ts tf a
                                        r = pre t ++ "return"
                                    return (unlines (identa (p ++ [r])))

semanticaDeclaracoes ds = insereTabelaSimbolos ds emptyRB 1
semanticaDeclFuncoes df a = do let tf = (insereTabelaFuncoes df emptyRB, a)
                               fs <- mapM (traduzFuncao tf) df
                               return (tf, fold fs)

traduzFuncao tf (Funcao r i ps b) = do let c = cabecalhoFuncao i (map assinatura ps) r
                                           ds = map declaraParametro ps
                                           ts = insereTabelaSimbolos ds emptyRB 0
                                       sb <- semanticaBlocoFuncao ts tf b (toInteger(length ds))
                                       let sb' = verificaRetorno r (adicionaRetorno sb)
                                       return (c ++ (unlines sb') ++ rodape)

semanticaExpressaoLogica ts tf e = do lv <- novoLabel
                                      lf <- novoLabel
                                      se <- desvios ts tf e lv lf
                                      return (se ++ [lv ++ ":"],lf)

semanticaExpressaoAritmetica ts tf e = traduzExpr ts tf e

adicionaRetorno bs = if ("return" `isInfixOf` last bs) then bs else (bs ++ identa ["return\n"])

verificaRetorno Void bs = if "\treturn\n" `isSuffixOf` last bs then bs else error ("Retorno encontrado em função para void")
verificaRetorno (R TInt) bs = if "\tireturn\n" `isSuffixOf` last bs then bs else error ("Retorno deveria ser inteiro")
verificaRetorno (R TString) bs = if "\tareturn\n" `isSuffixOf` last bs then bs else error ("Retorno deveria ser string")
verificaRetorno (R TFloat) bs = if "\tireturn\n" `isSuffixOf` last bs
                                then [(semReturn (last bs))] ++ identa(["i2f"] ++ ["freturn"])
                                else if "\tfreturn\n" `isSuffixOf` last bs then bs else error ("Retorno deveria ser float")
--------- Testes --------

testeTabelaDeSimbolos a = do ls <- parseFile a
                             case ls of
                               Left e -> print e
                               Right ls -> runLX (testeTabelaDeSimbolos' ls)
                             return ()
testeTabelaDeSimbolos' ls = do (p,_,_) <- semanticaPrograma "" ls
                               return (print p)

testeTabelaDeFuncoes a = do ls <- parseFile a
                            case ls of
                              Left e -> print e
                              Right ls -> runLX (testeTabelaDeFuncoes' ls)
                            return ()
testeTabelaDeFuncoes' ls = do (_,p,_) <- semanticaPrograma "" ls
                              return (print p)
