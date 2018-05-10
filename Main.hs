import Aux
import Head
import Parser

testeTabelaDeSimbolos a = do ls <- parseFile a
                             case ls of
                               Left e -> print e
                               Right ls -> print (semanticaPrograma ls)
                             return ()

semanticaPrograma (Prog fs b) = semanticaBlocoPrincipal b-- e semanticaFuncoes de alguma forma

semanticaFuncoes = undefined

semanticaBlocoPrincipal (Main ds b) = semanticaDeclaracoes ds -- e semanticaBloco de algum forma

semanticaBloco = undefined

semanticaDeclaracoes ds = insereTabelaSimbolos ds []
