import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Data.Char
import Head

import Control.Monad.Identity (Identity)

{-# LANGUAGE NoMonomorphismRestriction #-}

reservados = ",;.(){}"

parseFile a = do f <- readFile a
                 let e = parse programa "Error:" (f)
                 return e

--expressaoAritmetica :: Parsec String () ExpressaoAritmetica
expressaoAritmetica = buildExpressionParser operadoresAritmeticos numero <|> numero

operadoresAritmeticos :: [[Operator String u Identity ExpressaoAritmetica]]
operadoresAritmeticos = [[Prefix (spaces >> char '-' >> spaces >> return (Neg))],

                        [Infix  (spaces >> char '*' >> spaces >> return (Multiplicacao)) AssocLeft,
                         Infix  (spaces >> char '/' >> spaces >> return (Divisao)) AssocLeft],

                        [Infix  (spaces >> char '+' >> spaces >> return (Adicao)) AssocLeft,
                         Infix  (spaces >> char '-' >> spaces >> return (Subtracao)) AssocLeft]]

expressaoLogica = buildExpressionParser operadoresLogicos expressaoRelacional <|> expressaoRelacional


operadoresLogicos :: [[Operator String u Identity ExpressaoLogica]]
operadoresLogicos = [[Prefix (char '!' >> spaces >> return (Nao))],

                     [Infix  (string "&&" >> spaces >> return (E)) AssocLeft,
                      Infix  (string "||" >> spaces >> return (Ou)) AssocLeft]]

--operadoresRelacionais :: [[Operator String u m ExpressaoRelacional]]
operadoresRelacionais = (char '>' >> spaces >> return (Maior))
                        <|>
                        (char '<' >> spaces >> return (Menor))
                        <|>
                        (string ">=" >> spaces >> return (MaiorIgual))
                        <|>
                        (string "<=" >> spaces >> return (MenorIgual))
                        <|>
                        (string "==" >> spaces >> return (Igual))
                        <|>
                        (string "!=" >> spaces >> return (Diferente))

expressaoRelacional = do try $ do l <- expressaoAritmetica
                                  spaces
                                  o <- operadoresRelacionais
                                  r <- expressaoAritmetica
                                  spaces
                                  return (ER (o l r))

listaParametros = parametro `sepBy` (char ',')

parametro = do {e <- expressaoAritmetica; spaces; return (ParametroExpressao e)}
            <|>
            do {l <- literal; spaces; return (ParametroLiteral l)}

chamadaFuncao = do try $ do i <-identificador
                            spaces
                            char '('
                            spaces
                            ps <- listaParametros
                            spaces
                            char ')'
                            spaces
                            return (Chamada i ps)

chamadaProc = do f <- chamadaFuncao
                 char ';'
                 spaces
                 return (ChamadaProc f)

cmdLeitura = do try $ do string "read("
                         spaces
                         i <- identificador
                         char ')'
                         spaces
                         char ';'
                         spaces
                         return (Le i)

cmdEscrita = do try $ do string "print("
                         spaces
                         p <- parametro
                         char ')'
                         spaces
                         char ';'
                         spaces
                         return (Escreve p)

cmdAtrib = do try $ do i <- identificador
                       char '='
                       spaces
                       p <- parametro
                       spaces
                       char ';'
                       spaces
                       return (Atribui i p)

cmdEnquanto = do try $ do string "while("
                          spaces
                          el <- expressaoLogica
                          spaces
                          char ')'
                          b <- bloco
                          return (While el b)

cmdSe = do try $ do string "if"
                    spaces
                    char '('
                    spaces
                    el <- expressaoLogica
                    spaces
                    char ')'
                    spaces
                    b1 <- bloco
                    string "else"
                    spaces
                    b2 <- bloco
                    return (If el b1 b2)
       <|>
       do string "if"
          spaces
          char '('
          spaces
          el <- expressaoLogica
          spaces
          char ')'
          spaces
          b1 <- bloco
          return (If el b1 [])

retorno = do string "return "
             spaces
             p <- parametro
             spaces
             return (Ret p)

comando = cmdSe <|> cmdEnquanto <|> cmdAtrib <|> cmdEscrita <|> cmdLeitura <|> chamadaProc <|> retorno
listaCmd = many1 comando

bloco = do char '{'
           cs <- listaCmd
           char '}'
           spaces
           return cs

tipo = do {string "int"; spaces; return (Inteiro)}
       <|>
       do {string "string"; spaces; return (String')}
       <|>
       do {string "float"; spaces; return (Float)}

void = do {string "void"; spaces; return Void}

tipoRetorno = do {t <- tipo; return (R t)} <|> void

declaracao = do try $ do t <- tipo
                         spaces
                         is <- listaId
                         char ';'
                         spaces
                         return (Decl t is)

blocoPrincipal = do char '{'
                    ds <- many declaracao
                    cs <- listaCmd
                    char '}'
                    spaces
                    return (Main ds cs)

parametroFormal = do t <- tipo
                     i <- identificador
                     return (ParamFormal t i)

declParametros = parametroFormal `sepBy` (char ',')

funcao = do try $ do tr <- tipoRetorno
                     i <- identificador
                     ps <- declParametros
                     b <- blocoPrincipal
                     return (Funcao tr i ps b)

programa = do fs <- many funcao
              b <- blocoPrincipal
              return (Prog fs b)

identificador :: Parsec String () (Identificador)
identificador = do s <- oneOf (['a'..'z'] ++ ['_'])
                   ss <- many (oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['-','_']))
                   spaces
                   return (s:ss)

listaId = identificador `sepBy` (char ',')

literal = do try $ do digits <- many1 digit
                      let n = foldl (\x d -> 10*x + toInteger (digitToInt d)) 0 digits
                      spaces
                      return (Inte n)
          <|>
          do try $ do {cs <- many1 (noneOf reservados); return (Str cs)}

numero = do try $ do digits <- many1 digit
                     let n = foldl (\x d -> 10*x + toInteger (digitToInt d)) 0 digits
                     spaces
                     return (Numero n)
         <|>
         do char '('
            spaces
            e <- expressaoAritmetica
            spaces
            char ')'
            spaces
            return e
