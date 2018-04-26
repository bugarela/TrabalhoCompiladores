import Text.Parsec
import Text.Parsec.Expr
--import Text.Parsec.Token
import Text.Parsec.Language
import Data.Char
import Head

import Control.Monad.Identity (Identity)

{-# LANGUAGE NoMonomorphismRestriction #-}

parseFile a = do f <- readFile a
                 let ls = lines f
                 let e = parse expressaoAritmetica "Error:" (head ls)
                 return e

--expressaoAritmetica :: Parsec String () ExpressaoAritmetica
expressaoAritmetica = buildExpressionParser operadoresAritmeticos numero

operadoresAritmeticos :: [[Operator String u Identity ExpressaoAritmetica]]
operadoresAritmeticos = [[Prefix (spaces >> char '-' >> spaces >> return (Neg))],

                        [Infix  (spaces >> char '*' >> spaces >> return (Multiplicacao)) AssocLeft,
                         Infix  (spaces >> char '/' >> spaces >> return (Divisao)) AssocLeft],

                        [Infix  (spaces >> char '+' >> spaces >> return (Adicao)) AssocLeft,
                         Infix  (spaces >> char '-' >> spaces >> return (Subtracao)) AssocLeft]]

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
                                  return (o l r)

listaParametros = many1 parametros `sepBy` (char ',')

parametros = literal <|> expressaoAritmetica

identificador :: Parsec String () (Identificador)
identificador = do s <- oneOf (['a'..'z'] ++ ['_'])
                   ss <- many (oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['-','_']))
                   return (s:ss)

literal = undefined

numero = do many1 digit
            spaces
            return AA
         <|>
         do char '('
            spaces
            e <- expressaoAritmetica
            spaces
            char ')'
            spaces
            return e
