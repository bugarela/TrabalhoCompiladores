import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Char
import Head

import Control.Monad.Identity (Identity)

{-# LANGUAGE NoMonomorphismRestriction #-}

parseFile a = do f <- readFile a
                 let ls = lines f
                 let e = parse expressaoAritmetica "Error:" (head ls)
                 return e

expressaoAritmetica :: Parsec String () ExpressaoAritmetica
expressaoAritmetica = buildExpressionParser operadoresAritmeticos numero

operadoresAritmeticos = [[Prefix (char '-' >> return (Neg))],

                        [Infix  (char '*' >> return (Binario Multiplicacao)) AssocLeft,
                         Infix  (char '/' >> return (Binario Divisao)) AssocLeft],

                        [Infix  (char '+' >> return (Binario Adicao)) AssocLeft,
                         Infix  (char '-' >> return (Binario Subtracao)) AssocLeft]]

{-bOperators = [ [Prefix (reservedOp "not" >> return (Not             ))          ]
            , [Infix  (reservedOp "and" >> return (BBinary And     )) AssocLeft,
               Infix  (reservedOp "or"  >> return (BBinary Or      )) AssocLeft]
            ]-}

numero = do many1 digit
            return AA
