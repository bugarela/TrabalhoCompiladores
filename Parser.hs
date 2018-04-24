import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Char
import Head

import Control.Monad.Identity (Identity)

{-# LANGUAGE NoMonomorphismRestriction #-}

parseFile a = do f <- readFile a
                 let ls = lines f
                 let e = parse expressaoLogica "Error:" (head ls)
                 return e

reserv = "()->{}\n "


expressaoLogica :: Parsec String () (ExpressaoLogica)
expressaoLogica = (do try $ ouLogico) <|> (do try $ eLogico) <|> expressaoLogica'

expressaoLogica' :: Parsec String () (ExpressaoLogica)
expressaoLogica' = (do try $ naoLogico) <|> (do try $ do pExpressaoLogica) <|> expressaoRelacional

pExpressaoLogica = do char '('
                      spaces
                      e <- expressaoLogica
                      spaces
                      char ')'
                      spaces
                      return e

ouLogico = do l <- expressaoLogica'
              spaces
              string "||"
              spaces
              r <- expressaoLogica
              spaces
              return (Ou l r)

eLogico = do l <- expressaoLogica'
             spaces
             string "&&"
             spaces
             r <- expressaoLogica
             spaces
             return (E l r)

naoLogico = do char '!'
               spaces
               f <- expressaoLogica
               spaces
               return (Nao f)

expressaoRelacional = do string "er"
                         return Verdadeiro
