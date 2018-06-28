module Parser where

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language
import Data.Char
import Head

import Control.Monad.Identity (Identity)

reservados = ",;.(){}\""

ws = do {many (oneOf " \n"); return()}

parseFile a = do f <- readFile a
                 let e = parse programa "Error:" (f)
                 return e

--expressaoAritmetica :: Parsec String () ExpressaoAritmetica
expressaoAritmetica = buildExpressionParser operadoresAritmeticos atomoAritmetico <|> atomoAritmetico

operadoresAritmeticos :: [[Operator String u Identity ExpressaoAritmetica]]
operadoresAritmeticos = [[Prefix (ws >> char '-' >> ws >> return (Neg))],

                        [Infix  (ws >> char '*' >> ws >> return (Multiplicacao)) AssocLeft,
                         Infix  (ws >> char '/' >> ws >> return (Divisao)) AssocLeft],

                        [Infix  (ws >> char '+' >> ws >> return (Adicao)) AssocLeft,
                         Infix  (ws >> char '-' >> ws >> return (Subtracao)) AssocLeft]]

expressaoLogica = buildExpressionParser operadoresLogicos expressaoRelacional <|> expressaoRelacional


operadoresLogicos :: [[Operator String u Identity ExpressaoLogica]]
operadoresLogicos = [[Prefix (char '!' >> ws >> return (Nao))],

                     [Infix  (string "&&" >> ws >> return (E)) AssocLeft,
                      Infix  (string "||" >> ws >> return (Ou)) AssocLeft]]

operadoresRelacionais = do try $ (string ">=" >> ws >> return (MaiorIgual))
                        <|>
                        do try $ (string "<=" >> ws >> return (MenorIgual))
                        <|>
                        (char '>' >> ws >> return (Maior))
                        <|>
                        (char '<' >> ws >> return (Menor))
                        <|>
                        (string "==" >> ws >> return (Igual))
                        <|>
                        (string "!=" >> ws >> return (Diferente))


expressaoRelacional = do try $ do l <- expressaoAritmetica
                                  ws
                                  o <- operadoresRelacionais
                                  r <- expressaoAritmetica
                                  ws
                                  return (ER (o l r))

listaParametros = parametro `sepBy` (char ',')

parametro = do {e <- expressaoAritmetica; ws; return (ParametroExpressao e)}
            <|>
            do {l <- literal; ws; return (ParametroLiteral l)}

chamadaFuncao = do try $ do i <- identificador
                            char '('
                            ws
                            ps <- listaParametros
                            ws
                            char ')'
                            ws
                            return (Chamada i ps)

chamadaProc = do f <- chamadaFuncao
                 char ';'
                 ws
                 return (ChamadaProc f)

cmdLeitura = do try $ do string "read"
                         ws
                         char '('
                         ws
                         i <- identificador
                         char ')'
                         ws
                         char ';'
                         ws
                         return (Le i)

cmdEscrita = do try $ do string "print"
                         ws
                         char '('
                         ws
                         p <- parametro
                         char ')'
                         ws
                         char ';'
                         ws
                         return (Escreve p)

cmdAtrib = do try $ do i <- identificador
                       char '='
                       ws
                       p <- parametro
                       ws
                       char ';'
                       ws
                       return (Atribui i p)

cmdEnquanto = do try $ do string "while"
                          ws
                          char '('
                          ws
                          el <- expressaoLogica
                          ws
                          char ')'
                          b <- bloco
                          return (While el b)

cmdSe = do try $ do string "if"
                    ws
                    char '('
                    ws
                    el <- expressaoLogica
                    ws
                    char ')'
                    ws
                    b1 <- bloco
                    string "else"
                    ws
                    b2 <- bloco
                    return (If el b1 b2)
       <|>
       do try $ do string "if"
                   ws
                   char '('
                   ws
                   el <- expressaoLogica
                   ws
                   char ')'
                   ws
                   b1 <- bloco
                   return (If el b1 [])

retorno = do try $ do string "return "
                      ws
                      p <- parametro
                      ws
                      char ';'
                      ws
                      return (Ret p)

comando = cmdSe <|> cmdEnquanto <|> cmdAtrib <|> cmdEscrita <|> cmdLeitura <|> chamadaProc <|> retorno
listaCmd = many1 comando

bloco = do char '{'
           ws
           cs <- listaCmd
           ws
           char '}'
           ws
           return cs

tipo = do {string "int"; ws; return (TInt)}
       <|>
       do {string "string"; ws; return (TString)}
       <|>
       do {string "float"; ws; return (TFloat)}

void = do {string "void"; ws; return Void}

tipoRetorno = do {t <- tipo; return (R t)} <|> void

declaracao = do try $ do t <- tipo
                         ws
                         is <- listaId
                         char ';'
                         ws
                         return (Decl t is)

blocoPrincipal = do char '{'
                    ws
                    ds <- many declaracao
                    cs <- listaCmd
                    char '}'
                    ws
                    return (Main ds cs)

parametroFormal = do t <- tipo
                     i <- identificador
                     return (ParamFormal t i)

declParametros = parametroFormal `sepBy` (char ',')

funcao = do try $ do tr <- tipoRetorno
                     i <- identificador
                     char '('
                     ws
                     ps <- declParametros
                     char ')'
                     ws
                     b <- blocoPrincipal
                     ws
                     return (Funcao tr i ps b)

programa = do fs <- many funcao
              b <- blocoPrincipal
              ws
              eof
              return (Prog fs b)

identificador :: Parsec String () (Identificador)
identificador = do s <- oneOf (['a'..'z'] ++ ['_'])
                   ss <- many (oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['_'] ++ ['0'..'9']))
                   ws
                   return (s:ss)

listaId = identificador `sepBy` (char ',')

literal = do try $ do n <- numero
                      return (Numb n)
          <|>
          do try $ do {char '\"'; cs <- many1 (noneOf reservados); char '\"'; return (Str cs)}

atomoAritmetico = do {n <- numero; return (Numero n)}
                  <|>
                  do try $ do {f <- chamadaFuncao; ws; return (Fun f)}
                  <|>
                  do {i <- identificador; return (Var i)}
                  <|>
                  do char '('
                     ws
                     e <- expressaoAritmetica
                     ws
                     char ')'
                     ws
                     return e

numero = do try $ do f <- Token.float (Token.makeTokenParser emptyDef)
                     ws
                     return (Flutuante f)
         <|>
         do try $ do digits <- many1 digit
                     let n = foldl (\x d -> 10*x + toInteger (digitToInt d)) 0 digits
                     ws
                     return (Inteiro n)
