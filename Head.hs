module Head where

data Literal = String | Integer

type Identificador = String

--data Programa = Completo ListaFuncoes BlocoPrincipal
  --            | Main BlocoPrincipal

type ListaParametros = [Parametro]
data Parametro = ParametroLiteral Literal
               | ParametroExpressao ExpressaoAritmetica

data ExpressaoAritmetica = Multiplicacao ExpressaoAritmetica ExpressaoAritmetica
                         | Divisao ExpressaoAritmetica ExpressaoAritmetica
                         | Adicao ExpressaoAritmetica ExpressaoAritmetica
                         | Subtracao ExpressaoAritmetica ExpressaoAritmetica
                         | Neg ExpressaoAritmetica
                         | AA
                         deriving (Show)

data ExpressaoLogica = E ExpressaoLogica ExpressaoLogica
                     | Ou ExpressaoLogica ExpressaoLogica
                     | Nao ExpressaoLogica
                     | Verdadeiro
                     | Falso
                     deriving (Show)

data ExpressaoRelacional = Maior ExpressaoAritmetica ExpressaoAritmetica
                         | Menor ExpressaoAritmetica ExpressaoAritmetica
                         | MaiorIgual ExpressaoAritmetica ExpressaoAritmetica
                         | MenorIgual ExpressaoAritmetica ExpressaoAritmetica
                         | Igual ExpressaoAritmetica ExpressaoAritmetica
                         | Diferente ExpressaoAritmetica ExpressaoAritmetica
                         deriving (Show)



{-instance Show ExpressaoLogica where
   show (BinarioLogico E a b) = "(" ++ show a ++ " && " ++ show b ++ ")"
   show (Nao a) = "!" ++ show a
   show Verdadeiro = "Verdadeiro" -}
