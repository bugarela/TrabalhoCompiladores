module Head where

--data Programa = Completo ListaFuncoes BlocoPrincipal
  --            | Main BlocoPrincipal

data OperadorAritmetico = Multiplicacao | Divisao | Adicao | Subtracao deriving (Show)

data ExpressaoAritmetica = Binario OperadorAritmetico ExpressaoAritmetica ExpressaoAritmetica
                         | Neg ExpressaoAritmetica
                         | AA
                         deriving (Show)

data ExpressaoLogica = Ou ExpressaoLogica ExpressaoLogica
                     | E ExpressaoLogica ExpressaoLogica
                     | Nao ExpressaoLogica
                     | Verdadeiro
                     | Falso

instance Show ExpressaoLogica where
   show (E a b) = "(" ++ show a ++ " && " ++ show b ++ ")"
   show Verdadeiro = "Verdadeiro"
