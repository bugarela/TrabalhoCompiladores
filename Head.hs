module Head where

data Literal = Str String | Numb Numeral deriving(Show)

data Numeral = Inte Integer | Flutuante Double deriving(Show)

type Identificador = String

data Tipo = Inteiro | String' | Float deriving(Show)

data Retorno = R Tipo | Void deriving(Show)

data Programa = Prog [DeclaracaoFuncao] BlocoPrincipal deriving(Show)

data BlocoPrincipal = Main [Declaracao] Bloco deriving(Show)

data DeclaracaoFuncao = Funcao Retorno Identificador DeclParametros BlocoPrincipal deriving(Show)

data ChamadaFuncao = Chamada Identificador ListaParametros deriving(Show)

type DeclParametros = [ParametroFormal]
data ParametroFormal = ParamFormal Tipo Identificador deriving(Show)

type ListaParametros = [Parametro]
data Parametro = ParametroLiteral Literal
               | ParametroExpressao ExpressaoAritmetica
               deriving(Show)

data Declaracao = Decl Tipo [Identificador] deriving(Show)

type Bloco = [Comando]

data Comando = If ExpressaoLogica Bloco Bloco
             | While ExpressaoLogica Bloco
             | Atribui Identificador Parametro
             | Escreve Parametro
             | Le Identificador
             | ChamadaProc ChamadaFuncao
             | Ret Parametro
             deriving(Show)

data ExpressaoAritmetica = Multiplicacao ExpressaoAritmetica ExpressaoAritmetica
                         | Divisao ExpressaoAritmetica ExpressaoAritmetica
                         | Adicao ExpressaoAritmetica ExpressaoAritmetica
                         | Subtracao ExpressaoAritmetica ExpressaoAritmetica
                         | Neg ExpressaoAritmetica
                         | Numero Numeral
                         deriving (Show)

data ExpressaoLogica = E ExpressaoLogica ExpressaoLogica
                     | Ou ExpressaoLogica ExpressaoLogica
                     | Nao ExpressaoLogica
                     | ER ExpressaoRelacional
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
