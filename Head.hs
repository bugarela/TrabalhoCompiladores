module Head where

import RBTree

data LX a = LX (Int -> (a, Int))

data Programa = Prog [DeclaracaoFuncao] BlocoPrincipal deriving(Show)

data BlocoPrincipal = Main [Declaracao] Bloco deriving(Show)

type Bloco = [Comando]

data Literal = Str String | Numb Numeral deriving(Show)

data Numeral = Inteiro Integer | Flutuante Double

type Identificador = String

data Tipo = TInt | TString | TFloat deriving (Eq)

data RelacaoTipo = Identificador :>: Tipo deriving (Eq, Show)

data Retorno = R Tipo | Void deriving(Show,Eq)

data DeclaracaoFuncao = Funcao Retorno Identificador DeclParametros BlocoPrincipal deriving(Show)

data ChamadaFuncao = Chamada Identificador ListaParametros deriving(Show)

type DeclParametros = [ParametroFormal]
data ParametroFormal = ParamFormal Tipo Identificador deriving(Show,Eq)

type ListaParametros = [Parametro]
data Parametro = ParametroLiteral Literal
               | ParametroExpressao ExpressaoAritmetica
               deriving(Show)

data Declaracao = Decl Tipo [Identificador] deriving(Show)

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
                         | Var Identificador
                         | Fun ChamadaFuncao
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

type TabelaDeSimbolos = RBTree (RelacaoTipo,Integer)

type TabelaDeFuncoes = RBTree (Identificador,Retorno,DeclParametros)

data Java = Print

instance Show Numeral where
  show (Inteiro i) = show i
  show (Flutuante f) = show f

instance Show Tipo where
  show TInt = "int"
  show TFloat = "float"
  show TString = "string"
