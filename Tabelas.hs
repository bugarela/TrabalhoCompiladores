module Tabelas where

import Head
import RBTree

-- Tabela de Símbolos

busca = searchFast compVar
insere = insert compVar
compVar ((a :>: _),_) ((b :>: _),_) = compare a b

insereTabelaSimbolos :: [Declaracao] -> TabelaDeSimbolos -> Integer -> TabelaDeSimbolos
insereTabelaSimbolos [] ts _ = ts
insereTabelaSimbolos ((Decl t []):ds) ts p = insereTabelaSimbolos ds ts p
insereTabelaSimbolos ((Decl t (i:is)):ds) ts p = if (busca ts ((i :>: t), p) /= Nothing)
                                                     then error ("Variavel " ++ show i ++ " duplamente declarada")
                                                     else insereTabelaSimbolos ((Decl t is):ds) (insere ts ((i :>: t),p)) (p+1)

posicao i ts = case busca ts ((i :>: TInt), 0) of
                 Nothing -> error("Variavel " ++ show i ++ " indefinida")
                 Just ((_ :>: _),p) -> show p

tipoVariavel i ts = case busca ts ((i :>: TInt), 0) of
                      Nothing -> error("Variavel " ++ show i ++ " indefinida")
                      Just ((_ :>: t),_) -> t

-- Tabela de Funções
-- nome da funcao | retorno | lista de parametros -->

buscaFun = searchFast compFun
insereFun = insert compFun

compFun :: (Identificador,Retorno,DeclParametros) -> (Identificador,Retorno,DeclParametros) -> Ordering
compFun (a,_,_) (b,_,_) = compare a b

--Funcao Retorno Identificador DeclParametros BlocoPrincipal
insereTabelaFuncoes :: [DeclaracaoFuncao] -> TabelaDeFuncoes -> TabelaDeFuncoes
insereTabelaFuncoes [] tf = tf
insereTabelaFuncoes ((Funcao r i ps _):ds) tf = if (buscaFun tf (i,Void,[]) /= Nothing)
                                                 then error ("Funcao " ++ show i ++ " duplamente declarada")
                                                 else insereTabelaFuncoes ds (insereFun tf (i,r,ps))

tipoParam (ParamFormal t _) = t

buscaTipos i (tf,a) = case buscaFun tf (i,Void,[]) of
                           Nothing -> error("Funcao " ++ show i ++ " indefinida")
                           Just (_,r,p) -> (a ++ "." ++ i,r, map tipoParam p)
