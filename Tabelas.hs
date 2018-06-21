module Tabelas where

import Head
import RBTree

busca = searchFast compVar
insere = insert compVar
compVar ((a :>: _),_) ((b :>: _),_) = compare a b

----------------------------
-- Tabela de Símbolos

insereTabelaSimbolos :: [Declaracao] -> TabelaDeSimbolos -> Integer -> TabelaDeSimbolos
insereTabelaSimbolos [] ts p = ts
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
