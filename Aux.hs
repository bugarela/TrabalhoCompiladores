module Aux where

import Parser
import Head

-- TODO: mudar para uma estrutura de dados decente
insereTabelaSimbolos :: [Declaracao] -> TabelaDeSimbolos -> TabelaDeSimbolos
insereTabelaSimbolos [] ts = ts
insereTabelaSimbolos ((Decl t []):ds) ts = insereTabelaSimbolos ds ts
insereTabelaSimbolos ((Decl t (i:is)):ds) ts =  insereTabelaSimbolos ((Decl t is):ds) (ts ++ [i :>: t])
