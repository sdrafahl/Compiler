{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Parser.AST where

import Parser.CFG

type Child = AST
type Children = [Child]
type Symbol = NonTerminalOrTerminal
type Root = ASTNode

data ASTNode = ASTNode Symbol Children

data AST = AST Root
