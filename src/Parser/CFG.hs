{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Parser.CFG  where

import Data.Set

data NonTerminal = NonTerminal String
data Terminal = Terminal String
data NonTerminalOrTerminal = Term Terminal | NonTerm NonTerminal
data ProductionRule = ProductionRule (NonTerminalOrTerminal, NonTerminalOrTerminal)
data StartSymbol = StartSymbol String

data CFG = CFG {nonTerminals :: Set NonTerminal, terminals :: Set Terminal, productionRules :: Set ProductionRule, startSymbols :: Set StartSymbol}
