module Scanner.TokenType  where
data TokenType = TokenType String | BadTokenType deriving (Eq, Ord, Show)
