import Data.Char

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace 
    deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken "+" = Just Plus
asToken "-" = Just Minus
asToken "(" = Just LeftBrace
asToken ")" = Just RightBrace
asToken s = if (all isDigit s)
    then Just $ Number (read s)
    else Nothing 

tokenize :: String -> Maybe [Token]
tokenize s = foldr toList (Just []) (words s)
    where toList curr acc = do {
        t <- asToken curr;
        list <- acc;
        return $ t:list
    }
