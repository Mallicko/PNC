import Data.Char (isDigit, isSpace, toLower)
import Data.Ratio ((%), numerator, denominator)
import Text.Read (readMaybe)

-- | Main function to start the calculator.
main :: IO ()
main = evalLoop []

-- | The evaluation loop takes the history of computations.
evalLoop :: [Rational] -> IO ()
evalLoop history = do
    putStrLn "Enter an expression:"
    input <- getLine
    if map toLower input `elem` ["quit", "exit"]
        then putStrLn "Goodbye!"
        else do
            let parsed = parseExpression input history
            case parsed of
                Right (value, "") -> do
                    let newHistory = value : history
                    putStrLn ("Result: " ++ showRational value ++ " History ID: " ++ show (length newHistory))
                    evalLoop newHistory
                Right (_, _) -> do
                    putStrLn "Unexpected Trailing Characters"
                    evalLoop history
                Left err -> do
                    putStrLn err
                    evalLoop history

-- | Operator data type to represent supported operators.
data Operator = Add | Multiply | Divide deriving (Eq)

-- | Parses and evaluates the expression.
parseExpression :: String -> [Rational] -> Either String (Rational, String)
parseExpression input history =
    case dropWhile isSpace input of
        [] -> Left "Invalid Expression"
        (x:xs) ->
            case x of
                '+' -> parseBinaryOp Add xs history
                '*' -> parseBinaryOp Multiply xs history
                '/' -> parseBinaryOp Divide xs history
                '-' -> parseUnaryOp negate xs history
                '$' -> parseHistoryRef xs history
                _   -> parseNumber (x:xs)

-- | Parses binary operations and handles division by zero.
parseBinaryOp :: Operator -> String -> [Rational] -> Either String (Rational, String)
parseBinaryOp op xs history = do
    (first, rest1) <- parseExpression xs history
    (second, rest2) <- parseExpression rest1 history
    case op of
        Add      -> Right (first + second, rest2)
        Multiply -> Right (first * second, rest2)
        Divide   -> if second == 0
                        then Left "Invalid Expression"  -- Division by zero
                        else Right (first / second, rest2)

-- | Parses unary operations.
parseUnaryOp :: (Rational -> Rational) -> String -> [Rational] -> Either String (Rational, String)
parseUnaryOp op xs history = do
    (value, rest) <- parseExpression xs history
    Right (op value, rest)

-- | Parses history references.
parseHistoryRef :: String -> [Rational] -> Either String (Rational, String)
parseHistoryRef xs history =
    let (numStr, rest) = span isDigit xs
    in if null numStr 
        then Left "Invalid Expression"
        else let n = read numStr
                 index = length history - n
             in if index >= 0 && index < length history
                then Right (history !! index, rest)
                else Left "Invalid Expression"

-- | Parses numbers.
parseNumber :: String -> Either String (Rational, String)
parseNumber xs =
    let (numStr, rest) = span isDigit xs
    in if null numStr
        then Left "Invalid Expression"
        else if not (null rest) && head rest == '.'
            then Left "Unexpected Trailing Characters"
            else case readMaybe numStr :: Maybe Integer of
                    Just n  -> Right (fromIntegral n % 1, rest)
                    Nothing -> Left "Invalid Expression"

-- | Converts a Rational to a simplified fraction string
showRational :: Rational -> String
showRational r =
    let n = numerator r
        d = denominator r
    in if d == 1
        then show n
        else show n ++ "/" ++ show d