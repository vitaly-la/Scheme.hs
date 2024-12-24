{-# OPTIONS -Wno-missing-methods #-}

import Control.Monad.State.Lazy
import qualified Data.Map.Lazy as M
import Text.Read (readMaybe)

data Expression = Number Integer | Word String | List [Expression] | Function [Expression] Expression | Builtin String | End deriving (Eq, Ord)

instance Show Expression where
    show (Number number) = show number
    show (Word word) = word
    show (List exprs) = "(" ++ unwords (map show exprs) ++ ")"
    show (Function _ _) = "function"
    show (Builtin _) = "builtin"

instance Num Expression where
    Number x + Number y = Number $ x + y
    Number x - Number y = Number $ x - y
    Number x * Number y = Number $ x * y
    fromInteger x = Number x

builtins = M.fromList [(name, Builtin name) | name <- ["quote", "define", "if", "+", "-", "*", "=", "<", "cons", "car", "cdr", "length", "null"]]

unbracket [] = []
unbracket ('(':xt) = "(" : unbracket xt
unbracket ('\'':xt) = "'" : unbracket xt
unbracket (x:xt) = case unbracket xt of
    [] -> [[x]]
    ys@(")":_) -> [x] : ys
    (y:yt) -> (x : y) : yt

collectSubexprs = do
    expr <- readExpr
    case expr of
        End -> return []
        _ -> do
            rest <- collectSubexprs
            return (expr : rest)

readExpr = do
    tokens <- gets fst
    case tokens of
        [] -> return End
        (token:rest) -> do
            modify $ \(_, store) -> (rest, store)
            case token of
                "(" -> do
                    subexprs <- collectSubexprs
                    return $ List subexprs
                ")" -> return End
                "'" -> do
                    expr <- readExpr
                    return $ List [Word "quote", expr]
                _ -> case readMaybe token of
                    Just number -> return $ Number number
                    _ -> return $ Word token

getVariable name = gets $ (M.! name) . snd

setVariable name value = modify $ \(tokens, store) -> (tokens, M.insert name value store)

setVariables [] = return ()
setVariables ((Word name, value):rest) = do
    setVariable name value
    setVariables rest

eval expr = case expr of
    Number _ -> return expr
    Word name -> getVariable name
    List [] -> return expr
    List (fn:args) -> do
        fn <- eval fn
        case fn of
            Function fargs body -> do
                args <- traverse eval args
                backup <- get
                setVariables $ zip fargs args
                res <- eval body
                put backup
                return res
            Builtin fn -> case fn of
                "quote" -> return $ args !! 0
                "define" -> case args of
                    [Word name, value] -> do
                        setVariable name value
                        return $ Word name
                    [List (Word name:fargs), body] -> do
                        setVariable name $ Function fargs body
                        return $ Word name
                "if" -> do
                    value <- eval $ args !! 0
                    case value of
                        0 -> eval $ args !! 2
                        _ -> eval $ args !! 1
                _ -> do
                    args <- traverse eval args
                    case fn of
                        "+" -> return $ sum args
                        "-" -> case args of
                            [arg] -> return $ -arg
                            _ -> return $ head args - sum (tail args)
                        "*" -> return $ product args
                        "=" -> return $ if all (head args ==) $ tail args then 1 else 0
                        "<" -> return $ if args !! 0 < args !! 1 then 1 else 0
                        "cons" -> let [x, List xt] = args in return $ List (x : xt)
                        "car" -> let List (x:xt) = args !! 0 in return x
                        "cdr" -> let List (x:xt) = args !! 0 in return $ List xt
                        "length" -> let List xs = args !! 0 in return . Number . toInteger $ length xs
                        "null" -> let List xs = args !! 0 in return $ if length xs == 0 then 1 else 0

loop = do
    expr <- readExpr
    case expr of
        End -> return ()
        _ -> do
            value <- eval expr
            liftIO $ print value
            loop


main = do
    tokens <- fmap (concat . map unbracket . concat . map words . lines) getContents
    let store = builtins
    runStateT loop (tokens, store)
