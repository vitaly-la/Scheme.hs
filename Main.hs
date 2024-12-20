{-# OPTIONS -Wno-missing-methods #-}

import Control.Monad.State.Lazy
import Data.Map.Lazy ((!))
import qualified Data.Map.Lazy as M
import Text.Read (readMaybe)

data Expression = Number Integer | Word String | List [Expression] | End

instance Show Expression where
    show (Number number) = show number
    show (Word word) = word
    show (List exprs) = "(" ++ unwords (map show exprs) ++ ")"

instance Num Expression where
    Number x + Number y = Number $ x + y
    Number x - Number y = Number $ x - y
    Number x * Number y = Number $ x * y
    fromInteger x = Number x

instance Eq Expression where
    Number x == Number y = x == y
    Word x == Word y = x == y
    List xs == List ys = xs == ys

unbracket [] = []
unbracket ('(':xt) = "(" : unbracket xt
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
    (tokens, _, _) <- get
    case tokens of
        [] -> return End
        (token:_) -> do
            modify $ \(tokens, var, fns) -> (tail tokens, var, fns)
            case token of
                "(" -> do
                    subexprs <- collectSubexprs
                    return $ List subexprs
                ")" -> return End
                _ -> case readMaybe token of
                    Just number -> return $ Number number
                    _ -> return $ Word token

getVariable name = do
    (tokens, var, fns) <- get
    return $ var ! name

setVariable name value = modify $ \(tokens, var, fns) -> (tokens, M.insert name value var, fns)

setVariables [] = return ()
setVariables ((Word name, value):rest) = do
    setVariable name value
    setVariables rest

eval expr = case expr of
    Number _ -> return expr
    Word name -> getVariable name
    List [] -> return expr
    List (Word fn:args) -> case fn of
        "quote" -> return $ head args
        "define" -> case args of
            [Word name, value] -> do
                setVariable name value
                return $ Word name
            [List (Word name:fargs), body] -> do
                modify $ \(tokens, var, fns) -> (tokens, var, M.insert name (fargs, body) fns)
                return $ Word name
        "if" -> do
            value <- eval $ head args
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
                "cons" -> let [x, List xt] = args in return $ List (x : xt)
                _ -> do
                    (_, _, fns) <- get
                    let (fargs, body) = fns ! fn
                    setVariables $ zip fargs args
                    eval body

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
    runStateT loop (tokens, M.empty, M.empty)
