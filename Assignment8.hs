--Name: Maddi Clark
--KUID: 3162616
--Date: 04/27/2026
--File Name: Assignment8.hs
--Description: An arithmetic parser that can follow PEMDAS, handle errors, and use basic arithmetic operators
--Inputs: An arithmetic expression
--Outputs: The solution to a given arithmetic expression
--Outside Sources:
--Collaborators: Kaitlyn Bedgood

import Debug.Trace --dont forget to remove this or youre fucked

--Turns operators into the Op data type (from slides)
data Op = Add | Sub | Mul | Div | Mod | Exp
    deriving (Eq)
instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Mod = "%"
    show Exp = "**"

--Turns possible inputs into Token data type (from slides)
data Token = Num Int | Op Op | LeftParen | RightParen
    deriving (Eq)
instance Show Token where
    show (Num n) = show n
    show (Op o) = show o
    show LeftParen = "("
    show RightParen = ")"


--ChatGPT helped write this code, "Help me create a parse numbers function in haskell that takes in a list of tokens and combines minus signs and number tokens into negative numbers"
data Prev
  = Start
  | AfterOp
  | AfterLParen
  | AfterNum
  | AfterRParen
  deriving (Eq, Show)

--Turns all input characters into tokens of their respective data type and handles errors of unrecognized characters (from slides)
parseTokens :: [Char] -> [Token]
parseTokens [] = []

parseTokens (c:cs)
    | c >= '0' && c <= '9' =
        let (digits, rest) = span isDigitChar (c:cs)
        in Num (read digits) : parseTokens rest

parseTokens ('*': '*' : xs) = Op Exp : parseTokens xs
parseTokens ('%': xs) = Op Mod : parseTokens xs
parseTokens ('/': xs) = Op Div : parseTokens xs
parseTokens ('*': xs) = Op Mul : parseTokens xs
parseTokens ('-': xs) = Op Sub : parseTokens xs
parseTokens ('+': xs) = Op Add : parseTokens xs

parseTokens ('(' : xs) = LeftParen : parseTokens xs
parseTokens (')' : xs) = RightParen : parseTokens xs

parseTokens (' ' : xs) = parseTokens xs
parseTokens xs = error
    ("Invalid Characters: unrecognized token starting with " ++ xs)

isDigitChar :: Char -> Bool
isDigitChar c = c >= '0' && c <= '9'

--This code was written with both the notes and ChatGPT "Help me create a parse numbers function in haskell that takes in a list of tokens and combines minus signs and number tokens into negative numbers"
parseNumbers :: [Token] -> [Token] --adds negative number functionality
parseNumbers tokens = go Start tokens
    where
        go _ [] = []

        go prev (Op Sub : Num n : rest)
            | isUnaryContext prev =
                Num (-n) : go AfterNum rest
            
        go _ (Op Sub: rest) = 
            Op Sub : go AfterOp rest
        
        go _ (Num n: rest) = 
            Num n : go AfterNum rest

        go _ (LeftParen : rest) =
            LeftParen : go AfterLParen rest
        
        go _ (RightParen : rest) = 
            RightParen : go AfterRParen rest
        
        go _ (Op Add : rest) = 
            Op Add : go AfterOp rest

        go _ (Op Mul : rest) =
            Op Mul : go AfterOp rest

        go _ (Op Div : rest) = 
            Op Div : go AfterOp rest
        
        isUnaryContext Start = True
        isUnaryContext AfterOp = True
        isUnaryContext AfterLParen = True
        isUnaryContext _ = False


isOperator :: Token -> Bool
isOperator (Op _) = True
isOperator _ = False

prec :: Op -> Int
prec Add = 1
prec Sub = 1
prec Mul = 2
prec Div = 2
prec Mod = 2
prec Exp = 3
prec _ = 0

--Shunting yard, from notes
shunt :: ([Token], [Token]) -> [Token]
shunt (stk, []) =
  if LeftParen `elem` stk
    then error "Mismatched parentheses"
    else reverse stk

shunt (stk, Num n : xs) =
    Num n : shunt (stk, xs)

shunt (stk, Op o1 : xs) =
  let (stk', out) = pop stk
  in out ++ shunt (Op o1 : stk', xs)
  where
    pop [] = ([], [])
    pop (LeftParen : xs) = (LeftParen : xs, [])

    pop (Op o2 : xs)
      | prec o2 >= prec o1 =
          let (stk'', out) = pop xs
          in (stk'', Op o2 : out)
      | otherwise = (Op o2 : xs, [])

    pop xs = (xs, [])

shunt (stk, LeftParen : xs) =
    shunt (LeftParen : stk, xs)

shunt (stk, RightParen : xs) =
  case break (== LeftParen) stk of
    (before, _:after) ->
      before ++ shunt (after, xs)
    _ ->
      error "Mismatched parentheses"

apply :: Op -> Float -> Float -> Float
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x / y
apply Mod x y = x - (fromIntegral(floor (x/y)) * y)
apply Exp x y = x ** y

rpn :: ([Float], [Token]) -> Float
rpn ([x], []) = x

rpn (stk, Num n : xs) =
  rpn (fromIntegral n : stk, xs)

rpn (y:x:stk, Op o : xs) =
  rpn (apply o x y : stk, xs)

rpn _ =
  error "Malformed RPN expression (check parentheses/operator balance)"

shuntDebug xs = traceShow (shunt ([], xs)) (shunt ([], xs))


parse :: [Char] -> Float
parse xs =
  let tokens  = parseTokens xs
      fixed   = parseNumbers tokens
      postfix = shunt ([], fixed)
  in rpn ([], postfix)