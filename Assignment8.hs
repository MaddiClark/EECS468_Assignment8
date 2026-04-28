--Name: Maddi Clark
--KUID: 3162616
--Date: 04/27/2026
--File Name: Assignment8.hs
--Description: An arithmetic parser that can follow PEMDAS, handle errors, and use basic arithmetic operators
--Inputs: An arithmetic expression
--Outputs: The solution to a given arithmetic expression
--Outside Sources: Learnuahaskell, ChatGPT
--Collaborators: Kaitlyn Bedgood

--Turns operators into the Op data type
data Op = Add | Sub | Mul | Div | Mod | Exp
    deriving (Eq)
instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Mod = "%"
    show Exp = "**"

--Turns possible inputs into Token data type
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
--Allows use of multi digit numbers
parseTokens (c:cs)
    | c >= '0' && c <= '9' =
        let (digits, rest) = span isDigitChar (c:cs)
        in Num (read digits) : parseTokens rest
--Operators
parseTokens ('*': '*' : xs) = Op Exp : parseTokens xs
parseTokens ('%': xs) = Op Mod : parseTokens xs
parseTokens ('/': xs) = Op Div : parseTokens xs
parseTokens ('*': xs) = Op Mul : parseTokens xs
parseTokens ('-': xs) = Op Sub : parseTokens xs
parseTokens ('+': xs) = Op Add : parseTokens xs
--Parentheses
parseTokens ('(' : xs) = LeftParen : parseTokens xs
parseTokens (')' : xs) = RightParen : parseTokens xs
--Removing Spaces
parseTokens (' ' : xs) = parseTokens xs
--Adding an error if an unrecognized character is inputted
parseTokens xs = error
    ("Invalid Characters: unrecognized token starting with " ++ xs)

--Helper function to check if a character is a digit
isDigitChar :: Char -> Bool
isDigitChar c = c >= '0' && c <= '9'

--Helper function to check if an operator is binary or unary
isBinaryOp :: Op -> Bool
isBinaryOp Add = True
isBinaryOp Sub = True
isBinaryOp Mul = True
isBinaryOp Div = True
isBinaryOp Mod = True
isBinaryOp Exp = True

--This code was written with both the notes and ChatGPT "Help me create a parse numbers function in haskell that takes in a list of tokens and combines minus signs and number tokens into negative numbers"
parseNumbers :: [Token] -> [Token] --adds negative number functionality
parseNumbers tokens = go Start tokens --uses go to create cases so we can use negative numbers
    where
        go _ [] = []

        go prev (Op o : rest) = --base operator case
            Op o : go AfterOp rest

        go prev (Op Sub : Num n : rest) --Unary minus sign case
            | isUnaryContext prev =
                Num (-n) : go AfterNum rest
            
        go _ (Op Sub: rest) =  --Binary subtraction case
            Op Sub : go AfterOp rest
        
        go _ (Num n: rest) = --Number case
            Num n : go AfterNum rest

        go _ (LeftParen : rest) = --Left parenthesis case
            LeftParen : go AfterLParen rest
        
        go _ (RightParen : rest) = --Right parenthesis case
            RightParen : go AfterRParen rest
        
        isUnaryContext Start = True
        isUnaryContext AfterOp = True
        isUnaryContext AfterLParen = True
        isUnaryContext _ = False

parseNumbers (Op o : rest) --checks for valid operator placement
  | isBinaryOp o = Op o : parseNumbers rest
  | otherwise = error "Invalid operator placement"

--Helper function to check if something is an operator
isOperator :: Token -> Bool
isOperator (Op _) = True
isOperator _ = False

--Helper function to make exponent operator apply to the token to its right instead of left
assoc :: Op -> String
assoc Exp = "right"
assoc _   = "left"

--Helper function to add precedence to operators
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
shunt (stk, []) = --Base case to check if there are mismatched parentheses
  if LeftParen `elem` stk
    then error "Mismatched parentheses"
    else reverse stk

shunt (stk, Num n : xs) = --Shunts numbers
    Num n : shunt (stk, xs)

shunt (stk, Op o1 : xs) = --Shunts operators
    let (stk', popped) = pop stk
    in popped ++ shunt (Op o1 : stk', xs)
  where
    pop [] = ([], [])

    pop (LeftParen : xs) = (LeftParen : xs, [])

    pop (Op o2 : xs)
      | prec o2 > prec o1 ||
        (prec o2 == prec o1 && assoc o1 == "left") =
          let (stk'', popped) = pop xs
          in (stk'', Op o2 : popped)

      | otherwise = (Op o2 : xs, [])

    pop (x:xs) = --pops stuff
      let (stk'', popped) = pop xs
      in (stk'', x : popped)

shunt (stk, LeftParen : xs) = --shunts left parenthesis
    shunt (LeftParen : stk, xs)

shunt (stk, RightParen : xs) = --shunts right parenthesis and again checks for mismatched parenthesis
  case break (== LeftParen) stk of
    (before, _:after) -> shunt (after, xs)
    _ -> error "Mismatched parentheses"

apply :: Op -> Float -> Float -> Float --applies operators 
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x 0 = error "Division by zero"
apply Div x y = x / y

apply Mod x 0 = error "Modulo by zero"
apply Mod x y = x - (fromIntegral (floor (x / y)) * y)
apply Exp x y = x ** y

rpn :: ([Float], [Token]) -> Float --rpn base case
rpn ([x], []) = x

rpn (stk, Num n : xs) = --rpn num case
  rpn (fromIntegral n : stk, xs)

rpn (y:x:stk, Op o : xs) = --rpn op case
  rpn (apply o x y : stk, xs)

rpn _ = --error message
  error "Malformed RPN expression (check parentheses/operator balance)"



parse :: [Char] -> Float --parsing function
parse xs = 
  let tokens  = parseTokens xs
      fixed   = parseNumbers tokens
      postfix = shunt ([], fixed)
  in rpn ([], postfix)