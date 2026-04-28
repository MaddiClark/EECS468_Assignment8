--Name: Maddi Clark
--KUID: 3162616
--Date: 04/27/2026
--File Name: Assignment8.hs
--Description: An arithmetic parser that can follow PEMDAS, handle errors, and use basic arithmetic operators
--Inputs: An arithmetic expression
--Outputs: The solution to a given arithmetic expression
--Outside Sources:
--Collaborators: Kaitlyn Bedgood

--Turns operators into the Op data type (from slides)
data Op = Add | Sub | Mul | Div | Mod | Exp
instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Mod = "%"
    show Exp = "**"

--Turns possible inputs into Token data type (from slides)
data Token = Num Int | Op Op | LeftParen | RightParen
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
parseTokens ('*': '*' : xs) = Op Exp : parseTokens xs
parseTokens ('%': xs) = Op Mod : parseTokens xs
parseTokens ('/': xs) = Op Div : parseTokens xs
parseTokens ('*': xs) = Op Mul : parseTokens xs
parseTokens ('-': xs) = Op Sub : parseTokens xs
parseTokens ('+': xs) = Op Add : parseTokens xs
parseTokens ('(' : xs) = LeftParen : parseTokens xs
parseTokens (')' : xs) = RightParen : parseTokens xs
parseTokens ('0' : xs) = Num 0 : parseTokens xs
parseTokens ('1' : xs) = Num 1 : parseTokens xs
parseTokens ('2' : xs) = Num 2 : parseTokens xs
parseTokens ('3' : xs) = Num 3 : parseTokens xs
parseTokens ('4' : xs) = Num 4 : parseTokens xs
parseTokens ('5' : xs) = Num 5 : parseTokens xs
parseTokens ('6' : xs) = Num 6 : parseTokens xs
parseTokens ('7' : xs) = Num 7 : parseTokens xs
parseTokens ('8' : xs) = Num 8 : parseTokens xs
parseTokens ('9' : xs) = Num 9 : parseTokens xs
parseTokens (' ' : xs) = parseTokens xs
parseTokens xs = error
    ("Invalid Characters: unrecognized token starting with " ++ xs)

--This code was written with both the notes and ChatGPT "Help me create a parse numbers function in haskell that takes in a list of tokens and combines minus signs and number tokens into negative numbers"
parseNumbers :: [Token] -> [Token] --adds negative number functionality
parseNumbers tokens = go Start tokens
    where
        go _ [] = []

        go prev (Sub : Num n : rest)
            | isUnaryContext prev =
                Num (-n) : go AfterNum rest
            
        go _ (Sub: rest) = 
            Sub : go AfterOp reset
        
        go _ (Num n: rest) = 
            Num n : go AfterNum rest

        go _ (LeftParen : rest) =
            LeftParen : go AfterLParen rest
        
        go _ (RightParen : rest) = 
            RightParen : go AfterRParen rest
        
        go _ (Add : rest) = 
            Add : go AfterOp rest

        go _ (Mul : rest) =
            Mul : go AfterOp rest

        go _ (Div : rest) = 
            Div : go AfterOp rest
        
        isUnaryContext Start = True
        isUnaryContext AfterOp = True
        isUnaryContext AfterLParen = True
        isUnaryContext _ = False

--Shunting yard, from notes
shunt :: ([Token], [Token]) -> [Token]
shunt ([], []) = []
shunt (stk, (Num x : xs)) = Num x : shunt (stk, xs)

apply :: Op -> Float -> Float -> Float
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x / y
apply Mod x y = x - (fromIntegral(floor (x/y)) * y)
apply Exp x y = x ** y

rpn :: ([Float], [Token]) -> Float
rpn ([x], []) = x
rpn (stk, (Num n : xs)) = rpn ((fromIntegral n : stk), xs)
rpn ((x : y : stk), (Op o : xs)) = rpn ((apply o y x : stk), xs)


parse :: [Char] -> Float
parse xs = rpn (shunt (parseTokens xs))