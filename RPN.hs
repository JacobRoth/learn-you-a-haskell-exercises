data StackItem = Number Double | BinOp BinaryOperator | UnOp UnaryOperator | StackOp StackOperator deriving (Show, Eq)
data BinaryOperator = Plus | Minus | Times | Divide | Exponent deriving (Show,Eq)
data UnaryOperator = Sqrt | Ln | LogTen deriving (Show, Eq)
data StackOperator = Sum | Product deriving (Show,Eq)

type Stack = [StackItem] -- rpn stack. We store the stack backwards, I.E. the top of the stack is the head of the list. This means pushing/popping the stack is done with consing to the head, or stuff near the head.

crunchStack :: Stack -> Stack
crunchStack ((BinOp someoperator):(Number n2):(Number n1):xs) = crunchStack ((Number result): xs)
    where
        result = case someoperator of
            Plus     -> n1+n2
            Minus    -> n1-n2
            Times    -> n1*n2
            Divide   -> n1/n2
            Exponent -> n1 ** n2

crunchStack ((UnOp someoperator):(Number n):xs) = crunchStack ((Number result): xs)
    where
        result = case someoperator of
            Sqrt   -> sqrt n
            Ln     -> log n
            LogTen -> logBase 10 n

crunchStack ((StackOperator someoperator):xs) = crunchStack $ f xs
    where
        f = case someoperator of
            Sum -> sum
            Product -> product

crunchStack (x:xs) = x:(crunchStack xs)
crunchStack []  = []  -- base case

solveStack :: Stack -> Stack
solveStack = foldr (\x acc -> crunchStack (x:acc)) []
