data StackItem = Number Double | BinOp BinaryOperator | UnOp UnaryOperator deriving (Show, Eq)
data BinaryOperator = Plus | Minus | Times | Divide | Exponent deriving (Show,Eq)
data UnaryOperator = Sqrt | LogE | LogTen deriving (Show, Eq)

type Stack = [StackItem] -- rpn stack

processStackOnePass :: Stack -> Stack
processStackOnePass ((Number n1):(Number n2):(BinOp someoperator):xs) = ((Number result): xs)
    where
        result = case someoperator of
            Plus     -> n1+n2
            Minus    -> n1-n2
            Times    -> n1*n2
            Divide   -> n1/n2
            Exponent -> n1 ** n2

processStackOnePass ((Number n):(UnOp someoperator):xs) = processStack ((Number result):xs)
    where
        result = case someoperator of
            Sqrt   -> sqrt n
            LogE   -> log n
            LogTen -> logBase 10 n
            
processStackOnePass (x:xs) = x:(processStackOnePass xs) 
processStackOnePass [] = [] -- base case

processStack stack
    | newstack == stack = newstack
    | otherwise         = processStack newstack
    where
        newstack = processStackOnePass stack
