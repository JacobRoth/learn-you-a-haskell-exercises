{-

main = do
        line <- getLine
        if (null line)
                then do
                        putStrLn "Halting"
                        --return ()
                else do
                        putStrLn $ reverseWords line
                        main
-}


reverseWords :: String -> String
reverseWords = unwords . map reverse . words

printList :: Show a => [a] -> IO ()
printList = doListOfIO . map print

doListOfIO :: [IO ()] -> IO ()
doListOfIO [] = return ()
doListOfIO (x:xs) = do
        x
        doListOfIO xs

main = printList "ABCDefg"
