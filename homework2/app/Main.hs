import Log

parseMessage :: String ->LogMessage
parseMessage s =
    case words s of
    ("I":t:xs) -> LogMessage Info (read t) (unwords xs)
    ("W":t:xs) -> LogMessage Warning (read t) (unwords xs)
    ("E":sev:t:xs) -> LogMessage (Error (read sev)) (read t) (unwords xs)
    _ -> Unknown s

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt =  mt
insert lm@(LogMessage _ _ _ ) Leaf = Node Leaf lm Leaf
insert lm@(LogMessage _ ts _ ) (Node left toCompare@(LogMessage _ toCompareTs _) right) 
    | ts < toCompareTs = Node (insert lm left) toCompare right
    | otherwise = Node left toCompare (insert lm right)
insert _ mt = mt

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left lm right) = (inOrder left) ++ [lm] ++ (inOrder right)

getMessageFromSevereError :: LogMessage -> String
getMessageFromSevereError (LogMessage (Error sev) _ m)  
            | sev >= 50 = m
            | otherwise = []
getMessageFromSevereError _ = []

notEmpty :: String -> Bool
notEmpty s = s /= []

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong lms = filter notEmpty (map getMessageFromSevereError (inOrder (build lms)))
