{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import           Log

wrongFormatMsg :: String
wrongFormatMsg = "This is not in the right format"

parseMessage :: String -> LogMessage
parseMessage str = go (words str) where
  go :: [String] -> LogMessage
  go ("I" : xs) = parseInfo xs
  go ("W" : xs) = parseWarning xs
  go ("E" : xs) = parseError xs
  go _ = Unknown wrongFormatMsg

parseInfo :: [String] -> LogMessage
parseInfo (timestamp : contents) = LogMessage Info (read timestamp :: Int) (unwords contents)
parseInfo _ = Unknown wrongFormatMsg

parseWarning :: [String] -> LogMessage
parseWarning (timestamp : contents) = LogMessage Warning (read timestamp :: Int) (unwords contents)
parseWarning _ = Unknown wrongFormatMsg

parseError :: [String] -> LogMessage
parseError (severity : timestamp : contents) = LogMessage (Error (read severity :: Int)) (read timestamp :: Int) (unwords contents)
parseError _ = Unknown wrongFormatMsg

parse :: String -> [LogMessage]
parse str = map parseMessage (lines str)

insert :: LogMessage -> MessageTree -> MessageTree
insert msg1@(LogMessage _ timestamp1 _) (Node leftTree msg2@(LogMessage _ timestamp2 _ ) rightTree)
  | timestamp1 > timestamp2 = Node leftTree msg2 (insert msg1 rightTree)
  | timestamp1 < timestamp2 = Node (insert msg1 leftTree) msg2 rightTree
  | otherwise = error "There should be unique timestamps"
insert msg Leaf = Node Leaf msg Leaf
insert (Unknown _) tree = tree
insert _ _ = error "Not possible"

build :: [LogMessage] -> MessageTree
build xs = foldr fun Leaf xs where
  fun :: LogMessage -> MessageTree -> MessageTree
  fun msg tree = insert msg tree

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left message right) = (inOrder left) ++ [message] ++ (inOrder right)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong xs = foldr fun [] (inOrder (build xs)) where
  fun :: LogMessage -> [String] -> [String]
  fun (LogMessage (Error severity) _ contents) acc
    | severity > 50 = contents : acc
    | otherwise = acc
  fun _ acc = acc


logMessages10 :: IO([LogMessage])
logMessages10 = testParse parse 5523 "error.log"

main :: IO ()
main = do
  print $ parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
  print $ parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
  print $ parseMessage "This is not in the right format" == Unknown "This is not in the right format"
  messages <- logMessages10
  print $ whatWentWrong messages
