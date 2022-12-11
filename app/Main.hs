module Main where

data BatchedQueue a = BQ [a] [a] deriving(Show)

empty :: BatchedQueue a
empty = BQ [] []

isEmpty :: BatchedQueue a -> Bool
isEmpty (BQ outbox inbox) = null outbox

enqueue :: BatchedQueue a -> a -> BatchedQueue a
enqueue (BQ [] _) x = BQ [x] []
enqueue (BQ outbox inbox) x = BQ outbox (x:inbox)

dequeue :: BatchedQueue a -> BatchedQueue a
dequeue (BQ [] _) = BQ [] []
dequeue (BQ [x] inbox) = BQ (reverse inbox) []
dequeue (BQ (_:outbox) inbox) = BQ outbox inbox

main :: IO ()
main = putStrLn "Efficient Functional Queue"
