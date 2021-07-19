import Data.List
import Control.Monad.Trans.State.Lazy
import Control.Monad

(##>) :: Logger a -> Logger b -> Logger b
Logger la _ ##> Logger lb b = Logger (la++lb) b

filterLog :: (Eq a, Show a) => (a -> Bool) -> [a] -> Logger [a]
filterLog f [] = nomsg []
filterLog f (x:xs)
   | f x       = filterLog f xs #> (\xs' -> annotate ("keeping "++show x) (x:xs'))
   | otherwise = filterLog f xs #> (\xs' -> annotate ("dropping " ++ show x) xs')

(#>) :: Logger a -> (a -> Logger b) -> Logger b
Logger la a #> f = let Logger lb b = f a  -- feed value to next step
                   in Logger (la++lb) b   -- bundle result with all messages

data Logger a = Logger [String] a  deriving Show

getVal :: Logger a -> a
getVal (Logger _ a) = a
getLog :: Logger a -> [String]
getLog (Logger s _) = s

-- Primitive operations:
nomsg :: a -> Logger a
nomsg x = Logger [] x        -- a value, no message

annotate :: String -> a -> Logger a
annotate s x = Logger [s] x  -- a value and a message

msg :: String -> Logger ()
msg s = Logger [s] ()        -- just a message


a :: Maybe Integer
a = Just 1 >>= \x -> return (x+1)

increase :: Eq a => a -> Int -> [(a,Int)] -> Maybe [(a,Int)]
increase key val assocs =
  do oldVal <- lookup key assocs
     check oldVal
     return ((key,val) : delete (key,oldVal) assocs)
  where check x
           | val < x   = Nothing
           | otherwise = return x

test ::Maybe Integer
test = do
       return 0
       return 1

test3 = let increment = modify (+1) >> get
            ops = replicateM_ 4 increment
        in runState ops 0

rememberElements :: (a -> Bool) -> [a] -> State [a] ()
rememberElements f xs = mapM_ maybePut xs
  where maybePut x = when (f x) (modify (++[x]))

sfilter :: (a -> Bool) -> [a] -> [a]
sfilter f xs = finalState
  where (_, finalState) = runState (rememberElements f xs) []

mywhen b op = if b then op else return ()

perhapsDecrease :: Int -> Maybe Int
perhapsDecrease x = do
  mywhen (x<=0) Nothing
  return (x-1)

test4 = do
   word <- ["Blue", "Green"]
   number <- [1,2,3]
   return (word ++ show number)

findSum :: [Int] -> Int -> [(Int,Int)]
findSum xs k = do a <- xs
                  b <- xs
                  if (a+b==k) then [(a,b)] else []

findSum' :: [Int] -> Int -> [(Int,Int)]
findSum' xs k = do a <- zip [1..length xs] xs
                   b <- zip [1..length xs] xs
                   if (snd a+snd b==k && fst a /= fst b) then [(snd a,snd b)] else []

test5 :: Num a => Show a => [a] -> IO [String]
test5 xs = do
           a <- return xs
           b <- return xs
           return [show (a++b)]

test6 :: [Integer]
test6 = [1,2,3] >>= (\x -> [x+1])

parseNumbers :: [String] -> [Int]
parseNumbers strings = fmap read (strings >>= words)