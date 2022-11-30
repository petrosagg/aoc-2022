module Main

import Data.Vect

parseInput : (List String) -> (n: Nat ** Vect n Integer)
parseInput [] = (Z ** Nil)
parseInput (x :: xs) = let (tail_len ** tail) = parseInput xs in
                           ((S tail_len) ** (cast x) :: tail)

diff : Vect (S (S n)) Integer -> Vect (S n) Integer
diff {n = Z} (x :: y :: []) = (y - x) :: Nil
diff {n = (S k)} (x :: y :: z :: xs) = let tail = diff (y :: z :: xs) in
                                           (y - x) :: tail

solve : Vect n Integer -> Integer
solve xs = let (_ ** positives) = filter (\x => x > 0) xs in
               foldl (\acc => \elem => acc + 1) 0 positives

main : IO ()
main = do file <- readFile "input"
          case file of
               (Right contents) => let (len ** xs)  = parseInput (lines contents) in
                                       (case len of
                                             (S (S k)) => printLn ("resut: " ++ show (solve (diff xs)))
                                             _ => printLn "not enough data"
                                             )
               (Left err) => printLn err




