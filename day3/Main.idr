module Main

import Data.SortedSet
import Data.List.Views

priority : Char -> Integer
priority c = case isUpper c of
                  False => cast ((ord c) - (ord 'a') + 1)
                  True => cast ((ord c) - (ord 'A') + 27)

commonItem : List Char -> Maybe Char
commonItem input with (splitBalanced input)
  commonItem (left ++ right) | (MkSplitBal x) = let left = SortedSet.fromList left
                                                    right = SortedSet.fromList right
                                                    common = intersection left right in
                                                    head' (SortedSet.toList common)

groupBadge : List (List Char) -> Maybe Char
groupBadge xs = do let as_sets = map SortedSet.fromList xs
                   common <- foldl foldHelper Nothing as_sets
                   head' (SortedSet.toList common)
  where
    foldHelper : Maybe (SortedSet Char) -> SortedSet Char -> Maybe (SortedSet Char)
    foldHelper Nothing items = Just items
    foldHelper (Just common) items = Just (intersection common items)


-- Split a list into a list of groups of size at most n
groups : (n : Nat) -> List a -> List (List a)
groups n xs = groupHelper n [] xs
  where
    groupHelper : Nat -> List (List a) -> List a -> List (List a)
    groupHelper k groups [] = groups
    groupHelper k groups (y :: ys) = let (newGroup, tail) = splitAt k (y :: ys) in
                                         groupHelper k (groups ++ [newGroup]) tail

solvePart1 : List String -> Maybe Integer
solvePart1 xs = do commonItems <- sequence (map (commonItem . unpack) xs)
                   Just (sum (map priority commonItems))

solvePart2 : List String -> Maybe Integer
solvePart2 xs = do let g = groups 3 (map unpack xs)
                   badges <- sequence (map groupBadge g)
                   Just (sum (map priority badges))

main : IO ()
main = do file <- readFile "input"
          case file of
               (Right contents) => do printLn (solvePart1 (lines contents))
                                      printLn (solvePart2 (lines contents))
               (Left err) => printLn err
