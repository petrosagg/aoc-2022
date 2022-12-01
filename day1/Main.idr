module Main

-- Group a list by empty lines
group : List String -> List (List String)
group [] = []
group (x :: xs) = let tailGroups = group xs in
                      (case length x of
                            -- Empty line, we create a new group
                            Z => [] :: tailGroups
                            -- Non-empty line
                            (S k) => (case tailGroups of
                                           -- No previous group, create a new one
                                           [] => [[x]]
                                           -- There is a previous group, just prepend there
                                           (group :: rest) => (x :: group) :: rest))

-- Casts a list of strings to a list of Nats
castGroup : List String -> List Nat
castGroup [] = []
castGroup (x :: xs) = (cast x) :: castGroup xs

main : IO ()
main = do file <- readFile "input"
          case file of
               (Right contents) => let groups = (group (lines contents))
                                       totals = sort (map (sum . castGroup) groups)
                                       topThree = (take 3 (reverse totals)) in
                                       printLn (sum topThree)
               (Left err) => printLn err
