module Main

data Hand = Rock | Paper | Scissors

data Outcome = Loss | Draw | Win

parseOutcome : Char -> Maybe Outcome
parseOutcome 'X' = Just Loss
parseOutcome 'Y' = Just Draw
parseOutcome 'Z' = Just Win
parseOutcome _ = Nothing

parseHand : Char -> Maybe Hand
parseHand 'A' = Just Rock
parseHand 'B' = Just Paper
parseHand 'C' = Just Scissors
parseHand 'X' = Just Rock
parseHand 'Y' = Just Paper
parseHand 'Z' = Just Scissors
parseHand _ = Nothing

outcomeScore : Hand -> Hand -> Integer
outcomeScore Rock Rock = 3
outcomeScore Rock Paper = 6
outcomeScore Rock Scissors = 0
outcomeScore Paper Rock = 0
outcomeScore Paper Paper = 3
outcomeScore Paper Scissors = 6
outcomeScore Scissors Rock = 6
outcomeScore Scissors Paper = 0
outcomeScore Scissors Scissors = 3

handScore : Hand -> Integer
handScore Rock = 1
handScore Paper = 2
handScore Scissors = 3

gameScore : (Hand, Hand) -> Integer
gameScore (theirs, ours) = (outcomeScore theirs ours) + (handScore ours)

-- Parse a line of input using the first argument to parse the second component of the input
parseGame : (Char -> Maybe a) -> String -> Maybe (Hand, a)
parseGame parse s = let chars = unpack s in
                        do theirs <- index' 0 chars >>= parseHand
                           ours <- index' 2 chars >>= parse
                           Just (theirs, ours)

handForOutcome : Hand -> Outcome -> Hand
handForOutcome Rock Win = Paper
handForOutcome Paper Win = Scissors
handForOutcome Scissors Win = Rock
handForOutcome hand Draw = hand
handForOutcome Rock Loss = Scissors
handForOutcome Paper Loss = Rock
handForOutcome Scissors Loss = Paper

-- Produce the game that would lead to this outcome, given other hand
gameForOutcome : (Hand, Outcome) -> (Hand, Hand)
gameForOutcome (hand, outcome) = (hand, (handForOutcome hand outcome))

solvePart1 : List String -> Maybe Integer
solvePart1 raw = do games <- sequence (map (parseGame parseHand) raw)
                    scores <- pure (map gameScore games)
                    Just (sum scores)

solvePart2 : List String -> Maybe Integer
solvePart2 raw = do gameOutcomes <- sequence (map (parseGame parseOutcome) raw)
                    games <- pure (map gameForOutcome gameOutcomes)
                    scores <- pure (map gameScore games)
                    Just (sum scores)

main : IO ()
main = do file <- readFile "input"
          case file of
               (Right contents) => printLn (solvePart2 (lines contents))
               (Left err) => printLn err
