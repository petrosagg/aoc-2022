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

outcome : Hand -> Hand -> Outcome
outcome Rock Paper = Win
outcome Paper Scissors = Win
outcome Scissors Rock = Win
outcome Rock Scissors = Loss
outcome Paper Rock = Loss
outcome Scissors Paper = Loss
outcome Paper Paper = Draw
outcome Rock Rock = Draw
outcome Scissors Scissors = Draw

outcomeScore : Outcome -> Integer
outcomeScore Win = 6
outcomeScore Draw = 3
outcomeScore Loss = 0

handScore : Hand -> Integer
handScore Rock = 1
handScore Paper = 2
handScore Scissors = 3

gameScore : (Hand, Hand) -> Integer
gameScore (theirs, ours) = (outcomeScore (outcome theirs ours)) + (handScore ours)

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
