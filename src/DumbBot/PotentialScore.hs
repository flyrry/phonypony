module DumbBot.PotentialScore ( PotentialScore
                              , constant
                              , timespan
                              , negative
                              , value
                              , potentialScore
                              ) where

data PotentialScore a = Constant a
                      | Timespan Int Int (PotentialScore a)
                      | Negative (PotentialScore a)
                      | Value a

constant :: Num a => a -> PotentialScore a
constant = Constant

timespan :: Num a => Int -> Int -> PotentialScore a -> PotentialScore a
timespan = Timespan

negative :: Num a => PotentialScore a -> PotentialScore a
negative (Negative v) = v
negative v = Negative v

value :: Num a => a -> PotentialScore a
value = Value

potentialScore :: Num a => PotentialScore a -> Int -> Int -> a
potentialScore ps startTurn numberOfTurns =
    case ps of
      (Constant v) -> fromIntegral numberOfTurns * v
      (Timespan beforeGoal goalTurn afterGoal) ->
        let beforeTurns = max 0 (goalTurn - startTurn)
            afterTurns = max 0 (startTurn + numberOfTurns - goalTurn)
        in fromIntegral (beforeTurns * beforeGoal) + potentialScore afterGoal (goalTurn + 1) afterTurns
      (Negative v) -> negate (potentialScore v startTurn numberOfTurns) 
      (Value v) -> v
