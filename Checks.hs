import qualified Hsfuck as Hsfuck
import Test.QuickCheck

instance Arbitrary Hsfuck.Instruction where
    arbitrary = do n <- arbitrarySizedIntegral
                   elements [ Hsfuck.Plus n,
                              Hsfuck.Move n,
                              Hsfuck.Print,
                              Hsfuck.Read,
                              Hsfuck.StartLoop,
                              Hsfuck.EndLoop
                            ]

main = quickCheckWith args checkOptGroupOtherInstructions

args = stdArgs {maxSuccess = 200, maxSize = 10000, chatty = True}

-- tests whether optGroup removes other instructions than Plus and Move
checkOptGroupOtherInstructions :: [Hsfuck.Instruction] -> Bool
checkOptGroupOtherInstructions instructions =
    removePlusMove instructions == removePlusMove (Hsfuck.optGroup instructions)
    where removePlusMove = filter isNotPlusMove
          isNotPlusMove (Hsfuck.Plus _) = False
          isNotPlusMove (Hsfuck.Move _) = False
          isNotPlusMove _               = True
