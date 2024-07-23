-- -- {-# LANGUAGE TypeApplications #-}
-- -- {-# LANGUAGE NamedFieldPuns #-}
-- -- {-# LANGUAGE RecordWildCards #-}
-- module User.Actions.Battle where
-- import           System.Random (randomRIO)
-- import           Text.Read     (readMaybe)
-- data Golem = Golem
--   { gAttack :: Int
--   , gHp     :: Int
--   } deriving (Show)
-- data Player = Player
--   { pAttack :: Int
--   , pHp     :: Int
--   } deriving (Show)
-- data Battle
--   = Fight
--   | RunAway
--   deriving (Show, Read)
--       -- displayUser (newgHpGolem,gAttackGolem) (newpHpPlayer,pAttackPlayer)
-- displayUser :: (Int, Int) -> (Int, Int) -> IO ()
-- displayUser (gHpGolem, gAttackGolem) (pHpPlayer, pAttackPlayer) = do
--   putStrLn
--     $ "The Golem has "
--         ++ show gHpGolem
--         ++ " Health and "
--         ++ show gAttackGolem
--         ++ " attack."
--   putStrLn
--     $ "You has "
--         ++ show pHpPlayer
--         ++ " Health and "
--         ++ show pAttackPlayer
--         ++ " attack."
--   putStrLn "choose an action :Fight or RunAway?"
-- runAway :: (Int, Int) -> (Int, Int) -> Int -> Int -> Int -> IO Bool
-- runAway (gHpGolem, gAttackGolem) (pHpPlayer, pAttackPlayer) minusHealthPlayer repeatCount realCount = do
--   let newpHpPlayer = pHpPlayer - minusHealthPlayer
--   if repeatCount == realCount
--     then putStrLn "You've won against the Golem " >> return True
--     else if newpHpPlayer >= 0
--            then putStrLn "You've failed to run away! And the Golem hit you!"
--                   >> displayUser
--                        (gHpGolem, gAttackGolem)
--                        (newpHpPlayer, pAttackPlayer)
--                   >> optionResolve
--                        (gHpGolem, gAttackGolem)
--                        (newpHpPlayer, pAttackPlayer)
--                        (realCount + 1)
--            else putStrLn "You've lost to the Golem " >> return False
-- optionResolve :: (Int, Int) -> (Int, Int) -> Int -> IO Bool
-- optionResolve (gHpGolem, gAttackGolem) (pHpPlayer, pAttackPlayer) realCount = do
--   option <- getLine
--   minusHealthGolem <- randomRIO @Int (40, 40)
--   minusHealthPlayer <- randomRIO @Int (20, 30)
--   repeatCount <- randomRIO @Int (2, 4)
--   case readMaybe option of
--     Just Fight -> do
--       let newgHpGolem = gHpGolem - minusHealthGolem
--           newpHpPlayer = pHpPlayer - minusHealthPlayer
--       displayUser (newgHpGolem, gAttackGolem) (newpHpPlayer, pAttackPlayer)
--       if newpHpPlayer <= 0
--         then do
--           putStrLn "You've lost to the Golem "
--           return False
--         else if newgHpGolem <= 0
--                then putStrLn "You've won against the Golem " >> return True
--                else optionResolve
--                       (newgHpGolem, gAttackGolem)
--                       (newpHpPlayer, pAttackPlayer)
--                       realCount
--     Just RunAway ->
--       runAway
--         (gHpGolem, gAttackGolem)
--         (pHpPlayer, pAttackPlayer)
--         minusHealthPlayer
--         repeatCount
--         realCount
--     Nothing ->
--       putStrLn "Choose an action: Fight or RunAway?"
--         >> optionResolve
--              (gHpGolem, gAttackGolem)
--              (pHpPlayer, pAttackPlayer)
--              realCount
-- battle :: IO Bool
-- battle = do
--   putStrLn "You've encountered a Golem! Choose an action :Fight or RunAway?"
--   gAttackGolem <- randomRIO @Int (4, 10)
--   gHpGolem <- randomRIO @Int (40, 60)
--   pAttackPlayer <- randomRIO @Int (4, 10)
--   pHpPlayer <- randomRIO @Int (40, 60)
--   displayUser (gHpGolem, gAttackGolem) (pHpPlayer, pAttackPlayer)
--   optionResolve (gHpGolem, gAttackGolem) (pHpPlayer, pAttackPlayer) 1
module User.Actions.Battle where

import           System.Random (randomRIO)
import           Text.Read     (readMaybe)

data Golem = Golem
  { gAttack :: Int
  , gHp     :: Int
  } deriving (Show)

data Player = Player
  { pAttack :: Int
  , pHp     :: Int
  } deriving (Show)

data Battle
  = Fight
  | RunAway
  deriving (Show, Read)

displayUser :: Golem -> Player -> IO ()
displayUser Golem {..} Player {..} = do
  if gHp <= 0 || pHp <= 0
    then return ()
    else do
      putStrLn
        $ "\nThe Golem has "
            ++ show gHp
            ++ " Health and "
            ++ show gAttack
            ++ " attack."
      putStrLn
        $ "You have "
            ++ show pHp
            ++ " Health and "
            ++ show pAttack
            ++ " attack."
      putStrLn "Choose an action: Fight or RunAway?"

runAway :: Golem -> Player -> Int -> IO Bool
runAway golem@Golem {..} player@Player {..} minusHealthPlayer = do
  let newpHp = pHp - minusHealthPlayer
      newGAttack = gAttack - 1
  randomLife <- randomRIO @Int (1, 3)
  if randomLife == 2 || newGAttack <= 0
    then putStrLn "You've managed to run away!" >> return True
    else if newpHp >= 0
           then putStrLn "You've failed to run away! And the Golem hit you!"
                  >> displayUser
                       golem {gAttack = newGAttack}
                       player {pHp = newpHp}
                  >> optionResolve
                       golem {gAttack = newGAttack}
                       player {pHp = newpHp}
           else putStrLn "You've lost to the Golem " >> return False

optionResolve :: Golem -> Player -> IO Bool
optionResolve golem@Golem {..} player@Player {..} = do
  option <- getLine
  minusHealthGolem <- randomRIO @Int (40, 40)
  minusHealthPlayer <- randomRIO @Int (20, 30)
  case readMaybe option of
    Just Fight -> do
      let newgHp = gHp - minusHealthGolem
          newpHp = pHp - minusHealthPlayer
          newGAttack = gAttack - 1
          newPAttack = pAttack - 1
      displayUser
        golem {gHp = newgHp, gAttack = newGAttack}
        player {pHp = newpHp, pAttack = newPAttack}
      if newpHp <= 0 || newPAttack <= 0
        then putStrLn "You've lose the battle!" >> return False
        else if newgHp <= 0 || newGAttack <= 0
               then putStrLn "You've won the battle!" >> return True
               else optionResolve
                      golem {gHp = newgHp, gAttack = newGAttack}
                      player {pHp = newpHp, pAttack = newPAttack}
    Just RunAway -> runAway golem player minusHealthPlayer
    Nothing ->
      putStrLn "Choose an action: Fight or RunAway?"
        >> optionResolve golem player

battle :: IO Bool
battle = do
  putStrLn "\nYou've encountered a Golem! Choose an action: Fight or RunAway?"
  gAttack <- randomRIO @Int (4, 10)
  gHp <- randomRIO @Int (40, 60)
  pAttack <- randomRIO @Int (4, 10)
  pHp <- randomRIO @Int (40, 60)
  let golem = Golem {gAttack, gHp}
      player = Player {pAttack, pHp}
  displayUser golem player
  optionResolve golem player
-- {-# LANGUAGE NamedFieldPuns #-}
-- data Person = Person
--   { name :: String
--   , age :: Int
--   }
-- printPerson :: Person -> IO ()
-- printPerson Person { name, age } = do
--   putStrLn $ "Name: " ++ name
--   putStrLn $ "Age: " ++ show age
-- Ở đây, NamedFieldPuns cho phép bạn sử dụng trực tiếp các trường name và age của Person trong hàm printPerson, thay vì phải viết dài dòng printPerson Person { name = name, age = age }.
-- {-# LANGUAGE RecordWildCards #-}
-- data Person = Person
--   { name :: String
--   , age :: Int
--   }
-- printPerson :: Person -> IO ()
-- printPerson Person {..} = do
--   putStrLn $ "Name: " ++ name
--   putStrLn $ "Age: " ++ show age
-- Ở đây, RecordWildCards cho phép bạn sử dụng name và age trực tiếp, mặc dù chúng không được chỉ định một cách rõ ràng, bởi vì RecordWildCards tự động tạo ra các biến này từ các trường của Person.
