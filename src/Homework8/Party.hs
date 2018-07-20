----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 08
--
----------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Homework8.Party where
    
    import Homework8.Employee
    import Data.Tree
    import Data.Monoid

    ----------------------------------------------------------------------
    -- Exercise 1
    ----------------------------------------------------------------------
    
    instance Monoid GuestList where
        mempty                        = GL [] 0 --data GestList = GL [Employee] Fun --tomar en cuenta esto
        mappend (GL al af) (GL bl bf) = GL (al++bl) (af+bf)
    
    glCons :: Employee -> GuestList -> GuestList
    glCons e (GL emps f) = GL (e : emps) (empFun e + f)
    
    moreFun :: GuestList -> GuestList -> GuestList
    moreFun = max    
    
    ----------------------------------------------------------------------
    -- Exercise 2
    ----------------------------------------------------------------------
    treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
    treeFold f root Node{rootLabel = rl, subForest = sf}
        = f rl (map (treeFold f root) sf)
    
    ----------------------------------------------------------------------
    -- Exercise 3
    ----------------------------------------------------------------------
    
    nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
    nextLevel boss bestLists = (maximumS withBossL, maximumS withoutBossL)
        where withoutBossL   = map fst bestLists
                -- ^ The new withoutBossList has sub bosses in it.
              withoutSubBoss = map snd bestLists
              withBossL      = map (glCons boss) withoutSubBoss
                -- ^ The new withBossList doesn't have sub bosses in it.
    
    maximumS ::(Monoid a, Ord a) => [a] -> a
    maximumS [] = mempty
    maximumS lst = maximum lst
    ----------------------------------------------------------------------
    -- Exercise 4
    ----------------------------------------------------------------------
    
    maxFun :: Tree Employee -> GuestList
    maxFun tree = uncurry max res
        where res = treeFold nextLevel (mempty, mempty) tree
    
    ----------------------------------------------------------------------
    -- Exercise 5
    ----------------------------------------------------------------------
    {-
    main :: IO ()
    main = readFile "company.txt" >>= putStrLn . computeOutput

    computeOutput :: String -> String
    computeOutput = formatGL . maxFun . read

    formatGL :: GuestList -> String
    formatGL (GL lst fun) = "Total fun: " ++ show fun ++ "\n" ++ unlines employees
        where employees = map (\Emp{empName = name} -> name) lst

    -}

    --MODELO Y ANIO enviar correo a Kathy cc Cristina
    