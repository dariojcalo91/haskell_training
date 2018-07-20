module Main where

    import Homework8.Party
    import Homework8.Employee

    main :: IO ()
    main = readFile "src/Homework8/company.txt" >>= putStrLn . computeOutput

    computeOutput :: String -> String
    computeOutput = formatGL . maxFun . read

    formatGL :: GuestList -> String
    formatGL (GL lst fun) = "Total fun: " ++ show fun ++ "\n" ++ unlines employees
        where employees = map (\Emp{empName = name} -> name) lst