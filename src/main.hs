import Game.Puzzle.Towers

example :: LatinSquare
example = parse "3\n1\n 2\n  3"

hardWith3 :: ([Constraint], LatinSquare)
hardWith3 = ([tower L 1 2,
              tower B 1 1,
              tower R 3 2]++latins 3,replicate 3 . replicate 3 $ [1..3])

hardWith6 :: ([Constraint], LatinSquare)
hardWith6 =([tower L 1 2,
             tower L 2 3,
             tower L 3 1,
             tower L 4 2,
             tower T 3 3,
             tower T 4 2,
             tower R 2 1,
             tower B 2 1]++latins 6, parse ("6\n"++
                "      \n"++
                "      \n"++
                "   1  \n"++
                "     3\n"++
                "  3   \n"++
                "      "))

play :: ([Constraint],LatinSquare) -> IO ()
play game = mapM_ (\(msg,sq) -> putStrLn msg >> putStrLn (prettify sq)) (playthrough game)

main :: IO ()
main = play hardWith6
