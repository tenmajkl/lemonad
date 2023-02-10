import Lemonad

main :: IO ()
main = lemonad [
    route Get "/" (\_ -> text "hello epic haskell programmers"),
    route Get "/parkovar/rizkovar" (\_ -> text "rizkochleboparek")
    ]
