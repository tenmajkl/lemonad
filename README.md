# Lemonad 🍋>>= 🍋

*now this is what i call epic haskell programming* 

Cgi-based web framework in haskell. If you consider building websites, maybe check [lemon](https://github.com/Lemon-Framework/Lemon). Usage of monad here is kinda low I want to improve it and learn more about them.


very declarative: 

```hs
main :: IO ()
main = lemonad [
        route Get "/" (\x -> text "hello epic haskell programmers")
    ]
```

Maybe i should make it actualy as a library but idk
