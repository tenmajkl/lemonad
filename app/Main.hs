module Main where
import System.Environment (getEnv)
import Data.Char (toUpper)

data RequestMethod = 
    Get 
    | Post
    -- TODO im lazy lol
    deriving (Eq)

data Request = Request
    RequestMethod
    String

data Response = Response
    Int
    [(String, String)]
    String

instance Show Response where 
    show (Response code headers body) = 
        show code 
        ++ "\n" 
        ++ foldr (\(name, value) c -> name ++ ": " ++ value ++ "\n" ++ c) "" headers 
        ++ body

data Route = Route
    String
    RequestMethod
    (Request -> Response)

httpError :: Int -> Response
httpError code = Response code [("Content-Type", "text/html")] (show code)

text :: String -> Response
text = Response 200 [("Content-Type", "text/html")]

toMethod :: String -> RequestMethod
toMethod m = case map toUpper m of
    "POST" -> Post
    _ -> Get

route :: RequestMethod -> String -> (Request -> Response) -> Route
route method uri = Route uri method

dispatch :: Request -> [Route] -> Response
dispatch request routes = 
    case 
        dispatchByUri request routes
        >>= dispatchByMethod request
    of 
        Left x -> x
        Right (Route _ _ f) -> f request

dispatchByUri :: Request -> [Route] -> Either Response [Route]
dispatchByUri (Request _ uri) routes = 
    if null filtered then
        Left $ httpError 404
    else 
        Right filtered
    where filtered = filter (\(Route path _ _) -> uri == path) routes

dispatchByMethod :: Request -> [Route] -> Either Response Route
dispatchByMethod (Request method _ ) routes = 
    if null filtered then 
        Left $ httpError 400
    else 
        Right (head filtered)
    where filtered = filter (\(Route _ rMethod _) -> method == rMethod) routes

lemonad :: [Route] -> IO ()
lemonad routes = do
    method <- getEnv "REQUEST_METHOD"
    uri <- getEnv "PATH_INFO"
    print (dispatch (Request (toMethod method) uri) routes)

main :: IO ()
main = lemonad [
        route Get "/" (\x -> text "hello epic haskell programmers")
    ]
