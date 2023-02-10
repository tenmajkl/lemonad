module Lemonad where
import System.Environment (getEnv, withArgs)
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)

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
        "HTTP/1.1"
        ++ show code
        ++ " "
        ++ codeToString code
        ++ "\n" 
        ++ foldr (\(name, value) c -> name ++ ": " ++ value ++ "\n" ++ c) "" headers
        ++ "\n"
        ++ body

data Route = Route
    String
    RequestMethod
    (Request -> Response)

codes :: [(Int, String)]
codes = [
        (100, "Continue"),
        (101, "Switching Protocols"),
        (102, "Processing"),
        (103, "Checkpoint"),
        (200, "OK"),
        (201, "Created"),
        (202, "Accepted"),
        (203, "Non-Authoritative Information"),
        (204, "No Content"),
        (205, "Reset Content"),
        (206, "Partial Content"),
        (207, "Multi-Status"),
        (300, "Multiple Choices"),
        (301, "Moved Permanently"),
        (302, "Found"),
        (303, "See Other"),
        (304, "Not Modified"),
        (305, "Use Proxy"),
        (306, "Switch Proxy"),
        (307, "Temporary Redirect"),
        (400, "Bad Request"),
        (401, "Unauthorized"),
        (402, "Payment Required"),
        (403, "Forbidden"),
        (404, "Not Found"),
        (405, "Method Not Allowed"),
        (406, "Not Acceptable"),
        (407, "Proxy Authentication Required"),
        (408, "Request Timeout"),
        (409, "Conflict"),
        (410, "Gone"),
        (411, "Length Required"),
        (412, "Precondition Failed"),
        (413, "Request Entity Too Large"),
        (414, "Request-URI Too Long"),
        (415, "Unsupported Media Type"),
        (416, "Requested Range Not Satisfiable"),
        (417, "Expectation Failed"),
        (418, "I\'m a teapot"),
        (422, "Unprocessable Entity"),
        (423, "Locked"),
        (424, "Failed Dependency"),
        (425, "Unordered Collection"),
        (426, "Upgrade Required"),
        (449, "Retry With"),
        (450, "Blocked by Windows Parental Controls"),
        (500, "Internal Server Error"),
        (501, "Not Implemented"),
        (502, "Bad Gateway"),
        (503, "Service Unavailable"),
        (504, "Gateway Timeout"),
        (505, "HTTP Version Not Supported"),
        (506, "Variant Also Negotiates"),
        (507, "Insufficient Storage"),
        (509, "Bandwidth Limit Exceeded"),
        (510, "Not Extended")
        ]

codeToString :: Int -> String
codeToString code = fromMaybe "OK" $ lookup code codes

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
