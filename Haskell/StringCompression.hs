module StringCompression where
import Data.List (group)

compress :: String -> String
compress str = concatMap (\x -> if length x > 1 then head x : show (length x) else x) (group str)
