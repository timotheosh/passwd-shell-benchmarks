{-# LANGUAGE OverloadedStrings #-}
import System.IO
import Text.Printf
import Data.List (sort, group)
import Data.Map hiding (drop)
import qualified Data.Text as T

{-# INLINE (|>) #-}
infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

lastColumn :: T.Text -> T.Text
lastColumn = last . T.splitOn ":"

prettyPrint :: T.Text -> Int -> String
prettyPrint k v = printf "%v : %v\n" k v

main :: IO ()
main = do
    entries <- T.pack <$> readFile "passwd"

    let shells = T.lines entries
              |> fmap lastColumn
              |> fmap (\x -> (x,1 :: Int))
              |> fromListWith (+)

    -- format each key-value pair, wrap it with putStr, and finally execute each print action
    mapWithKey (\ k v -> putStr $ prettyPrint k v) shells
        |> sequence_

