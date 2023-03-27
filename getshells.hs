{-# LANGUAGE OverloadedStrings #-}
import System.IO
import Text.Printf
import Data.Map hiding (drop)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

{-# INLINE (|>) #-}
infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

lastColumn :: T.Text -> T.Text
lastColumn = snd . T.breakOnEnd ":"

prettyPrint :: T.Text -> Int -> String
prettyPrint k v = printf "%v : %v\n" k v

main :: IO ()
main = do
    -- we're using the strict version of Data.Text, so this uses a *tonne* of memory
    entries <- TIO.readFile "passwd"

    let shells = T.lines entries
              |> fmap lastColumn
              |> fmap (\x -> (x,1 :: Int))
              |> fromListWith (+)

    -- format each key-value pair, wrap it with putStr, and finally execute each print action
    mapWithKey (\k v -> putStr $ prettyPrint k v) shells
        |> sequence_

