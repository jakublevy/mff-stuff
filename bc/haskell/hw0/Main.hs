import Numeric

main :: IO ()
main =
  interact $ \inp ->
    let w = length . words <$> lines inp
     in showFFloat (Just 2) (fromIntegral (sum w) / fromIntegral (length w)) ""
