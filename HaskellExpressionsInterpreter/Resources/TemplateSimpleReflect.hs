import Debug.SimpleReflect


main :: IO ()
main = mapM_ print . reduction $ 