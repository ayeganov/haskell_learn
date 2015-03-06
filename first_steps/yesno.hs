module Main where

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance (YesNo a) => YesNo (Maybe a) where
    yesno (Just v) = yesno v
    yesno Nothing = False



main :: IO()
main = do
    let b = 0
    if yesno $ Just b then
        putStrLn("Hello")
    else
        putStrLn("Oh man.")
{-    putStrLn ("Maybe [], " ++ show (yesno 2)) -}
