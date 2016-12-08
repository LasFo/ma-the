
{-# RULES "TESTRULE" forall a. return a >>= print = print "Hello World" #-}
main = do
 a <- return "Hello World!" 
 print a
