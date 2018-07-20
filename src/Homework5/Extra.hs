-- data MyNum = One | Two | Three

-- let a = One

-- :t a regresa a:: MyNum
-- >a enter regresa error "NO INSTANCE FOR (Show MyNum"
-- >show a regresa error "NO INSTANCE FOR (Show MyNum"

-- >data MyNum = One | Two | Three deriving Show
-- >a regresa error "NO INSTANCE FOR (Show Ghci1.MyNum"
-- >let a = One
-- >show a rergesa "One" --con deriving precrea codigo que convierte al nombre del constructor en String

-- > data MyNum = One | Two | Three 
-- > :{
-- |   instance Show MyNum where
-- |       show _ = "asdasdasd"
-- |:}
-- > let a = One
-- > a regresa asdasdasd
-- > Two regresa asdasdasd

-- > :{
-- |    instance Show MyNum where
-- |       show One = "uno"
-- |        show Two = "dos"
-- |        show _ = "no es uno ni dos"
-- |:}
-- > let a = One
-- > a regresa uno
-- > Two regresa dos
-- > Three regresa no es uno ni dos

-- > let f x y = x+y
-- > One + Two regresa error "No instance for (Num MyNum"
-- > :t f regresa f:: Num a => a -> a -> a
-- > :t show regresa show:: Show a -> String

