{-# LANGUAGE FlexibleInstances #-}

data Optional a = Some a | None

instance Show a => Show (Optional a) where
    show None = "Nothing"
    show (Some a) = show a

-- requires FlexibleInstances and OVERLAPPING pragma
instance {-# OVERLAPPING #-} Show (Optional String) where
    show None = "Nothing"
    show (Some a) = a

{-
now `Optional String` can behave differently than `Optional Int`/`Optional a`

*Main> "a"
"a"
*Main> Some "a"
a
*Main> Some 1
1
*Main> 
-}