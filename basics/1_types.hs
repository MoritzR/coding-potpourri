-- simple sum type
data RBG = Red | Green | Blue

-- with a type parameter
data Optional a = Some a | None

-- type alias
type MyMaybe = Optional

type TelefonNumber = String

-- Product types
data Point = Point Int Int

--    ^       ^ 	  ^   ^
--    name    constructor
--                  First argument
--                      Second Argument

-- Record types
data RecordPoint = RecordPoint
  { x :: Int,
    y :: Int
  }