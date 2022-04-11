data MyType = SomeThing | SomeOtherThing

instance Show MyType where
    show SomeThing = "some thing"
    show SomeOtherThing = "other thing"