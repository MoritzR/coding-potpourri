/* Let's create a typing for a simpler version of Ramda's `prop` function,
    see https://ramdajs.com/docs/#prop
   Which we want to call like this:
    pipe(
         {a: 3},
         prop("a")
    ) // returns 3
*/

const pipe2 = <A, B>(a: A, ab: (a: A) => B): B => ab(a)
const pipe3 = <A, B, C>(a: A, ab: (a: A) => B, bc: (b: B) => C): C => bc(ab(a))
const flow2 = <A, B, C>(ab: (a: A) => B, bc: (b: B) => C): ((a: A) => C) => a => bc(ab(a))

const myObject = { a: 3 }

// let's try our first version, very simple
const prop = (key: string) => <T>(obj: Record<string, T>) => obj[key]

pipe2(
    myObject,
    prop("a") // compiles
)
/* also compiles, but value will be `undefined` instead of number
    potentially causing runtime errors :( */
const value: number = pipe2(
    myObject,
    prop("not a prop")
)

// let's try again, but with better type safety
const propBetter = <K extends string>(key: K) => <T extends { [k in K] }>(obj: T) => obj[key];


// nice, it compiles, but we don't get any autocompletion on the key from the IDE :(
pipe2(
    myObject,
    propBetter("a")
)
pipe2(
    myObject,
    propBetter("not a prop") // it correctly rejects property names that are not part of the object
)

// let's chain two props together
const gettingTwoProps = flow2(
    propBetter("not a prop"),
    propBetter("also not a prop")
)
/* ouch, a runtime error because we typescript infers an `any` somewhere, 
even though we never wrote `any` ourselves :( */
gettingTwoProps({ "not a prop": { "a prop": 123 } })


// let's try again with even better type safety, and also IDE autocompletion

const propBest =
    <A, Path extends keyof A extends never ? string : keyof A>(path: Path) =>
        <B extends { [k in Path]: unknown }>(obj: keyof A extends never ? B : A) =>
            (obj as B)[path] as Path extends keyof A ? A[Path] : B[Path];

pipe2(
    myObject,
    propBest("a") // nice, and we also get autocompletion
)
pipe2(
    myObject,
    propBest("not a prop") // it correctly rejects property names that are not part of the object
)
flow2(
    propBest("not a prop"), // this time we can't even chain them like before, nice
    propBest("also not a prop")
)
