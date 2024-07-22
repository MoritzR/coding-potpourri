/* Let's create a typing for a simpler version of Ramda's `prop` function,
    see https://ramdajs.com/docs/#prop
   Which we want to call like this:
    pipe(
         {a: 3},
         prop("a")
    ) // returns 3
*/

const pipe2 = <A, B>(a: A, ab: (a: A) => B): B => ab(a)
const flow3 = <A, B, C, D>(ab: (a: A) => B, bc: (b: B) => C, cd: (c: C) => D): ((a: A) => D) => a => cd(bc(ab(a)))

const myObject = { aProp: 3 }

// let's try our first version, very simple
const prop = (key: string) => <T>(obj: Record<string, T>) => obj[key]

pipe2(
    myObject,
    prop("aProp") // compiles
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
    propBetter("aProp")
)
pipe2(
    myObject,
    propBetter("not a prop") // it correctly rejects property names that are not part of the object
)

// let's chain some props together
const gettingNestedProp = flow3(
    propBetter("aProp"),
    propBetter("not a prop"),
    propBetter("also very much not a prop")
)
/* ouch, a runtime error because typescript infers an `any` somewhere, 
even though we never wrote `any` ourselves :( */
gettingNestedProp({ "aProp": { "also a prop": 123 } })


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
flow3(
    propBest("not a prop"), // this time we can't even chain them like before, nice
    propBest("also not a prop"),
    propBest("still not a prop")
)
