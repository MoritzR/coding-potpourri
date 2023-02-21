// how to put a string into an array of numbers
const intArray: number[] = [1, 2, 3]
const list: unknown[] = intArray
list.push("not a number") //compiles

// runtime error: `int.toPrecision is not a function`
const printInts = (ints: number[]) => ints.forEach(int => { console.log(int.toPrecision(1)) })

printInts(intArray);

// This is not possible if you try to define your own array type
interface MyArray<T> {
    push: (element: T) => void
}

const myIntArray: MyArray<number> = { push: a => { console.log("I pushed the number", a) } };
// type error: `Type 'unknown' is not assignable to type 'number'`
const myUnknownArray: MyArray<unknown> = myIntArray;

// The built in array type seems to behave similar to the following Payload type
interface Payload<T> { payload: T }
const intPayload: Payload<number> = { payload: 1 };
const unknownPayload: Payload<unknown> = intPayload;
unknownPayload.payload = "hello world"
// runtime error: `int.toPrecision is not a function`
const printIntPayload = (intPayload: Payload<number>) => { console.log(intPayload.payload.toPrecision(1)) }


// We can fix this by adding invaraint annotations (in out)
interface InvariantPayload<in out T> { payload: T }
const invariantIntPayload: InvariantPayload<number> = { payload: 1 };
// type error: `Type 'unknown' is not assignable to type 'number'`, nice!
const invariantUnknownPayload: InvariantPayload<unknown> = invariantIntPayload;
// but we can still assing `intPayload` to it!
const invariantUnknownPayload2: InvariantPayload<unknown> = intPayload;
unknownPayload.payload = "hello world"


// unknowns can also appear without us using unknown at all
// this is especially problematic when when it appears in contravariant position
// let's see this in action using Ramda's map() function

// this is one of the overloads for map, see https://github.com/DefinitelyTyped/DefinitelyTyped/blob/f7ec78508c6797e42f87a4390735bc2c650a1bfd/types/ramda/index.d.ts#L1033
// the other overloads are omitted for breverity, because they don't cause the issue

declare function map<T, U>(fn: (x: T[keyof T & keyof U] | ValueOfUnion<T>) => U[keyof T & keyof U]): (list: T) => U;
type ValueOfUnion<T> = T extends infer U ? U[keyof U] : never;

// let's create some function we want to use 'map' with

const getName = (hasName: { name: string }): string => hasName.name;
const getFirst = (l: string[]): string | undefined => l[0]
const flow2 = <A, B, C>(ab: (a: A) => B, bc: (b: B) => C): ((a: A) => C) =>
    a => bc(ab(a))

const john = getName({ name: "John" })

// now let's use it
const getFirstOfName = flow2(map(getName), getFirst)

// If you hover over 'getFirstOfName' you will notice that it takes an 'unknown'
// even though we never used 'unknown' in our definitions!
// That means we can call the functions with anything, according to Typescript.

// ouch, a runtime error, good luck getting a name out of the number 3
getFirstOfName(3)

// With a better `map` typing, this works correctly
declare function mapBetter<A, B>(fn: ((a: A) => B)): (a: A[]) => B[]

const getFirstOfName2 = flow2(mapBetter(getName), getFirst)

getFirstOfName2(3)
getFirstOfName2([{ name: "Boop" }])