// An example that I ran into when using Ramda.last()

// Ramdas last is defined as follows
declare function last(str: string): string;
declare function last(list: readonly []): undefined;
declare function last<T>(list: readonly T[]): T | undefined;

const emptyArray: Array<number> = []

const aNumberOrUndefined = last(emptyArray) // correctly inferred as number | undefined

// now let's wrap it into our own last function
// no cast is necessary, no type error, still, now we return T instead of T | undefined
const myLast: <T>(list: Array<T>) => T = last;

const aNumberButActuallyUndefined = myLast(emptyArray)

// runtime error!
aNumberButActuallyUndefined.toFixed()


// Another example from Ramda, this time using map()

// this is the overload for map
declare function map<T, U>(fn: (x: T) => U, list: readonly T[]): U[];
declare function map<T, U>(fn: (x: T) => U): (list: readonly T[]) => U[];
declare function map<T, U>(fn: (x: T[keyof T & keyof U] | ValueOfUnion<T>) => U[keyof T & keyof U], list: T): U;
declare function map<T, U>(fn: (x: T[keyof T & keyof U] | ValueOfUnion<T>) => U[keyof T & keyof U]): (list: T) => U;
declare function map<T, U>(fn: (x: T) => U, obj: Functor<T>): Functor<U>; // used in functors
declare function map<T, U>(fn: (x: T) => U): (obj: Functor<T>) => Functor<U>; // used in functors
type ValueOfUnion<T> = T extends infer U ? U[keyof U] : never;
type Functor<A> =
    | { ['fantasy-land/map']: <B>(fn: (a: A) => B) => Functor<B>;[key: string]: any }
    | { map: <B>(fn: (a: A) => B) => Functor<B>;[key: string]: any };

// let's create some function we want to use 'map' with

// the "T extends" is important to surface the following error
const getName = <T extends string>(hasName: { name: T }): T => hasName.name;
const getFirst = (l: string[]) => l[0] // unsafe, but let's assume l is non-empty
const compose = <A, B, C>(ab: (a: A) => B, bc: (b: B) => C): ((a: A) => C) =>
    a => bc(ab(a))

// "T extends string" make this infer "John" instead of just a string
const john = getName({ name: "John" })

// now let's use it
const getFirstName = compose(map(getName), getFirst)

// ouch, a runtime error, this function must be called with an array of objects
getFirstName(3)

// this is related to unknown, as 'getFirstName' incorrectly accepts
// an 'unknown', which means we can pass in anything


// Another example from Ramda, this time using applyTo().

// for applyTo there is a curried version
declare function applyToCurried<T>(el: T): <U>(fn: (t: T) => U) => U;

// and a non curried version
declare function applyToUncurried<T, U>(el: T, fn: (t: T) => U): U;

// in Ramda, both are combined together in an overload
declare function applyToOverloaded<T>(el: T): <U>(fn: (t: T) => U) => U;
declare function applyToOverloaded<T, U>(el: T, fn: (t: T) => U): U;

// now let's set up some types to use these with
type MyFunction<A, B> = [A, (a: A) => B];
type ExistentialFunction<B> = <A, R>(cont: (f: MyFunction<A, B>) => R) => R;

type Make = <A, B>(f: MyFunction<A, B>) => ExistentialFunction<B>

// all of these (correctly) cause type errors
const make: Make = a => b => b(a)
const make2: Make = a => b => applyToUncurried(a, b)
const make3: Make = applyToCurried
const make4: Make = a => b => applyToOverloaded(a)(b)
const make5: Make = a => b => applyToOverloaded(a, b)

// but this somehow doesn't
const make6: Make = applyToOverloaded

// now we can use this to create a runtime error
// without typescript complaining
const unsafeFunction: ExistentialFunction<string> =
    make6([1, a => (a + 1).toString()])

// ouch, runtime error, unsafeFunction actually takes in a number
unsafeFunction(f => f[1]([""]))
// adding type annotations doesn't help
unsafeFunction((f: MyFunction<string[], string>) => f[1]([""]))

// There is an issue in our Typing, let's fix that
type ExistentialFunctionImproved<B> = <R>(cont: <A>(f: MyFunction<A, B>) => R) => R;
type MakeImproved = <A, B>(f: MyFunction<A, B>) => ExistentialFunctionImproved<B>

// now all type check, as they should
const makeImproved: MakeImproved = a => b => b(a)
const makeImproved2: MakeImproved = a => b => applyToUncurried(a, b)
const makeImproved3: MakeImproved = applyToCurried
const makeImproved4: MakeImproved = a => b => applyToOverloaded(a)(b)
const makeImproved5: MakeImproved = a => b => applyToOverloaded(a, b)
const makeImproved6: MakeImproved = applyToOverloaded

const safeFuntion = makeImproved([1, a => (a + 1).toString()])
// now we get a type error, as it should be
safeFuntion(f => f[1]([""]))
// the following is the only correct way to call safeFunction
safeFuntion(f => f[1](f[0]))