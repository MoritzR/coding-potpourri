// from https://rpeszek.github.io/posts/2022-01-03-ts-types-part3.html

type Hello = { hello: string }
type World = { world: string }
type HelloWorld = Hello & World

function compareHelloAndWorld(hello: Hello, world: World) {
    /**
     * compile error: `This condition will always return 'false'...`
     * but this is not true, as seen below
     */
    return hello === world
}

const helloWorld1: HelloWorld = { hello: "Hello", world: "world" }
const helloWorld2: HelloWorld = { hello: "A great hello to", world: "the nice world outside" }

const returnedFalse = compareHelloAndWorld(helloWorld1, helloWorld2)
const returnedTrue = compareHelloAndWorld(helloWorld1, helloWorld1)

console.log(returnedFalse)
console.log(returnedTrue)

// but next, these types also have no overlap, but compile nonetheless
if (1 === null) {
    console.log("1 equals null")
}
if (1 === undefined) {
    console.log("1 equals undefined")
}
if (undefined === null) {
    console.log("undefined equals null")
}

// also interesting how this interacts with the type windening

// compile error
if ("hello" === "world") {
    console.log("hello equals world")
}

// compiles!
if ({ value: "hello" } === { value: "world" }) {
    console.log("hello equals world")
}


// lets try to defined an equals function (ramda defined it that way for example)
function myEquals<T>(a: T, b: T): boolean {
    return a === b;
}
myEquals(1, null)
// error: `Argument of type 'string' is not assignable to parameter of type 'number'`
myEquals(1, "hello")
// error: `Argument of type '"hello"' is not assignable to parameter of type '1'`
myEquals(1 as 1, "hello" as "hello")

// no error, so `1` is assignable to `2`?
myEquals(1 as 1, 2 as 2)
// no it's not
let one: 1 = 1
let two: 2 = 2
one = two
two = one

// why is that? Because `myEquals(1 as 1, 2 as 2)`
// widens to the type `myEquals<1 | 2>(a: 1 | 2, b: 1 | 2): boolean`

// We now provide type annotation to make our previous example compile
myEquals<number | string>(1, "hello")
myEquals<1 | "hello">(1, "hello")

// `(1 | "hello") & (2 | "hello")` extends "hello" but
// `(1 | "hello") & ({} | "hello")` does not
// apparently object types are treated differently
function verifyExtends<T2 extends T1, T1>() { }
verifyExtends<(1 | "hello") & (2 | "hello"), "hello">()
verifyExtends<(1 | "hello") & ("unused" | "hello"), "hello">()
verifyExtends<(1 | "hello") & ({} | "hello"), "hello">()