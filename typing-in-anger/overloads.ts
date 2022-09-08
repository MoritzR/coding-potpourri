// from https://rpeszek.github.io/posts/2021-12-12-ts-types-part1.html
// se also https://github.com/microsoft/TypeScript/issues/43187

function overload(p: "PARAM1"): number
function overload(p: "PARAM2"): string
function overload(p: "PARAM1" | "PARAM2"): number | string {
    switch (p) {
        case 'PARAM1':
            return 1
        case 'PARAM2':
            return 'result'
    }
}

function call<A, B>(fn: ((a: A) => B), input: A) {
    return fn(input)
}

const overload1 = overload("PARAM1")
const overload2 = overload("PARAM2")

// does not compile, chaning the order of overloads in `overload` switches
// which of the following lines work and which does not
const callOverload1 = call(overload, "PARAM1")
// works by providing explicit type annotations
const callOverload1Works = call<"PARAM1", number>(overload, "PARAM1")
const callOverload2 = call(overload, "PARAM2")

/**
 * The above program should compile, but does not.
 * It also works the other way around, the following program
 * should not compile, but it does and throws a runtime error
 */

function overloadWithOneOrTwoParameters(number1: number, number2: number): number
function overloadWithOneOrTwoParameters(number: number): string
function overloadWithOneOrTwoParameters(number1: number, optionalNumber?: number): string | number {
    if (optionalNumber !== undefined) {
        return number1 + optionalNumber
    }
    return number1.toString()
}

function call2<A, B, C>(fn: ((a: A, b: B) => C), input1: A, input2: B) {
    return fn(input1, input2)
}

/**
 * infered as string, but is actually a number
 * this is because it picks the one parameter overload, even though call2 expects a two parameter callback
 * addtionally: call2 is inferred as
 *     `call2<1, number, string>(fn: (a: 1, b: number) => string, input1: 1, input2: number): string`
 * and I am not sure why one parameter is infered as a literal `1` while the other is `number`
 */
const aNumber = call2(overloadWithOneOrTwoParameters, 1, 2)

// runtime error! g.split is not a function
console.log(aNumber.split(""))

 