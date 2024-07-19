const f1 = (x: number | string | boolean): x is number => typeof x === "number"
const f2 = (x: number | string | boolean): x is string => typeof x === "string"
const f = f1 as typeof f1 | typeof f2
const n = "hi" as number | string | boolean

if (f(n)) {
    console.log(n)
    n.valueOf
} else {
    console.log(n) // n should NOT be a boolean
}