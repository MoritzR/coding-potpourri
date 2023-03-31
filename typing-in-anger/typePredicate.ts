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


interface A {
    a: "A"
}

interface AB extends A {
    b: "B"
}

function isAB1(
    a: A
): a is AB {
    return 'b' in a;
}

const isAB2 = (a: A): a is AB => 'b' in a

type TypePredicate<A, B extends A> = (
    input: A
) => input is B

const isAB3: TypePredicate<A, AB> = (a): a is AB => 'b' in a

// This errors because it is missing the specific 'a is AB'
// predicate, even though it is contained in TypePredicate<>
const isAB4: TypePredicate<A, AB> = a => 'b' in a
