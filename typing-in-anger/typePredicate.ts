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
