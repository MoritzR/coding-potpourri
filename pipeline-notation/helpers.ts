export function pipe<A, B, C, D>(a: A, ab: (_: A) => B, bc: (_: B) => C, cd: (_: C) => D): D {
    return cd(bc(ab(a)))
}

export function toString(a: { toString: () => string }) {
    return a.toString()
}

export const filter = <T>(predicate: (_: T) => boolean) => (array: ReadonlyArray<T>) => array.filter(predicate)

export const sum = (array: ReadonlyArray<number>): number => array.reduce((a, b) => a + b, 0)

export const isEven = (n: number): boolean => n % 2 === 0