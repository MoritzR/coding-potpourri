// TypeScript infers different types here, but why?

const record: <K extends string, V>(r: Record<K, V>) => Record<K, V> = a => a;

const a = {a: {required: 1, optional: 2}, b: {required: 3}}

const recordFromInlined = record({a: {required: 1, optional: 2}, b: {required: 3}})
const recordFromA = record(a)

recordFromInlined.b.optional // no error
recordFromA.b.optional // error