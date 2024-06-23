// TypeScript infers different types here, but why?

const record: <K extends string, V>(r: Record<K, V>) => Record<K, V> = a => a;

const a = {a: {required: 1, optional: 2}, b: {required: 3}}

const recordFromInlined = record({a: {required: 1, optional: 2}, b: {required: 3}})
const recordFromA = record(a)

recordFromInlined.b.optional // no error
recordFromA.b.optional // error


// produce a runtime error by extracting a function
type A = { prop: number }
type B = { prop: string }
type AB = A | B

let ab: AB = { prop: "a string" }

const update = () => {
  ab = { prop: 1 }
}

update()

ab.prop.split("") // runtime error :(

ab = { prop: 2 }

ab.prop.split("") // here it gets the type correctly


