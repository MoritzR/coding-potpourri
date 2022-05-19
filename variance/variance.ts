// covariant types

const array: Array<number> = [1, 2, 3]

type Optional<T> = T | null
const optional: Optional<number> = 3

type Stream<T> = [T, Stream<T>]
const makeStream: () => Stream<number> = () => [1, makeStream()]

type MapOptional<A, B> = (f: (_: A) => B) => (optional: Optional<A>) => Optional<B>
const mapOptional: MapOptional<number, string> = f => optional => optional === null ? null : f(optional)

// contravaraint types

type Compare<T> = (a: T) => (b: T) => -1 | 0 | 1

type ContraMapCompare<A, B> = (f: (_: A) => B) => (compare: Compare<B>) => Compare<A>
const contraMapCompare: ContraMapCompare<number, string> = f => compare => a => b => compare(f(a))(f(b));




// variance in action

type Builder<T> = () => T // covariant
type ToStringer<T> = (_: T) => string // contravariant

type Parent = { foo: number }
type Child = { bar: number } & Parent

var buildParent: Builder<Parent> = () => ({ foo: 3 })
var buildChild: Builder<Child> = () => ({ foo: 2, bar: 4 })

var parentToString: ToStringer<Parent> = parent => `Foo ${parent.foo}`
var childToString: ToStringer<Child> = child => `Bar ${child.bar}, Foo ${child.foo}`

// covariant, when a Child is-a Parent, then a Builder<Child> is-a Builder<Parent>, but not the other way around
buildParent = buildChild
buildChild = buildParent

// contravariant, when a Child is-a Parent, then a ToStringer<Parent> is-a ToStringer<Child>, but not the other way around
parentToString = childToString
childToString = parentToString

// both covariant and contravariant in T, also known as invariant in T
type Transform<T> = (_: T) => T;

var transformParent: Transform<Parent> = parent => ({ foo: parent.foo * 2 })
var transformChild: Transform<Child> = child => ({ foo: child.foo + 1, bar: child.bar - 1 })

// Transform<T> is invariant in T, so no two Transforms can be assigned to each other, unless T is the same type
transformParent = transformChild
transformChild = transformParent