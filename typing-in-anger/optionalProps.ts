// Optional properties can cause runtime errors.

type Foo = { readonly foo: string };
type Bar = { readonly foo: string, bar?: number };
// This won't compile
// const foo: Foo = { foo: "foo", bar: "bar" };
// But this will
const thing = { foo: "foo", bar: "bar" };
const foo: Foo = thing;
// Uh oh...
const bar: Bar = foo;

// runtime error because bar.bar is a string not a number :(
bar.bar?.toFixed()
