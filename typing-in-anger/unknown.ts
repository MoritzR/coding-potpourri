// how to put a string into an array of numbers
const intArray: number[] = [1, 2, 3]
const list: unknown[] = intArray
list.push("not a number") //compiles

// runtime error: `int.toPrecision is not a function`
const printInts = (ints: number[]) => ints.forEach(int => { console.log(int.toPrecision(1)) })

printInts(intArray);


// This is not possible if you try to define your own array type
interface MyArray<T> {
    push: (element: T) => void
}

const myIntArray: MyArray<number> = { push: a => { console.log("I pushed the number", a) } };
// type error: `Type 'unknown' is not assignable to type 'number'`
const myUnknownArray: MyArray<unknown> = myIntArray;

// The built in array type seems to behave similar to the following Payload type
interface Payload<T> { payload: T }
const intPayload: Payload<number> = { payload: 1 };
const unknownPayload: Payload<unknown> = intPayload;
unknownPayload.payload = "hello world"
// runtime error: `int.toPrecision is not a function`
const printIntPayload = (intPayload: Payload<number>) => { console.log(intPayload.payload.toPrecision(1)) }


// We can fix this by adding invaraint annotations (in out)
interface InvariantPayload<in out T> { payload: T }
const invariantIntPayload: InvariantPayload<number> = { payload: 1 };
// type error: `Type 'unknown' is not assignable to type 'number'`, nice!
const invariantUnknownPayload: InvariantPayload<unknown> = invariantIntPayload;
// but we can still assing `intPayload` to it!
const invariantUnknownPayload2: InvariantPayload<unknown> = intPayload;
unknownPayload.payload = "hello world"