import * as R from "ramda"
import M from "sanctuary-maybe"


const addNumbers = a => b => a + b


// lists

// this is `map` but with two arguments instead of one
const addListsOfNumbers = R.lift(addNumbers)

const listOfNumbers = addListsOfNumbers([1, 2], [10, 11])

console.log("Add lists: ", listOfNumbers)

const incrementListOfNumbers = R.map(a => a + 1) // could also use R.lift here
const incrementedResult = incrementListOfNumbers([1, 2])
console.log("Increment List: ", incrementedResult)

// maybies
const maybeNumber1 = M.Just(3)
const maybeNumber2 = M.Just(4)

const addMaybes = R.lift(addNumbers)

const maybeResult = addMaybes(maybeNumber1, maybeNumber2)

console.log("Add maybes: ", maybeResult)

// functions
const mul2 = a => a * 2
const add5 = a => a + 5

const addFunctions = R.lift(addNumbers)

const resultFunction = addFunctions(mul2, add5)

console.log("Function", resultFunction(3))
