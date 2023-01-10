import { filter, pipe, sum } from "./helpers"

const numbers = [1, 2, 3, 8, 1, 2, 3, 1, 10]

// Task: with 'array' as input, return the sum of all even numbers as a string (with .toString())
// You also have the 'sum' function available

// a solution could be the folling:
const result =
    sum(
        numbers
            .filter(n => n % 2 === 0)
    ).toString()

// Annoyingly, the ways to read this function in the order that each step is executed is the following:
sum( // 3.
    numbers // 1.
        .filter(n => n % 2 === 0) // 2. 
).toString() // 4.

// We start in the middle, the jump to the top, then back to the end

// What if we could read it more like 'pipeline', from top to bottom?
// Is would be possible if 'sum' was a function on arrays, or if we use 'pipe':
pipe(
    numbers,
    filter(n => n % 2 === 0),
    sum,
    toString
)
