import { filter, isEven, pipe, sum } from "./helpers"

const numbers = [1, 2, 3, 8, 1, 2, 3, 1, 10]

// Task: with 'array' as input, return the sum of all even numbers as a string (with .toString())
// You also have the 'sum' and the 'isEven' function available

// a solution could be the folling:
const result =
    sum(
        numbers
            .filter(isEven)
    ).toString()

// Annoyingly, the way to read this function in the order that each step is executed is the following:
sum( // 3.
    numbers // 1.
        .filter(isEven) // 2. 
).toString() // 4.

// We start in the middle, then jump to the top, then back to the end

// What if we could read it more like a 'pipeline', from top to bottom?
// This would be possible if 'sum' was a function on arrays ... or if we use 'pipe':
pipe(
    numbers,
    filter(isEven),
    sum,
    toString
)

// Alternatively we could create a variable for each step
const evenNumbersArray = numbers.filter(isEven)
const evenNumbersSum = sum(evenNumbersArray)
const stringOfSum = evenNumbersArray.toString()

/* I intentially made a mistake here.
I don't stringify the sum but the array.
This error will be harder to spot in more complex examples.
'pipe' ensures that there is only ever one values present that is passed from the top
to the bottom. When using variables, the dependencies can be more complex, as there is
no rule ensuring that each variable is only used in the following line.
*/
