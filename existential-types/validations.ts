// check this blog post for more insight on what's happening here: https://rubenpieters.github.io/programming/typescript/2018/07/13/existential-types-typescript.html

type RawValidation<A, T, B> = {
    preprocess: (_: A) => B | null
    getError: (_: B) => T
}
type MyError = { error: string }
type ErrorMessage = MyError | "allfine"

/**
 * Like {@link RawValidation}, but it hides its second generic argument B.
 * This is needed to have a list of validations with the same input A, but
 * different intermediate values B, for example `Array<Validation<string>>`
 */
type Validation<A> =
    (callback: <B>(rawValidation: RawValidation<A, ErrorMessage, B>) => ErrorMessage | null)
        => ErrorMessage | null

type MakeValidation = <A, B>(validation: RawValidation<A, ErrorMessage, B>) => Validation<A>

const applyTo = <A>(input: A) => <B>(func: (input: A) => B): B => func(input)

const makeValidation: MakeValidation = applyTo

const runRawValidation: <A>(toValidate: A) => <B>(validation: RawValidation<A, ErrorMessage, B>) => ErrorMessage | null =
    toValidate => rawValidation => {
        const preprocessed = rawValidation.preprocess(toValidate);
        return preprocessed === null ? null : rawValidation.getError(preprocessed);
    }

const runValidations: <A>(toValidate: A) => (validations: Array<Validation<A>>) => Array<ErrorMessage | null> =
    toValidate => validations => validations.map(applyTo(runRawValidation(toValidate)))

const validations: Array<Validation<string>> = [
    makeValidation({
        preprocess: s => s[0],
        getError: first => first === "o" ? { error: "should not start with 'o'" } : "allfine"
    }),
    makeValidation({
        preprocess: s => s.length,
        getError: len => len > 3 ? { error: "too long" } : "allfine"
    })
]

function isNotNull<T>(value: T | null): value is T {
    return value !== null
}
function isError(message: ErrorMessage): message is MyError {
    return typeof message === "object"
}

const asText: (messages: Array<ErrorMessage | null>) => string =
    validationResults => {
        const validationsThatRan = validationResults.filter(isNotNull)
        const errors = validationsThatRan.filter(isError).map(error => error.error)
        return `I had ${validationResults.length}\
 validations and ran ${validationsThatRan.length}\
 of them. ${errors.length}\
 returned errors. Here are all errors:\
 ${errors.join(", ")}`
    }


console.log(asText(runValidations('A string to test')(validations)))
