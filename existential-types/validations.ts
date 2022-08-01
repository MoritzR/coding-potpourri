// check this blog post for more insight on what's happening here: https://rubenpieters.github.io/programming/typescript/2018/07/13/existential-types-typescript.html

type Validation<A, B> = {
    preprocess: (_: A) => B | null
    getError: (_: B) => ErrorMessage
}
type Error = { error: string }
type ErrorMessage = Error | "allfine"

/**
 * Like {@link Validation}, but it hides its second generic argument B.
 * This is needed to have a list of validations with the same input A, but
 * different intermediate values B, for example `Array<ValidationHiding<string>>`
 */
type HidingValidation<A> = (validationToMessage: ValidationToMessage<A>) => ErrorMessage | null
type MakeValidation = <A, B>(validation: Validation<A, B>) => HidingValidation<A>

type ValidationToMessage<A> = <B>(validation: Validation<A, B>) => ErrorMessage | null

const apply = <T>(input: T) => <R>(func: (input: T) => R) => func(input)

const makeValidation: MakeValidation = apply

const runValidation: <A>(toValidate: A) => ValidationToMessage<A> =
    toValidate =>
        validation => {
            const magic = validation.preprocess(toValidate);
            return magic === null ? null : validation.getError(magic);
        }

const runHidingValidations: <A>(toValidate: A) => (validations: Array<HidingValidation<A>>) => Array<ErrorMessage | null> =
    toValidate => validations =>
        validations.map(apply(runValidation(toValidate)))


const testLength = makeValidation({
    preprocess: (s: string) => s.length,
    getError: len => len > 3 ? { error: "too long" } : "allfine"
})

const testFirst = makeValidation({
    preprocess: (s: string) => s[0],
    getError: head => head === "o" ? { error: "should not start with 'o'" } : "allfine"
})

export function isNotNull<T>(value: T | null): value is T {
    return value !== null
}
export function isError<T>(message: ErrorMessage): message is Error {
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


console.log(asText(runHidingValidations('A string to test')([testLength, testFirst])))
