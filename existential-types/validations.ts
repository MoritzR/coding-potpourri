type Validation<A, B> = {
    preprocess: (_: A) => B | null
    getError: (_: B) => ErrorMessage
}
type ErrorMessage = { error: string } | "allfine"

/**
 * Like {@link Validation}, but it hides its second generic argument B.
 * This is needed to have a list of validations with the same input A, but
 * different intermediate values B, for example `Array<ValidationHiding<string>>`
 */
type HidingValidation<A> = (validationToMessage: ValidationToMessage<A>) => ErrorMessage
type MakeValidation = <A, B>(recipe: Validation<A, B>) => HidingValidation<A>

type ValidationToMessage<A> = <B>(validation: Validation<A, B>) => ErrorMessage

const apply = <T>(input: T) => <R>(func: (input: T) => R) => func(input)

const makeValidation: MakeValidation = apply

const runValidation: <A>(toValidate: A) => ValidationToMessage<A> =
    toValidate =>
        validation => {
            const magic = validation.preprocess(toValidate);
            return magic === null ? "allfine" : validation.getError(magic);
        }

const runHidingValidations: <A>(toValidate: A) => (validations: ReadonlyArray<HidingValidation<A>>) => ReadonlyArray<ErrorMessage> =
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


console.log(runHidingValidations('A string to test')([testLength, testFirst]))
