const foo: { readonly a: string | number } = { a: "hello world" }

const outer = () => {
    if (typeof foo.a === "string") {
        return
    }

    foo.a.toFixed() // foo.a is a number

    const callback = () => foo.a.toFixed() // foo.a is no longer a number, but why?
}