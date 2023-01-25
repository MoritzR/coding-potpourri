import { Gross, Net, Project, Tax } from "./0_null_checking";

/*  Let's refactor our calculate function to no longer
    return an Optional, but a Promise instead.
    Instead of checking of the Optional is empty, we
    could now check if the promise rejects.
*/
declare function calculateNetPromise(project: Project): Promise<Net>
declare function calculateTaxPromise(project: Project, net: Net): Promise<Tax>
declare function calculateGrossPromise(net: Net, tax: Tax): Promise<Gross>

/*  Promises have a `then` method. For us this works just like the
    `flatMap` method from before.
    We can therefor rewrite our function like this:
*/
function calculatePricePromise(project: Project): Promise<[Net, Tax, Gross]> {
    return calculateNetPromise(project)
        .then(net =>
            calculateTaxPromise(project, net)
                .then(tax =>
                    calculateGrossPromise(net, tax)
                        .then(gross => [net, tax, gross])));
}

/*  Notice how we only had to swap `flatMap` with `then` and `Optional` with `Promise`.
    Still, we have not gained anything, yet. Our IDE is already suggesting a transformation,
    though. Let's apply it.
*/

async function calculatePriceAsync(project: Project): Promise<[Net, Tax, Gross]> {
    const net = await calculateNetPromise(project);
    const tax = await calculateTaxPromise(project, net);
    const gross = await calculateGrossPromise(net, tax);

    return [net, tax, gross];
}

/*  Wow, this is much more readable!
    The only issue now is, that our function needs to be async to be able
    to have nice nice syntax. Can we have the same syntax but without async?
    Sadly no, at least not exactly like that.
    Next, you can check out how the do-notation looks in Haskell (or C#/F#/Kotlin).
    Additionally, you can check out an emulation of the do-notation in TypeScript.
*/