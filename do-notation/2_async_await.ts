import { Gross, Net, Project, Tax } from "./0_null_checking";


declare function calculateNetPromise(project: Project): Promise<Net>
declare function calculateTaxPromise(project: Project, net: Net): Promise<Tax>
declare function calculateGrossPromise(net: Net, tax: Tax): Promise<Gross>

// If, instead of an Optional, we return a Promise, we can get this nice readable code.
// But now all of the calling code has to be async aswell, when in reality our function
// is not async at all. Can we get this nice syntax without Promises?
// Sadly no, but next you can check out how this looks in Haskell (or other languages like C# and F#)
async function calculatePriceAsync(project: Project): Promise<[Net, Tax, Gross]> {
    const net = await calculateNetPromise(project);
    const tax = await calculateTaxPromise(project, net);
    const gross = await calculateGrossPromise(net, tax);

    return [net, tax, gross];
}

function calculatePricePromise(project: Project): Promise<[Net, Tax, Gross]> {
    return calculateNetPromise(project)
        .then(net =>
            calculateTaxPromise(project, net)
                .then(tax =>
                    calculateGrossPromise(net, tax)
                        .then(gross => [net, tax, gross])));
}

