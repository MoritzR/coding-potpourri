declare function calculateNetPromise(project: Project): Promise<Net>
declare function calculateTaxPromise(project: Project, net: Net): Promise<Tax>
declare function calculateGrossPromise(net: Net, tax: Tax): Promise<Gross>

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

