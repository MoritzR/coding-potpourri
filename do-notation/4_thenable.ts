import { Gross, Net, Project, Tax } from "./0_null_checking";


interface OptionalThenable<T> {
    value: T | null;
    then<TResult1 = T, TResult2 = never>(onfulfilled?: ((value: T) => TResult1 | OptionalThenable<TResult1>) | undefined | null, onrejected?: ((reason: any) => TResult2 | OptionalThenable<TResult2>) | undefined | null): OptionalThenable<TResult1 | TResult2>;
};

// class OptionalImpl<T> implements OptionalThenable<T> {
//     value: T | null;
//     constructor(value: T | null) {
//         this.value = value;
//     }
//     then<TResult1 = T, TResult2 = never>(
//         onfulfilled?: (value: T) => TResult1 | OptionalThenable<TResult1>,
//         onrejected?: (reason: any) => TResult2 | OptionalThenable<TResult2>): OptionalThenable<TResult1 | TResult2> {

//         if (this.value === null) {
//             return this;
//         }
//         return onfulfilled(this.value)
//     }

// }

declare function calculateNetThenable(project: Project): OptionalThenable<Net>
declare function calculateTaxThenable(project: Project, net: Net): OptionalThenable<Tax>
declare function calculateGrossThenable(net: Net, tax: Tax): OptionalThenable<Gross>

async function calculatePriceThenable(project: Project): Promise<[Net, Tax, Gross]> {
    const net = await calculateNetThenable(project);
    const tax = await calculateTaxThenable(project, net);
    const gross = await calculateGrossThenable(net, tax);

    return [net, tax, gross];
}

function calculatePriceThenable2(project: Project): OptionalThenable<[Net, Tax, Gross]> {
    return calculateNetThenable(project)
        .then(net =>
            calculateTaxThenable(project, net)
                .then(tax =>
                    calculateGrossThenable(net, tax)
                        .then(gross => [net, tax, gross])));
}

