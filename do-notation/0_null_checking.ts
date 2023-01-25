export type Project = {}
export type Optional<T> = Array<T> // = [] | [T]
export type Net = number
export type Tax = number
export type Gross = number

// Task: Given the following three functions, calculate the net, tax and gross of a Project
export declare function calculateNet(project: Project): Optional<Net>
export declare function calculateTax(project: Project, net: Net): Optional<Tax>
export declare function calculateGross(net: Net, tax: Tax): Optional<Gross>

// Let's try explicit null checking first.
function calculatePriceNullChecking(project: Project): Optional<[Net, Tax, Gross]> {
    const net = calculateNet(project);
    if (net.length === 0) {
        return [];
    }

    const tax = calculateTax(project, net[0]);
    if (tax.length === 0) {
        return [];
    }

    const gross = calculateGross(net[0], tax[0]);
    if (gross.length === 0) {
        return [];
    }

    return [[net[0], tax[0], gross[0]]]
}

// This is fine but seems to contain a lot of manual "null"-checking
// (or array emptiness checking in this case)