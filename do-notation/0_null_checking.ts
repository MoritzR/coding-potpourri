type Project = {}
type Optional<T> = Array<T> // = [] | [T]
type Net = number
type Tax = number
type Gross = number

// Task: Given the following three functions, calculate the net, tax and gross of a Project
declare function calculateNet(project: Project): Optional<Net>
declare function calculateTax(project: Project, net: Net): Optional<Tax>
declare function calculateGross(net: Net, tax: Tax): Optional<Gross>

// Let's try explicit null checking first.
function calculatePriceNullChecking(project: Project): Optional<[Net, Tax, Gross]> {
    const net = calculateNet(project);
    if (net === []) {
        return [];
    }

    const tax = calculateTax(project, net[0]);
    if (tax === []) {
        return [];
    }

    const gross = calculateGross(net[0], tax[0]);
    if (gross === []) {
        return [];
    }

    return [[net[0], tax[0], gross[0]]]
}
