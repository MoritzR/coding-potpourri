type Project = {}
type Optional<T> = Array<T> // = [] | [T]
type Net = number
type Tax = number
type Gross = number

declare function calculateNet(project: Project): Optional<Net>
declare function calculateTax(project: Project, net: Net): Optional<Tax>
declare function calculateGross(net: Net, tax: Tax): Optional<Gross>

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
