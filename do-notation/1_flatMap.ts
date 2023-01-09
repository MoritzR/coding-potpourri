import { Project, Optional, Net, Tax, Gross, calculateNet, calculateTax, calculateGross } from "./0_null_checking";

// Now replace explicit null checking with flatMapping. This doesn't seem nicer/more readable.
function calculatePriceFlatMap(project: Project): Optional<[Net, Tax, Gross]> {
    return calculateNet(project)
        .flatMap(net =>
            calculateTax(project, net)
                .flatMap(tax =>
                    calculateGross(net, tax)
                        .map(gross => [net, tax, gross] as [Net, Tax, Gross])));
}
