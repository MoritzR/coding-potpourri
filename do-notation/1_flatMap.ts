import { Project, Optional, Net, Tax, Gross, calculateNet, calculateTax, calculateGross } from "./0_null_checking";

// Let's replace explicit null checking with flatMapping.
function calculatePriceFlatMap(project: Project): Optional<[Net, Tax, Gross]> {
    return calculateNet(project)
        .flatMap(net =>
            calculateTax(project, net)
                .flatMap(tax =>
                    calculateGross(net, tax)
                        .map(gross => [net, tax, gross] as [Net, Tax, Gross])));
}
// The behavior is the same and we need less lines,
// but it actually seems less readable now :(
