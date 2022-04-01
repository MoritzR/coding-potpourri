function calculatePriceFlatMap(project: Project): Optional<[Net, Tax, Gross]> {
    return calculateNet(project)
        .flatMap(net =>
            calculateTax(project, net)
                .flatMap(tax =>
                    calculateGross(net, tax)
                        .map(gross => [net, tax, gross] as [Net, Tax, Gross])));
}
