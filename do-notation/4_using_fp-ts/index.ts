import { Gross, Net, Project, Tax } from "../0_null_checking";
import { Option, Do, bind, map } from "fp-ts/Option"
import { pipe } from "fp-ts/function";

// To emulate the do-notation we can use Option type from fp-ts.
declare function calculateNetOption(project: Project): Option<Net>
declare function calculateTaxOption(project: Project, net: Net): Option<Tax>
declare function calculateGrossOption(net: Net, tax: Tax): Option<Gross>

const calculatePriceOption = (project: Project): Option<[Net, Tax, Gross]> =>
    pipe(
        Do,
        bind("net", () => calculateNetOption(project)),
        bind("tax", ({ net }) => calculateTaxOption(project, net)),
        bind("gross", ({ net, tax }) => calculateGrossOption(net, tax)),
        map(({ net, tax, gross }) => [net, tax, gross])
    )
