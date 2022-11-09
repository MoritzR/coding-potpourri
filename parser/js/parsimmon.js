import P from "parsimmon"
import fs from "fs/promises"

const between = (left, right, parser) =>
    left
        .then(parser)
        .chain(result => right.result(result))

async function main() {
    const sqlQuery = (await fs.readFile('../query.txt')).toString();
    console.log(runParser(P.any.many().then(P.string("a")), "uuua"))
    console.log(runParser(enumParser, sqlQuery))
}

const runParser = (parser, string) => parser.tryParse(string)

const anyString = P.any.many() // is this too greedy? apparently yes

const stringInQuotationMarks =
    between(
        P.string("'"),
        P.string("'"),
        P.takeWhile(c => c !== "'"))

const sqlList =
    between(
        P.string("("),
        P.string(")"),
        P.sepBy(
            stringInQuotationMarks,
            P.string(", ")
        ))

const enumParser = anyString
    .then(between(
        P.string('check ("status" in '),
        P.string(")"),
        sqlList))

main()