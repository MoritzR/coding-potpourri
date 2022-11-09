import fs from "fs/promises"
import P from "parjs"
import C from "parjs/combinators.js"

async function main() {
    const sqlQuery = (await fs.readFile('../query.txt')).toString();
    console.log(runParser(enumParser, sqlQuery))
}

const between = (left, right, parser) =>
    left
        .pipe(C.qthen(parser))
        .pipe(C.thenq(right))

const anyString = P.anyChar().pipe(C.many())

const stringInQuotationMarks = between(
    P.string("'"),
    P.string("'"),
    C.manyTill(P.string("'")))

const sqlList = between(
    P.string("("),
    P.string(")"),
    C.manySepBy(
        stringInQuotationMarks,
        P.string(", ")
    ))

const enumParser = anyString
    .pipe(C.qthen(between(
        P.string('check ("status" in '),
        P.string(")"),
        sqlList)))

const runParser = (parser, string) => parser.parse(string)

main()