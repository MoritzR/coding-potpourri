import fs from "fs/promises"
import P from "parser-ts/Parser"
import S, { run } from "parser-ts/string"

async function main() {
    const sqlQuery = (await fs.readFile('../query.txt')).toString();
    console.log(runParser(P.many(P.sat(() => true)), "uuua"))
}

const runParser = (parser, string) => run(string)(parser)


main()