// tested on typescript 4.8

type Cat = {
    lineage: string,
    type: "house" | "stray"
}
type Dog = {
    owner: string
    type: "good" | "bad"
}

const houseCat: Cat = {
    lineage: "von snuffles",
    type: "house"
}
const typeEror: Cat = {
    // setting 'type' to something not included in 'Cat' correctly raises a type error
    // @ts-expect-error
    type: "i am a dog"
}

function updateType<T extends Cat | Dog>(input: T): T {
    return {
        ...input,
        type: 99 // what now? that is clearly not something that should be possible
    }
}

const updatedCat = updateType(houseCat)

// ouch, a runtime error :(
updatedCat.type.split("")

// this is a know limitation of typescript, see https://github.com/microsoft/TypeScript/issues/10727#issuecomment-725729117
