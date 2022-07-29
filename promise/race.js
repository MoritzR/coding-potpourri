const fetchFast = () => new Promise(resolve => setTimeout(() => resolve("fast"), 1_000))
const fetchSlow = () => new Promise(resolve => setTimeout(() => {resolve("slow") }, 10_000))

async function program() {
    const result = await Promise.race([fetchFast(), fetchSlow()])
    await fetchFast()

    console.log("finished", result)
}

program()

// 'node race.js' finishes after how many seconds?