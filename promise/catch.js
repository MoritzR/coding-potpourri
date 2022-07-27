const throwsSync = () => { throw new Error("thrown in throwsSync") }
const throwsAsync = async () => { throw new Error("thrown in throwsAsync") }
const logsSyncThrowsAsync = async () => {
    console.log("log in logsSyncThrowsAsync")
    throw new Error("thrown in logsSyncThrowsAsync")
}
const logsAsyncThrowsAsync = async () => {
    Promise.resolve().then(() => console.log("log in logsAsyncThrowsAsync"))
    throw new Error("thrown in logsAsyncThrowsAsync")
}

const program = async () => {
    try {
        throwsSync()
    } catch (e) {
        // catches
        console.log("catched1")
    }

    try {
        throwsAsync()
    } catch (e) {
        // doesn't catch
        console.log("catched2")
    }

    try {
        logsSyncThrowsAsync()
    } catch (e) {
        // doesn't catch
        console.log("catched3")
    }

    try {
        logsAsyncThrowsAsync()
    } catch (e) {
        // doesn't catch
        console.log("catched4")
    }

    try {
        await throwsAsync()
    } catch (e) {
        // catches
        console.log("catched5")
    }
    try {
        await logsSyncThrowsAsync()
    } catch (e) {
        // catches
        console.log("catched6")
    }
}

program()