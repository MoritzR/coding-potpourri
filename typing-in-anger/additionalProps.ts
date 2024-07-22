// Let's run a web app. It need some settings for starting the app.
declare function startApp(startupSettings: Partial<Settings>): void

type Settings = {
    waitForDebugger: boolean,
    logLevel: "debug" | "warn" | "info" | "error"
    developerMode: boolean
}

startApp({
    waitForDebugger: false,
    logLevel: "warn",
    devMode: true // oh, devMode was renamed to developerMode, good that typescript complains
})

// Let's do the same but now our app is able to start with different settings based on the environment

type Env = "production" | "development"

// the environment is provided by the app startup process itself
const startAppEnvironmentSpecific = (getSettings: (env: Env) => Partial<Settings>) => {
    const env: Env = "development" // get Env from somewhere
    const settings = getSettings(env)
    console.log("App starting...", settings)
}

startAppEnvironmentSpecific(env => ({
    logLevel: "warn",
    waitForDebugger: false,
    /* Now typescript doesn't complain, it looks like we are setting a dev mode,
        but in fact, there is no such thing! */
    devMode: env === "development" // no typescript error :(
})) // we can fix this by adding 'satisfies Settings'