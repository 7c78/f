import path from "path"
import { defineConfig } from "vitest/config"

export default defineConfig({
    test: {
        environment: "jsdom",
        alias: {
            "@PlainText": path.resolve(__dirname, "plain-text/src/PlainText")
        },
        exclude: [
            "**/.direnv/**",
            "**/.spago/**",
            "**/output/**"
        ],
        watch: false
    }
})
