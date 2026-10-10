// Builds the Feliz daily games (feliz/site) into dist-feliz/. Fable compiles the F# first;
// see the build:feliz script.
import { fileURLToPath } from "url";

const site = (page) => fileURLToPath(new URL(`feliz/site/${page}`, import.meta.url));

export default {
    root: "feliz/site",
    // relative asset paths, so the games work under any URL path
    base: "./",
    build: {
        outDir: "../../dist-feliz",
        emptyOutDir: true,
        rollupOptions: {
            input: {
                index: site("index.html"),
                aureliadle: site("aureliadle/index.html"),
                numberdle: site("numberdle/index.html"),
                whichwitch: site("whichwitch/index.html"),
            },
        },
    },
    server: { fs: { allow: ["../.."] } },
};
