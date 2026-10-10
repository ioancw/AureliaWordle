// Builds the daily games (feliz/site) into dist/. Fable compiles the F# first; see the build script.
// Aureliadle is the site root; the other games are in subfolders.
import { fileURLToPath } from "url";

const site = (page) => fileURLToPath(new URL(`feliz/site/${page}`, import.meta.url));

export default {
    root: "feliz/site",
    // relative asset paths, so the games work under any URL path
    base: "./",
    build: {
        outDir: "../../dist",
        emptyOutDir: true,
        rollupOptions: {
            input: {
                aureliadle: site("index.html"),
                numberdle: site("numberdle/index.html"),
                whichwitch: site("whichwitch/index.html"), fractions: site("fractions/index.html"),
                // redirects from where the games were previewed
                previewIndex: site("games/index.html"),
                previewAureliadle: site("aureliadle/index.html"),
            },
        },
    },
    server: { fs: { allow: ["../.."] } },
};
