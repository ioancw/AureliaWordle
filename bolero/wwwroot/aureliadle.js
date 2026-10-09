// Browser access the F# code can't do directly from WebAssembly.
window.aureliadle = (() => {
    let removeListeners = () => {};

    // Storage can be unavailable or full (e.g. some private browsing modes); the game still works without it.
    const get = key => { try { return localStorage.getItem(key); } catch { return null; } };
    const set = (key, value) => { try { localStorage.setItem(key, value); } catch { } };

    // Physical keys -> on-screen key names; anything else is ignored.
    const keyName = ev => {
        if (ev.ctrlKey || ev.metaKey || ev.altKey) return null;
        if (ev.key === "Enter") return "Ent";
        if (ev.key === "Backspace") return "Del";
        return /^[a-z]$/i.test(ev.key) ? ev.key.toLowerCase() : null;
    };

    return {
        get,
        set,

        // Calls back into F# for key presses and for anything that may have changed the saved game:
        // another tab saving, the page coming back into view, or the date changing (checked each minute).
        listen(dotnet) {
            const onKey = ev => {
                const key = keyName(ev);
                if (key) { ev.preventDefault(); dotnet.invokeMethodAsync("OnKey", key); }
            };
            const onStorage = ev => dotnet.invokeMethodAsync("OnStorageChanged");
            const onVisible = () => { if (document.visibilityState === "visible") onStorage(); };
            window.addEventListener("keydown", onKey);
            window.addEventListener("storage", onStorage);
            window.addEventListener("pageshow", onVisible);
            document.addEventListener("visibilitychange", onVisible);
            const timer = setInterval(onStorage, 60000);
            removeListeners = () => {
                window.removeEventListener("keydown", onKey);
                window.removeEventListener("storage", onStorage);
                window.removeEventListener("pageshow", onVisible);
                document.removeEventListener("visibilitychange", onVisible);
                clearInterval(timer);
            };
        },

        unlisten() { removeListeners(); },

        // Share sheet on phones, clipboard elsewhere. Resolves to "shared", "copied", "cancelled" or "failed".
        share(text) {
            return navigator.share && matchMedia("(pointer: coarse)").matches
                ? navigator.share({ text }).then(() => "shared", () => "cancelled")
                : navigator.clipboard.writeText(text).then(() => "copied", () => "failed");
        },

        // Stops an on-screen key keeping focus, so a later physical Enter/Space doesn't press it again.
        blurActive() { document.activeElement?.blur(); }
    };
})();
