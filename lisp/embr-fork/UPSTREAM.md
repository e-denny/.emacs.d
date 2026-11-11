# Fork, not a submodule

This directory is a **fork** of https://github.com/emacs-os/embr.el, copied
into the Emacs configuration as ordinary files.  It is no longer a git
submodule, so edits here are just edits: no patch file to reapply, no
`git submodule update` to conflict with, and no divergence bookkeeping.

- Forked from upstream commit `2b6725d`
  ("Run setup.sh in a comint buffer so installer prompts can be answered").
- Local modifications already applied at fork time, carried over from
  `patches/embr-system-chromium.patch` and the stealth patch on top of it:
  - launch Arch's `/usr/bin/chromium` instead of Playwright's bundled
    build (`embr-chromium-executable`);
  - `--disable-blink-features=AutomationControlled` and a headless-free
    user agent for the vanilla engine.

## What changes next

The Playwright driver has been replaced with a standard-library-only CDP
client that speaks to `chromium --headless=new --remote-debugging-pipe`
over fds 3/4.  Consequences:

- No `~/.local/share/embr/.venv`, no `playwright` package, no Playwright
  Chromium download.  `embr.py` runs on the system `python3`.
- `embr-browser-engine` loses its `'cloakbrowser` value: CloakBrowser is a
  Playwright wrapper, so the engine is now simply the Chromium binary named
  by `embr-chromium-executable`.
- `setup.sh`'s venv/CloakBrowser/extension branches are obsolete; only the
  blocklist download and the native canvas build still matter.

Deliberately dropped upstream features that depended on the Playwright
driver (extensions, headed-mode UI, `expect_download` semantics) are
reimplemented or removed one at a time, each noted where it happens in
`embr.py`.

## Local UI deviations

- The browser tab strip lives in the header line, to the left of the URL,
  instead of in `tab-line-format`.  Upstream put it in the tab line,
  which hides `global-tab-line-mode`'s per-window tabs; the header line
  sits below the tab line, so both stay visible.  There is no `[x]`
  close button — tabs are closed from the keyboard.
- Header-line clicks are mapped to a tab by column
  (`embr--tab-strip-columns`), because `posn-string` is unreliable for
  header-line text: `posn-at-x-y` reports no string there, though it does
  for the mode line.
