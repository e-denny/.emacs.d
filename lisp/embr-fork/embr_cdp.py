"""Standard-library-only Chrome DevTools Protocol driver for embr.

This module replaces Playwright.  It launches the system Chromium with
`--headless=new --remote-debugging-pipe' and speaks CDP over file
descriptors 3/4 -- the transport Puppeteer uses -- so there is no
websocket library, no npm tree and no browser download: the daemon runs
on the system `python3' and drives whatever Chromium the user already has.

The classes deliberately mirror the small slice of the Playwright API that
`embr.py' calls (`Browser' stands in for Playwright's persistent context,
`Page' for its page), so the daemon's logic -- framing, tabs, hints,
session restore -- stayed unchanged when the driver was swapped.

Two deviations from Playwright worth knowing about:

- `Page.on' takes CDP method names ("Page.screencastFrame") as well as the
  lifecycle events embr listens for ("domcontentloaded", "crash", "close").
  Playwright's separate CDP session object is therefore just the page.
- `await page.detach()' is a no-op.  embr's screencast stop/start cycle
  treats attaching as acquiring a session; here the session is the page.

Local fork of embr.el; not upstream (see UPSTREAM.md).
"""

import asyncio
import base64
import json
import os
import re
import shutil
import struct
import subprocess
import sys
import time
from pathlib import Path

_MISSING = object()


def log(msg):
    print(f"embr: {msg}", file=sys.stderr)


# Warnings this daemon provokes on purpose: capturing screenshots while a
# screencast runs, and using software GL.  Chromium repeats them every few
# frames, and they would bury real errors in Emacs's stderr buffer.
_BENIGN_CHROMIUM = (
    "SharedImageManager::ProduceMemory",
    "Automatic fallback to software WebGL",
    "gles2_cmd_decoder_passthrough",
    "Failed to send GetTerminationStatus",
    "dbus/bus.cc",
)


def _noisy_chromium_line(line):
    return any(marker in line for marker in _BENIGN_CHROMIUM)


class CDPError(RuntimeError):
    """A CDP command returned an error, or the connection died."""


# ── Chromium launch ──────────────────────────────────────────────────


def find_executable(preferred=None):
    """Return the Chromium binary to drive."""
    if preferred:
        if not os.path.isfile(preferred):
            raise CDPError(f"chromium executable not found: {preferred}")
        return preferred
    for name in ("chromium", "chromium-browser", "google-chrome-stable",
                 "google-chrome", "chrome"):
        path = shutil.which(name)
        if path:
            return path
    raise CDPError("no chromium binary found; set embr-chromium-executable")


def chrome_version(executable):
    """Return the "a.b.c.d" version of EXECUTABLE, or None."""
    try:
        out = subprocess.run([executable, "--version"], capture_output=True,
                             text=True, timeout=5).stdout
    except Exception as exc:
        log(f"cannot read chromium version from {executable}: {exc}")
        return None
    match = re.search(r"(\d+\.\d+\.\d+\.\d+)", out)
    if not match:
        log(f"unparsable chromium --version: {out.strip()!r}")
        return None
    return match.group(1)


def desktop_user_agent(executable):
    """A plain desktop Chrome user agent for EXECUTABLE.

    Headless Chromium announces itself as "HeadlessChrome/156.0.0.0" in
    `navigator.userAgent', which is the first thing a bot check reads.
    Pass the same version with the stock "Chrome" product token instead,
    from the command line so no init script has to win a race with the
    page.  Return None if the version cannot be read, leaving the real
    user agent alone rather than inventing a mismatched one.
    """
    version = chrome_version(executable)
    if not version:
        return None
    machine = os.uname().machine or "x86_64"
    return (f"Mozilla/5.0 (X11; Linux {machine}) AppleWebKit/537.36 "
            f"(KHTML, like Gecko) Chrome/{version} Safari/537.36")


def _launch_flags(*, headless, display_mode, profile_dir, extensions,
                  proxy_pac, user_agent):
    """Chromium switches for a browser embr can drive and screenshot."""
    flags = [
        # The transport: fds 3/4 instead of a TCP port, so no port to
        # report, discover or collide on.
        "--remote-debugging-pipe",
        f"--user-data-dir={profile_dir}",
        # Keep first-run noise out of the frames.
        "--no-first-run",
        "--no-default-browser-check",
        "--disable-search-engine-choice-screen",
        "--disable-sync",
        "--disable-component-update",
        "--disable-background-networking",
        "--disable-breakpad",
        "--disable-dev-shm-usage",
        "--force-color-profile=srgb",
        "--password-store=basic",
        "--use-mock-keychain",
        "--metrics-recording-only",
        "--allow-pre-commit-input",
        "--disable-popup-blocking",
        "--disable-prompt-on-repost",
        "--disable-hang-monitor",
        # Software GL keeps WebGL alive in containers without a GPU;
        # without it Chromium 156 refuses to create WebGL contexts.
        "--enable-unsafe-swiftshader",
        # Chromium reports navigator.webdriver whenever the automation
        # pipe is attached.  Bot checks read that first; this is the
        # switch that clears it.
        "--disable-blink-features=AutomationControlled",
        # The window a fresh profile would otherwise remember.
        "--window-size=1280,720",
    ]
    if headless:
        flags.insert(0, "--headless=new")
    if display_mode == "headed-offscreen":
        # xvfb-run provides the display; force the X11 backend so the
        # window lands there rather than on a Wayland socket.
        flags.append("--ozone-platform=x11")
    if extensions:
        flags.append("--load-extension=" + ",".join(extensions))
    if proxy_pac:
        flags.append(f"--proxy-pac-url=file://{proxy_pac}")
    if user_agent:
        flags.append(f"--user-agent={user_agent}")
    return flags


# ──  Transport ───────────────────────────────────────────────────────


class _Session:
    """One flat CDP session: the browser itself, or a page."""

    def __init__(self, conn, session_id):
        self._conn = conn
        self.session_id = session_id
        self._handlers = {}
        self.on_closed = []

    async def send(self, method, params=None):
        return await self._conn._send(method, params, self.session_id)

    def on(self, method, callback):
        self._handlers.setdefault(method, []).append(callback)

    async def detach(self):
        """Playwright-compat no-op; see the module docstring."""

    def _dispatch(self, method, params):
        for callback in list(self._handlers.get(method, ())):
            try:
                callback(params)
            except Exception as exc:  # A handler must not kill the loop.
                log(f"handler for {method} failed: {exc}")


class Connection:
    """CDP over `--remote-debugging-pipe' (fds 3 and 4)."""

    def __init__(self, argv):
        self.argv = argv
        self._proc = None
        self._read_fd = None
        self._write_fd = None
        self._read_buf = b""
        self._pending = {}
        self._next_id = 0
        self._sessions = {}
        self.browser_session = _Session(self, None)
        self._closed = False
        self._stderr_tail = []

    async def start(self):
        # Two pipes.  Chromium's --remote-debugging-pipe reads fd 3 and
        # writes fd 4, and it insists they are open when it starts.  They
        # are created by a shell redirection rather than by preexec_fn:
        # CPython closes every fd the child inherited *after* preexec_fn
        # runs, which silently undoes a dup2 there (cost me an afternoon).
        # `exec` keeps the pid, so the shell is not an extra process.
        to_browser_r, to_browser_w = os.pipe()
        from_browser_r, from_browser_w = os.pipe()
        script = f'exec "$0" "$@" 3<&{to_browser_r} 4>&{from_browser_w}'
        try:
            self._proc = await asyncio.create_subprocess_exec(
                "/bin/sh", "-c", script, *self.argv,
                pass_fds=(to_browser_r, from_browser_w),
                stdin=subprocess.DEVNULL,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.PIPE,
            )
        except Exception as exc:
            raise CDPError(f"cannot launch chromium: {exc}") from exc
        finally:
            for fd in (to_browser_r, from_browser_w):
                try:
                    os.close(fd)
                except OSError:
                    pass
        self._read_fd = from_browser_r
        self._write_fd = to_browser_w
        asyncio.get_running_loop().add_reader(self._read_fd, self._on_readable)
        asyncio.ensure_future(self._drain_stderr())

    async def _drain_stderr(self):
        """Keep Chromium's stderr out of Emacs, but not out of reach."""
        if not self._proc or not self._proc.stderr:
            return
        while True:
            line = await self._proc.stderr.readline()
            if not line:
                return
            text = line.decode(errors="replace").rstrip()
            if not text:
                continue
            # Chromium logs every noisy dbus/gpu warning here.  Keep a
            # ring buffer for diagnosis, forward only real failures.
            self._stderr_tail.append(text)
            del self._stderr_tail[:-80]
            if _noisy_chromium_line(text):
                continue
            if "ERROR" in text or "FATAL" in text:
                log(f"chromium: {text}")

    # -- reading -----------------------------------------------------

    def _on_readable(self):
        try:
            chunk = os.read(self._read_fd, 262144)
        except (BlockingIOError, InterruptedError):
            return
        except OSError as exc:
            self._fail_all(f"pipe read failed: {exc}")
            return
        if not chunk:
            self._fail_all("chromium closed the debugging pipe")
            return
        self._read_buf += chunk
        while b"\0" in self._read_buf:
            raw, self._read_buf = self._read_buf.split(b"\0", 1)
            if not raw.strip():
                continue
            try:
                message = json.loads(raw)
            except ValueError as exc:
                log(f"unparsable CDP message ({exc}): {raw[:120]!r}")
                continue
            self._route(message)

    def _route(self, message):
        if "id" in message:
            pending = self._pending.pop(message["id"], None)
            if pending is None:
                return
            future = pending
            if future.done():
                return
            if "error" in message:
                error = message["error"]
                future.set_exception(CDPError(
                    f"{error.get('message', 'error')}"
                    f" ({error.get('code', '?')})"))
            else:
                future.set_result(message.get("result", {}))
            return
        method = message.get("method")
        if not method:
            return
        session_id = message.get("sessionId")
        params = message.get("params", {})
        if session_id is None:
            self.browser_session._dispatch(method, params)
            return
        session = self._sessions.get(session_id)
        if session is not None:
            session._dispatch(method, params)

    def _fail_all(self, reason):
        if self._closed:
            return
        self._closed = True
        try:
            asyncio.get_running_loop().remove_reader(self._read_fd)
        except Exception:
            pass
        for future in self._pending.values():
            if not future.done():
                future.set_exception(CDPError(reason))
        self._pending.clear()
        for session in list(self._sessions.values()):
            for callback in list(session.on_closed):
                try:
                    callback()
                except Exception:
                    pass

    # -- writing -----------------------------------------------------

    async def _send(self, method, params=None, session_id=None):
        if self._closed:
            raise CDPError("connection to chromium is closed")
        self._next_id += 1
        message = {"id": self._next_id, "method": method,
                   "params": params or {}}
        if session_id:
            message["sessionId"] = session_id
        future = asyncio.get_running_loop().create_future()
        self._pending[self._next_id] = future
        await self._write(json.dumps(message).encode() + b"\0")
        return await future

    async def _write(self, data):
        """Write DATA to the pipe, waiting for writability if it is full."""
        while data:
            try:
                written = os.write(self._write_fd, data)
                data = data[written:]
            except (BlockingIOError, InterruptedError):
                future = asyncio.get_running_loop().create_future()
                loop = asyncio.get_running_loop()

                def _writable():
                    loop.remove_writer(self._write_fd)
                    if not future.done():
                        future.set_result(None)

                loop.add_writer(self._write_fd, _writable)
                await future
            except OSError as exc:
                raise CDPError(f"pipe write failed: {exc}") from exc

    def session(self, session_id):
        session = self._sessions.get(session_id)
        if session is None:
            session = _Session(self, session_id)
            self._sessions[session_id] = session
        return session

    async def close(self):
        if self._closed:
            return
        self._closed = True
        try:
            asyncio.get_running_loop().remove_reader(self._read_fd)
        except Exception:
            pass
        proc = self._proc
        if proc and proc.returncode is None:
            try:
                proc.terminate()
                await asyncio.wait_for(proc.wait(), timeout=5)
            except Exception:
                try:
                    proc.kill()
                except Exception:
                    pass
        for fd in (self._read_fd, self._write_fd):
            try:
                os.close(fd)
            except OSError:
                pass
        self._read_fd = self._write_fd = None

    def diagnostics(self):
        return "\n".join(self._stderr_tail[-20:])


# ── Downloads ────────────────────────────────────────────────────────


class Download:
    """One in-flight download (Browser.downloadWillBegin ... Progress)."""

    def __init__(self, browser, guid, suggested_filename):
        self._browser = browser
        self.guid = guid
        self.suggested_filename = suggested_filename or ""
        self._done = asyncio.get_running_loop().create_future()
        self._cancelled = False

    async def cancel(self):
        if self._cancelled:
            return
        self._cancelled = True
        try:
            await self._browser.send("Browser.cancelDownload",
                                     {"guid": self.guid})
        except CDPError:
            pass

    async def save_as(self, path):
        """Move the finished download to PATH (deduplicated by the caller)."""
        await self._done
        source = self._browser.download_dir / self._suggested_basename
        if not source.exists():
            raise CDPError(f"download vanished: {source}")
        os.replace(source, path)

    @property
    def _suggested_basename(self):
        # Chromium writes the suggested name into download_dir.
        return os.path.basename(self.suggested_filename) or self.guid

    def _finish(self, state):
        if self._done.done():
            return
        if state == "canceled":
            self._done.set_exception(CDPError("download canceled"))
        else:
            self._done.set_result(None)


class DownloadInfo:
    """What `page.expect_download() as dl_info' yields: `await dl_info.value'."""

    def __init__(self):
        self.value = asyncio.get_running_loop().create_future()

    async def __aenter__(self):
        return self

    async def __aexit__(self, *exc_info):
        return False


# ── Browser (Playwright's persistent context) ────────────────────────


class Browser(_Session):
    """The browser process plus the page targets embr calls tabs."""

    def __init__(self, conn, *, viewport, screen, color_scheme,
                 download_dir, init_scripts=None):
        super().__init__(conn, None)
        self.viewport = dict(viewport)
        self.screen = dict(screen)
        self.color_scheme = color_scheme
        self.download_dir = Path(download_dir)
        self.pages = []
        self.init_scripts = list(init_scripts or ())
        self._page_handlers = []
        self._download_handlers = []
        self._by_target = {}
        self._routes = []
        self._routes_enabled = False
        self._attach_lock = asyncio.Lock()
        self._download_waiter = None
        self._downloads = {}

    # -- launch ------------------------------------------------------

    @classmethod
    async def launch(cls, *, executable=None, profile_dir, headless,
                     display_mode, width, height, screen, color_scheme,
                     extensions=(), proxy_pac=None, download_dir=None):
        executable = find_executable(executable)
        user_agent = None
        if headless or display_mode == "headed-offscreen":
            user_agent = desktop_user_agent(executable)
            if user_agent:
                log(f"user agent {user_agent}")
        flags = _launch_flags(
            headless=headless, display_mode=display_mode,
            profile_dir=str(profile_dir), extensions=extensions,
            proxy_pac=proxy_pac, user_agent=user_agent)
        argv = [executable, *flags, "about:blank"]
        log(f"executable {executable} ({' '.join(flags[:2])})")
        conn = Connection(argv)
        await conn.start()
        download_dir = Path(download_dir or (Path(profile_dir) / "downloads"))
        download_dir.mkdir(parents=True, exist_ok=True)
        browser = cls(conn, viewport={"width": width, "height": height},
                      screen=screen, color_scheme=color_scheme,
                      download_dir=download_dir)
        conn.browser_session = browser
        try:
            await browser._setup()
        except Exception as exc:
            await conn.close()
            if conn._stderr_tail:
                log("chromium said:\n" + conn.diagnostics())
            if any("SingletonLock" in line or "ProcessSingleton" in line
                   for line in conn._stderr_tail):
                raise CDPError(
                    f"the profile {profile_dir} is in use by another "
                    "Chromium; quit that browser (or its embr session) "
                    "and try again") from exc
            raise
        return browser

    async def _setup(self):
        # Downloads land in our own directory so save_as() can move them.
        await self.send("Browser.setDownloadBehavior", {
            "behavior": "allow",
            "downloadPath": str(self.download_dir),
            "eventsEnabled": True,
        })
        await self.send("Target.setDiscoverTargets", {"discover": True})
        self.on("Target.targetCreated", self._on_target_created)
        self.on("Target.targetDestroyed", self._on_target_destroyed)
        self.on("Target.targetInfoChanged", self._on_target_info_changed)
        self.on("Target.detachedFromTarget", self._on_detached_from_target)
        self.on("Browser.downloadWillBegin", self._on_download_will_begin)
        self.on("Browser.downloadProgress", self._on_download_progress)
        targets = await self.send("Target.getTargets")
        for info in targets.get("targetInfos", []):
            if info.get("type") == "page":
                await self._attach(info["targetId"])
        if not self.pages:
            await self.new_page()

    # -- targets and pages -------------------------------------------

    async def _attach(self, target_id):
        # One attach per target, ever: Target.targetCreated races with
        # our own new_page() call, and two sessions on one target means
        # two Page objects for one tab.
        async with self._attach_lock:
            existing = self._by_target.get(target_id)
            if existing is not None:
                return existing
            result = await self.send("Target.attachToTarget",
                                     {"targetId": target_id, "flatten": True})
            session_id = result.get("sessionId")
            if not session_id:
                raise CDPError(f"attach to {target_id} returned no session")
            page = Page(self, self._conn, session_id, target_id)
            # Events for this session must reach the Page, not a bare
            # _Session, so routing and URL tracking happen in one place.
            self._conn._sessions[session_id] = page
            self._by_target[target_id] = page
            self.pages.append(page)
        try:
            await page.setup()
        except Exception as exc:
            log(f"page setup failed: {exc}")
        # Same contract as Playwright: every new page, ours or the
        # site's, reaches the 'page' listeners.
        for callback in list(self._page_handlers):
            try:
                callback(page)
            except Exception as exc:
                log(f"page handler failed: {exc}")
        return page

    def _on_target_created(self, params):
        info = params.get("targetInfo", {})
        if info.get("type") != "page":
            return
        if info.get("targetId") in self._by_target:
            return
        asyncio.ensure_future(self._adopt_target(info["targetId"]))

    async def _adopt_target(self, target_id):
        try:
            await self._attach(target_id)
        except Exception as exc:
            log(f"could not adopt new tab: {exc}")

    def _on_target_destroyed(self, params):
        page = self._by_target.pop(params.get("targetId"), None)
        if page is not None:
            page._fire_closed()

    def _on_target_info_changed(self, params):
        info = params.get("targetInfo", {})
        page = self._by_target.get(info.get("targetId"))
        if page is None:
            return
        if info.get("title"):
            page._title = info["title"]
        if info.get("url"):
            page._url = info["url"]

    def _on_detached_from_target(self, params):
        page = self._conn._sessions.get(params.get("sessionId"))
        if isinstance(page, Page):
            page._fire_closed()

    # -- Playwright-compatible surface -------------------------------

    async def new_page(self):
        created = await self.send("Target.createTarget",
                                  {"url": "about:blank"})
        target_id = created.get("targetId")
        if not target_id:
            raise CDPError("Target.createTarget returned no target")
        return await self._attach(target_id)

    def on(self, event, callback):
        if event == "page":
            self._page_handlers.append(callback)
        elif event == "download":
            self._download_handlers.append(callback)
        else:
            super().on(event, callback)

    async def cookies(self):
        result = await self.send("Storage.getCookies")
        return [
            {"name": c.get("name", ""), "domain": c.get("domain", ""),
             "path": c.get("path", "/"), "secure": c.get("secure", False),
             "httpOnly": c.get("httpOnly", False)}
            for c in result.get("cookies", [])
        ]

    async def add_init_script(self, script):
        self.init_scripts.append(script)
        for page in self.pages:
            await page.add_init_script(script)

    async def route(self, pattern, handler):
        """Intercept requests through Fetch.enable, one handler for all."""
        self._routes.append((pattern, handler))
        for page in self.pages:
            await page.enable_routing()

    async def new_cdp_session(self, page):
        """Playwright hands out a second CDP session here; the page is one."""
        return page

    async def close(self):
        try:
            await asyncio.wait_for(self.send("Browser.close"), timeout=10)
        except Exception:
            pass
        await self._conn.close()

    # -- downloads ---------------------------------------------------

    def _on_download_will_begin(self, params):
        download = Download(self, params.get("guid", ""),
                            params.get("suggestedFilename", ""))
        self._downloads[download.guid] = download
        waiter = self._download_waiter
        if waiter is not None and not waiter.done():
            waiter.set_result(download)
            self._download_waiter = None
        for callback in list(self._download_handlers):
            asyncio.ensure_future(self._call_download_handler(callback,
                                                              download))

    async def _call_download_handler(self, callback, download):
        try:
            await callback(download)
        except Exception as exc:
            log(f"download handler failed: {exc}")

    def _on_download_progress(self, params):
        download = self._downloads.get(params.get("guid", ""))
        if download is None:
            return
        download._finish(params.get("state", ""))

    def expect_download(self, timeout=30000):
        browser = self

        class _Expect:
            info = None

            async def __aenter__(self):
                info = DownloadInfo()
                browser._download_waiter = info.value
                self.info = info
                self.expiry = asyncio.get_running_loop().call_later(
                    timeout / 1000.0, self._expire, info)
                return info

            def _expire(self, info):
                if not info.value.done():
                    info.value.set_exception(
                        CDPError("download did not start in time"))
                if browser._download_waiter is info.value:
                    browser._download_waiter = None

            async def __aexit__(self, *exc_info):
                self.expiry.cancel()
                # If the block raised before a download began, drop the
                # waiter so a later download does not resolve it.
                if (self.info is not None
                        and not self.info.value.done()):
                    if browser._download_waiter is self.info.value:
                        browser._download_waiter = None
                    self.info.value.cancel()
                return False

        return _Expect()


# ── Page (Playwright's page) ─────────────────────────────────────────

_FUNC_KW_RE = re.compile(r"^\s*(?:async\s+)?function\b")
_ARROW_RE = re.compile(r"^[A-Za-z_$][\w$]*\s*=>")


def _is_function_expression(expression):
    """True when EXPRESSION evaluates to a function to be called.

    A leading paren is not enough: `(() => x)()' also starts with one and
    is already invoked, and wrapping it again yields "not a function".
    So match the parens and look for the arrow after the matching one.
    """
    source = expression.strip()
    if _FUNC_KW_RE.match(source):
        return True
    if source.startswith("("):
        depth = 0
        for index, char in enumerate(source):
            if char == "(":
                depth += 1
            elif char == ")":
                depth -= 1
                if depth == 0:
                    return source[index + 1:].lstrip().startswith("=>")
        return False
    return bool(_ARROW_RE.match(source))

_KEYS = {
    "Enter": ("Enter", 13, "\r"),
    "Tab": ("Tab", 9, "\t"),
    "Backspace": ("Backspace", 8, None),
    "Delete": ("Delete", 46, None),
    "Home": ("Home", 36, None),
    "End": ("End", 35, None),
    "ArrowLeft": ("ArrowLeft", 37, None),
    "ArrowUp": ("ArrowUp", 38, None),
    "ArrowRight": ("ArrowRight", 39, None),
    "ArrowDown": ("ArrowDown", 40, None),
    "PageUp": ("PageUp", 33, None),
    "PageDown": ("PageDown", 34, None),
    "Escape": ("Escape", 27, None),
    "F5": ("F5", 116, None),
    " ": (" ", 32, " "),
}
_MODIFIERS = {"Alt": 1, "Control": 2, "Meta": 4, "Shift": 8}


class Mouse:
    def __init__(self, page):
        self._page = page

    async def move(self, x, y):
        self._page._mouse = (float(x), float(y))
        await self._page.send("Input.dispatchMouseEvent", {
            "type": "mouseMoved", "x": float(x), "y": float(y),
            "button": "none", "buttons": 0,
        })

    async def down(self, x=None, y=None):
        if x is not None:
            self._page._mouse = (float(x), float(y))
        await self._page.send("Input.dispatchMouseEvent", {
            "type": "mousePressed",
            "x": self._page._mouse[0],
            "y": self._page._mouse[1],
            "button": "left", "buttons": 1, "clickCount": 1,
        })

    async def up(self, x=None, y=None):
        if x is not None:
            self._page._mouse = (float(x), float(y))
        await self._page.send("Input.dispatchMouseEvent", {
            "type": "mouseReleased",
            "x": self._page._mouse[0],
            "y": self._page._mouse[1],
            "button": "left", "buttons": 0, "clickCount": 1,
        })


class Keyboard:
    def __init__(self, page):
        self._page = page

    async def type(self, text):
        """Insert TEXT as if typed; dispatchKeyEvent per char would too."""
        await self._page.send("Input.insertText", {"text": text})

    async def press(self, key):
        """Press a Playwright-style key name: "Enter", "Shift+Tab", ..."""
        modifiers = 0
        base = key
        if "+" in key:
            parts = key.split("+")
            base = parts[-1]
            for modifier in parts[:-1]:
                modifiers |= _MODIFIERS.get(modifier, 0)
        name, vk, text = _KEYS.get(base, (base, 0, None))
        if base not in _KEYS:
            log(f"unknown key {base!r}; sending it with no key code")
        common = {
            "key": name,
            "code": "Space" if name == " " else (name if len(name) > 1 else ""),
            "windowsVirtualKeyCode": vk,
            "nativeVirtualKeyCode": vk,
            "modifiers": modifiers,
        }
        down = dict(common, type="keyDown")
        # Text is only what the key types on its own; a held modifier
        # (Control+ArrowRight, Shift+Tab) types nothing.
        if text and not modifiers:
            down["text"] = text
        await self._page.send("Input.dispatchKeyEvent", down)
        up = dict(common, type="keyUp")
        await self._page.send("Input.dispatchKeyEvent", up)


class Route:
    """The handful of Playwright route methods embr's blocklist needs."""

    def __init__(self, page, params):
        self._page = page
        self._request_id = params.get("requestId")
        self.request = _RouteRequest(params.get("request", {}))
        self._settled = False

    async def abort(self):
        if self._settled:
            return
        self._settled = True
        try:
            await self._page.send("Fetch.failRequest", {
                "requestId": self._request_id, "errorReason": "BlockedByClient",
            })
        except CDPError:
            pass

    async def continue_(self):
        if self._settled:
            return
        self._settled = True
        try:
            await self._page.send("Fetch.continueRequest",
                                  {"requestId": self._request_id})
        except CDPError:
            pass


class _RouteRequest:
    def __init__(self, params):
        self.url = params.get("url", "")
        self.method = params.get("method", "GET")
        self.resource_type = params.get("resourceType", "")
        self.headers = params.get("headers", {})


class Page(_Session):
    def __init__(self, browser, conn, session_id, target_id):
        super().__init__(conn, session_id)
        self.browser = browser
        self.target_id = target_id
        self._url = "about:blank"
        self._title = ""
        self._loader_id = ""
        self._viewport = dict(browser.viewport)
        self._closed = False
        self._mouse = (0.0, 0.0)
        self._routing_enabled = False
        self.mouse = Mouse(self)
        self.keyboard = Keyboard(self)

    # -- wiring ------------------------------------------------------

    async def setup(self):
        await self.send("Page.enable")
        await self.send("Inspector.enable")
        await self.send("Emulation.setFocusEmulationEnabled", {"enabled": True})
        await self._apply_viewport()
        await self._apply_color_scheme()
        for script in self.browser.init_scripts:
            await self.add_init_script(script)
        if self.browser._routes:
            await self.enable_routing()

    async def enable_routing(self):
        if self._routing_enabled:
            return
        await self.send("Fetch.enable", {"patterns": [
            {"urlPattern": "*", "requestStage": "Request"}]})
        self._routing_enabled = True

    async def add_init_script(self, script):
        # Chromium wants source that runs on its own; embr's caret and
        # link-preview bodies are function bodies (they use `return'), so
        # wrap them the way Playwright does.
        wrapped = "(() => {\n" + script + "\n})()"
        await self.send("Page.addScriptToEvaluateOnNewDocument",
                        {"source": wrapped})
        # Also run it in the document that is already there.
        try:
            await self.evaluate(wrapped)
        except CDPError:
            pass

    async def _apply_viewport(self):
        await self.send("Emulation.setDeviceMetricsOverride", {
            "width": int(self._viewport["width"]),
            "height": int(self._viewport["height"]),
            "deviceScaleFactor": 1,
            "mobile": False,
            "screenWidth": int(self.browser.screen.get("width") or 1920),
            "screenHeight": int(self.browser.screen.get("height") or 1080),
            "screenOrientation": {"type": "portraitPrimary", "angle": 0},
        })

    async def _apply_color_scheme(self):
        scheme = self.browser.color_scheme
        if not scheme:
            return
        await self.send("Emulation.setEmulatedMedia", {"features": [
            {"name": "prefers-color-scheme", "value": scheme}]})

    # -- state -------------------------------------------------------

    @property
    def context(self):
        """Playwright-compat: page.context is the browser/context object."""
        return self.browser

    @property
    def url(self):
        return self._url

    @property
    def viewport_size(self):
        return self._viewport

    def is_closed(self):
        return self._closed

    def _fire_closed(self):
        if self._closed:
            return
        self._closed = True
        if self in self.browser.pages:
            self.browser.pages.remove(self)
        self.browser._by_target.pop(self.target_id, None)
        for callback in list(self._handlers.get("close", ())):
            try:
                callback(self)
            except Exception as exc:
                log(f"close handler failed: {exc}")

    def _dispatch(self, method, params):
        # Keep the cheap state embr reads synchronously up to date before
        # the daemon's own handlers see the event.
        if method == "Page.frameNavigated":
            frame = params.get("frame", {})
            if not frame.get("parentId"):
                self._url = frame.get("url") or self._url
                self._loader_id = frame.get("loaderId") or self._loader_id
        elif method == "Page.navigatedWithinDocument":
            self._url = params.get("url") or self._url
        elif method == "Page.domContentEventFired":
            method = "domcontentloaded"
        elif method == "Inspector.targetCrashed":
            method = "crash"
        elif method == "Page.javascriptDialogOpening":
            asyncio.ensure_future(self._dismiss_dialog(params))
            return
        elif method == "Fetch.requestPaused":
            self._on_request_paused(params)
            return
        super()._dispatch(method, params)

    async def _dismiss_dialog(self, params):
        try:
            await self.send("Page.handleJavaScriptDialog", {"accept": True})
        except CDPError:
            pass

    def _on_request_paused(self, params):
        route = Route(self, params)
        for _pattern, handler in self.browser._routes:
            asyncio.ensure_future(self._run_route(handler, route))

    async def _run_route(self, handler, route):
        try:
            await handler(route)
        except Exception as exc:
            log(f"route handler failed: {exc}")
        finally:
            if not route._settled:
                await route.continue_()

    # -- evaluation --------------------------------------------------

    async def evaluate(self, expression, arg=_MISSING):
        """Evaluate a JS expression or function, Playwright style."""
        if _is_function_expression(expression):
            if arg is _MISSING:
                source = f"({expression})()"
            else:
                source = f"({expression})({json.dumps(arg)})"
        else:
            source = expression
        result = await self.send("Runtime.evaluate", {
            "expression": source,
            "returnByValue": True,
            "awaitPromise": True,
            "userGesture": True,
            "allowUnsafeEvalBlockedByCSP": True,
        })
        if result.get("exceptionDetails"):
            details = result["exceptionDetails"]
            message = (details.get("exception", {}).get("description")
                       or details.get("text") or "evaluation failed")
            raise CDPError(message)
        return result.get("result", {}).get("value")

    async def title(self):
        try:
            return await self.evaluate("document.title") or ""
        except CDPError:
            return self._title

    # -- navigation --------------------------------------------------

    async def goto(self, url, wait_until="load", timeout=30000):
        result = await self.send("Page.navigate", {"url": url})
        if result.get("errorText"):
            raise CDPError(f"{result['errorText']}: {url}")
        return await self._wait_ready(result.get("loaderId"), timeout)

    async def reload(self, wait_until="load", timeout=30000):
        await self.send("Page.reload", {"ignoreCache": False})
        return await self._wait_ready(None, timeout)

    async def go_back(self, wait_until="load", timeout=30000):
        return await self._history_step(-1, timeout)

    async def go_forward(self, wait_until="load", timeout=30000):
        return await self._history_step(1, timeout)

    async def _history_step(self, delta, timeout):
        history = await self.send("Page.getNavigationHistory")
        index = history.get("currentIndex", 0)
        entries = history.get("entries", [])
        target = index + delta
        if target < 0 or target >= len(entries):
            raise CDPError("no history entry in that direction")
        await self.send("Page.navigateToHistoryEntry",
                        {"entryId": entries[target]["id"]})
        await self._wait_ready(None, timeout)

    async def _wait_ready(self, loader_id, timeout):
        """Wait for the new document's DOM content, by polling readyState.

        Polling rather than waiting on Page.domContentEventFired: the
        event can fire between Page.navigate returning and the listener
        being installed, which would hang the daemon.
        """
        deadline = time.monotonic() + timeout / 1000.0
        if loader_id:
            # Wait for the navigation to commit first, else readyState
            # still describes the outgoing document.
            while time.monotonic() < deadline:
                if self._loader_id == loader_id:
                    break
                await asyncio.sleep(0.01)
        while time.monotonic() < deadline:
            try:
                state = await self.evaluate("document.readyState")
            except CDPError:
                await asyncio.sleep(0.02)
                continue
            if state in ("interactive", "complete"):
                return
            await asyncio.sleep(0.02)
        raise CDPError("timed out waiting for the page to load")

    async def close(self):
        """Close this tab (Target.closeTarget) and wait for it to vanish."""
        if self._closed:
            return
        try:
            await self.send("Target.closeTarget", {"targetId": self.target_id})
        except CDPError as exc:
            log(f"close tab failed: {exc}")
        deadline = time.monotonic() + 5
        while not self._closed and time.monotonic() < deadline:
            await asyncio.sleep(0.01)
        self._fire_closed()

    async def bring_to_front(self):
        try:
            await self.send("Target.activateTarget", {"targetId": self.target_id})
        except CDPError:
            pass

    async def set_viewport_size(self, viewport):
        self._viewport = {"width": int(viewport["width"]),
                          "height": int(viewport["height"])}
        await self._apply_viewport()

    # -- content -----------------------------------------------------

    async def content(self):
        return await self.evaluate("document.documentElement.outerHTML")

    async def inner_text(self, selector="body"):
        return await self.evaluate(
            "document.body ? document.body.innerText : ''")

    async def fill(self, selector, value):
        return await self.evaluate("""(sel, val) => {
            const el = document.querySelector(sel);
            if (!el) return false;
            const proto = (el instanceof HTMLTextAreaElement)
                ? HTMLTextAreaElement.prototype
                : HTMLInputElement.prototype;
            const setter = Object.getOwnPropertyDescriptor(proto, 'value')
                && Object.getOwnPropertyDescriptor(proto, 'value').set;
            el.focus();
            if (setter) setter.call(el, val); else el.value = val;
            el.dispatchEvent(new Event('input', {bubbles: true}));
            el.dispatchEvent(new Event('change', {bubbles: true}));
            return true;
        }""", [selector, value])

    async def select_option(self, selector, value):
        return await self.evaluate("""(sel, val) => {
            const el = document.querySelector(sel);
            if (!el) return false;
            el.value = val;
            if (el.value !== val) {
                const opt = Array.from(el.options || [])
                    .find(o => o.label === val || o.text === val);
                if (opt) el.value = opt.value;
            }
            el.dispatchEvent(new Event('input', {bubbles: true}));
            el.dispatchEvent(new Event('change', {bubbles: true}));
            return true;
        }""", [selector, value])

    # -- pixels ------------------------------------------------------

    async def screenshot(self, path=None, type="png", quality=None,
                        full_page=False):
        params = {"format": "jpeg" if type == "jpeg" else "png"}
        if quality is not None:
            params["quality"] = int(quality)
        if full_page:
            metrics = await self.send("Page.getLayoutMetrics")
            size = metrics.get("cssContentSize") or metrics.get("contentSize")
            params["captureBeyondViewport"] = True
            if size:
                params["clip"] = {"x": 0, "y": 0,
                                  "width": size["width"],
                                  "height": size["height"],
                                  "scale": 1}
        result = await self.send("Page.captureScreenshot", params)
        data = base64.b64decode(result.get("data", ""))
        if path:
            with open(path, "wb") as handle:
                handle.write(data)
            return None
        return data

    async def pdf(self):
        result = await self.send("Page.printToPDF", {
            "printBackground": True,
            "preferCSSPageSize": True,
        })
        return base64.b64decode(result.get("data", ""))
