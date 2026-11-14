"""Regenerate the README screenshots in man/figures.

Run from the package root:

    uv run --no-project --with playwright python tools/screenshots.py

It serves an example file with the working copy of the package (through
pkgload), opens the page in the installed Google Chrome (no browser download
needed), and writes livecode.png and follow.png. The selection, the
follower's route, and the progress ring are set on the page directly, so no
editor or tailcat broadcast is involved.
"""

import random
import socket
import subprocess
import sys
import tempfile
import time
import urllib.request
from pathlib import Path

from playwright.sync_api import sync_playwright

ROOT = Path(__file__).resolve().parent.parent
OUT = ROOT / "man" / "figures"

EXAMPLE = """\
library(dplyr)
library(nycflights13)

# How many flights to Los Angeles (LAX) did each carrier
# have in May from JFK, and what was the average duration?

flights |>
  filter(dest == "LAX") |>
  filter(carrier %in% c("AA", "UA", "DL", "US")) |>
  filter(month == 5) |>
  filter(origin == "JFK") |>
  group_by(carrier) |>
  summarize(n = n(), avg_dur = mean(air_time, na.rm = TRUE))
"""

# name, viewport width and height (CSS pixels, captured at 2x), route shown
# in the status bar (None for the instructor's page).
SHOTS = [
    ("livecode.png", 784, 392, None),
    ("follow.png", 640, 400, {"kind": "direct", "relay": ""}),
]

# Waits for the first update, then sets the selection and route and holds the
# ring partway through filling with its dot at rest.
PREPARE = """async (route) => {
  const pb = document.getElementById('progressbar');
  for (let i = 0; i < 100 && !pb.hasAttribute('data-active'); i++)
    await new Promise(r => setTimeout(r, 50));
  window.livecode.handleFrame(JSON.stringify({selection: '8-11', route: route}));
  const arc = document.querySelector('.livecode-progress-arc').getAnimations()[0];
  arc.pause();
  arc.currentTime = arc.effect.getTiming().duration * 0.62;
  document.querySelector('.livecode-progress-dot').getAnimations().forEach(a => a.finish());
}"""


# livecode accepts ports 1024 to 49151 only, below the OS's ephemeral range.
def free_port():
    for port in random.sample(range(20000, 49151), 50):
        with socket.socket() as s:
            try:
                s.bind(("127.0.0.1", port))
                return port
            except OSError:
                continue
    sys.exit("No free port found.")


def wait_for(url, timeout=60):
    deadline = time.time() + timeout
    while time.time() < deadline:
        try:
            urllib.request.urlopen(url, timeout=1)
            return
        except OSError:
            time.sleep(0.5)
    sys.exit(f"The server at {url} did not start.")


def main():
    port = free_port()
    url = f"http://127.0.0.1:{port}/"

    with tempfile.TemporaryDirectory() as tmp:
        example = Path(tmp) / "example.R"
        example.write_text(EXAMPLE)
        # A long interval, so no update restarts the ring while it is held.
        code = (
            "options(livecode.editor = 'none'); "
            "pkgload::load_all(quiet = TRUE); "
            f"livecode::serve_file('{example}', ip = '127.0.0.1', port = {port}, "
            "open_browser = FALSE, block = TRUE, interval = 60)"
        )
        server = subprocess.Popen(
            ["Rscript", "-e", code], cwd=ROOT,
            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
        )
        try:
            wait_for(url)
            with sync_playwright() as p:
                browser = p.chromium.launch(channel="chrome")
                for name, width, height, route in SHOTS:
                    page = browser.new_page(
                        viewport={"width": width, "height": height},
                        device_scale_factor=2,
                    )
                    page.goto(url)
                    page.evaluate(PREPARE, route)
                    page.wait_for_timeout(300)
                    page.screenshot(path=OUT / name)
                    page.close()
                    print(f"Wrote {OUT / name}")
                browser.close()
        finally:
            server.terminate()
            server.wait(timeout=10)


if __name__ == "__main__":
    main()
