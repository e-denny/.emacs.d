#!/bin/bash
# Setup for the forked embr: no venv, no Playwright, no browser download.
# The daemon runs on the system python3 and drives an existing Chromium, so
# this script only fetches the ad blocklist, optional extensions, and can
# build the native canvas module.
set -euo pipefail

DATA_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/embr"
HERE="$(cd "$(dirname "$0")" && pwd)"

MODE="${1:---blocklist}"

mkdir -p "$DATA_DIR"

# The canvas backend needs the C module; it is optional (the JPEG path
# works without it, at a third more input latency).
do_native() {
    make -C "$HERE/native" clean all
}

do_blocklist() {
    BLOCKLIST="$DATA_DIR/blocklist.txt"
    echo "Downloading ad blocklist..."
    curl -fsSL "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts" \
        | grep "^0\.0\.0\.0 " \
        | awk '{print $2}' \
        | sort -u > "$BLOCKLIST.tmp"
    # A truncated download must not replace a good blocklist.
    if [ "$(wc -l < "$BLOCKLIST.tmp")" -lt 1000 ]; then
        rm -f "$BLOCKLIST.tmp"
        echo "ERROR: Blocklist download looks incomplete; keeping the existing one." >&2
        exit 1
    fi
    mv "$BLOCKLIST.tmp" "$BLOCKLIST"
    echo "Blocklist: $(wc -l < "$BLOCKLIST") domains"
}

# install_extension NAME LABEL REPO ASSET-SUFFIX
# Download the latest GitHub release asset of REPO whose name ends in
# ASSET-SUFFIX and unpack it into extensions/NAME, replacing the old
# copy only once the new one has unpacked.
install_extension() {
    local name="$1" label="$2" repo="$3" suffix="$4"
    local dir="$DATA_DIR/extensions/$name"
    local zip="$DATA_DIR/$name.zip"
    local tmp_dir="$dir.tmp"
    local url
    echo "Fetching latest $label release..."
    url=$(curl -fsSL -H "Accept: application/vnd.github+json" \
            "https://api.github.com/repos/$repo/releases/latest" \
        | grep -o "\"browser_download_url\": *\"[^\"]*$suffix\"" \
        | head -1 \
        | cut -d'"' -f4) || true
    if [ -z "$url" ]; then
        echo "ERROR: Could not find the $label release download URL." >&2
        exit 1
    fi
    curl -fsSL -o "$zip" "$url"
    rm -rf "$tmp_dir"
    mkdir -p "$tmp_dir"
    unzip -qo "$zip" -d "$tmp_dir"
    rm -f "$zip"
    rm -rf "$dir"
    mv "$tmp_dir" "$dir"
    echo "$label installed to $dir"
}

case "$MODE" in
    --blocklist)
        do_blocklist
        ;;
    --ublock)
        install_extension ublock "uBlock Origin" gorhill/uBlock 'chromium\.zip'
        ;;
    --darkreader)
        install_extension darkreader "Dark Reader" darkreader/darkreader 'darkreader-chrome\.zip'
        ;;
    --native)
        do_native
        ;;
    *)
        echo "Usage: setup.sh [--blocklist|--ublock|--darkreader|--native]" >&2
        exit 1
        ;;
esac

echo "Done."
