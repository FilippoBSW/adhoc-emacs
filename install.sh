#!/usr/bin/env bash
set -euo pipefail

for cmd in git cargo emacs; do
    if ! command -v "$cmd" &>/dev/null; then
        echo "[error] '$cmd' not found in PATH." >&2
        exit 1
    fi
done

for cmd in rg zoxide; do
    if ! command -v "$cmd" &>/dev/null; then
        echo "[warning] '$cmd' not found in PATH." >&2
    fi
done

EMACS_DIR="$HOME/.emacs.d"
FD_DIR="$EMACS_DIR/opt/fd"
THEMES_DIR="$EMACS_DIR/themes"

mkdir -p "$THEMES_DIR"
if [[ -d "$THEMES_DIR/gruber-material-dark/.git" ]]; then
    git -C "$THEMES_DIR/gruber-material-dark" pull --ff-only
else
    git clone --depth=1 https://github.com/FilippoBSW/gruber-material-dark.git "$THEMES_DIR/gruber-material-dark"
fi

mkdir -p "$FD_DIR"
if [[ -d "$FD_DIR/.git" ]]; then
    git -C "$FD_DIR" pull --ff-only
else
    git clone -b feature/sort_by_depth https://github.com/FilippoBSW/fd.git "$FD_DIR"
fi

pushd "$FD_DIR" >/dev/null
cargo install --path . --force --locked --root "$FD_DIR"
popd >/dev/null

emacs --batch --eval "(progn
 (load-file \"$EMACS_DIR/init.el\")
 (require 'treesit-auto)
 (let ((yes-or-no-p (lambda (&rest _args) t)))
   (treesit-auto-install-all)))"
