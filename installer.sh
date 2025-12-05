#!/bin/bash
set -e

BASE="$HOME/.wapl"
BIN="$BASE/bin"
VERSIONS="$BASE/versions"

REPO_USER="kazanefu"
REPO_NAME="WapL_Compiler"

mkdir -p "$BIN" "$VERSIONS"

echo "Installing WapL toolchain..."

####################################
# 1. waplup
####################################
cat << 'EOF' > "$BIN/waplup"
#!/bin/bash
set -e

BASE="$HOME/.wapl"
VERSIONS="$BASE/versions"
CURRENT="$BASE/current"
BIN="$BASE/bin"

REPO_USER="kazanefu"
REPO_NAME="WapL_Compiler"

cmd=$1
shift

# GitHub API を使って最新バージョン取得
latest_version() {
    curl -s "https://api.github.com/repos/$REPO_USER/$REPO_NAME/releases/latest" \
        | grep '"tag_name":' \
        | sed -E 's/.*"tag_name": "waplc_v([^"]+)".*/\1/'
}

case "$cmd" in
  install)
    VERSION=$1
    FORCE=0

    # 例: install latest
    if [[ "$VERSION" == "latest" || -z "$VERSION" ]]; then
        VERSION=$(latest_version)
        echo "Resolved latest version: $VERSION"
    fi

    # --force をチェック
    if [[ "$2" == "--force" ]]; then
        FORCE=1
    fi

    if [[ -e "$VERSIONS/$VERSION" && $FORCE -eq 0 ]]; then
        echo "Version $VERSION is already installed. Use --force to reinstall."
        exit 1
    fi

    # 上書き時は削除
    if [[ -e "$VERSIONS/$VERSION" && $FORCE -eq 1 ]]; then
        echo "Reinstalling version $VERSION..."
        rm -rf "$VERSIONS/$VERSION"
    fi

    mkdir -p "$VERSIONS/$VERSION"

    URL="https://github.com/$REPO_USER/$REPO_NAME/releases/download/waplc_v$VERSION/waplc"

    echo "Downloading waplc $VERSION..."
    if ! curl -L "$URL" -o "$VERSIONS/$VERSION/waplc"; then
        echo "Download failed. Cleaning up..."
        rm -rf "$VERSIONS/$VERSION"
        exit 1
    fi

    chmod +x "$VERSIONS/$VERSION/waplc"
    ln -sf "$VERSIONS/$VERSION/waplc" "$BIN/waplc"
    ln -sf "$VERSIONS/$VERSION" "$CURRENT"

    echo "Installed version $VERSION"
    ;;


  uninstall)
    VERSION=$1
    if [[ "$VERSION" == "all" ]]; then
      echo "Removing entire WapL toolchain at $BASE ..."
      rm -rf "$BASE"
      echo "All versions uninstalled."
      exit 0
    fi

    # default のバージョンと同じなら解除
    if [[ "$(readlink -f "$CURRENT")" == "$(readlink -f "$VERSIONS/$VERSION")" ]]; then
        echo "Removing default version. Default will be unset."
        rm -f "$CURRENT"
        rm -f "$BIN/waplc"
    fi

    rm -rf "$VERSIONS/$VERSION"
    echo "Uninstalled version $VERSION"
    ;;

  list)
    echo "Installed versions:"
    ls "$VERSIONS"
    ;;

  default)
    VERSION=$1

    # 存在チェック
    if [[ ! -e "$VERSIONS/$VERSION" ]]; then
        echo "Version $VERSION is not installed."
        exit 1
    fi

    if [[ ! -f "$VERSIONS/$VERSION/waplc" ]]; then
        echo "Compiler binary missing for $VERSION."
        exit 1
    fi

    ln -sf "$VERSIONS/$VERSION" "$CURRENT"
    ln -sf "$VERSIONS/$VERSION/waplc" "$BIN/waplc"

    echo "Default compiler set to $VERSION"
    ;;

  update)
    LATEST=$(latest_version)
    echo "Latest version: $LATEST"
    "$BIN/waplup" install "$LATEST"
    "$BIN/waplup" default "$LATEST"
    ;;

  show)
    if [ ! -e "$CURRENT" ]; then
      echo "No default version set."
      exit 1
    fi
    echo -n "Current version: "
    basename "$(readlink -f "$CURRENT")"
    ;;

  which)
    readlink -f "$BIN/waplc"
    ;;

  *)
    echo "waplup commands:"
    echo "  install <version>      Install specific WapL compiler version"
    echo "  install <version> [--force]  Install specific version"
    echo "  install latest               Install latest version"
    echo "  uninstall <version>    Remove a specific version"
    echo "  uninstall all          Remove entire WapL toolchain"
    echo "  list                   Show installed versions"
    echo "  default <version>      Set default version"
    echo "  update                 Install latest version"
    echo "  show                   Show current version"
    echo "  which                  Show full path of current compiler"
    ;;
esac
EOF

chmod +x "$BIN/waplup"


####################################
# 2. wapl-cli
####################################
cat << 'EOF' > "$BIN/wapl-cli"
#!/bin/bash

cmd=$1
shift

case "$cmd" in
  new)
    NAME=$1
    mkdir -p "$NAME/src"
    mkdir -p "$NAME/target"
    echo 'print("Hello, WapL!");' > "$NAME/src/main.wapl"
    echo "Created new WapL project: $NAME"
    ;;

  build)
    SRC="./src/main.wapl"
    OUT="./target/main"
    mkdir -p "./target"
    "$HOME/.wapl/bin/waplc" -i "$SRC" -o "$OUT" -O O2
    echo "Build complete: $OUT"
    ;;

  release)
    SRC="./src/main.wapl"
    OUT="./target/main"
    mkdir -p "./target"
    "$HOME/.wapl/bin/waplc" -i "$SRC" -o "$OUT" -O O3
    echo "Build complete: $OUT"
    ;;

  run)
    "$0" build
    ./target/main
    ;;


  *)
    echo "wapl-cli commands:"
    echo "  new <name>"
    echo "  build"
    echo "  run"
    ;;
esac
EOF

chmod +x "$BIN/wapl-cli"


####################################
# 3. PATH設定
####################################
if ! grep -q ".wapl/bin" "$HOME/.bashrc"; then
  echo 'export PATH="$HOME/.wapl/bin:$PATH"' >> "$HOME/.bashrc"
  echo "PATH added to ~/.bashrc"
fi

echo "Installation complete!"
echo "Please reload your shell: source ~/.bashrc"
echo "Example: waplup install latest"
echo "Require: if you don't have Clang installed yet, install it."
