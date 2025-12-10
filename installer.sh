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

    TARGET="$(readlink -f "$VERSIONS/$VERSION")"

    ln -sfn "$TARGET" "$CURRENT"
    ln -sfn "$TARGET/waplc" "$BIN/waplc"

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

BASE="$HOME/.wapl"
CURRENT="$BASE/current"

REPO_USER="kazanefu"
REPO_NAME="WapL_Compiler"

# ===== TOML Reader =====
read_toml() {
    local section=$1
    local key=$2
    local file=$3
    awk -F'=' -v sec="[$section]" -v key="$key" '
        $0 == sec { in=1; next }
        /^\[/ { in=0 }
        in && $1 ~ key {
            gsub(/^[ \t"]+|[ \t"]+$/, "", $2)
            print $2
        }
    ' "$file"
}


current_dir=$(basename "$PWD")

cmd=$1
shift

# 現在のデフォルトバージョンを取得
get_default_version() {
    if [[ ! -e "$CURRENT" ]]; then
        echo ""
        return
    fi
    basename "$(readlink -f "$CURRENT")"
}

case "$cmd" in
  new)
    NAME=$1
    mkdir -p "$NAME/src"
    mkdir -p "$NAME/target"
    echo 'fn main():i32{ println("Hello, WapL!"); return 0s; }' > "$NAME/src/main.wapl"
    echo "Created new WapL project: $NAME"

    VERSION=$(get_default_version)
    if [[ -z "$VERSION" ]]; then
        echo "No default WapL version is set. std library will NOT be downloaded."
        exit 0
    fi
    STD_URL="https://github.com/$REPO_USER/$REPO_NAME/releases/download/waplc_v$VERSION/std.tar.gz"
    STD_TAR="$NAME/std.tar.gz"
    echo "Downloading WapL standard library (version $VERSION)..."
    if curl -L "$STD_URL" -o "$STD_TAR"; then
        mkdir -p "$NAME"
        tar -xzf "$STD_TAR" -C "$NAME"
        rm "$STD_TAR"
        echo "Standard library installed to $NAME/std/"
    else
        echo "[WARN] Failed to download std.tar.gz"
    fi
    cat <<EOF > "$NAME/wapl.toml"
[build]
input = "src/main.wapl"
output = "target/$NAME"
opt = "O2"
clang = "clang"

[release]
input = "src/main.wapl"
output = "target/$NAME"
opt = "O3"
clang = "clang"
    EOF
    ;;

  build)
    TOML="wapl.toml"
    SRC="./src/main.wapl"
    OUT="./target/$current_dir"
    OPT=$(read_toml build opt "$TOML")
    CLANG=$(read_toml build clang "$TOML")
    mkdir -p "./target"
    "$HOME/.wapl/bin/waplc" -i "$SRC" -o "$OUT" -O "$OPT" --clang "$CLANG"
    echo "Build complete: $OUT"
    ;;

  release)
    TOML="wapl.toml"
    SRC="./src/main.wapl"
    OUT="./target/$current_dir"
    OPT=$(read_toml build opt "$TOML")
    CLANG=$(read_toml build clang "$TOML")
    mkdir -p "./target"
    "$HOME/.wapl/bin/waplc" -i "$SRC" -o "$OUT" -O "$OPT" --clang "$CLANG"
    echo "Build complete: $OUT"
    ;;

  run)
    "$0" build
    ./target/$current_dir
    ;;
  std_load)
    VERSION=$(get_default_version)
    if [[ -z "$VERSION" ]]; then
        echo "No default WapL version is set. std library will NOT be downloaded."
        exit 0
    fi
    STD_URL="https://github.com/$REPO_USER/$REPO_NAME/releases/download/waplc_v$VERSION/std.tar.gz"
    STD_TAR="./std.tar.gz"
    echo "Downloading WapL standard library (version $VERSION)..."
    if curl -L "$STD_URL" -o "$STD_TAR"; then
        tar -xzf "$STD_TAR" -C "./"
        rm "$STD_TAR"
        echo "Standard library installed to ./std/"
    else
        echo "[WARN] Failed to download std.tar.gz"
    fi
    ;;

  *)
    echo "wapl-cli commands:"
    echo "  new <name>"
    echo "  build"
    echo "  std_load"
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
echo "Recommendation: if you need syntax highlighting for VScode, visit here https://github.com/kazanefu/WapL_SyntaxHighLight/releases/"
echo "Require: if you don't have Clang installed yet, install it."
