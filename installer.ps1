# --------------------------------------------
# WapL Windows Installer (No setx)
# --------------------------------------------
$BASE      = "$env:USERPROFILE\.wapl"
$BIN       = "$BASE\bin"
$VERSIONS  = "$BASE\versions"
$CURRENT   = "$BASE\current"

$REPO_USER = "kazanefu"
$REPO_NAME = "WapL_Compiler"

Write-Host "Installing WapL toolchain for Windows..."

New-Item -ItemType Directory -Force -Path $BIN | Out-Null
New-Item -ItemType Directory -Force -Path $VERSIONS | Out-Null


# --------------------------------------------
# write waplup
# --------------------------------------------
$waplup = @'
param(
    [string]$cmd,
    [string]$arg1,
    [string]$arg2
)

$BASE     = "$env:USERPROFILE\.wapl"
$VERSIONS = "$BASE\versions"
$CURRENT  = "$BASE\current"
$BIN      = "$BASE\bin"

$REPO_USER = "kazanefu"
$REPO_NAME = "WapL_Compiler"

function Latest-Version {
    $url = "https://api.github.com/repos/$REPO_USER/$REPO_NAME/releases/latest"
    $json = Invoke-RestMethod $url -UseBasicParsing
    return ($json.tag_name -replace "waplc_v","")
}

switch ($cmd) {

"install" {
    $VERSION = $arg1
    $FORCE = $arg2 -eq "--force"

    if ($VERSION -eq "" -or $VERSION -eq "latest") {
        $VERSION = Latest-Version
        Write-Host "Resolved latest version: $VERSION"
    }

    $dest = "$VERSIONS\$VERSION"

    if ((Test-Path $dest) -and -not $FORCE) {
        Write-Host "Version $VERSION is already installed. Use --force to reinstall."
        exit
    }

    if ((Test-Path $dest) -and $FORCE) {
        Remove-Item -Recurse -Force $dest
    }

    New-Item -ItemType Directory -Force -Path $dest | Out-Null

    $url = "https://github.com/$REPO_USER/$REPO_NAME/releases/download/waplc_v$VERSION/waplc.exe"
    $outFile = "$dest\waplc.exe"

    Write-Host "Downloading waplc.exe $VERSION..."
    try {
        Invoke-WebRequest -Uri $url -OutFile $outFile -UseBasicParsing
    } catch {
        Write-Host "Download failed."
        Remove-Item -Recurse -Force $dest
        exit
    }

    Copy-Item $outFile "$BIN\waplc.exe" -Force

    if (Test-Path $CURRENT) { Remove-Item $CURRENT -Force }
    New-Item -ItemType SymbolicLink -Path $CURRENT -Target $dest | Out-Null

    Write-Host "Installed version $VERSION"
}

"uninstall" {
    $VERSION = $arg1

    if ($VERSION -eq "all") {
        Write-Host "Removing all WapL toolchain..."
        Remove-Item -Recurse -Force $BASE
        exit
    }

    $target = "$VERSIONS\$VERSION"

    if ((Test-Path $CURRENT) -and ((Get-Item $CURRENT).Target -eq $target)) {
        Remove-Item $CURRENT -Force
        Remove-Item "$BIN\waplc.exe" -Force
    }

    Remove-Item -Recurse -Force $target
    Write-Host "Uninstalled version $VERSION"
}

"list" {
    Get-ChildItem $VERSIONS | Select-Object Name
}

"default" {
    $VERSION = $arg1
    $target = "$VERSIONS\$VERSION"

    if (-not (Test-Path $target)) {
        Write-Host "Version $VERSION is not installed."
        exit
    }
    if (-not (Test-Path "$target\waplc.exe")) {
        Write-Host "Compiler missing."
        exit
    }

    if (Test-Path $CURRENT) { Remove-Item $CURRENT -Force }
    New-Item -ItemType SymbolicLink -Path $CURRENT -Target $target | Out-Null
    Copy-Item "$target\waplc.exe" "$BIN\waplc.exe" -Force

    Write-Host "Default version set to $VERSION"
}

"update" {
    $LATEST = Latest-Version
    Write-Host "Latest: $LATEST"
    & "$BIN\waplup.ps1" install $LATEST
    & "$BIN\waplup.ps1" default $LATEST
}

"show" {
    if (-not (Test-Path $CURRENT)) {
        Write-Host "No default version set."
        exit
    }
    Write-Host ("Current version: " + (Split-Path (Get-Item $CURRENT).Target -Leaf))
}

"which" {
    Write-Host "$BIN\waplc.exe"
}

default {
    Write-Host "waplup commands:"
    Write-Host "  install <version>"
    Write-Host "  install latest"
    Write-Host "  uninstall <version>"
    Write-Host "  uninstall all"
    Write-Host "  list"
    Write-Host "  default <version>"
    Write-Host "  update"
    Write-Host "  show"
    Write-Host "  which"
}

}
'@

Set-Content -Path "$BIN\waplup.ps1" -Value $waplup -Encoding UTF8


# --------------------------------------------
# write wapl-cli
# --------------------------------------------
$waplcli = @'
param([string]$cmd, [string]$arg1)

$BASE    = "$env:USERPROFILE\.wapl"
$CURRENT = "$BASE\current"

function Get-Default-Version {
    if (-not (Test-Path $CURRENT)) { return "" }
    return (Split-Path (Get-Item $CURRENT).Target -Leaf)
}

switch ($cmd) {

"new" {
    $NAME = $arg1
    New-Item -ItemType Directory -Force -Path "$NAME\src" | Out-Null
    New-Item -ItemType Directory -Force -Path "$NAME\target" | Out-Null

    Set-Content "$NAME\src\main.wapl" 'fn main():i64{ println("Hello, WapL!"); return 0; } main();'

    Write-Host "Created project $NAME"

    $VERSION = Get-Default-Version
    if ($VERSION -eq "") {
        Write-Host "No default version. std NOT installed."
        exit
    }

    $url = "https://github.com/kazanefu/WapL_Compiler/releases/download/waplc_v$VERSION/std.tar.gz"
    $tar = "$NAME\std.tar.gz"

    Write-Host "Downloading std library..."
    try {
        Invoke-WebRequest $url -OutFile $tar -UseBasicParsing
        tar -xzf $tar -C $NAME
        Remove-Item $tar -Force
        Write-Host "std installed."
    } catch {
        Write-Host "[WARN] failed to download std."
    }
}

"build" {
    & "$BASE\bin\waplc.exe" -i ".\src\main.wapl" -o ".\target\main.exe" -O O2
    Write-Host "Build complete."
}

"release" {
    & "$BASE\bin\waplc.exe" -i ".\src\main.wapl" -o ".\target\main.exe" -O O3
    Write-Host "Release build complete."
}

"run" {
    & "$PSCommandPath" build
    & ".\target\main.exe"
}

default {
    Write-Host "wapl-cli commands:"
    Write-Host "  new <name>"
    Write-Host "  build"
    Write-Host "  run"
}

}
'@

Set-Content "$BIN\wapl-cli.ps1" $waplcli -Encoding UTF8


# --------------------------------------------
# Add to PATH via registry (NO setx)
# --------------------------------------------
$path = [Environment]::GetEnvironmentVariable("Path", "User")
if ($path -notlike "*\.wapl\bin*") {
    $newPath = "$path;$env:USERPROFILE\.wapl\bin"
    [Environment]::SetEnvironmentVariable("Path", $newPath, "User")
    Write-Host "PATH updated in registry (User)"
    Write-Host "Restart PowerShell or reboot to apply."
}


Write-Host "`nInstallation complete!"
Write-Host "Try: waplup.ps1 install latest"
