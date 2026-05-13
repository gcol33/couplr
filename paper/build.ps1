[CmdletBinding()]
param(
    [switch]$SkipFigure,
    [switch]$SkipPdf
)

$ErrorActionPreference = 'Stop'
$PaperDir = Split-Path -Parent $MyInvocation.MyCommand.Path
Set-Location $PaperDir

# ---------- step 1: figure -------------------------------------------------

if (-not $SkipFigure) {
    $Rscript = Get-ChildItem 'C:\Program Files\R\R-*\bin\Rscript.exe' |
               Sort-Object FullName |
               Select-Object -Last 1
    if (-not $Rscript) { throw "No Rscript.exe found under 'C:\Program Files\R'." }

    Write-Host "[1/2] Regenerating figure via $($Rscript.FullName)..." -ForegroundColor Cyan
    & $Rscript.FullName "$PaperDir\make-figure.R"
    if ($LASTEXITCODE -ne 0) { throw "make-figure.R exited with code $LASTEXITCODE." }
} else {
    Write-Host "[1/2] Skipping figure (--SkipFigure)." -ForegroundColor DarkGray
}

# ---------- step 2: pdf via inara ------------------------------------------

if (-not $SkipPdf) {
    try { docker info --format '{{.ServerVersion}}' *> $null } catch {}
    if ($LASTEXITCODE -ne 0) {
        Write-Host "Docker daemon not reachable. Launching Docker Desktop..." -ForegroundColor Yellow
        $dd = 'C:\Program Files\Docker\Docker\Docker Desktop.exe'
        if (-not (Test-Path $dd)) { throw "Docker Desktop not installed at $dd." }
        Start-Process $dd
        $ready = $false
        foreach ($i in 1..18) {
            Start-Sleep -Seconds 5
            docker info --format '{{.ServerVersion}}' *> $null
            if ($LASTEXITCODE -eq 0) { $ready = $true; break }
        }
        if (-not $ready) { throw "Docker daemon did not become ready within 90s." }
    }

    Write-Host "[2/2] Building PDF via openjournals/inara..." -ForegroundColor Cyan
    # docker writes [INFO] lines to stderr; PS 5.1 with ErrorActionPreference=Stop
    # would otherwise wrap each as a NativeCommandError and abort the script.
    $prevPref = $ErrorActionPreference
    $ErrorActionPreference = 'Continue'
    try {
        & cmd.exe /c "docker run --rm --volume `"${PaperDir}:/data`" --env JOURNAL=joss openjournals/inara:latest -o pdf paper.md 2>&1"
        $rc = $LASTEXITCODE
    } finally {
        $ErrorActionPreference = $prevPref
    }
    if ($rc -ne 0) { throw "inara exited with code $rc." }

    $pdf = Join-Path $PaperDir 'paper.pdf'
    if (Test-Path $pdf) {
        $size = [math]::Round((Get-Item $pdf).Length / 1KB, 1)
        Write-Host "Wrote paper.pdf ($size KB)." -ForegroundColor Green
    }
} else {
    Write-Host "[2/2] Skipping PDF (--SkipPdf)." -ForegroundColor DarkGray
}
