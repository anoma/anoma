param (
    [string] $LibPath,
    [string] $DestinationPath
)

function Copy-Dependencies {
    param (
        [string] $RootPath,
        [string] $DestinationPath
    )
    $excludelist = "C:\Windows\system32;C:\Windows;C:\Windows\System32\Wbem;C:\Windows\System32\WindowsPowerShell\v1.0\;C:\Windows\System32\OpenSSH\;C:\Program Files\Amazon\cfn-bootstrap\;C:\Users\runneradmin\AppData\Local\Microsoft\WindowsApps;" -split ";"
    $paths = $env:path -split ";"
    $shared_libs = (dumpbin /dependents $RootPath)
    $libs_start = $shared_libs | select-string -Pattern "^  Image has the following dependencies:$"
    $libs_end = $shared_libs | select-string -Pattern "^  Summary$"
    $shared_libs = $shared_libs | Select-Object -Index (($libs_start.LineNumber+1)..($libs_end.LineNumber-3))
    foreach($shared_lib in $shared_libs) {
        foreach($path in $paths) {
            if($shared_lib -and $path -and !$excludelist.contains($path)){
                $shared_lib_path = (Join-Path -Path $path -ChildPath $shared_lib.Trim())
                $target_lib_path = (Join-Path -Path $DestinationPath -ChildPath $shared_lib.Trim())
                if ((Test-Path $shared_lib_path) -and !(Test-Path $target_lib_path)) {
                    echo "Copying $shared_lib_path for $RootPath"
                    Copy-Item $shared_lib_path -Destination $DestinationPath
                    Copy-Dependencies -RootPath $shared_lib_path -DestinationPath $DestinationPath
                }
            }
        }
    }
}

echo $env:path
& "C:\Program Files\Microsoft Visual Studio\2022\Enterprise\Common7\Tools\Launch-VsDevShell.ps1" -Arch amd64 -SkipAutomaticLocation
$shared_libs = Get-ChildItem -Path $LibPath -Filter *.dll -Recurse
foreach($shared_lib in $shared_libs) {
    Copy-Dependencies -RootPath $shared_lib.FullName -DestinationPath $DestinationPath
}
