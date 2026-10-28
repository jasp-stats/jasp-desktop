$hex = (Select-String -Path "$env:LOCALAPPDATA\Packages\JASP.EFSPoC_qfqszy29ya4s6\LocalCache\poc-engine.log" -Pattern 'raw hex: ([0-9A-F ]+)' | ForEach-Object { $_.Matches[0].Groups[1].Value })
$bytes = ($hex -split ' ' | Where-Object { $_ -match '^[0-9A-F]{2}$' }) | ForEach-Object { [Convert]::ToByte($_, 16) }
Write-Host ("decoded {0} bytes" -f $bytes.Count)
$text = [Text.Encoding]::Unicode.GetString($bytes)
Write-Host "=== claim-name strings found in the token attribute buffer ==="
($text -split "[\x00-\x1F]+" | Where-Object { $_ -match 'WIN://' -or $_ -match 'JASP' }) | Select-Object -First 10
