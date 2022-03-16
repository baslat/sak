$wrd = New-Object -ComObject "Word.Application"
$file = Get-ChildItem "REPLACE_ME_DIR" -Filter "REPLACE_ME_FILENAME" -Exclude "*.docx" -Recurse | % { $_.FullName }

$doc = $wrd.Documents.Open($file)
$doc.SaveAs($file + ".docx")
$doc.Close()
