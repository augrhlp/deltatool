FUNCTION conversionFileTest, startupFile, dirIn, dirOut, out_name, logTextWID
  ;***************************************
  res=file_test(startupFile)
  if res eq 0 then begin
    txt='==> STOP! STARTUP_FILE file does not exist'
    addLogText, logTextWID, txt
    return, 1
  endif
  res=file_test(dirIn,/directory)
  if res eq 0 then begin
    txt='==> STOP! Directory INPUT_DIR does not exist'
    addLogText, logTextWID, txt
    return, 1
  endif
  res=file_test(dirOut,/directory)
  if res eq 0 then begin
    txt='==> STOP! Directory OUTPUT_DIR does not exist'
    addLogText, logTextWID, txt
    return, 1
  endif
  return, 0
;***************************************
  
end