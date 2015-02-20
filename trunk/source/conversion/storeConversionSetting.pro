PRO storeConversionSetting, infoList, savefile
  
  lines=strarr(22)
  lines[0]='#SaveInfo File  => Do not remove the # lines <='
  lines[1]='#STARTUP Full Path to Startup.ini file'
  lines[2]=infoList[0]
  lines[3]='#INITRUN'
  lines[4]=infoList[1]
  lines[5]='#ENDRUN'
  lines[6]=infoList[2]
  lines[7]='#INPUT_DIR'
  lines[8]=infoList[3]
  lines[9]='#INPUT_FILE_ID'
  lines[10]=infoList[4]
  lines[11]='#OUTPUT_DIR'
  lines[12]=infoList[5]
  lines[13]='#YEAR'
  lines[14]=infoList[6]
  lines[15]='#MODEL_NAME'
  lines[16]=infoList[7]
  lines[17]='#POSTFIX'
  lines[18]=infoList[8]
  lines[19]='#OBSERVED_FLAG'
  lines[20]=infoList[9]
  lines[21]='#END'
  
  close,1 & openw,1,savefile
  nlines=n_elements(lines)
  for i=0,nlines-1 do begin
    printf,1,lines(i)
  endfor
  close,1
  
end


