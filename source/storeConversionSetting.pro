PRO storeConversionSetting, infoList, savefile
  
  atxt=' '
  slash=path_sep()
  
  if index eq 0 then begin          ; write
    close,1 & openw,1,savefile
    nlines=n_elements(infoList)
    if infoList(10) eq '' then infoList(10)='NOMODEL'
    for i=0,nlines-1 do begin
      printf,1,infoList(i)
    endfor
    close,1
  endif

end