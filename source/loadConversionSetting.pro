FUNCTION loadConversionSetting, savefile

  atxt=' '
  slash=path_sep()
  
  infoList=''
  close,1 & openr,1,savefile
  i=0
  while ~eof(1) do begin
    readf,1,atxt
    infoList=[infoList, atxt]
    i++
  endwhile
  close,1
  nlines=i
  infoList=infoList(1:nlines-1)
  
  for i=0,nlines-1 do begin
    if strmid(strcompress(infoList(i),/remove_all),0,5) eq '#STAR' then begin
      startup=strcompress(infoList(i+1),/remove_all)
      break
    endif
  endfor
  for i=0,nlines-1 do begin
    if strmid(strcompress(infoList(i),/remove_all),0,5) eq '#INIT' then begin
      initrun=strcompress(infoList(i+1),/remove_all)
      break
    endif
  endfor
  
  year=fix(strmid(initrun,0,4))
  leapyear=0
  if (year mod 4) eq 0 then leapyear=1
  months=['01','02','03','04','05','06','07','08','09','10','11','12']
  days=[31,28,31,30,31,30,31,31,30,31,30,31]
  if leapyear eq 1 then days[1]=29
  mnthInit=fix(strmid(initrun,4,2))
  dayInit=fix(strmid(initrun,6,2))
  hrs=0
  if mnthInit ge 2 then begin
    for im=0,mnthInit-2 do hrs=hrs+days(im)*24
  endif
  hrs=hrs+(dayInit-1)*24
  hour0=hrs
  for i=0,nlines-1 do begin
    if strmid(strcompress(infoList(i),/remove_all),0,5) eq '#ENDR' then begin
      endrun=strcompress(infoList(i+1),/remove_all)
      break
    endif
  endfor

  mnthEnd=fix(strmid(endrun,4,2))
  dayEnd=fix(strmid(endrun,6,2))
  hrs=0
  if mnthEnd ge 2 then begin
    for im=0,mnthEnd-2 do hrs=hrs+days(im)*24
  endif
  hrs=hrs+(dayEnd-1)*24 + 23
  hour1=hrs
  
  for i=0,nlines-1 do begin
    if strmid(strcompress(infoList(i),/remove_all),0,10) eq '#INPUT_DIR' then begin
      dir_in=strcompress(infoList(i+1),/remove_all)
      if strmid(dir_in,0,1,/reverse_offset) ne slash then dir_in=dir_in+slash
      break
    endif
  endfor

  for i=0,nlines-1 do begin
    if strmid(strcompress(infoList(i),/remove_all),0,10) eq '#INPUT_FIL' then begin
      if infoList(i+1) eq 'NOMODEL' then infoList(i+1)=''
      inputID=strcompress(infoList(i+1),/remove_all)
      break
    endif
  endfor
  next3:
  for i=0,nlines-1 do begin
    if strmid(strcompress(infoList(i),/remove_all),0,11) eq '#OUTPUT_DIR' then begin
      dir_out=strcompress(infoList(i+1),/remove_all)
      if strmid(dir_out,0,1,/reverse_offset) ne slash then dir_out=dir_out+slash
      break
    endif
  endfor

;  for i=0,nlines-1 do begin
;    if strmid(strcompress(infoList(i),/remove_all),0,11) eq '#OUTPUT_FIL' then begin
;      model=strcompress(infoList(i+1),/remove_all)
;      res=strsplit(model,'_',/extract)
;      res=strcompress(res,/remove_all)
;      if n_elements(res) ne 3 or res(n_elements(res)-1) ne 'TIME.cdf' then begin
;        txt=['==> WARNING! ModelName/OutputFile not correct: SCEN_MODELNAME_TIME.cdf','         Ex. 2009_CHIM07_TIME.cdf']
;        txtall=[txt,txtall]
;        widget_control,labcom_txt,set_value=txtall
;      endif
;      break
;    endif
;  endfor
  
  for i=0,nlines-1 do begin
    if strpos(infoList(i), '#YEAR') ne -1 then begin
      year=strcompress(infoList(i+1),/remove_all)
      break
    endif
  endfor

  for i=0,nlines-1 do begin
    if strpos(infoList(i),'#MODEL_NAME') ne -1 then begin
      modelName=strcompress(infoList(i+1),/remove_all)
      break
    endif
  endfor

  for i=0,nlines-1 do begin
    if strpos(infoList(i), '#POSTFIX') ne -1 then begin
      postFix=strcompress(infoList(i+1),/remove_all)
      break
    endif
  endfor

  for i=0,nlines-1 do begin
    if strpos(infoList(i), '#OBSERVED_FLAG') ne -1 then begin
      obsFlag=strcompress(infoList(i+1),/remove_all)
      break
    endif
  endfor

  return, [startup, initrun, endrun, dir_in, inputID, dir_out, year, modelName, postFix, obsFlag, strcompress(hour0, /REMOVE), strcompress(hour1, /REMOVE)]
  
END