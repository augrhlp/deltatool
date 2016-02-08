FUNCTION ReadSpecStats, startupfile, logWin=logWin, statnames=statnames, spec_stations=spec_stations, fileyear=fileyear

  spec=strarr(100)
  statnames=strarr(5000)
  spec_stations=strarr(5000)

  addLogText, logWin, 'Reading: '+ startupfile
  ;txtall=[txt,txtall]
  ;widget_control,labcom_txt,set_value=txtall
  atxt=' ' 
  
  close,1 & openr,1,startupfile
  while ~eof(1) do begin
    readf,1,atxt
;KeesC 4FEB2016
    atxt=strtrim(atxt,2)
    if atxt eq '[MODEL]' then begin
      for i=1,10 do begin
        readf,1,atxt
;KeesC 4FEB2016        
        atxt=strtrim(atxt,2)
        if strmid(atxt,0,1) ne ';' then begin
          year=fix(atxt)
          fileyear=year
          goto,jump1
        endif  
      endfor     
    endif
  endwhile
  
  jump1:
  close,1 & openr,1,startupFile
  i=0
  while ~eof(1) do begin
    readf,1,atxt
;KeesC 4FEB2016
    atxt=strtrim(atxt,2)
    if atxt eq '[PARAMETERS]' then begin
      readf,1,atxt
      for i=0,1000 do begin
        readf,1,atxt
;KeesC 4FEB2016
        atxt=strtrim(atxt,2)
        if atxt eq '[MONITORING]' then goto,jump2
        res=strsplit(atxt,';',/extract)
        spec(i)=res(0)
      endfor
    endif    
  endwhile
  
  jump2:
  nspec=i
  spec=reform(spec(0:nspec-1))
  
  close,1 & openr,1,startupFile
  i=0
  while ~eof(1) do begin 
    readf,1,atxt
    if atxt eq '[MONITORING]' then begin
      readf,1,atxt
      while ~eof(1) do begin
        readf,1,atxt
;KeesC 4FEB2016
        atxt=strtrim(atxt,2)
        firstchar=strmid(atxt,0,1)
        res=strsplit(atxt,';',/extract)
        if firstchar ne '[' and firstchar ne ';' and firstchar ne '#' and res(0) ne 'Station Code' and $
          firstchar ne ' ' and firstchar ne '' then begin
          statnames(i)=res(1)
          spec_stations(i)=res(n_elements(res)-1)
          i++
        endif
      endwhile
    endif    
  endwhile
  close,1
  nstat=i
  statnames=reform(statnames(0:nstat-1))
  spec_stations=reform(spec_stations(0:nstat-1))
  spec_stations=strcompress(spec_stations,/remove_all)
  txt='Year from Startup = '+strtrim(fileyear,2)
  addLogText, logWin, txt
  txt=strtrim(nspec,2)+' Variables found in Startup'
  addLogText, logWin, txt
  txt=strtrim(nstat,2)+' Stations found in Startup'
  addLogText, logWin, txt
  return, spec

end
