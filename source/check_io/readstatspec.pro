PRO ReadStatSpec
common keesc1
;close,1 & openr,1,dir_res+startup
close,1 & openr,1,startup
i=0
atxt=' '
staterror(*)=0
while ~eof(1) do begin
  atxt=discardComments(1)
  if atxt eq '[MONITORING]' then break
endwhile
;First one discarded 'cause is a sample header
atxt=discardComments(1)
while ~eof(1) do begin
  atxt=discardComments(1)
  if strmid(atxt,0,1) eq ';' or strmid(atxt,0,1) eq '#' or strmid(atxt,0,1) eq '[' or strmid(atxt,0,1) eq '' then continue
  res=strsplit(atxt,';',/extract)
  res=strcompress(res,/remove_all)
  if n_elements(res) ne 12 then begin
    staterror(i)=1
  endif
  statnames(i)=res(1)
;KeesC 18JAN2015 2 lines  
  statcodes(i)=res(0)  
  statabbr(i)=res(2)
  print,statnames(i)
  spec_stations(i)=res(n_elements(res)-1)
  i++
endwhile
close,1 
statnames=reform(statnames(0:i-1))
statcodes=reform(statcodes(0:i-1))
statabbr=reform(statabbr(0:i-1))
spec_stations=reform(spec_stations(0:i-1))
spec_stations=strcompress(spec_stations,/remove_all)
cc=where(strupcase(spec_stations) eq 'NOOBS',numb_NoOBS)
nstat=n_elements(statnames)
numb_OBS=nstat-numb_NoOBS
end
