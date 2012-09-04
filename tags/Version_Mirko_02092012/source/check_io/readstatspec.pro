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
  atxt=strcompress(atxt,/remove_all)
  if strmid(atxt,0,1) eq ';' or strmid(atxt,0,1) eq '#' or strmid(atxt,0,1) eq '[' then continue
  res=strsplit(atxt,';',/extract)
  if n_elements(res) ne 12 then begin
    staterror(i)=1
  endif
  statnames(i)=res(1)
  spec_stations(i)=res(n_elements(res)-1)
  i++
endwhile
close,1 
statnames=reform(statnames(0:i-1))
spec_stations=reform(spec_stations(0:i-1))
end