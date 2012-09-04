PRO ReadSpec
common keesc1
;close,1 & openr,1,dir_res+startup
close,1 & openr,1,startup
i=0
atxt=' '
staterror(*)=0
while ~eof(1) do begin
  atxt=discardComments(1)
  ;readf,1,atxt
  if atxt eq '[PARAMETERS]' then break
endwhile
while ~eof(1) do begin   
  atxt=discardComments(1)
  if atxt eq '[MONITORING]' then break
  res=strsplit(atxt,';',/extract)
  if n_elements(res) ne 3 then begin
    staterror(i)=1
  endif
  spec(i)=res(0)
  types(i)=res(1)
  units(i)=res(2)
  i++
endwhile
spec=reform(spec(0:i-1))
types=reform(types(0:i-1))
units=reform(units(0:i-1))
close,1
end