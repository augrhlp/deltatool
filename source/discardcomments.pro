function discardComments, unit, ENDOFFILE=ENDOFFILE

  ENDOFFILE=0
  atxt=''
  readf,unit,atxt
  firstChar=strmid(atxt, 0, 1)
  while firstChar eq '[' or firstChar eq ';' or firstChar eq '#' do begin
  readf,unit,atxt
  firstChar=strmid(atxt, 0, 1)
endwhile
ENDOFFILE=eof(unit)
return, atxt

end
