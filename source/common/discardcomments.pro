function discardComments, unit, BRACKETISCOMMENT=BRACKETISCOMMENT, ENDOFFILE=ENDOFFILE

  ENDOFFILE=0
  discardList=[';', '#']
  atxt=''
  if keyword_set(BRACKETISCOMMENT) then discardList=[discardList, '[']
  
  readf,unit,atxt
  firstChar=strmid(atxt, 0, 1)
  check=where(firstChar eq discardList, count)
  while count eq 1 do begin
    readf,unit,atxt
    firstChar=strmid(atxt, 0, 1)
    check=where(firstChar eq discardList, count)
  endwhile
  ENDOFFILE=eof(unit)
  return, atxt
  
end
