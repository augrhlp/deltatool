@structure_definition.pro
function getUserFont, RESET=RESET, userFontName, fontDesc=fontDesc

  COMMON lastFontSetting, fontList, fontType, fontName, charSize, charThick, fontSet, fontSelected
  
  CATCH, Error_status
  
;  if Error_status ne 0 then begin
;    catch, /CANCEL
;    return, 0
;  endif
  
  return, fontSelected
  
end