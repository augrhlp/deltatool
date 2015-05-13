@structure_definition.pro
pro setUserFont, RESET=RESET, userFontName, fontDesc=fontDesc, forceLog=forceLog

  COMMON lastFontSetting, fontList, fontType, fontName, charSize, charThick, fontSet, fontSelected
  
  ERROR=0
  catch, error_status
  
  if error_status ne 0 THEN BEGIN
    ERROR=1
    catch, /CANCEL
    rsult=dialogMsg([['problem with Font Setting, try another one'], ['setUserFont']],/error, FORCELOG=FORCELOG)
    return
  endif
  
  if keyword_set(RESET) then begin
    device, set_font=fontName
    !P.font=fontType
    !P.charsize=charSize
    !P.charthick=charThick
    return
  endif
  
  if n_elements(fontDesc) ne 1 then fontDesc=fontList->getFontByName(userFontName)
  fontSelected=fontDesc.displayName
  ;getBestFont(fontName=fontName, fontSize=fontSize, fontType=fontType, STANDARD=STANDARD, FINE=FINE)
  !P.font=fontDesc.type
  fontString=''
  if fontDesc.fontName ne '' then fontString=fontDesc.fontName
  if fontDesc.modifier ne '' and fontDesc.type eq 1 $
    and strupcase(!version.os_family) NE 'UNIX' and fontDesc.modifier ne 'N/A' then fontString=fontString+'*'+fontDesc.modifier
  if fontDesc.type ge 0 then begin
    if fontDesc.type eq 1 then TT_FONT=1
    device,set_font=fontString, TT_FONT=TT_FONT
  endif else begin
    if fontDesc.charSize ne 0 then !P.charsize=fontDesc.charSize
    if fontDesc.charThick ne 0 then !P.charThick=fontDesc.charThick
  endelse
  print,'***'
  device, get_current_font=fontName
  print, fontString, '!p.font', !p.font, '!P.charsize', !P.charsize, '!P.charthick', !P.charthick, 'TT_FONT', keyword_set(TT_FONT)
  print, fontName, '!p.font', !p.font, '!P.charsize', !P.charsize, '!P.charthick', !P.charthick, 'TT_FONT', keyword_set(TT_FONT)
  print,'***'
  
end