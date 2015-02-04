@getbestfont
pro setDeviceFont, fontName=fontName, fontSize=fontSize, fontType=fontType, STANDARD=STANDARD, FINE=FINE

  TT_FONT=1
  CATCH, Error_status
  
  if Error_status ne 0 then begin
    if TT_FONT eq 0 then message, 'set a valid font name for the setup.ini variable *DEVICE_STANDARD_FONT_NAME*'+fontName
    ;if TT_FONT eq 0 then message, 'set a valid font name for the setup.ini variable *DEVICE_FINE_FONT_NAME*'+fontName
    TT_FONT=0
    catch, /CANCEL
  endif
  fontName=getBestFont(fontName=fontName, fontSize=fontSize, fontType=fontType, STANDARD=STANDARD, FINE=FINE)
  device,set_font=fontName, TT_FONT=TT_FONT
  
end