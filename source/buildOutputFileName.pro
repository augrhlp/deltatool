FUNCTION buildOutputFileName, locationDir, year, modelName, postfix, MODELTYPE=MODELTYPE, OBSERVEDTYPE=OBSERVEDTYPE
    
  if keyword_set(MODELTYPE) then begin
    fileName=locationDir+strcompress(year, /REMOVE)+'_'+strcompress(modelName, /REMOVE)+'_'+postfix
    return, fileName 
  endif
  
  return, locationDir+strcompress(year, /REMOVE)+'_'+'OBS'+'_'+postfix

END



