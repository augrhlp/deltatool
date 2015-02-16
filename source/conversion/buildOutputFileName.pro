FUNCTION buildOutputFileName, locationDir, year, modelName, postfix, MODELTYPE=MODELTYPE, OBSERVEDTYPE=OBSERVEDTYPE
    
  last=strpos(locationDir, path_sep(), /REVERSE_SEARCH)
  if last+1 ne strlen(locationDir) then locationDir=locationDir+path_sep() 

  if keyword_set(MODELTYPE) then begin
    fileName=locationDir+strcompress(year, /REMOVE)+'_'+strcompress(modelName, /REMOVE)+'_'+postfix
    return, fileName 
  endif
  
  if strlen(year) eq 0 then obsFName='OBS'+'_'+postfix else obsFName=strcompress(year, /REMOVE)+'_'+'OBS'+'_'+postfix 
  return, locationDir+obsFName

END



