FUNCTION buildInputTemplateFileName, locationDir, prefix
    
  last=strpos(locationDir, path_sep(), /REVERSE_SEARCH)
  if (last+1) ne strlen(locationDir) then locationDir=locationDir+path_sep() 
  return, locationDir+prefix+'$STATION$.csv'

END



