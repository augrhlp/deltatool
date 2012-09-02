FUNCTION Observed::buildParameterCodes, fileField, NULL=NULL

 allList=['-1']
 NULL=0
 parameterCodes=''
 if (fileField eq 'ALL') then begin
  ; get all parameters codes from main parameter table.
  parameterCodes=fix(allList)
 endif else begin
  if fileField eq 'NONE' then NULL=1 else parameterCodes=strsplit(fileField, '*', /EXTRACT)
 endelse
 return, parameterCodes

END

FUNCTION Observed::getRecord, index

 thisList=*self.list
 return, thisList[index]

END

FUNCTION Observed::getDisplayNames

 thisList=*self.list
 return, thisList[*].displayName

END

FUNCTION Observed::getShortNames

 thisList=*self.list
 return, thisList[*].shortName

END

FUNCTION Observed::getHeightAboveSeas

 thisList=*self.list
 return, thisList[*].heightAboveSea

END

FUNCTION Observed::getXGeoLocations

 thisList=*self.list
 return, thisList[*].xGeoLocation

END

FUNCTION Observed::getYGeoLocations

 thisList=*self.list
 return, thisList[*].yGeoLocation

END

FUNCTION Observed::getCountries

 thisList=*self.list
 return, thisList[*].country

END

FUNCTION Observed::getCountryGMTs

 thisList=*self.list
 return, thisList[*].countryGMT

END

FUNCTION Observed::getParameters

 thisList=*self.list
 return, thisList[*].parameters

END

FUNCTION Observed::getParametersAtIndex, index

 thisList=*self.list
 return, *(thisList[index].parameters)

END

FUNCTION Observed::getCodes

 thisList=*self.list
 return, thisList[*].code

END

FUNCTION Observed::getDescriptionsList

 thisList=*self.list
 return, thisList[*].description

END

PRO Observed::streamPrint

 print, '***********************'
 print, '**Start of<',OBJ_CLASS(self),'>**'

 print, '**** fileName:<', self.fileName,'>'
 thisList=*self.list
 for i=0, n_elements(thisList)-1 do begin
	print, '**element n.<', strcompress(i, /REMOVE),'>'
	print, '**** code:<', thisList[i].code,'>'
	print, '**** name:<', thisList[i].name,'>'
	print, '**** displayName:<', thisList[i].name,'>'
	print, '**** xGeoLocation:<', thisList[i].xGeoLocation,'>'
	print, '**** yGeoLocation:<', thisList[i].yGeoLocation,'>'
	print, '**** heightAboveSea:<', thisList[i].heightAboveSea,'>'
	print, '**** description:<', thisList[i].description,'>'
	print, '**** country:<', thisList[i].country,'>'
  print, '**** countryGMT:<', thisList[i].countryGMT,'>'
  print, '**** parameters:<', thisList[i].parameters,'>'
	print, '**'
 endfor

 print, '***********************'
 print, '**End of:<',OBJ_CLASS(self),'>**'

END

PRO Observed::fillDataFromFile, fileName

 self.fileName=fileName

 ERROR=0
 catch, error_status

 if error_status NE 0 THEN BEGIN
	ERROR=1
	catch, /CANCEL
	errMsg=dialog_message('problem with file: <'+fileName+'> check existence or read permission.', /ERROR)
 endif

 openr, unit, fileName, /GET_LUN

 bufferString=''
 i=0
 observeds=getFMObserved()
 while not(eof(unit)) do begin
	readf, unit, bufferString
	i++
	checkFirst=strmid(bufferString, 0,1)
	check1=(strpos(checkFirst, '[')+1) > 0
	check2=(strpos(checkFirst, ';')+1) > 0
	check3=(strpos(checkFirst, '#')+1) > 0
	null=strlen(strcompress(bufferString, /REMOVE)) eq 0
	; #, ; is a comment
	; void string string is discarded
	; [ header is discarded
	if (check1+check2+check3) gt 0 or null then begin
;		print, 'Discard row', i
;		print, bufferString
	endif else begin
		info=strsplit(bufferString, ';', /EXTRACT)
		if n_elements(info) eq 11 then begin
			thisObserved=getFMObserved()
			thisObserved.code=info[0]
 			thisObserved.name=info[1]
      thisObserved.displayName=info[2]
      thisObserved.shortName=info[3]
			thisObserved.xGeoLocation=float(info[4])
			thisObserved.yGeoLocation=float(info[5])
			thisObserved.country=info[6]
			thisObserved.countryGMT=info[7]
			thisObserved.heightAboveSea=float(info[8])
      thisObserved.description=info[9]
      ;print, '**********'
      ;print, info[0], info[9]
      ;print, '**********'
      thisObserved.parameters=ptr_new(self->buildParameterCodes(info[10]), /NO_COPY)
			observeds=[observeds, thisObserved]
		endif else begin
			print, 'Bad conf file at line', i, bufferString
		endelse
	endelse
 endwhile
 close, unit & free_lun, unit
 self.list=ptr_new(observeds[1:*], /NO_COPY)

END

FUNCTION Observed::init

 if not(self -> ConfigurableData::init()) then return, 0
 return, 1

END

PRO Observed::cleanUp

 self -> ConfigurableData::cleanUp

END

PRO Observed__Define

Struct = { Observed , $
		Inherits ConfigurableData $
		 }

END