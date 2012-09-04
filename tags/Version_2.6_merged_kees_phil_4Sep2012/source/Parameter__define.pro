;********************
@structure_definition
;********************
FUNCTION Parameter::getNamesByCodes, codes

 allNames=self->getDisplayNames()
 allCodes=self->getCodes()
 howManyCodes=n_elements(codes)
 idxs=intarr(howManyCodes)
 for i=0, howManyCodes-1 do idxs[i]=(where(codes[i] eq allCodes))[0]
 return, allNames[idxs]

END

FUNCTION Parameter::getMeasureUnitsByCodes, codes

 allMU=self->getMeasureUnits()
 allCodes=self->getCodes()
 howManyCodes=n_elements(codes)
 idxs=intarr(howManyCodes)
 for i=0, howManyCodes-1 do idxs[i]=(where(codes[i] eq allCodes))[0]
 return, allMU[idxs]

END

FUNCTION Parameter::getTypeCodes

 thisList=*self.list
 return, thisList[*].typeCode

END

FUNCTION Parameter::getDisplayNames

 thisList=*self.list
 return, thisList[*].displayName

END

FUNCTION Parameter::getCodes

 thisList=*self.list
 return, thisList[*].code

END

FUNCTION Parameter::getDescriptions

 thisList=*self.list
 return, thisList[*].description

END

FUNCTION Parameter::getMeasureUnits

 thisList=*self.list
 return, thisList[*].measureUnit

END

PRO Parameter::streamPrint

 print, '***********************'
 print, '**Start of<',OBJ_CLASS(self),'>**'

 print, '**** fileName:<', self.fileName,'>'
 thisList=*self.list
 for i=0, n_elements(thisList)-1 do begin
	print, '**element n.<', strcompress(i, /REMOVE),'>'
	print, '**** code:<', thisList[i].code,'>'
	print, '**** typeCode:<', thisList[i].typeCode,'>'
	print, '**** displayName:<', thisList[i].displayName,'>'
	print, '**** measureUnit:<', thisList[i].measureUnit,'>'
	print, '**** description:<', thisList[i].description,'>'
	print, '**'
 endfor

 print, '***********************'
 print, '**End of:<',OBJ_CLASS(self),'>**'

END

PRO Parameter::fillDataFromFile, fileName

 self.fileName=fileName

 ERROR=0
 catch, error_status

 if error_status NE 0 THEN BEGIN
	ERROR=1
	catch, /CANCEL
	errMsg=dialog_message('problem with file: <'+fileName+'> check existence or read permission.', /ERROR)
	return
 endif

 openr, unit, fileName, /GET_LUN

 bufferString=''
 i=0
 parameters=getFMParameter()
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
		if n_elements(info) eq 5 then begin
			thisParameter=getFMparameter()
			thisParameter.code=info[0]
 			thisParameter.typeCode=info[1]
 			thisParameter.displayName=info[2]
			thisParameter.measureUnit=info[3]
			thisParameter.description=info[4]
			parameters=[parameters, thisParameter]
		endif else begin
			print, 'Bad conf file at line', i, bufferString
		endelse
	endelse
 endwhile
 close, unit & free_lun, unit
 self.list=ptr_new(parameters[1:*], /NO_COPY)

END

FUNCTION Parameter::init

 if not(self -> ConfigurableData::init()) then return, 0
 return, 1

END

PRO Parameter::cleanUp

 self -> ConfigurableData::cleanUp

END

PRO Parameter__Define

Struct = { Parameter , $
		Inherits ConfigurableData $
		 }

END