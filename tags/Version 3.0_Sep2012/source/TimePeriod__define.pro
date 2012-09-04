;********************
@structure_definition
@DateTimeUtility_define
;********************
FUNCTION TimePeriod::getInfoByCode, code

 allCodes=self->getCodes()
 idx=(where(code eq allCodes))[0]
 startEnd=replicate(getTimeStampStruct(), 2)
 thisList=*self.list
 thisPeriod=thisList[idx]
 
 startEnd[0].value=thisPeriod.startValue
 startEnd[0].template=thisPeriod.template
 startEnd[1].value=thisPeriod.endValue
 startEnd[1].template=thisPeriod.template
 
 return,  startEnd

END

FUNCTION TimePeriod::getStartValues

 thisList=*self.list
 return, thisList[*].startValue

END

FUNCTION TimePeriod::getEndValues

 thisList=*self.list
 return, thisList[*].endValue

END

FUNCTION TimePeriod::getTemplates

 thisList=*self.list
 return, thisList[*].template

END

FUNCTION TimePeriod::getDisplayNames

 thisList=*self.list
 return, thisList[*].displayName

END

FUNCTION TimePeriod::getCodes

 thisList=*self.list
 return, thisList[*].code

END

FUNCTION TimePeriod::getDescriptions

 thisList=*self.list
 return, thisList[*].description

END

PRO TimePeriod::streamPrint

 print, '***********************'
 print, '**Start of<',OBJ_CLASS(self),'>**'

 print, '**** fileName:<', self.fileName,'>'
 thisList=*self.list
 for i=0, n_elements(thisList)-1 do begin
	print, '**element n.<', strcompress(i, /REMOVE),'>'
	print, '**** code:<', thisList[i].code,'>'
	print, '**** displayName:<', thisList[i].displayName,'>'
	print, '**** startValue:<', thisList[i].startValue,'>'
	print, '**** endValue:<', thisList[i].endValue,'>'
	print, '**** template:<', thisList[i].template,'>'
	print, '**** description:<', thisList[i].description,'>'
	print, '**'
 endfor

 print, '***********************'
 print, '**End of:<',OBJ_CLASS(self),'>**'

END

PRO TimePeriod::fillDataFromFile, fileName

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
 periods=getFMTimePeriod()
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
		if n_elements(info) eq 6 then begin
			thisPeriod=getFMTimePeriod()
			thisPeriod.code=fix(info[0])
 			thisPeriod.displayName=info[1]
			thisPeriod.startValue=info[2]
			thisPeriod.endValue=info[3]
      thisPeriod.template=info[4]
			thisPeriod.description=info[5]
			periods=[periods, thisPeriod]
		endif else begin
			print, 'Bad conf file at line', i, bufferString
		endelse
	endelse
 endwhile
 close, unit & free_lun, unit
 self.list=ptr_new(periods[1:*], /NO_COPY)

END

FUNCTION TimePeriod::init

 if not(self -> ConfigurableData::init()) then return, 0
 return, 1

END

PRO TimePeriod::cleanUp

 self -> ConfigurableData::cleanUp

END

PRO TimePeriod__Define

Struct = { TimePeriod , $
		Inherits ConfigurableData $
		 }

END