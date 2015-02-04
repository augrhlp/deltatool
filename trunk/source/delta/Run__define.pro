;********************
@structure_definition
;********************
FUNCTION Run::getFileNamesByCodes, codes

 thisList=*self.list
 return, thisList[codes].fileName

END

FUNCTION Run::getModelCodes

 thisList=*self.list
 return, thisList[*].modelCode

END

FUNCTION Run::getScenarioCodes

 thisList=*self.list
 return, thisList[*].scenarioCode

END

FUNCTION Run::getDisplayNames

 thisList=*self.list
 return, thisList[*].displayName

END

FUNCTION Run::getCodes

 thisList=*self.list
 return, thisList[*].code

END

FUNCTION Run::getDescriptions

 thisList=*self.list
 return, thisList[*].description

END

PRO Run::streamPrint

 print, '***********************'
 print, '**Start of<',OBJ_CLASS(self),'**'

 print, '**** fileName:<', self.fileName,'>'
 thisList=*self.list
 for i=0, n_elements(thisList)-1 do begin
	print, '**element n.<', strcompress(i, /REMOVE),'>'
	print, '**** code:<', thisList[i].code,'>'
	print, '**** scenarioCode:<', thisList[i].scenarioCode,'>'
	print, '**** modelCode:<', thisList[i].modelCode,'>'
	print, '**** areaCode:<', thisList[i].areaCode,'>'
	print, '**** displayName:<', thisList[i].displayName,'>'
	print, '**** gridXStart:<', thisList[i].gridXStart,'>'
	print, '**** gridXStep:<', thisList[i].gridXStep,'>'
	print, '**** gridXStepNumber:<', thisList[i].gridXStepNumber,'>'
	print, '**** gridYStart:<', thisList[i].gridYStart,'>'
	print, '**** gridYStep:<', thisList[i].gridYStep,'>'
	print, '**** gridYStepNumber:<', thisList[i].gridYStepNumber,'>'
	print, '**** gridVerticalInterpolation:<', thisList[i].gridVerticalInterpolation,'>'
	print, '**** gridHorizontalInterpolation:<', thisList[i].gridHorizontalInterpolation,'>'
	print, '**** executionDate:<', thisList[i].executionDate,'>'
	print, '**** fileName:<', thisList[i].fileName,'>'
	print, '**** description:<', thisList[i].description,'>'
	print, '**'
 endfor

 print, '***********************'
 print, '**End of:<',OBJ_CLASS(self),'>**'

END

PRO Run::fillDataFromFile, fileName

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
 runs=getFMRun()
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
		if n_elements(info) eq 16 then begin
			thisRun=getFMRun()
			thisRun.code=fix(info[0])
 			thisRun.scenarioCode=info[1]
 			thisRun.modelCode=info[2]
			thisRun.areaCode=fix(info[3])
			thisRun.displayName=info[4]
			thisRun.gridXStart=float(info[5])
			thisRun.gridXStep=float(info[6])
			thisRun.gridXStepNumber=fix(info[7])
			thisRun.gridYStart=float(info[8])
			thisRun.gridYStep=float(info[9])
			thisRun.gridYStepNumber=fix(info[10])
			thisRun.gridVerticalInterpolation=float(info[11])
			thisRun.gridHorizontalInterpolation=float(info[12])
			thisRun.executionDate=info[13] ; try to manage a dateTime?
			thisRun.fileName=info[14]
			thisRun.description=info[15]
			runs=[runs, thisRun]
		endif else begin
			print, 'Bad conf file at line', i, bufferString
		endelse
	endelse
 endwhile
 close, unit & free_lun, unit
 self.list=ptr_new(runs[1:*], /NO_COPY)

END

FUNCTION Run::init

 if not(self -> ConfigurableData::init()) then return, 0
 return, 1

END

PRO Run::cleanUp

 self -> ConfigurableData::cleanUp

END

PRO Run__Define

Struct = { Run , $
		Inherits ConfigurableData $
		 }

END