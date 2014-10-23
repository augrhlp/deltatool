;********************
@structure_definition
;********************
FUNCTION Scenario::getDisplayNames

 thisList=*self.list
 return, thisList[*].displayName

END

FUNCTION Scenario::getCodes

 thisList=*self.list
 return, thisList[*].code

END

FUNCTION Scenario::getDescriptions

 thisList=*self.list
 return, thisList[*].description

END

PRO Scenario::streamPrint

 print, '***********************'
 print, '**Start of<',OBJ_CLASS(self),'>**'

 print, '**** fileName:<', self.fileName,'>'
 thisList=*self.list
 for i=0, n_elements(thisList)-1 do begin
	print, '**element n.<', strcompress(i, /REMOVE),'>'
	print, '**** code:<', thisList[i].code,'>'
	print, '**** displayName:<', thisList[i].displayName,'>'
	print, '**** description:<', thisList[i].description,'>'
	print, '**'
 endfor

 print, '***********************'
 print, '**End of:<',OBJ_CLASS(self),'>**'

END

PRO Scenario::fillDataFromFile, fileName

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
 scenarios=getFMScenario()
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
		if n_elements(info) eq 3 then begin
			thisScenario=getFMScenario()
			thisScenario.code=info[0]
 			thisScenario.displayName=info[1]
			thisScenario.description=info[2]
			scenarios=[scenarios, thisScenario]
		endif else begin
			print, 'Bad conf file at line', i, bufferString
		endelse
	endelse
 endwhile
 close, unit & free_lun, unit
 self.list=ptr_new(scenarios[1:*], /NO_COPY)

END

FUNCTION Scenario::init

 if not(self -> ConfigurableData::init()) then return, 0
 return, 1

END

PRO Scenario::cleanUp

 self -> ConfigurableData::cleanUp

END

PRO Scenario__Define

Struct = { Scenario , $
		Inherits ConfigurableData $
		 }

END