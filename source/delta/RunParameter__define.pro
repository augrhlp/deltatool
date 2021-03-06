;********************
@structure_definition
;********************
PRO RunParameter::streamPrint

 print, '***********************'
 print, '**Start of<',OBJ_CLASS(self),'>**'

 print, '**** fileName:<', self.fileName,'>'
 thisList=*self.list
 for i=0, n_elements(thisList)-1 do begin
	print, '**element n.<', strcompress(i, /REMOVE),'>'
	print, '**** code:<', thisList[i].code,'>'
	print, '**** runCode:<', thisList[i].runCode,'>'
	print, '**** parameterCode:<', thisList[i].parameterCode,'>'
	print, '**** fileName:<', thisList[i].fileName,'>'
	print, '**** monitorSampling:<', thisList[i].monitorSampling,'>'
	print, '**** startAvailability:<', thisList[i].startAvailability,'>'
	print, '**** endAvailability:<', thisList[i].endAvailability,'>'
	print, '**** internalparameterCode:<', thisList[i].internalparameterCode,'>'
	print, '**** description:<', thisList[i].description,'>'
	print, '**'
 endfor

 print, '***********************'
 print, '**End of:<',OBJ_CLASS(self),'>**'

END

PRO RunParameter::fillDataFromFile, fileName

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
 runPars=getFMRunParameter()
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
		if n_elements(info) eq 9 then begin
			thisRunPar=getFMRunParameter()
			thisRunPar.code=fix(info[0])
 			thisRunPar.runCode=fix(info[1])
 			thisRunPar.parameterCode=fix(info[2])
			thisRunPar.fileName=info[3]
			thisRunPar.monitorSampling=info[4] ; time stamp hh24:mi
			thisRunPar.startAvailability=info[5] ; standard time stamp is: yyyymmddhh24:mi
			thisRunPar.endAvailability=info[6] ; standard time stamp is: yyyymmddhh24:mi
			thisRunPar.internalparameterCode=info[7]
			thisRunPar.description=info[8]
			runPars=[runPars, thisRunPar]
		endif else begin
			print, 'Bad conf file at line', i, bufferString
		endelse
	endelse
 endwhile
 close, unit & free_lun, unit
 self.list=ptr_new(runPars[1:*], /NO_COPY)

END

FUNCTION RunParameter::init

 if not(self -> ConfigurableData::init()) then return, 0
 return, 1

END

PRO RunParameter::cleanUp

 self -> ConfigurableData::cleanUp

END

PRO RunParameter__Define

Struct = { RunParameter , $
		Inherits ConfigurableData $
		 }

END