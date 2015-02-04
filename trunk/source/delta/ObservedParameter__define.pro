;********************
@structure_definition
;********************
PRO ObservedParameter::streamPrint

 print, '***********************'
 print, '**Start of<',OBJ_CLASS(self),'>**'

 print, '**** fileName:<', self.fileName,'>'
 thisList=*self.list
 for i=0, n_elements(thisList)-1 do begin
	print, '**element n.<', strcompress(i, /REMOVE),'>'
	print, '**** code:<', thisList[i].code,'>'
	print, '**** observedCode:<', thisList[i].observedCode,'>'
	print, '**** parameterCode:<', thisList[i].parameterCode,'>'
	print, '**** fileName:<', thisList[i].fileName,'>'
	print, '**** monitorSampling:<', thisList[i].monitorSampling,'>'
	print, '**** startAvailability:<', thisList[i].startAvailability,'>'
	print, '**** endAvailability:<', thisList[i].endAvailability,'>'
	print, '**** internalParameterCode:<', thisList[i].internalParameterCode,'>'
	print, '**** description:<', thisList[i].description,'>'
	print, '**'
 endfor

 print, '***********************'
 print, '**End of:<',OBJ_CLASS(self),'>**'

END

PRO ObservedParameter::fillDataFromFile, fileName

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
 obsPars=getFMObservedParameter()
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
			thisObsPar=getFMObservedparameter()
			thisObsPar.code=fix(info[0])
 			thisObsPar.observedCode=fix(info[1])
 			thisObsPar.parameterCode=fix(info[2])
			thisObsPar.fileName=info[3]
			thisObsPar.monitorSampling=info[4] ; timestamp hh24:mi
			thisObsPar.startAvailability=info[5] ; time stamp is: yyyymmddhh24:mi
			thisObsPar.endAvailability=info[6] ; time stamp is: yyyymmddhh24:mi
			thisObsPar.internalParameterCode=info[7]
			thisObsPar.description=info[8]
			obsPars=[obsPars, thisObsPar]
		endif else begin
			print, 'Bad conf file at line', i, bufferString
		endelse
	endelse
 endwhile
 close, unit & free_lun, unit
 self.list=ptr_new(obsPars[1:*], /NO_COPY)

END

FUNCTION ObservedParameter::init

 if not(self -> ConfigurableData::init()) then return, 0
 return, 1

END

PRO ObservedParameter::cleanUp

 self -> ConfigurableData::cleanUp

END

PRO ObservedParameter__Define

Struct = { ObservedParameter , $
		Inherits ConfigurableData $
		 }

END