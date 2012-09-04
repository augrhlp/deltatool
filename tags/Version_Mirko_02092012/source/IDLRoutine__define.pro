;********************
@structure_definition
;********************
FUNCTION IDLRoutine::getRoutineNameByCode, code

 thisList=*self.list
 return, thisList[code].routineName

END

PRO IDLRoutine::streamPrint

 print, '***********************'
 print, '**Start of <',OBJ_CLASS(self),'>**'

 print, '**** fileName:<', self.fileName,'>'
 thisList=*self.list
 for i=0, n_elements(thisList)-1 do begin
	print, '**element n.<', strcompress(i, /REMOVE), '>'
	print, '**** code:<', thisList[i].code, '>'
	print, '**** routineName:<', thisList[i].routineName, '>'
;	if ptr_valid(thisList[i].inParameters) then print, '**** inParameters:<', *thisList[i].inParameters, '>' else print, '**** inParameters:', '<NULL_POINTER>'
;	if ptr_valid(thisList[i].outParameters) then print, '**** outParameters:<', *thisList[i].outParameters, '>' else print, '**** outParameters:', '<NULL_POINTER>'
	print, '**** description:<', thisList[i].description, '>'
	print, '**'
 endfor

 print, '***********************'
 print, '**End of:<',OBJ_CLASS(self),'>**'

END

;FUNCTION IDLRoutine::buildConfigInParameter, fileField
;
; ;manage multiple choice: ALL or "*" separated or NONE
; inParameters=strsplit(fileField, '*', /EXTRACT)
; return, inParameters
;
;END
;
;FUNCTION IDLRoutine::buildConfigOutParameter, fileField
;
; ;manage multiple choice: ALL or "*" separated or NONE
; outParameters=strsplit(fileField, '*', /EXTRACT)
; return, outParameters
;
;END

PRO IDLRoutine::fillDataFromFile, fileName

 self.fileName=fileName

 ERROR=0
 catch, error_status

 if error_status NE 0 THEN BEGIN
	ERROR=1
	catch, /CANCEL
	close, unit
	errMsg=dialog_message('problem with file: <'+fileName+'> check existence or read permission.', /ERROR)
	return
 endif

 openr, unit, fileName, /GET_LUN

 bufferString=''
 i=0
 iDLRoutines=getFMIDLRoutine()
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
			thisIDLRoutine=getFMIDLRoutine()
			thisIDLRoutine.code=fix(info[0])
 			thisIDLRoutine.routineName=info[1]
; 			thisIDLRoutine.inParameters=ptr_new(self->buildConfigInParameter(info[2]), /NO_COPY)
;			thisIDLRoutine.outParameters=ptr_new(self->buildConfigOutParameter(info[3]), /NO_COPY)
			thisIDLRoutine.description=info[2]
			iDLRoutines=[iDLRoutines, thisIDLRoutine]
		endif else begin
			print, 'Bad conf file at line', i, bufferString
		endelse
	endelse
 endwhile
 close, unit & free_lun, unit
 self.list=ptr_new(iDLRoutines[1:*], /NO_COPY)

END

FUNCTION IDLRoutine::init

 if not(self -> ConfigurableData::init()) then return, 0
 return, 1

END

PRO IDLRoutine::cleanUp

 self -> ConfigurableData::cleanUp

END

PRO IDLRoutine__Define

Struct = { IDLRoutine , $
		Inherits ConfigurableData $
		 }

END