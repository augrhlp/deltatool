;********************
@structure_definition
;********************
FUNCTION Diagram::getMaxMultipleChoiceByCode, code

 thisList=*self.list
 idx=(where(codeSelected eq thisList.code))[0]
 return, thisList[idx].maxMultipleChoice

END

FUNCTION Diagram::getMaxMultipleChoice

 thisList=*self.list
 return, thisList[*].maxMultipleChoice

END

;FUNCTION Diagram::getAxisCodeByCode, code
;
; thisList=*self.list
; idx=(where(codeSelected eq thisList.code))[0]
; return, thisList[idx].axisCodes
;
;END
;
;FUNCTION Diagram::getAxisCodes, NOFILTER=NOFILTER
;
; thisList=*self.list
; return, thisList[*].axisCodes
;
;END
;
;FUNCTION Diagram::buildAxisCodes, fileField, NULL=NULL
;
; axisCodes=strsplit(fileField, '*', /EXTRACT)
; return, fix(axisCodes)
;
;END

FUNCTION Diagram::getPlotRoutineByCode, code

 codes=self->getCodes()
 idlRoutineNames=self->getPlotRoutines()
 idx=(where(code eq codes))[0]
 return, idlRoutineNames[idx]

END

FUNCTION Diagram::getPlotRoutines

 thisList=*self.list
 return, thisList[*].plotRoutine

END

FUNCTION Diagram::getDisplayNames

 thisList=*self.list
 return, thisList[*].displayName

END

FUNCTION Diagram::getCodes

 thisList=*self.list
 return, thisList[*].code

END

FUNCTION Diagram::getDescriptions

 thisList=*self.list
 return, thisList[*].description

END

PRO Diagram::streamPrint

 print, '***********************'
 print, '**Start of <',OBJ_CLASS(self),'>**'

 print, '**** fileName:<', self.fileName,'>'
 thisList=*self.list
 for i=0, n_elements(thisList)-1 do begin
	print, '**element n.<', strcompress(i, /REMOVE), '>'
  print, '**** code:', thisList[i].code, '>'
	print, '**** displayName:<', thisList[i].displayName, '>'
  print, '**** plotRoutine:', thisList[i].plotRoutine, '>'
  print, '**** maxMultipleChoice:', thisList[i].maxMultipleChoice, '>'
  ;if ptr_valid(thisList[i].axisCodes) then print, '**** axisCodes:<', *thisList[i].axisCodes, '>' else print, '**** axisCodes:', '<NULL_POINTER>'
	print, '**** description:<', thisList[i].description, '>'
	print, '**'
 endfor

 print, '***********************'
 print, '**End of:',OBJ_CLASS(self),'**'

END

PRO Diagram::fillDataFromFile, fileName

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
 Diagrams=getFMDiagram()
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
		;print, 'Discard row', i
		;print, bufferString
	endif else begin
		info=strsplit(bufferString, ';', /EXTRACT)
		if n_elements(info) eq 5 then begin
			thisDiagram=getFMDiagram()
      thisDiagram.code=fix(info[0])
 			thisDiagram.displayName=info[1]
      thisDiagram.plotRoutine=info[2]
      ;aCodes=self->buildAxisCodes(info[3])
      thisDiagram.maxMultipleChoice=fix(info[3]);, /NO_COPY)
      ;thisDiagram.axisCodes=ptr_new(aCodes, /NO_COPY)
			thisDiagram.description=info[4]
			help, thisDiagram, /str
			Diagrams=[Diagrams, thisDiagram]
		endif else begin
			print, 'Bad conf file at line', i, bufferString
		endelse
	endelse
 endwhile
 close, unit & free_lun, unit
 self.list=ptr_new(Diagrams[1:*], /NO_COPY)

END

PRO Diagram::cleanUp

 self -> ConfigurableData::cleanUp

END

FUNCTION Diagram::Init

 if not(self -> ConfigurableData::init()) then return, 0
 return, 1

END

PRO Diagram__Define

Struct = { Diagram , $
		Inherits ConfigurableData $
		 }

END