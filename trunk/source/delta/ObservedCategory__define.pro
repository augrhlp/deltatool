;********************
@structure_definition
;********************
FUNCTION ObservedCategory::getRegionOfObs, obsCode

 regionCode=self->getValuesByObserved(obsCode)
 return, regionCode[0]

END

FUNCTION ObservedCategory::getValuesByCategory, CATEGORYCODES=CATEGORYCODES

 thisList=*self.list
 allCatCodes=thisList.categoryCode
 allValues=thisList.value
 avCategoryCodes=allCatCodes[UNIQ(allCatCodes, SORT(allCatCodes))]
 categoryNumber=n_elements(avCategoryCodes)
 values=ptrarr(categoryNumber)
 for i=0, categoryNumber-1 do begin
 	idxsForACategory=where(allCatCodes eq avCategoryCodes[i])
	categoryValues=allValues[idxsForACategory]
	singlesValues=categoryValues[UNIQ(categoryValues, SORT(categoryValues))]
	values[i]=ptr_new(singlesValues)
	;print, 'category', avCategoryCodes[i], 'values', singlesValues
 endfor
 CATEGORYCODES=avCategoryCodes
 return, values

END

FUNCTION ObservedCategory::getValuesByObserved, obsCode

 thisList=*self.list
 allObservedCodes=thisList.observedCode
 idxs=where(allObservedCodes eq obsCode)
 obsValues=thisList[idxs].value
 return, obsValues

END

FUNCTION ObservedCategory::getCodes

 thisList=*self.list
 return, thisList.code

END

FUNCTION ObservedCategory::getObservedCodes

 thisList=*self.list
 return, thisList.observedCode

END

FUNCTION ObservedCategory::getCategoryCodes

 thisList=*self.list
 return, thisList.categoryCode

END

FUNCTION ObservedCategory::getValues

 thisList=*self.list
 return, thisList.value

END

PRO ObservedCategory::streamPrint

 print, '***********************'
 print, '**Start of<',OBJ_CLASS(self),'>**'

 print, '**** fileName:<', self.fileName,'>'
 thisList=*self.list
 for i=0, n_elements(thisList)-1 do begin
	print, '**element n.<', strcompress(i, /REMOVE),'>'
	print, '**** code:<', thisList[i].code,'>'
	print, '**** observedCode:<', thisList[i].observedCode,'>'
	print, '**** categoryCode:<', thisList[i].categoryCode,'>'
	print, '**** value:<', thisList[i].value,'>'
	print, '**'
 endfor

 print, '***********************'
 print, '**End of:<',OBJ_CLASS(self),'>**'

END

PRO ObservedCategory::fillDataFromFile, fileName

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
 obsCats=getFMObservedCategory()
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
		if n_elements(info) eq 4 then begin
			thisObsCat=getFMObservedCategory()
			thisObsCat.code=fix(info[0])
 			thisObsCat.observedCode=info[1]
 			thisObsCat.categoryCode=info[2]
			thisObsCat.value=info[3]
			obsCats=[obsCats, thisObsCat]
		endif else begin
			print, 'Bad conf file at line', i, bufferString
		endelse
	endelse
 endwhile
 close, unit & free_lun, unit
 self.list=ptr_new(obsCats[1:*], /NO_COPY)

END

FUNCTION ObservedCategory::init

 if not(self -> ConfigurableData::init()) then return, 0
 return, 1

END

PRO ObservedCategory::cleanUp

 self -> ConfigurableData::cleanUp

END

PRO ObservedCategory__Define

Struct = { ObservedCategory , $
		Inherits ConfigurableData $
		 }

END