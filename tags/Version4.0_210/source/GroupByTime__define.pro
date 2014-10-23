;********************
@structure_definition
;********************
FUNCTION GroupByTime::getValueByIndex, index

 thisList=*self.list
 return, thisList[index].value

END

FUNCTION GroupByTime::getTimeStampByIndex, index

 thisList=*self.list
 return, thisList[index].timeStamp

END

FUNCTION GroupByTime::getTitle

 return, 'Time Avg'

END

FUNCTION GroupByTime::getDisplayNames

 thisList=*self.list
; elemNo=n_elements(thisList)
; dNList=strarr(elemNo)
; for i=0, elemNo-1 do dNList[i]=thisList[*].displayName
 return, thisList[*].displayName

END

FUNCTION GroupByTime::getCodes

 thisList=*self.list
; elemNo=n_elements(thisList)
; codeList=strarr(elemNo)
; for i=0, elemNo-1 do codeList[i]=thisList[*].code
 return, thisList[*].code

END

FUNCTION GroupByTime::getValues

 thisList=*self.list
; elemNo=n_elements(thisList)
; codeList=strarr(elemNo)
; for i=0, elemNo-1 do codeList[i]=thisList[*].code
 return, thisList[*].value

END

FUNCTION GroupByTime::getTimeStamps

 thisList=*self.list
; elemNo=n_elements(thisList)
; codeList=strarr(elemNo)
; for i=0, elemNo-1 do codeList[i]=thisList[*].code
 return, thisList[*].timeStamp

END

PRO GroupByTime::streamPrint

 print, '***********************'
 print, '**Start of<',OBJ_CLASS(self),'>**'

 print, '**** fileName:<', self.fileName,'>'
 thisList=*self.list
 for i=0, n_elements(thisList)-1 do begin
	print, '**element n.<', strcompress(i, /REMOVE),'>'
	print, '**** code:<', thisList[i].code,'>'
	print, '**** displayName:<', thisList[i].displayName,'>'
	print, '**** value:<', thisList[i].value,'>'
	print, '**** timeStamp:<', thisList[i].timeStamp,'>'
	print, '**'
 endfor

 print, '***********************'
 print, '**End of:<',OBJ_CLASS(self),'>**'

END

PRO GroupByTime::fillDataFromFile, fileName

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
 groupByTimes=getFMGroupByTime()
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
			thisGroupByTime=getFMGroupByTime()
			thisGroupByTime.code=fix(info[0])
 			thisGroupByTime.displayName=info[1]
			thisGroupByTime.value=info[2]
			thisGroupByTime.timeStamp=info[3]
			groupByTimes=[groupByTimes, thisGroupByTime]
		endif else begin
			print, 'Bad conf file at line', i, bufferString
		endelse
	endelse
 endwhile
 close, unit & free_lun, unit
 self.list=ptr_new(groupByTimes[1:*], /NO_COPY)

END

FUNCTION GroupByTime::init

 if not(self -> ConfigurableData::init()) then return, 0
 return, 1

END

PRO GroupByTime::cleanUp

 self -> ConfigurableData::cleanUp

END

PRO GroupByTime__Define

Struct = { GroupByTime , $
		Inherits ConfigurableData $
		 }

END