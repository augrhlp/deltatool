;********************
@structure_definition
;********************
FUNCTION MonitoringGroupStat::getTitle

 return, 'Monitoring Group stats'

END

FUNCTION MonitoringGroupStat::getDisplayNames

 thisList=*self.list
 return, thisList[*].displayName

END

FUNCTION MonitoringGroupStat::getCodes

 thisList=*self.list
 return, thisList[*].code

END

FUNCTION MonitoringGroupStat::getValues

 thisList=*self.list
 return, thisList[*].value

END

FUNCTION MonitoringGroupStat::getOperations

 thisList=*self.list
 return, thisList[*].operation

END

PRO MonitoringGroupStat::streamPrint

 print, '***********************'
 print, '**Start of <',OBJ_CLASS(self),'>**'

 print, '**** fileName:<', self.fileName,'>'
 thisList=*self.list
 for i=0, n_elements(thisList)-1 do begin
	print, '**element n.<', strcompress(i, /REMOVE), '>'
	print, '**** code:<', thisList[i].code, '>'
	print, '**** displayName:<', thisList[i].displayName, '>'
	print, '**** value:<', thisList[i].value, '>'
	print, '**** operation:<', thisList[i].operation, '>'
	print, '**'
 endfor

 print, '***********************'
 print, '**End of:<',OBJ_CLASS(self),'>**'

END

PRO MonitoringGroupStat::fillDataFromFile, fileName

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
 MonitoringGroupStats=getFMMonitoringGroupStat()
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
			thisMonitoringGroupStat=getFMMonitoringGroupStat()
			thisMonitoringGroupStat.code=info[0]
 			thisMonitoringGroupStat.displayName=info[1]
			thisMonitoringGroupStat.value=info[2]
			thisMonitoringGroupStat.operation=info[3]
			MonitoringGroupStats=[MonitoringGroupStats, thisMonitoringGroupStat]
		endif else begin
			print, 'Bad conf file at line', i, bufferString
		endelse
	endelse
 endwhile
 close, unit & free_lun, unit
 self.list=ptr_new(MonitoringGroupStats[1:*], /NO_COPY)

END

FUNCTION MonitoringGroupStat::init

 if not(self -> ConfigurableData::init()) then return, 0
 return, 1

END

PRO MonitoringGroupStat::cleanUp

 self -> ConfigurableData::cleanUp

END

PRO MonitoringGroupStat__Define

Struct = { MonitoringGroupStat , $
		Inherits ConfigurableData $
		 }

END