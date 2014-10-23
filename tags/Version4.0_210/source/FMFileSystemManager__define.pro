;********************
@structure_definition
;********************
FUNCTION FMFileSystemManager::checkStartupFileContents, txt=txt, alltxt=alltxt

  alltxt=''
  openr, unit, self->getStartUpFileName(), /GET_LUN
  
  ; copied from Check_io
  check_MODEL=0
  check_PARAMETERS=0
  check_MONITORING=0
  while ~eof(unit) do begin
    atxt=discardComments(unit)
    if atxt eq '[MODEL]' then begin
      check_MODEL=1
      atxt=discardComments(unit)
      fileYear=atxt
      utility=obj_new('FMUtility')
      if not(utility->IsNumber(fileYear)) then begin
        txt='STEP 03: STOP! MODEL first line is NE a year : See MODEL section in STARTUPfile'
        alltxt=[txt,alltxt]
        ;          widget_control,labcom_txt,set_value=alltxt
        alltxt=['STOP',alltxt]
        ;          widget_control,labcom_txt,set_value=alltxt
        ;          close,11
        ;          close,12
        ;          ierror=1
        return,0
      endif
      atxt=discardComments(unit)
      frequency=strlowcase(atxt)
      if strupcase(frequency) ne 'HOUR' and strupcase(frequency) ne 'YEAR' then begin
        txt='STEP 03: STOP! MODEL second line is NE a hour and NE to year : See MODEL section in STARTUPfile'
        alltxt=[txt,alltxt]
        ;          widget_control,labcom_txt,set_value=alltxt
        alltxt=['STOP',alltxt]
        ;          widget_control,labcom_txt,set_value=alltxt
        ;          close,11
        ;          close,12
        ;          ierror=1
        return,0
      endif
      atxt=discardComments(unit)
      scale=strlowcase(atxt)
      if (scale ne 'local' and scale ne 'urban' and scale ne 'regional') then begin
        txt='STEP 03: STOP! MODEL third line is NE local/urban/regional: See MODEL section in STARTUPfile'
        alltxt=[txt,alltxt]
        ;          widget_control,labcom_txt,set_value=alltxt
        alltxt=['STOP',alltxt]
        ;          widget_control,labcom_txt,set_value=alltxt
        ;          close,11
        ;          close,12
        ;          ierror=1
        return,0
      endif
      txt='STEP 03 OK: MODEL / '+fileYear+' '+frequency+' '+scale+' section exists in STARTUPfile'
      ;        printf,11,txt
      alltxt=[txt,alltxt]
      ;        widget_control,labcom_txt,set_value=alltxt
      continue
      if fix(fileYear) lt 1900 or fix(fileYear) gt 2100 then begin
        txt='STEP 03: WARNING! YEAR LT 1900 or YEAR GT 2100. MODEL section in STARTUPfile'
        alltxt=[txt,alltxt]
        ;          widget_control,labcom_txt,set_value=alltxt
        alltxt=['WARNING',alltxt]
      ;          widget_control,labcom_txt,set_value=alltxt
      endif else begin
        ;          printf,11,'STEP 03 OK: 1900 < YEAR < 2100'
        txt='STEP 03 OK: 1900 < YEAR < 2100'
        alltxt=[txt,alltxt]
        ;          widget_control,labcom_txt,set_value=alltxt
        continue
      endelse
    endif
    if atxt eq '[PARAMETERS]' then begin
      check_PARAMETERS=1
      readf,unit,atxt
      if strmid(atxt,0,1) ne ';' then begin
      txt='STEP 03: STOP! Line after [PARAMETERS] in STARTUPfile does not start with ;'
      alltxt=[txt,alltxt]
      ;        widget_control,labcom_txt,set_value=alltxt
      alltxt=['STOP',alltxt]
      ;        widget_control,labcom_txt,set_value=alltxt
      ;          close,11
      ;          close,12
      ;          ierror=1
      return,0
    endif else begin
      ;        printf,11,'STEP 03 OK: PARAMETERS section exists in STARTUPfile'
      txt='STEP 03 OK: PARAMETERS section exists in STARTUPfile'
      alltxt=[txt,alltxt]
    ;        widget_control,labcom_txt,set_value=alltxt
    endelse
  endif
  if atxt eq '[MONITORING]' then begin
    check_MONITORING=1
    atxt=discardComments(unit)
    if strmid(atxt,0,4) ne 'Stat' then begin
      txt='STEP 03: STOP! Line after [MONITORING] in STARTUPfile does not start with Stat'
      alltxt=[txt,alltxt]
      ;        widget_control,labcom_txt,set_value=alltxt
      alltxt=['STOP',alltxt]
      ;        widget_control,labcom_txt,set_value=alltxt
      ;          close,11
      ;          close,12
      ;          ierror=1
      return,0
    endif else begin
      ;        printf,11,'STEP 03 OK: MONITORING section exists in STARTUPfile'
      txt='STEP 03 OK: MONITORING section exists in STARTUPfile'
      alltxt=[txt,alltxt]
      ;        widget_control,labcom_txt,set_value=alltxt
      continue
    endelse
  endif
endwhile
if check_MODEL eq 0 then begin
  txt='STEP 03: STOP! No [MODEL] section in STARTUPfile file or check spelling'
  alltxt=[txt,alltxt]
  ;    widget_control,labcom_txt,set_value=alltxt
  alltxt=['STOP',alltxt]
  ;    widget_control,labcom_txt,set_value=alltxt
  ;          close,11
  ;          close,12
  ;          ierror=1
  return,0
endif
if check_PARAMETERS eq 0 then begin
  txt='STEP 03: STOP! No [PARAMETERS] section in STARTUPfile file or check spelling'
  alltxt=[txt,alltxt]
  ;    widget_control,labcom_txt,set_value=alltxt
  alltxt=['STOP',alltxt]
  ;    widget_control,labcom_txt,set_value=alltxt
  ;          close,11
  ;          close,12
  ;          ierror=1
  return,0
endif
if check_MONITORING eq 0 then begin
  txt='STEP 03: STOP! No [MONITORING] section in STARTUPfile file or check spelling'
  alltxt=[txt,alltxt]
  ;    widget_control,labcom_txt,set_value=alltxt
  alltxt=['STOP',alltxt]
  ;    widget_control,labcom_txt,set_value=alltxt
  ;          close,11
  ;          close,12
  ;          ierror=1
  return,0
endif
;  printf,11,' '
;  widget_control,labok(3),set_value=' *OK* '

;  while ~eof(unit) do begin
;    atxt=discardComments(unit)
;    if atxt eq '[MODEL]' then begin
;      ;if atxt eq '[SCALE]' then begin
;      check_appl=0
;      check_MODEL=1
;      atxt=discardComments(unit)
;      fileYear=atxt
;      utility=obj_new('FMUtility')
;      if not(utility->IsNumber(fileYear)) then begin
;        txt='STEP 03: STOP! Model first line is NE a year : See MODEL section in STARTUPfile'
;        alltxt=[txt,alltxt]
;        alltxt=['STOP',alltxt]
;        return, 0
;      endif
;      atxt=discardComments(unit)
;      frequency=atxt
;      ;here add Data Assimilation control, now just skip it...
;      atxt=discardComments(unit)
;      scale=atxt
;      if (strlowcase(scale) eq 'local' or strlowcase(scale) eq 'urban' or strlowcase(scale) eq 'regional' or $
;        strlowcase(scale) eq 'traffic') then check_appl=1
;      if check_appl eq 0 then begin
;        txt='STEP 03: STOP! Model third line is NE local/urban/regional/traffic: See MODEL section in STARTUPfile'
;        alltxt=[txt,alltxt]
;        alltxt=['STOP',alltxt]
;        return, 0
;      endif else begin
;        txt='STEP 03 OK: MODEL / '+fileYear+' '+frequency+' '+scale+' section exists in STARTUPfile'
;        alltxt=[txt,alltxt]
;        continue
;      endelse
;      if fix(fileYear) lt 1900 or fix(fileYear) gt 2100 then begin
;        txt='STEP 03: WARNING! YEAR LT 1900 or YEAR GT 2100. MODEL section in STARTUPfile'
;        alltxt=[txt,alltxt]
;        alltxt=['WARNING',alltxt]
;      endif else begin
;        txt='STEP 03 OK: 1900 < YEAR < 2100'
;        alltxt=[txt,alltxt]
;        continue
;      endelse
;    endif
;    ;atxt=discardComments(1)
;    if atxt eq '[PARAMETERS]' then begin
;      check_PARAMETERS=1
;      readf,unit,atxt
;      if strmid(atxt,0,1) ne ';' then begin
;      txt='STEP 03: STOP! Line after [PARAMETERS] in STARTUPfile does not start with ;'
;      alltxt=[txt,alltxt]
;      alltxt=['STOP',alltxt]
;      return, 0
;    endif else begin
;      txt='STEP 03 OK: PARAMETERS section exists in STARTUPfile'
;      alltxt=[txt,alltxt]
;    endelse
;  endif
;  ;atxt=discardComments(unit)
;  if atxt eq '[MONITORING]' then begin
;    check_MONITORING=1
;    atxt=discardComments(unit)
;    if strmid(atxt,0,4) ne 'Stat' then begin
;      txt='STEP 03: STOP! Line after [MONITORING] in STARTUPfile does not start with Stat'
;      alltxt=[txt,alltxt]
;      alltxt=['STOP',alltxt]
;      return, 0
;    endif else begin
;      txt='STEP 03 OK: MONITORING section exists in STARTUPfile'
;      alltxt=[txt,alltxt]
;      continue
;    endelse
;  endif
;endwhile
close, unit & free_lun, unit
return, 1

END

PRO FMFileSystemManager::modelFrequencyRename, dir, fileName, frequencyType

  prevExtension=self->getExtension(fileName)
  newFileName=self->getBaseFileName(fileName)
  newFileName=newFileName+'_'+frequencyType
  newFileName=self->setExtension(newFileName, prevExtension)
  newFullFileName=dir+newFileName
  fullFileName=dir+fileName
  self->fileCopy, newFullFileName, fullFileName, /OVERWRITE
  
END

PRO  FMFileSystemManager::fileCopy, sourceFile, targetFile, OVERWRITE=OVERWRITE

  file_copy, sourceFile, targetFile, OVERWRITE=OVERWRITE
  
END

PRO  FMFileSystemManager::fileRename, sourceFile, targetFile, OVERWRITE=OVERWRITE

  file_move, sourceFile, targetFile, OVERWRITE=OVERWRITE
  
END

FUNCTION FMFileSystemManager::getBaseFileName, fileName, PRESERVE_PATH=PRESERVE_PATH, PRESERVE_EXTENSION=PRESERVE_EXTENSION

  bName=fileName
  if ~keyword_set(PRESERVE_PATH) then begin
    bNameStartPos=strpos(bName, self->getSystemDirSeparator(), /REVERSE_SEARCH)
    if bNameStartPos ne -1 then bName=strmid(fileName, bNameStartPos+1, strlen(fileName)-bNameStartPos)
  endif
  if ~keyword_set(PRESERVE_EXTENSION) then begin
    bNameDotStartPos=strpos(bName, '.', /REVERSE_SEARCH)
    if bNameDotStartPos ne -1 then bName=strmid(bName, 0, bNameDotStartPos)
  endif
  return,  bName
  
END

FUNCTION FMFileSystemManager::getFileName, fileName

  fName=strsplit(fileName, self.oSDirSeparator, /EXTRACT)
  return, fName[n_elements(fName)-1]
  
END

FUNCTION FMFileSystemManager::setExtension, filename, newExtension

  newName=self->getBaseFileName(filename, /PRESERVE_PATH)
  ;newName=prefix[0]
  return, newName+ newExtension
  
END

FUNCTION FMFileSystemManager::getExtension, filename

  extPos=strpos(filename, '.', /REVERSE_SEARCH)
  return, strmid(filename, extPos, strlen(filename)-extPos)
  
END

FUNCTION FMFileSystemManager::buildValuesStream, pars

  no=n_elements(pars)
  values=strcompress(randomu(seed, no), /REMOVE)
  vals=''
  for i=0, no-1 do vals=vals+values[i]+';'
  return, vals
  
END

FUNCTION FMFileSystemManager::buildUniqParameterList, monitored, run

  monNo=n_elements(monitored)
  runNo=n_elements(run)
  parList=*(monitored[0])
  
  for i=1, monNo-1 do parList=[parList, *(monitored[i])]
  for i=0, monNo-1 do parList=[parList, *(run[i])]
  parList=parList[UNIQ(parList, SORT(parList))]
  return, parList
  
END
; *****************************
; resource building
; *****************************
PRO FMFileSystemManager::buildElaboration, unit, filename, elabs, parList, WRITE=WRITE

  if keyword_set(WRITE) then openw, unit, filename ;"ELABORATION"
  
  totParNumbers=n_elements(parList)
  parCodes=parList
  
  ;**********************************
  ;Previous version
  ;Code,Type_Code,parameter_Code,Predef_parameter_Code,Plot_Code,Goals_Criteria_OC_Code,
  ;Group_By_Stat_Code,Group_By_Time_Code,IDL_Routine_Code,$
  ;Period_Code,Display_Name,User_Group_By_Stat_Selection,
  ;User_Group_By_Time_Selection,User_Par_Selection,Need_Ref_Value,$
  ;Par_Min_Number,Spec_Max_Number,Description
  ;Confirm version
  ;Code,Diagram_Code,Parameter_Codes,Axis_Code,Goals_Criteria_OC_Code,
  ;Group_By_Stat_Code,Group_By_Time_Code,IDL_Routine_Code,$
  ;Period_Code,Display_Name,Description
  
  header='[ELABORATION: Code,DisplayName,DiagramCode,AxisCodes,'
  header=header+'ParameterCodes,GroupByTimeCode,GroupByStatCode,'
  header=header+'DailyCode,SeasonCode,GoalsCriteriaOCCode,'
  header=header+'IDLRoutineCode,NumberRefValue,Description'
  
  ;  elaborationList=['Mean_T','Mean_max_T','Mean_min_T','Stddev_T','CorrCoef_T','Bias_T','NBias_T','RMSE_T',$
  ;    'NRMSE_T','CRMSE_T','SigM/sigO_T','ExcDays','AOTx','SOMOx','PERCx','TimeSeries','ScatPlot','ScatPlotAll',$
  ;    'FreqA','FreqA_err','Q-Q Plot','Taylor']
  ;  elabsNumber=n_elements(elaborationList)
  
  if keyword_set(WRITE) then printf, unit, header else print, header
  k=0
  predefExclusive=['FREE','NONE']
  
  for i=0, n_elements(elabs.bpElaborationList)-1 do begin
    code=strcompress(k, /REMOVE)
    displayName=elabs.bpElaborationList[i]
    diagramCode='0'; BarPlot
    axisCodes='0*1*2*3*4*5*6';
    k++
    parCodes=['']
    parNumber=fix(randomu(seed)*1000) mod totParNumbers
    parNumber=  (parNumber > 1) < 6
    for j=0, parNumber-1 do begin
      thisParIndex=fix(randomu(seed)*1000 mod totParNumbers)
      thisParCode=parList[thisParIndex]
      parCodes=[parCodes, thisParCode]
    endfor
    ;print, '********', parCodes, '*********'
    if randomu(seed) gt .7 then parCodes=strmid(self.utility->buildWithAsterisk(parCodes), 1, 100) else parCodes="ALL"
    ;if randomu(seed) gt .9 then predefParCodes=strmid(self.utility->buildWithAsterisk(predefParCodes), 1, 100) else predefParCodes="NONE"
    groupByTimeIdx=fix(randomu(seed)*1000) mod 3 & groupByTimeVal=fix(randomu(seed)*1000) mod 5
    if groupByTimeIdx eq 2 then groupByTimeCode=strcompress(groupByTimeVal, /REMOVE) else groupByTimeCode=predefExclusive[groupByTimeIdx] ;FREE*NONE*PREDEFINED if a value is set is predefined and fixed
    groupByStatIdx=fix(randomu(seed)*1000) mod 3 & groupByStatVal=fix(randomu(seed)*1000) mod 4
    if groupByStatIdx eq 2 then groupByStatCode=strcompress(groupByStatVal, /REMOVE) else groupByStatCode=predefExclusive[groupByStatIdx] ;FREE*NONE*PREDEFINED if a value is set is predefined and fixed
    seasonIdx=fix(randomu(seed)*1000) mod 3 & seasonVal=fix(randomu(seed)*1000) mod 3
    if seasonIdx eq 2 then seasonCode=strcompress(seasonVal, /REMOVE) else seasonCode=predefExclusive[seasonIdx] ;FREE*NONE*PREDEFINED if a value is set is predefined and fixed
    dailyIdx=fix(randomu(seed)*1000) mod 3 & dailyVal=fix(randomu(seed)*1000) mod 3
    if dailyIdx eq 2 then dailyCode=strcompress(dailyVal, /REMOVE) else dailyCode=predefExclusive[dailyIdx] ;FREE*NONE*PREDEFINED if a value is set is predefined and fixed
    if randomu(seed) gt .99 then goalsCriteriaCode='2' else goalsCriteriaCode="NONE"
    if randomu(seed) gt .8 then numberRefValue=strcompress(fix(randomu(seed)*1000) mod 3, /REMOVE)  else numberRefValue='0'
    iDLRoutinCode=strcompress(k, /REMOVE)
    ;periodCodes="ALL" ;ALL, if value(s) is set is predefined and fixed
    ;userGByStatSel="TRUE" ;TRUE or FALSE
    ;userGByTimeSel="FALSE" ;TRUE or FALSE
    ;userParSel="TRUE" ;TRUE or FALSE
    ;parMinNumber=strcompress(1, /REMOVE_ALL)
    ;parMaxNumber=strcompress(1, /REMOVE_ALL)
    description='Descr of: '+elabs.bpElaborationList[i]
    record=code+';'+displayName+';'+diagramCode+';'+axisCodes+';'+$
    parCodes+';'+groupByTimeCode+';'+groupByStatCode+';'+$
    dailyCode+';'+seasonCode+';'+goalsCriteriaCode+';'+$
    iDLRoutinCode+';'+numberRefValue+';'+description
    ;    record=code+';'+thisDiagramCode+';'+avParCodes+';'+$
    ;    predefParCodes+';'+axisCode+';'+goalsCriteriaCode+';'+groupByStatCodes+';'+groupByTimeCodes+';'+$
    ;    iDLRoutinCode+';'+periodCodes+';'+displayName+';'+userGByStatSel+';'+userGByTimeSel+';'+$
    ;    userParSel+';'+needRefValue+';'+parMinNumber+';'+specMaxNumber+';'+descr
    if keyword_set(WRITE) then printf, unit, record else print,record
  endfor
  
  for i=0, n_elements(elabs.tsElaborationList)-1 do begin
    code=strcompress(k, /REMOVE)
    displayName=elabs.tsElaborationList[i]
    diagramCode='1'; Time Series
    axisCodes='7*8*9*10*11';
    k++
    parCodes=['']
    parNumber=fix(randomu(seed)*1000) mod totParNumbers
    parNumber=  (parNumber > 1) < 6
    for j=0, parNumber-1 do begin
      thisParIndex=fix(randomu(seed)*1000 mod totParNumbers)
      thisParCode=parList[thisParIndex]
      parCodes=[parCodes, thisParCode]
    endfor
    if randomu(seed) gt .7 then parCodes=strmid(self.utility->buildWithAsterisk(parCodes), 1, 100) else parCodes="ALL"
    groupByTimeIdx=fix(randomu(seed)*1000) mod 3 & groupByTimeVal=fix(randomu(seed)*1000) mod 5
    if groupByTimeIdx eq 2 then groupByTimeCode=strcompress(groupByTimeVal, /REMOVE) else groupByTimeCode=predefExclusive[groupByTimeIdx] ;FREE*N/A*PREDEFINED if a value is set is predefined and fixed
    groupByStatIdx=fix(randomu(seed)*1000) mod 3 & groupByStatVal=fix(randomu(seed)*1000) mod 4
    if groupByStatIdx eq 2 then groupByStatCode=strcompress(groupByStatVal, /REMOVE) else groupByStatCode=predefExclusive[groupByStatIdx] ;FREE*N/A*PREDEFINED if a value is set is predefined and fixed
    seasonIdx=fix(randomu(seed)*1000) mod 3 & seasonVal=fix(randomu(seed)*1000) mod 3
    if seasonIdx eq 2 then seasonCode=strcompress(seasonVal, /REMOVE) else seasonCode=predefExclusive[seasonIdx] ;FREE*N/A*PREDEFINED if a value is set is predefined and fixed
    dailyIdx=fix(randomu(seed)*1000) mod 3 & dailyVal=fix(randomu(seed)*1000) mod 3
    if dailyIdx eq 2 then dailyCode=strcompress(dailyVal, /REMOVE) else dailyCode=predefExclusive[dailyIdx] ;FREE*N/A*PREDEFINED if a value is set is predefined and fixed
    if randomu(seed) gt .99 then goalsCriteriaCode='2' else goalsCriteriaCode="NONE"
    if randomu(seed) gt .8 then numberRefValue=strcompress(fix(randomu(seed)*1000) mod 3, /REMOVE)  else numberRefValue='0'
    iDLRoutinCode=strcompress(k, /REMOVE)
    description='Descr of: '+elabs.tsElaborationList[i]
    record=code+';'+displayName+';'+diagramCode+';'+axisCodes+';'+$
    parCodes+';'+groupByTimeCode+';'+groupByStatCode+';'+$
    dailyCode+';'+seasonCode+';'+goalsCriteriaCode+';'+$
    iDLRoutinCode+';'+numberRefValue+';'+description
    if keyword_set(WRITE) then printf, unit, record else print,record
  endfor
  
  for i=0, n_elements(elabs.spElaborationList)-1 do begin
    code=strcompress(k, /REMOVE)
    displayName=elabs.spElaborationList[i]
    diagramCode='2'; Scatter Plot
    axisCodes='7*8*9*10*11';
    k++
    parCodes=['']
    parNumber=fix(randomu(seed)*1000) mod totParNumbers
    parNumber=  (parNumber > 1) < 6
    for j=0, parNumber-1 do begin
      thisParIndex=fix(randomu(seed)*1000 mod totParNumbers)
      thisParCode=parList[thisParIndex]
      parCodes=[parCodes, thisParCode]
    endfor
    if randomu(seed) gt .7 then parCodes=strmid(self.utility->buildWithAsterisk(parCodes), 1, 100) else parCodes="ALL"
    groupByTimeIdx=fix(randomu(seed)*1000) mod 3 & groupByTimeVal=fix(randomu(seed)*1000) mod 5
    if groupByTimeIdx eq 2 then groupByTimeCode=strcompress(groupByTimeVal, /REMOVE) else groupByTimeCode=predefExclusive[groupByTimeIdx] ;FREE*N/A*PREDEFINED if a value is set is predefined and fixed
    groupByStatIdx=fix(randomu(seed)*1000) mod 3 & groupByStatVal=fix(randomu(seed)*1000) mod 4
    if groupByStatIdx eq 2 then groupByStatCode=strcompress(groupByStatVal, /REMOVE) else groupByStatCode=predefExclusive[groupByStatIdx] ;FREE*N/A*PREDEFINED if a value is set is predefined and fixed
    seasonIdx=fix(randomu(seed)*1000) mod 3 & seasonVal=fix(randomu(seed)*1000) mod 3
    if seasonIdx eq 2 then seasonCode=strcompress(seasonVal, /REMOVE) else seasonCode=predefExclusive[seasonIdx] ;FREE*N/A*PREDEFINED if a value is set is predefined and fixed
    dailyIdx=fix(randomu(seed)*1000) mod 3 & dailyVal=fix(randomu(seed)*1000) mod 3
    if dailyIdx eq 2 then dailyCode=strcompress(dailyVal, /REMOVE) else dailyCode=predefExclusive[dailyIdx] ;FREE*N/A*PREDEFINED if a value is set is predefined and fixed
    if randomu(seed) gt .99 then goalsCriteriaCode='2' else goalsCriteriaCode="NONE"
    if randomu(seed) gt .8 then numberRefValue=strcompress(fix(randomu(seed)*1000) mod 3, /REMOVE)  else numberRefValue='0'
    iDLRoutinCode=strcompress(k, /REMOVE)
    description='Descr of: '+elabs.spElaborationList[i]
    record=code+';'+displayName+';'+diagramCode+';'+axisCodes+';'+$
    parCodes+';'+groupByTimeCode+';'+groupByStatCode+';'+$
    dailyCode+';'+seasonCode+';'+goalsCriteriaCode+';'+$
    iDLRoutinCode+';'+numberRefValue+';'+description
    if keyword_set(WRITE) then printf, unit, record else print,record
  endfor
  
  for i=0, n_elements(elabs.tpElaborationList)-1 do begin
    code=strcompress(k, /REMOVE)
    displayName=elabs.tpElaborationList[i]
    diagramCode='3'; Taylor Plot
    axisCodes='7*8*9*10*11';
    k++
    parCodes=['']
    parNumber=fix(randomu(seed)*1000) mod totParNumbers
    parNumber=  (parNumber > 1) < 6
    for j=0, parNumber-1 do begin
      thisParIndex=fix(randomu(seed)*1000 mod totParNumbers)
      thisParCode=parList[thisParIndex]
      parCodes=[parCodes, thisParCode]
    endfor
    if randomu(seed) gt .7 then parCodes=strmid(self.utility->buildWithAsterisk(parCodes), 1, 100) else parCodes="ALL"
    groupByTimeIdx=fix(randomu(seed)*1000) mod 3 & groupByTimeVal=fix(randomu(seed)*1000) mod 5
    if groupByTimeIdx eq 2 then groupByTimeCode=strcompress(groupByTimeVal, /REMOVE) else groupByTimeCode=predefExclusive[groupByTimeIdx] ;FREE*N/A*PREDEFINED if a value is set is predefined and fixed
    groupByStatIdx=fix(randomu(seed)*1000) mod 3 & groupByStatVal=fix(randomu(seed)*1000) mod 4
    if groupByStatIdx eq 2 then groupByStatCode=strcompress(groupByStatVal, /REMOVE) else groupByStatCode=predefExclusive[groupByStatIdx] ;FREE*N/A*PREDEFINED if a value is set is predefined and fixed
    seasonIdx=fix(randomu(seed)*1000) mod 3 & seasonVal=fix(randomu(seed)*1000) mod 3
    if seasonIdx eq 2 then seasonCode=strcompress(seasonVal, /REMOVE) else seasonCode=predefExclusive[seasonIdx] ;FREE*N/A*PREDEFINED if a value is set is predefined and fixed
    dailyIdx=fix(randomu(seed)*1000) mod 3 & dailyVal=fix(randomu(seed)*1000) mod 3
    if dailyIdx eq 2 then dailyCode=strcompress(dailyVal, /REMOVE) else dailyCode=predefExclusive[dailyIdx] ;FREE*N/A*PREDEFINED if a value is set is predefined and fixed
    if randomu(seed) gt .99 then goalsCriteriaCode='2' else goalsCriteriaCode="NONE"
    if randomu(seed) gt .8 then numberRefValue=strcompress(fix(randomu(seed)*1000) mod 3, /REMOVE)  else numberRefValue='0'
    iDLRoutinCode=strcompress(k, /REMOVE)
    description='Descr of: '+elabs.tpElaborationList[i]
    record=code+';'+displayName+';'+diagramCode+';'+axisCodes+';'+$
    parCodes+';'+groupByTimeCode+';'+groupByStatCode+';'+$
    dailyCode+';'+seasonCode+';'+goalsCriteriaCode+';'+$
    iDLRoutinCode+';'+numberRefValue+';'+description
    if keyword_set(WRITE) then printf, unit, record else print,record
  endfor
  
  for i=0, n_elements(elabs.tdElaborationList)-1 do begin
    code=strcompress(k, /REMOVE)
    displayName=elabs.tdElaborationList[i]
    diagramCode='4'; 2D
    axisCodes='10*11';
    k++
    parCodes=['']
    parNumber=fix(randomu(seed)*1000) mod totParNumbers
    parNumber=  (specNumber > 1) < 6
    for j=0, parNumber-1 do begin
      thisParIndex=fix(randomu(seed)*1000 mod totParNumbers)
      thisParCode=parList[thisParIndex]
      parCodes=[parCodes, thisParCode]
    endfor
    if randomu(seed) gt .7 then parCodes=strmid(self.utility->buildWithAsterisk(parCodes), 1, 100) else parCodes="ALL"
    groupByTimeIdx=fix(randomu(seed)*1000) mod 3 & groupByTimeVal=fix(randomu(seed)*1000) mod 5
    if groupByTimeIdx eq 2 then groupByTimeCode=strcompress(groupByTimeVal, /REMOVE) else groupByTimeCode=predefExclusive[groupByTimeIdx] ;FREE*N/A*PREDEFINED if a value is set is predefined and fixed
    groupByStatIdx=fix(randomu(seed)*1000) mod 3 & groupByStatVal=fix(randomu(seed)*1000) mod 4
    if groupByStatIdx eq 2 then groupByStatCode=strcompress(groupByStatVal, /REMOVE) else groupByStatCode=predefExclusive[groupByStatIdx] ;FREE*N/A*PREDEFINED if a value is set is predefined and fixed
    seasonIdx=fix(randomu(seed)*1000) mod 3 & seasonVal=fix(randomu(seed)*1000) mod 3
    if seasonIdx eq 2 then seasonCode=strcompress(seasonVal, /REMOVE) else seasonCode=predefExclusive[seasonIdx] ;FREE*N/A*PREDEFINED if a value is set is predefined and fixed
    dailyIdx=fix(randomu(seed)*1000) mod 3 & dailyVal=fix(randomu(seed)*1000) mod 3
    if dailyIdx eq 2 then dailyCode=strcompress(dailyVal, /REMOVE) else dailyCode=predefExclusive[dailyIdx] ;FREE*N/A*PREDEFINED if a value is set is predefined and fixed
    if randomu(seed) gt .99 then goalsCriteriaCode='2' else goalsCriteriaCode="NONE"
    if randomu(seed) gt .8 then numberRefValue=strcompress(fix(randomu(seed)*1000) mod 3, /REMOVE)  else numberRefValue='0'
    iDLRoutinCode=strcompress(k, /REMOVE)
    description='Descr of: '+elabs.tdElaborationList[i]
    record=code+';'+displayName+';'+diagramCode+';'+axisCodes+';'+$
    parCodes+';'+groupByTimeCode+';'+groupByStatCode+';'+$
    dailyCode+';'+seasonCode+';'+goalsCriteriaCode+';'+$
    iDLRoutinCode+';'+numberRefValue+';'+description
    if keyword_set(WRITE) then printf, unit, record else print,record
  endfor
  
  if keyword_set(WRITE) then close, unit else print, '************EOF***************'
  
END

PRO FMFileSystemManager::buildModel, unit, fileName, modelList, WRITE=WRITE
  ; ****************** Model section
  if keyword_set(WRITE) then openw, unit, filename ;"MODEL"
  ;Code,DisplayName,Description
  
  modelNumbers=n_elements(modelList)
  
  header='["MODEL": Code,DisplayName,Description]'
  if keyword_set(WRITE) then printf, unit, header else print, header
  for i=0, modelNumbers-1 do begin
    ;code=strcompress(i, /REMOVE)
    code=modelList[i]
    displayName=modelList[i]
    descr='Descr of: '+modelList[i]
    record=code+';'+displayName+';'+descr
    if keyword_set(WRITE) then printf, unit, record else print,record
  endfor
  if keyword_set(WRITE) then close, unit else print, '************EOF***************'
  
END

PRO FMFileSystemManager::buildScenario, unit, fileName, scenarioList, WRITE=WRITE

  if keyword_set(WRITE) then openw, unit, filename ;"SCENARIO"
  ;Code,DisplayName,Description
  
  scenarioNumbers=n_elements(scenarioList)
  
  header='["SCENARIO": Code,DisplayName,Description]'
  if keyword_set(WRITE) then printf, unit, header else print, header
  for i=0, n_elements(scenarioList)-1 do begin
    ;code=strcompress(i, /REMOVE)
    code=scenarioList[i]
    displayName=scenarioList[i]
    descr='Descr of: '+scenarioList[i]
    record=code+';'+displayName+';'+descr
    if keyword_set(WRITE) then printf, unit, record else print,record
  endfor
  if keyword_set(WRITE) then close, unit else print, '************EOF***************'
  
END

PRO   FMFileSystemManager::buildGroupByTime, unit, filename, groupByTimeValues, groupByTimeTS, WRITE=WRITE

  if keyword_set(WRITE) then OPENW, unit, filename ;"GROUP_BY_TIME"
  groupByTimeValues=['preserve', '01', '08', '01', '01']
  groupByTimeTS=['none', 'hh', 'hh', 'dd', 'mm']
  
  header='["GROUP_BY_TIME": Code, DisplayName, Value, TimeStamp]'
  if keyword_set(WRITE) then printf, unit, header else print, header
  for i=0, n_elements(groupByTimeValues)-1 do begin
    Code=strcompress(i, /REMOVE)
    displayName=groupByTimeValues[i]+' ('+groupByTimeTS[i]+')'
    value=groupByTimeValues[i]
    timeStamp=groupByTimeTS[i]
    record=code+';'+displayName+';'+value+';'+timeStamp
    if keyword_set(WRITE) then printf, unit, record else print,record
  endfor
  if keyword_set(WRITE) then close, unit else print, '************EOF***************'
  
END

PRO FMFileSystemManager::OLDbuildElabType, unit, filename, elaborationTypeList, WRITE=WRITE

  if keyword_set(WRITE) then OPENW, unit, filename ;"ELAB_TYPE"
  ;Code,DisplayName,Description
  
  elabTypeNum=n_elements(elaborationTypeList)
  
  header='["ELAB_TYPE": Code,DisplayName,Description]'
  if keyword_set(WRITE) then printf, unit, header else print, header
  for i=0, n_elements(elaborationTypeList)-1 do begin
    code=strcompress(i, /REMOVE)
    displayName=elaborationTypeList[i]
    descr='Descr of:'+elaborationTypeList[i]
    record=code+';'+displayName+';'+descr
    if keyword_set(WRITE) then printf, unit, record else print,record
  endfor
  if keyword_set(WRITE) then close, unit else print, '************EOF***************'
  
END

PRO   FMFileSystemManager::buildDiagramType, unit, filename, elaborationDiagramList, WRITE=WRITE

  if keyword_set(WRITE) then OPENW, unit, filename ;"ELAB_TYPE"
  ;Code,DisplayName,Description
  
  elabDiagramNum=n_elements(elaborationDiagramList)
  
  header='["DIAGRAM_TYPE": Code,DisplayName,Description]'
  if keyword_set(WRITE) then printf, unit, header else print, header
  for i=0, n_elements(elaborationDiagramList)-1 do begin
    code=strcompress(i, /REMOVE)
    displayName=elaborationDiagramList[i]
    descr='Descr of:'+elaborationDiagramList[i]
    record=code+';'+displayName+';'+descr
    if keyword_set(WRITE) then printf, unit, record else print,record
  endfor
  if keyword_set(WRITE) then close, unit else print, '************EOF***************'
  
END

PRO   FMFileSystemManager::buildGroupByStat, unit, filename, groupByStatsValues, groupByStatsOps, WRITE=WRITE

  ;Code,Display_Name,Value,Measure_Unit
  if keyword_set(WRITE) then OPENW, unit, fileName ;"GROUP_BY_STAT"
  ;Code,DisplayName,Value,Operation
  
  groupByStatsValues=['0', '1', '2', '3']
  groupByStatsOps=['Mean', 'Max', 'Min', 'Sum']
  
  
  header='["GROUP_BY_STAT": Code,DisplayName,Value,Operation]'
  if keyword_set(WRITE) then printf, unit, header else print, header
  for i=0, n_elements(groupByStatsValues)-1 do begin
    code=strcompress(i, /REMOVE)
    displayName=groupByStatsOps[i]
    value=groupByStatsValues[i]
    operation=groupByStatsOps[i]
    record=code+';'+displayName+';'+value+';'+operation
    if keyword_set(WRITE) then printf, unit, record else print,record
  endfor
  if keyword_set(WRITE) then close, unit else print, '************EOF***************'
  
END

PRO FMFileSystemManager::buildTimePeriod, unit, filename, periodName, periodDisplayNames, periodStartValues, periodEndValues, periodTemplates, WRITE=WRITE

  if keyword_set(WRITE) then OPENW, unit, filename ;"SEASON"/"DAY" TIMEPERIOD
  ;Code,DisplayName,StartValue,EndValue,Template,Description
  
  header='['+periodName+': Code,DisplayName,StartValue,EndValue,Template,Description]'
  if keyword_set(WRITE) then printf, unit, header else print, header
  for i=0, n_elements(periodDisplayNames)-1 do begin
    code=strcompress(i, /REMOVE)
    displayName=periodDisplayNames[i]
    startValue=periodStartValues[i]
    endValue=periodEndValues[i]
    descr='Desc:'+periodDisplayNames[i]
    template=periodTemplates[i]
    record=code+';'+displayName+';'+startValue+';'+endValue+';'+$
    template+';'+descr
    if keyword_set(WRITE) then printf, unit, record else print,record
  endfor
  if keyword_set(WRITE) then close, unit else print, '************EOF***************'
  
END

PRO FMFileSystemManager::buildCategory, unit, fileName, categoryObsList, WRITE=WRITE

  if keyword_set(WRITE) then OPENW, unit, fileName ;"CATEGORY"
  ;Code,Display_Name,Description
  
  ;categoryObsList=['Zone','Type','Topo']
  catNumber=n_elements(CategoryObsList)
  
  header='["CATEGORY": Code,DisplayName,Description]'
  if keyword_set(WRITE) then printf, unit, header else print, header
  for i=0, n_elements(CategoryObsList)-1 do begin
    code=strcompress(i, /REMOVE)
    displayName=categoryObsList[i]
    descr='Descr of:'+categoryObsList[i]
    record=code+';'+displayName+';'+descr
    if keyword_set(WRITE) then printf, unit, record else print,record
  endfor
  if keyword_set(WRITE) then close, unit else print, '************EOF***************'
  
END

PRO FMFileSystemManager::buildParameterType, unit, fileName, parameterTypeList, WRITE=WRITE

  if keyword_set(WRITE) then OPENW, unit, filename ;"PARAMETER_TYPE"
  ;Code,DisplayName,Description
  ; extract unique type...
  avTypes=parameterTypeList[UNIQ(parameterTypeList, SORT(parameterTypeList))]
  header='["PARAMETER_TYPE": Code,DisplayName,Description]'
  if keyword_set(WRITE) then printf, unit, header else print, header
  for i=0, n_elements(avTypes)-1 do begin
    ;code=strcompress(i, /REMOVE)
    code=avTypes[i]
    displayName=avTypes[i]
    descr='Descr of:'+parameterTypeList[i]
    record=code+';'+displayName+';'+descr
    if keyword_set(WRITE) then  printf, unit, record else print,record
  endfor
  if keyword_set(WRITE) then close, unit else print, '************EOF***************'
  
END

PRO FMFileSystemManager::buildIDLRoutine, unit, fileName, routineNameList, WRITE=WRITE

  if keyword_set(WRITE) then OPENW, unit, fileName ;"IDL_ROUTINE"
  ;Code,Routine_Name,In_Parameter,Out_Parameter,Description
  inVarPrefix='VarIn'
  outVarPrefix='VarOut'
  routineNumber=n_elements(routineNameList)
  
  header='["IDL_ROUTINE": Code,RoutineName,InParameters,OutParameters,Description]'
  if keyword_set(WRITE) then printf, unit, header else print, header
  for i=0, n_elements(routineNameList)-1 do begin
    code=strcompress(i, /REMOVE)
    routineName=routineNameList[i]
    inParams=self.utility->buildWithAsterisk(prefix=inVarPrefix, numbers=ceil(RANDOMu(seed)*100 mod 4))
    outParams=self.utility->buildWithAsterisk(prefix=outVarPrefix, numbers=ceil(RANDOMu(seed)*100 mod 4))
    descr='Descr of:'+routineNameList[i]
    record=code+';'+routineName+';'+inParams+';'+outParams+';'+descr
    if keyword_set(WRITE) then printf, unit, record else print,record
  endfor
  if keyword_set(WRITE) then close, unit else print, '************EOF***************'
  
END

PRO FMFileSystemManager::OLDbuildPlotType, unit, fileName, plotTypeNameList, plotMultipleChoice, idlRoutineIndex, WRITE=WRITE

  if keyword_set(WRITE) then OPENW, unit, fileName ;"PLOT_TYPE"
  ;Code,IDL_Routine_Code,Description,Name
  plotTypeNumber=n_elements(plotTypeNameList)
  header='["PLOT_TYPE": Code,Name,IDLRoutineCode,MultipleChoice, Description]'
  if keyword_set(WRITE) then printf, unit, header else print, header
  for i=0, plotTypeNumber-1 do begin
    code=strcompress(i, /REMOVE)
    idlRoutineCode=idlRoutineIndex[i]
    plotName=plotTypeNameList[i]
    multipleChoice=plotMultipleChoice[i]
    descr='Descr of:'+plotName
    record=code+';'+plotName+';'+idlRoutineCode+';'+multipleChoice+';'+descr+';'
    if keyword_set(WRITE) then printf, unit, record else print,record
  endfor
  if keyword_set(WRITE) then close, unit else print, '************EOF***************'
  
END

;PRO FMFileSystemManager::buildAxis, unit, fileName, axisNameList, axisMultipleChoice, idlRoutineIndex, WRITE=WRITE
;
;  if keyword_set(WRITE) then OPENW, unit, fileName ;"AXIS_TYPE"
;  ;Code,IDL_Routine_Code,Description,Name
;  axisNumber=n_elements(axisNameList)
;  header='["AXIS_TYPE": Code,Name,IDLRoutineCode,MultipleChoice, Description]'
;  if keyword_set(WRITE) then printf, unit, header else print, header
;  for i=0, axisNumber-1 do begin
;    code=strcompress(i, /REMOVE)
;    idlRoutineCode=idlRoutineIndex[i]
;    axisName=axisNameList[i]
;    multipleChoice=axisMultipleChoice[i]
;    descr='Descr of:'+axisName
;    record=code+';'+axisName+';'+idlRoutineCode+';'+multipleChoice+';'+descr+';'
;    if keyword_set(WRITE) then printf, unit, record else print,record
;  endfor
;  if keyword_set(WRITE) then close, unit else print, '************EOF***************'
;
;END

PRO   FMFileSystemManager::buildModeConfig, unit, filename, modeConfigList, WRITE=WRITE

  ;Code,DisplayName,Description
  if keyword_set(WRITE) then OPENW, unit, filename ;"MODE_CONFIG"
  
  header='["MODE_CONFIG": Code,DisplayName,Description]'
  if keyword_set(WRITE) then printf, unit, header else print, header
  for i=0, n_elements(modeConfigList)-1 do begin
    record=strcompress(i, /REMOVE)+';'+modeConfigList[i]+';'+'description number:'+string(i)
    if keyword_set(WRITE) then printf, unit, record else print,record
  endfor
  if keyword_set(WRITE) then close, unit else print, '************EOF***************'
  
END

PRO FMFileSystemManager::buildObserved, unit, filename, monitData, WRITE=WRITE

  if keyword_set(WRITE) then openw, unit, filename ;"OBSERVED"
  ;Code,Name,Display_Name,XGeoLocation,YGeoLocation,Country,Country_GMT,Height_Above_Sea,Description
  
  statNumber=n_elements(monitData)
  countryName='Italy'
  countryUMT='+1'
  
  header='["OBSERVED": Code,Name,DisplayName,ShortName,XGeoLocation,YGeoLocation,Country,CountryGMT,HeightAboveSea,Parameters,Description]'
  if keyword_set(WRITE) then printf, unit, header else print, header
  for i=0, statNumber-1 do begin
    code=monitData[i].stationCode
    statName=monitData[i].stationName
    displayName=monitData[i].stationName
    shortName=monitData[i].shortName
    xLoc=strcompress(monitData[i].lon, /REMOVE)
    yLoc=strcompress(monitData[i].lat, /REMOVE)
    countryTime=monitData[i].GMTLag
    parameters=monitData[i].parameters
    country=countryName
    heightAboveSeas=strcompress(monitData[i].altitude, /REMOVE)
    descr='Descr of: '+statName
    record=code+';'+statName+';'+displayName+';'+shortName+';'+xLoc+";"+yLoc+";"+$
    country+";"+countryTime+';'+heightAboveSeas+';'+descr+';'+parameters
    if keyword_set(WRITE) then printf, unit, record else print,record
  endfor
  if keyword_set(WRITE) then close, unit else print, '************EOF***************'
  
END

PRO FMFileSystemManager::buildObservedCategory, unit, fileName, monitData, WRITE=WRITE

  if keyword_set(WRITE) then openw, unit, fileName ;"OBSERVED_CATEGORY"
  ;Code,Observed_Code,Category_Code,Value
  statNumber=n_elements(monitData)
  header='["OBSERVED_CATEGORY": Code,ObservedCode,CategoryCode,Value]'
  if keyword_set(WRITE) then printf, unit, header else print, header
  for i=0, statNumber-1 do begin
    code=strcompress(i, /REMOVE)
    obsCode=monitData[i].stationCode
    values=[monitData[i].region,monitData[i].stationType,monitData[i].areaType,monitData[i].siting]
    for j=0, 3 do begin
      catCode=strcompress(j, /REMOVE)
      catValue=values[j]
      record=code+';'+obsCode+';'+catCode+';'+catValue
      if keyword_set(WRITE) then printf, unit, record else print,record
    endfor
  endfor
  if keyword_set(WRITE) then close, unit else print, '************EOF***************'
  
END

PRO FMFileSystemManager::buildParameter, unit, filename, parametersSummary, WRITE=WRITE

  if keyword_set(WRITE) then openw, unit, fileName ;"parameter"
  ;Code,TypeCode,DisplayName,MeasureUnit,Description
  
  allParList=parametersSummary.name
  totParNumbers=n_elements(allParList)
  
  header='["PARAMETER": Code,TypeCode,DisplayName,MeasureUnit,Description]'
  if keyword_set(WRITE) then printf, unit, header else print, header
  for i=0, n_elements(allParList)-1 do begin
    code=parametersSummary[i].name
    typeCode=parametersSummary[i].typeCode
    displayName=parametersSummary[i].name
    ;measureUnit='Mu of:'+airQualityList[i]
    measureUnit=parametersSummary[i].measureUnit
    descr='Descr of: '+parametersSummary[i].name
    record=code+';'+typeCode+';'+displayName+';'+measureUnit+';'+descr
    if keyword_set(WRITE) then printf, unit, record else print,record
  endfor
  if keyword_set(WRITE) then close, unit else print, '************EOF***************'
  
  
END

PRO FMFileSystemManager::buildRun, unit, fileName, runData, WRITE=WRITE
  if keyword_set(WRITE) then openw, unit, fileName ;"RUN"
  ;Code,ScenarioCode,ModelCode,AreaCode,DisplayName,GridXStart (?),GridXStep (?),GridXStepNumber (?),GridYStart (?),GridYStep (?),GridYStepNumber (?),VerticalInterpolation,HorizontalInterpolation,ExecutionDate,FileName,Description
  k=0
  
  ; Build up Run from all files *.cdf in $HOME$/data/models dir ($MODELNAME$_$SCENARIONAME$_2D.cdf or $MODELNAME$_$SCENARIONAME$_TIME.cdf
  header='["RUN": Code,ScenarioCode,ModelCode,AreaCode,DisplayName,GridXStart (?),GridXStep (?),GridXStepNumber (?),GridYStart (?),GridYStep (?),GridYStepNumber (?),VerticalInterpolation,HorizontalInterpolation,ExecutionDate,FileName,Description]'
  
  if keyword_set(WRITE) then printf, unit, header else print, header
  
  runNumbers=n_elements(runData)
  areaCode='2' ;fixed
  
  for i=0, runNumbers-1 do begin
    code=strcompress(i, /REMOVE)
    scenCode=runData[i].scenario
    modelCode=runData[i].model
    displayName=runData[i].model+'('+runData[i].scenario+')'
    gridInfo='39.00'+';'+'0.20'+';'+'30'+';'+'10.00'+';'+'0.15'+';'+'25'
    interpolationInfo='30.00'+';'+'15.00'
    executionDate=runData[i].execDate
    fileName=runData[i].filename
    descr='Descr of: '+displayName+' ('+executionDate+')'
    record=code+';'+scenCode+';'+modelCode+';'+areaCode+';'+displayName+';'+gridInfo+';'+ $
    interpolationInfo+";"+executionDate+';'+fileName+';'+descr
    if keyword_set(WRITE) then printf, unit, record else print,record
  endfor
  
  if keyword_set(WRITE) then close, unit else print, '************EOF***************'
  
END

PRO  FMFileSystemManager::test_BuildRun, unit, fileName, scenarioList, modelList, WRITE=WRITE
  if keyword_set(WRITE) then OPENW, unit, fileName ;"RUN"
  ;Code,ScenarioCode,ModelCode,AreaCode,DisplayName,GridXStart (?),GridXStep (?),GridXStepNumber (?),GridYStart (?),GridYStep (?),GridYStepNumber (?),VerticalInterpolation,HorizontalInterpolation,ExecutionDate,FileName,Description
  k=0
  scenarioNumbers=n_elements(scenarioList)
  modelNumbers=n_elements(modelList)
  header='["RUN": Code,ScenarioCode,ModelCode,AreaCode,DisplayName,GridXStart (?),GridXStep (?),GridXStepNumber (?),GridYStart (?),GridYStep (?),GridYStepNumber (?),VerticalInterpolation,HorizontalInterpolation,ExecutionDate,FileName,Description]'
  
  if keyword_set(WRITE) then printf, unit, header else print, header
  
  areaCode='2' ;fixed
  
  for i=0, scenarioNumbers-1 do begin
    scenIndex=i
    scenCode=strcompress(scenIndex, /REMOVE)
    for j=0, modelNumbers-1 do begin
      code=strcompress(k, /REMOVE)
      k++
      modelIndex=j
      modelCode=strcompress(modelIndex, /REMOVE)
      displayName=modelList[modelIndex]+'('+scenarioList[scenIndex]+')'
      gridInfo='39.00'+';'+'0.20'+';'+'30'+';'+'10.00'+';'+'0.15'+';'+'25'
      interpolationInfo='30.00'+';'+'15.00'
      executionDate=systime()
      fileName=strmid(scenarioList[scenIndex], 0, 4)+'_'+strmid(modelList[modelIndex], 0, 3)+'.dat'
      descr='Descr of:'+'RUN_'+strmid(scenarioList[scenIndex], 0, 4)+'_'+strmid(modelList[modelIndex], 0, 5)
      record=code+';'+scenCode+';'+modelCode+';'+areaCode+';'+displayName+';'+gridInfo+';'+ $
      interpolationInfo+";"+executionDate+';'+fileName+';'+descr
      if keyword_set(WRITE) then printf, unit, record else print,record
    endfor
  endfor
  runNumbers=k
  if keyword_set(WRITE) then close, unit else print, '************EOF***************'
  
END

PRO FMFileSystemManager::test_BuildObservedParameter, unit, fileName, statNumber, allParList, WRITE=WRITE
  if keyword_set(WRITE) then OPENW, unit, fileName ;"OBSERVED_PARAMETER"
  ;Code,ObservedCode,ParameterCode,FileName,MonitorSampling,StartAvailability,EndAvailability,InternalParameterCode,Description
  
  header='[OBSERVED_PARAMETER: Code,ObservedCode,ParameterCode,FileName,MonitorSampling,StartAvailability,EndAvailability,'
  header=header+'InternalParameterCode,Description]'
  ;randomu(seed, statNumber)
  parNumber=n_elements(allParList)
  
  if keyword_set(WRITE) then printf, unit, header else print, header
  k=0
  for i=0, statNumber-1 do begin
    parNumber=fix(randomu(seed)*1000 mod parNumber)
    for j=0, parNumber-1 do begin
      k++
      code=strcompress(k, /REMOVE)
      obsCode=strcompress(i, /REMOVE)
      thisParCode=fix(randomu(seed)*1000 mod parNumber)
      parCode=strcompress(thisParCode, /REMOVE)
      thisParName=allParList[thisParCode]
      fileName='OBS_'+strcompress(i, /REMOVE)+'_'+thisParName+'.dat'
      monitorSampl='01:00' ; time stamp hh24:mi
      startAv='2005010100:00'  ; standard time stamp is: yyyymmddhh24:mi
      endAv='2005123123:30'  ; standard time stamp is: yyyymmddhh24:mi
      internalParameterCode=strcompress(thisParCode, /REMOVE)
      desc='Desc of:'+fileName
      record=code+';'+obsCode+';'+parCode+';'+fileName+';'+monitorSampl+';'+startAv+';'+endAv+';'+internalparameterCode+';'+desc
      if keyword_set(WRITE) then printf, unit, record else print,record
    endfor
  endfor
  if keyword_set(WRITE) then close, unit else print, '************EOF***************'
  
END

PRO FMFileSystemManager::test_BuildRunparameter, unit, fileName, runNumbers, allParList, WRITE=WRITE
  if keyword_set(WRITE) then OPENW, unit, fileName ;"RUN_PARAMETER"
  ;Code,RunCode,parameterCode,MonitorSampling,FileName,StartAvailability,EndAvailability,InternalparameterCode (?),Description
  
  header='[RUN_parameter: Code,RunCode,parameterCode,FileName,MonitorSampling,StartAvailability,EndAvailability,'
  header=header+'InternalparameterCode,Description]'
  totParNumbers=n_elements(allParList)
  
  if keyword_set(WRITE) then printf, unit, header else print, header
  k=0
  for i=0, runNumbers-1 do begin
    parNumber=fix(randomu(seed)*1000 mod totParNumbers)
    for j=0, parNumber-1 do begin
      code=strcompress(k, /REMOVE)
      k++
      runCode=strcompress(i, /REMOVE)
      thisParCode=fix(randomu(seed)*1000 mod totParNumbers)
      parCode=strcompress(thisParCode, /REMOVE)
      thisParName=allParList[thisParCode]
      fileName='RUN_'+strcompress(i, /REMOVE)+'_'+thisParName+'.dat'
      monitorSampl='12:00' ; time stamp hh24:mi
      startAv='2005010100:00' ; time stamp is: yyyymmddhh24:mi
      endAv='2005123123:30' ; time stamp is: yyyymmddhh24:mi
      internalParameterCode=strcompress(thisParCode, /REMOVE)
      desc='Desc of:'+fileName
      record=code+';'+runCode+';'+parCode+';'+fileName+';'+monitorSampl+';'+startAv+';'+endAv+';'+internalParameterCode+';'+desc
      if keyword_set(WRITE) then printf, unit, record else print,record
    endfor
  endfor
  
  if keyword_set(WRITE) then close, unit else print, '************EOF***************'
  
END

PRO FMFileSystemManager::test_BuildGCOC, unit, filename, elaborationList, allParList, seasonNumber, dayNumber, WRITE=WRITE

  ;  if keyword_set(WRITE) then OPENW, units[12], testDir+names[12]+'.dat' ;"GOALS_CRITERIA_OC"
  ;  ;Code,ObservedParameterCode (?),PeriodCode (?), AreaCode, Description,CategoryValue (?),Values
  ;
  ;  header="[GOALS_CRITERIA_OC:  Code,elaborationCode, parameterCode,periodCode, AreaCode,Values, Description]"
  ;
  ;  if keyword_set(WRITE) then printf, units[12], header else print, header
  ;
  ;  for i=0, elabsNumber-1 do begin
  ;    if randomu(seed) gt .85 then begin
  ;      code=strcompress(i, /REMOVE)
  ;      parCode=fix(randomu(seed)*1000 mod totParNumbers)
  ;      elabCode=strcompress(fix(randomu(seed)*1000 mod elabsNumber), /REMOVE)
  ;      thisParName=allParList[parCode]
  ;      parCode=strcompress(parCode, /REMOVE)
  ;      periodCode=strcompress(fix(randomu(seed)*1000 mod n_elements(periodDisplayName)), /REMOVE)
  ;      ;areaCode=strcompress(fix(randomu(seed))*1000 mod 10, /REMOVE)
  ;      areaCode='2' ; fixed
  ;      values=strmid(strcompress(randomu(seed, 2), /REMOVE), 0, 4)
  ;      gcocValues=self.utility->buildWithAsterisk(values)
  ;      descr='Descr of: '+elaborationList[i]+'_('+thisParName+')'
  ;      record=code+';'+elabCode+';'+parCode+';'+$
  ;      periodCode+';'+areaCode+';'+gcocValues+';'+descr
  ;      if keyword_set(WRITE) then printf, units[12], record else print,record
  ;    endif
  ;  endfor
  ;  if keyword_set(WRITE) then close, units[12] else print, '************EOF***************'
  if keyword_set(WRITE) then OPENW, unit, filename ;"GOALS_CRITERIA_OC"
  ;Code,ObservedparameterCode (?),PeriodCode (?), AreaCode, Description,CategoryValue (?),Values
  
  header="[GOALS_CRITERIA_OC:  Code,elaborationCode, parameterCode,periodCode, AreaCode,Values, Description]"
  totParNumbers=n_elements(allParList)
  if keyword_set(WRITE) then printf, unit, header else print, header
  
  for i=0, elabsNumber-1 do begin
    if randomu(seed) gt .85 then begin
      code=strcompress(i, /REMOVE)
      parCode=fix(randomu(seed)*1000 mod totParNumbers)
      elabCode=strcompress(fix(randomu(seed)*1000 mod elabsNumber), /REMOVE)
      thisParName=allSpecList[parCode]
      parCode=strcompress(parCode, /REMOVE)
      periodCode=strcompress(fix(randomu(seed)*1000 mod n_elements(periodDisplayName)), /REMOVE)
      ;areaCode=strcompress(fix(randomu(seed))*1000 mod 10, /REMOVE)
      areaCode='2' ; fixed
      values=strmid(strcompress(randomu(seed, 2), /REMOVE), 0, 4)
      gcocValues=self.utility->buildWithAsterisk(values)
      descr='Descr of: '+elaborationList[i]+'_('+thisParName+')'
      record=code+';'+elabCode+';'+parCode+';'+$
      periodCode+';'+areaCode+';'+gcocValues+';'+descr
      if keyword_set(WRITE) then printf, units[12], record else print,record
    endif
  endfor
  if keyword_set(WRITE) then close, units[12] else print, '************EOF***************'
  
END

;PRO FMFileSystemManager::test_BuildInternalSystemConfig, WRITE=WRITE
;
;  testDir="E:\mirko\develop\FAIR_MODE\test_resource\"
;  names=strlowcase(["CATEGORY","OBSERVED_CATEGORY","MODE_CONFIG",$
;    "MODEL","RUN","SCENARIO","OBSERVED","OBSERVED_PARAMETER","PARAMETER",$
;    "RUN_PARAMETER","IDL_ROUTINE","ELABORATION","GOALS_CRITERIA_OC",$
;    "GROUP_BY_TIME","GROUP_BY_STAT","DIAGRAM_TYPE",$
;    "SEASON", "PARAMETER_TYPE", "DAYPERIOD"])
;  units=indgen(n_elements(names))+1
;
;  ; ************************ fixed table section  ************
;  ; indexes=2, 12, 14, 15, 17, 19
;
;  modeConfigList=['User Standard', 'BenchMark1', 'BenchMark2', 'BenchMark3']
;  self->buildModeConfig, units[2], testDir+names[2]+'.dat', modeConfigList, WRITE=WRITE
;
;  groupByTimeValues=['preserve', '01', '08', '01', '01']
;  groupByTimeTS=['none', 'hh', 'hh', 'dd', 'mm']
;  self->buildGroupByTime, units[15], testDir+names[15]+'.dat', groupByTimeValues, groupByTimeTS, WRITE=WRITE
;
;  groupByStatsValues=['0', '1', '2', '3']
;  groupByStatsOps=['Mean', 'Max', 'Min', 'Sum']
;  self->buildGroupByStat, units[15], testDir+names[15]+'.dat', groupByStatsValues, groupByStatsOps, WRITE=WRITE
;
;  seasonTimeStamps=['ddmm','ddmm','ddmm']
;  seasonDisplayNames=['Summer', 'Winter', 'All']
;  seasonStartValues=['0104', '0101*0110', '0101']
;  seasonEndValues=['3009', '3103*3112', '3112']
;  self->buildTimePeriod, units[17], testDir+names[17]+'.dat', 'SEASON', seasonDisplayNames, seasonStartValues, seasonEndValues, seasonTimeStamps, WRITE=WRITE
;
;  dayDisplayNames=['Day', 'Night', 'All']
;  dayTimeStamps=['hh','hh', 'hh']
;  dayStartValues=['08', '00*21', '00']
;  dayEndValues=['20', '07*23', '23']
;  self->buildTimePeriod, units[19], testDir+names[19]+'.dat', 'PERIODDAY', dayDisplayNames, dayStartValues, dayEndValues, dayTimeStamps, WRITE=WRITE
;
;  ; ********* classification section
;  ; indexes=0, 16, 18
;
;  categoryObsList=['Zone','Type','Topo']
;  catNumber=n_elements(categoryObsList)
;  self->buildCategory, units[0], testDir+names[0]+'.dat', categoryObsList, WRITE=WRITE
;
;  elaborationDiagramList=['BarPlot', 'TimeSeries','Scatter plot', 'Taylor plot', '2D']
;  self->buildElabDiagram, units[16], testDir+names[16]+'.dat', elaborationDiagramList, WRITE=WRITE
;
;  parameterTypeList=['Air quality', 'Meteo']
;  self->buildParameterType, units[18], testDir+names[18]+'.dat', parameterTypeList, WRITE=WRITE
;
;  ; ******** software related section
;  ; indexes=10, 13
;
;  routineNameList=['FM_MEANT','FM_MEANMINT','FM_MEANMAXT','FM_STDDEVT',$
;    'FM_CORRCOEFT','FM_BIAST','FM_NBIAST','FM_RMSET',$
;    'FM_NRMSET', 'FM_CRMSET','FM_SIGMSIGOT','FM_EXCDAYS','FM_AOTX',$
;    'FM_SOMOX','FM_PERCX','FM_TIMESERIES','FM_SCATPLOT','FM_SCATPLOTALL',$
;    'FM_FREQA','FM_FREQAERR','FM_QQPLOT','FM_TAYLOR']
;  self->buildIDLRoutine, units[10], testDir+names[10]+'.dat', routineNameList, WRITE=WRITE
;
;  axisNameList=['(x)-Models*(y)-Scenarios', '(x)-Observations*(y)-Models', $
;    '(x)-Observations*(y)-Scenarios', '(x)-Observations*(y)-Variables', '(x)-Observations*(y)-Season', $
;    '(x)-Observations*(y)-Days', '(x)-Models*(y)-Seasons', '(x)-Time*(y)-Scenarios', $
;    '(x)-Time*(y)-Models', '(x)-Time*(y)-Variables', $
;    '(x)-Time*(y)-Seasons', '(x)-Time*(y)-Days']
;  axisMultipleChoice=['0110000', '0101000', $
;    '0011000', '0001100', '0001010', $
;    '0001001', '0100010', '1010000', $
;    '1100000', '1000100', $
;    '1000010', '1000001']
;  idlRoutineIndex=['100', '100', $
;    '100', '100', '100', $
;    '100', '100', '100', $
;    '100', '100', $
;    '100', '100']
;
;  ;MultipleChoice=[Time, Models, Scenarios, Observations, Parameters, Seasons, Days]
;  self->buildAxis, units[13], testDir+names[13]+'.dat', axisNameList, axisMultipleChoice, idlRoutineIndex, WRITE=WRITE
;  statNumber=30
;  self->test_BuildObserved,units[6], testDir+names[6]+'.dat' , statNumber, WRITE=WRITE
;
;  self->test_BuildObservedCategory, units[1], testDir+names[1]+'.dat', statNumber, catNumber , WRITE=WRITE
;
;  ; ****************** parameter section
;  ; indexes=8
;  airQualityList=['NO2','NO','NOx','SO2','O3','PM10','PM10g','PM25','PM25g']
;  meteoList=['u','v','uv','diruv','w','t','pres','q','rhum','clw','rnw','kzmom','kzheat','graupel','molngth',$
;    'ground_t','rain_con','rain_non','rain_tot','pbl_hgt','ust','t2','q2','rhum2','tseasfc',$
;    'shflux','lhflux','soil_t_1','soil_m_1','soil_w_1','u10','v10',$
;    'uv10','diruv10','PT_10-2','PT_50-10','PT_100-10','PT_150-10']
;  allparameters=[airQualityList, meteoList]
;  parNumbers=n_elements(allparameters)
;  self->test_BuildParameter, units[8], testDir+names[8]+'.dat', airQualityList, meteoList, WRITE=WRITE
;
;  modelList=['MM5','MM5ng','WRF','WRF2','TRAM','MINNI','TVM']
;  self->buildModel, units[3], testDir+names[3]+'.dat', modelList, WRITE=WRITE
;
;  scenarioList=['2005','2008','2010']
;  self->buildScenario, units[5], testDir+names[5]+'.dat', WRITE=WRITE
;
;  self->test_BuildRun, units[4], testDir+names[4]+'.dat', scenarioList, modelList, WRITE=WRITE
;
;  ; Crossed relations section
;  ; indexes=7, 9, 11, 12
;  self->test_BuildObservedParameter, units[7], testDir+names[7]+'.dat', statNumber, allParList, WRITE=WRITE
;
;  runNumbers=n_elements(modelList)*n_elements(scenarioList)
;  self->test_BuildRunParameter, units[9], testDir+names[9]+'.dat', runNumbers, allParList, WRITE=WRITE
;
;  elaborationDiagramList=['BarPlot', 'TimeSeries','Scatter plot', 'Taylor plot', '2D']
;  elabDiagramNum=n_elements(elaborationDiagramList)
;  routineNameList=['FMMean_T','FMMean_max_T','FMMean_min_T','FMStddev_T',$
;    'FMCorrCoef_T','FMBias_T','FMNBias_T','FMRMSE_T',$
;    'FMNRMSE_T','FMCRMSE_T','FMSigM/sigO_T','FMExcDays','FMAOTx',$
;    'FMSOMOx','FMPERCx','FMTimeSeries','FMScatPlot','FMScatPlotAll',$
;    'FMFreqA','FMFreqA_err','FMQ-Q Plot','FMTaylor']
;  routineNumber=n_elements(routineNameList)
;  bpElaborationList=['Mean_T','CorrCoef_T','Stddev_T','RMSE_T','Bias_T','ExcDays']
;  tsElaborationList=['Value','Bias_T']
;  spElaborationList=['Value']
;  tpElaborationList=['Value']
;  tdElaborationList=['Value', 'Difference']
;  elabsNumber=n_elements(elaborationList)
;  elabs=self->test_GetElaboration(nelabs=nelabs)
;
;  self->buildElaboration, units[11], testDir+names[11]+'.dat', elabs, allParameters, WRITE=WRITE
;;  self->buildElaboration, unit, FileName, bpElaborationList, tsElaborationList, spElaborationList, $
;;  tpElaborationList, tdElaborationList, allParList, WRITE=WRITE
;
;;ToDo: fix this when we know how to use GCoc
;;self->test_BuildGCOC, units[12], testDir+names[12]+'.dat', elabsNumber, allParList, seasonNumber, dayNumber, WRITE=WRITE
;
;END
; ****************************
; startup methods
; ****************************
FUNCTION FMFileSystemManager::readStartUpFile, filename

  ;  ERROR=0
  ;  catch, error_status
  ;
  ;  if error_status NE 0 THEN BEGIN
  ;    ERROR=1
  ;    catch, /CANCEL
  ;    errMsg=dialog_message('problem with file: <'+fileName+'> check existence, contents or read permission.', /ERROR)
  ;  endif
  openr, unit, fileName, /GET_LUN
  
  bufferString=''
  lines=''
  ;MM summer 2012 start
  modelTypeInfo=getFMModelInfoStruct()
  OLDVERSION=0
  ;MM summer 2012 end
  i=0
  while not(eof(unit)) do begin
    readf, unit, bufferString
    lines=[lines, bufferString]
    i++
    checkFirst=strmid(bufferString, 0,1)
    ;check1=(strpos(checkFirst, '[')+1) > 0
    check1=(strpos(checkFirst, ';')+1) > 0
    check2=(strpos(checkFirst, '#')+1) > 0
    null=strlen(strcompress(bufferString, /REMOVE)) eq 0
    ; #, ; is a comment
    ; void string string is discarded
    ; [ header is discarded
    if (check1+check2) gt 0 or null then begin
    ;      print, 'Discard row', i
    ;      print, bufferString
    endif else begin
      ; MM summer 2012 Start
      if strupcase(bufferString) eq self.scaleHeader then begin
        warningMessage = strarr(9)
        warningMessage[n_elements(lines)]='check version of your startup.ini'
        warningMessage[1]='replace [SCALE] section with'
        warningMessage[2]='[MODEL]'
        warningMessage[3]=';Year'
        warningMessage[4]=';frequency'
        warningMessage[5]=';Scale'
        warningMessage[6]='2009'
        warningMessage[7]='hour'
        warningMessage[8]='urban'
        ;warningMsg=dialog_message('Old configuration of startup.ini found')
        modelInfo=self->readOldScaleAsModelSection(unit)
        self->replaceModelLines, lines, modelInfo
        OLDVERSION=1
      endif
      if strupcase(bufferString) eq self.modelHeader then begin
        modelInfo=self->readModelSection(unit, lines=lines)
      endif
      ; MM summer 2012 End
      if strupcase(bufferString) eq self.parameterHeader then begin
        parameterInfo=self->readParametersSection(unit, monitInfo=monitInfo, lines=lines)
      endif
      if strupcase(bufferString) eq self.monitoringHeader then begin
        monitInfo=self->readMonitoringSection(unit, parameterInfo=parameterInfo, lines=lines)
      endif
    endelse
  endwhile
  ;extracts all categories values
  regionValueList=monitInfo.data.region
  regionValueList=regionValueList[UNIQ(regionValueList, SORT(regionValueList))]
  
  stationTypeValueList=monitInfo.data.stationType
  stationTypeValueList=stationTypeValueList[UNIQ(stationTypeValueList, SORT(stationTypeValueList))]
  
  areaTypeValueList=monitInfo.data.areaType
  areaTypeValueList=areaTypeValueList[UNIQ(areaTypeValueList, SORT(areaTypeValueList))]
  
  sitingValueList=monitInfo.data.siting
  sitingValueList=sitingValueList[UNIQ(sitingValueList, SORT(sitingValueList))]
  
  statSummary={titles:monitInfo.titles, regions:regionValueList, stationTypes:stationTypeValueList, areaTypes:areaTypeValueList, sitings:sitingValueList}
  ;parametersSummary={titles:titles, regions:regionValueList, stationTypes:stationTypeValueList, areaTypes:areaTypeValueList, sitings:sitingValueList}
  
  close, unit & free_lun, unit
  if (OLDVERSION) then begin
    startUpFile=self->getStartUpFileName()
    self->writeStartUpFile, startUpFile, lines
  endif
  ; New february 1st 2011 MM
  ;return, {statData:monitInfo.data, statSummary:statSummary, parametersSummary:parameterInfo, scaleSummary:scaleInfo, modelTypeSummary:modelTypeInfo}
  ; MM summer 2012 MM
  return, {statData:monitInfo.data, statSummary:statSummary, parametersSummary:parameterInfo, modelSummary:modelInfo}
  
END

PRO FMFileSystemManager::writeStartUpFile, fileName, lines

  ;file_copy,
  openw, unit, fileName, /GET_LUN
  for i=1, n_elements(lines)-1 do begin
    printf, unit, lines[i]
  endfor
  close, unit & free_lun, unit
  
END

PRO FMFileSystemManager::replaceModelLines, lines, modelInfo

  lines[n_elements(lines)-1]='[MODEL]'
  lines=[lines, ';Year']
  lines=[lines, ';frequency']
  lines=[lines, ';Scale']
  lines=[lines, strcompress(modelInfo.year, /REMOVE)]
  lines=[lines, modelInfo.frequency]
  lines=[lines, modelInfo.scale]
  
END

PRO FMFileSystemManager::loadInitFileData, parameterNames=parameterNames, parameterValues=parameterValues

  ;get all configuration that system need from a configuration file!!!
  ERROR=0
  catch, error_status
  
  if error_status NE 0 THEN BEGIN
    ERROR=1
    catch, /CANCEL
    errMsg=dialog_message('problem with file '+fileName+' check existence or read permission.', /ERROR)
  endif
  
  fileName=self->getInitFileName()
  
  bufferString=''
  openr, unit, fileName, /GET_LUN
  
  ;a=dialog_message('loading...'+observedDataRoot+observed.dataFile)
  bufferString=''
  i=0
  
  parameterNames=['']
  parameterValues=['']
  
  while not(eof(unit)) do begin
    readf, unit, bufferString
    i++
    ;bufferString=strcompress(bufferString, /REMOVE)
    void=strcompress(bufferString, /REMOVE)
    if strlen(void) eq 0 then void=1 else void=0
    checkFirst=strmid(bufferString, 0,1)
    check1=(strpos(checkFirst, '[')+1) > 0
    check2=(strpos(checkFirst, ';')+1) > 0
    check3=(strpos(checkFirst, '#')+1) > 0
    null=strlen(checkFirst) eq 0
    if (check1+check2+check3+void) gt 0 or null then begin
    ;      print, 'Discard row', i
    ;      print, bufferString
    endif else begin
      info=strsplit(bufferString, '=', /EXTRACT)
      parameterNames=[parameterNames, info[0]]
      parameterValues=[parameterValues, info[1]]
    endelse
  endwhile
  close, unit & free_lun, unit
  parameterNames=parameterNames[1:*]
  parameterValues=parameterValues[1:*]
  
END

; Modified summer 2012 MM (V 3_0)
PRO FMFileSystemManager::lookUpSystemData, modelInfo=modelInfo

  ; parameters from startup.ini
  ; stations from startup.ini
  ; model/scenario/run from file list ($HOME$/data/model)
  ; link between run/observed and parameters not checked before execution?

  monitoringDir=self->getObservedDataDir(/WITH)
  runDir=self->getRunDataDir(/WITH)
  ;monitoringFile=self->getMonitoringFileName()
  startUpFile=self->getStartUpFileName()
  
  startUpInfo=self->readStartUpFile(startUpFile)
  ;startUpInfo.monitInfo.data & startUpInfo.monitInfo.summary & startUpInfo.parameterInfo
  
  ;self->readMonitoringParameters, monitoringDir, monitStruct
  
  runInfo=self->readRunFiles(runDir)
  ;{runInfo.data & runInfo.summary}
  ;parameters=self->buildUniqParameterList(monitStruct.parameters, runStruct.parameters)
  self->writeConfigurationFile, startUpInfo.statData, startUpInfo.statSummary, runInfo.data, runInfo.summary, startUpInfo.parametersSummary
  ; Start Modified february 1st 2011 MM
  ;  scaleInfo=startUpInfo.scaleSummary
  ;  modelTypeInfo=startUpInfo.modelTypeSummary
  ; End Modified february 1st 2011 MM
  ; MM summer 2012 MM start
  modelInfo=startUpInfo.modelSummary
; MM summer 2012 MM end
;stations data file - csv (category + observed_category + observed)
;parameter data - header of station_data files and header of monitoring (parameter, run_parameter, observed_parameter)
;model_file model+scenario (scenario, model, run)
  
END

FUNCTION FMFileSystemManager::readMonitoringSection, unit, parameterInfo=parameterInfo, lines=lines

  bufferString=''
  i=0
  firstRow=1
  monitoreds=getFMMonitor()
  while not(eof(unit)) do begin
    readf, unit, bufferString
    lines=[lines, bufferString]
    if strupcase(bufferString) eq self.parameterHeader then begin
      parameterInfo=self->readParametersSection(unit, monitInfo=monitInfo, lines=lines)
      return, monitInfo
      break
    endif
    i++
    checkFirst=strmid(bufferString, 0,1)
    check1=(strpos(checkFirst, ';')+1) > 0
    check2=(strpos(checkFirst, '#')+1) > 0
    null=strlen(strcompress(bufferString, /REMOVE)) eq 0
    ; #, ; is a comment
    ; void string string is discarded
    if (check1+check2) gt 0 or null then begin
    ;      print, 'Discard row', i
    ;      print, bufferString
    endif else begin
      info=strsplit(bufferString, ';', /EXTRACT)
      if n_elements(info) eq 12 then begin
        if firstRow eq 1 then begin
          titles=[info[7],info[8],info[9],info[10]]
          firstRow=0
        endif else begin
          if info[6] ne 'GMT+1' then print, info[1]
          thisMonitored=getFMMonitor()
          thisMonitored.stationCode=info[0]
          thisMonitored.stationName=info[1]
          thisMonitored.shortName=info[2]
          thisMonitored.altitude=float(info[3])
          thisMonitored.lon=float(info[4])
          thisMonitored.lat=float(info[5])
          thisMonitored.gmtLag=info[6]
          thisMonitored.region=info[7]
          thisMonitored.stationType=info[8]
          thisMonitored.areaType=info[9]
          thisMonitored.siting=info[10]
          thisMonitored.parameters=info[11];ptr_new(self->buildParameterCodes(info[10]), /NO_COPY)
          monitoreds=[monitoreds, thisMonitored]
        endelse
      endif else begin
        print, 'Bad conf file at line', i, bufferString
      endelse
    endelse
  endwhile
  return, {data:monitoreds[1:*], titles:titles}
  
END

FUNCTION FMFileSystemManager::readRunFiles, runDir
  ; KeesC 31MAY2012
  wild='*.cdf'
  filenamescdf=file_search(runDir+wild)
  wild='*.csv'
  filenamescsv=file_search(runDir+wild)
  filenames=[filenamescdf,filenamescsv]
  if filenamescsv[0] eq '' then filenames=filenamescdf
  if filenamescdf[0] eq '' then filenames=filenamescsv
  fInfo=file_info(filenames)
  execDate=fInfo.ctime
  models=['']
  scenarios=['']
  prevModel=''
  prevScen=''
  runInfos=getFMRunFile()
  for i=0, n_elements(filenames)-1 do begin
    ;cut extensions...
    filename=strsplit(filenames[i], self->getSystemDirSeparator(), /EXTRACT)
    filename=filename[n_elements(filename)-1]
    extPos=strpos(filename, '.', /REVERSE_SEARCH)
    ;    name=strmid(filename, 0, extPos-1)  ; KeesC 31MAY2012
    name=strmid(filename, 0, extPos)
    ext=strmid(filename,extPos+1,3)
    info=strsplit(name, '_', /EXTRACT)
    testScen=info[0] & testModel=info[1]
    if prevModel eq testModel and prevScen eq testScen then begin
      ;print, '--> Double', filename
      continue
    endif
    prevModel=testModel
    prevScen=testScen
    runInfo=getFMRunFile()
    runInfo.model=testModel
    runInfo.scenario=testScen
    runInfo.filename=info[0]+'_'+info[1]+'.'+ext
    runInfo.execDate=systime(0, execDate[i])
    ;print, info
    runInfos=[runInfos, runInfo]
  endfor
  runInfos=runInfos[1:*]
  models=runInfos.model
  scenarios=runInfos.scenario
  modelList=models[UNIQ(models, SORT(models))]
  scenarioList=scenarios[UNIQ(scenarios, SORT(scenarios))]
  summaryRunInfo={models:modelList, scenarios:scenarioList}
  runInfo={data:runInfos, summary:summaryRunInfo}
  
  return, runInfo
  
END

FUNCTION FMFileSystemManager::readOldScaleAsModelSection, unit, lines=lines

  bufferString=''
  i=0
  lines=''
  modelInfo=getFMModelInfoStruct()
  modelInfo.frequency='hour'
  while not(eof(unit)) do begin
    readf, unit, bufferString
    lines=[lines, bufferString]
    i++
    checkFirst=strmid(bufferString, 0,1)
    check1=(strpos(checkFirst, ';')+1) > 0
    check2=(strpos(checkFirst, '#')+1) > 0
    null=strlen(strcompress(bufferString, /REMOVE)) eq 0
    ; #, ; is a comment
    ; void string string is discarded
    if (check1+check2) gt 0 or null then begin
    ;      print, 'Discard row', i
    ;      print, bufferString
    endif else begin
      sInfo=strsplit(bufferString, ';', /EXTRACT)
      modelInfo.scale=sInfo[0]
      modelInfo.year=fix(sInfo[1])
      if modelInfo.year le 1800 then modelInfo.year=2009
      break
    endelse
  endwhile
  return, modelInfo
  
END

; MM summer 2012 Start
FUNCTION FMFileSystemManager::readModelSection, unit, lines=lines

  bufferString=''
  undefined='Undefined'
  ;default values
  modelInfo=getFMModelInfoStruct()
  modelInfo.scale=undefined
  modelInfo.frequency=undefined
  modelInfo.year=2009
  i=0
  pos=1
  while not(eof(unit)) do begin
    readf, unit, bufferString
    lines=[lines, bufferString]
    i++
    checkFirst=strmid(bufferString, 0,1)
    check1=(strpos(checkFirst, ';')+1) > 0
    check2=(strpos(checkFirst, '#')+1) > 0
    null=strlen(strcompress(bufferString, /REMOVE)) eq 0
    ; #, ; is a comment
    ; void string string is discarded
    if (check1+check2) gt 0 or null then begin
    ;      print, 'Discard row', i
    ;      print, bufferString
    endif else begin
      if pos eq 1 then modelInfo.year=fix(bufferString)
      if pos eq 2 then modelInfo.frequency=bufferString
      if pos eq 3 then begin
        modelInfo.scale=bufferString
        break
      endif
      pos++
    endelse
  endwhile
  return, modelInfo
  
END
; MM summer 2012 End

FUNCTION FMFileSystemManager::readParametersSection, unit, monitInfo=monitInfo, lines=lines

  bufferString=''
  i=0
  parameters=getFMParameterFile()
  while not(eof(unit)) do begin
    readf, unit, bufferString
    lines=[lines, bufferString]
    if strupcase(bufferString) eq self.monitoringHeader then begin
      monitInfo=self->readMonitoringSection(unit, parameterInfo=parameterInfo, lines=lines)
      return, parameters[1:*]
    ;break
    endif
    i++
    checkFirst=strmid(bufferString, 0,1)
    check1=(strpos(checkFirst, ';')+1) > 0
    check2=(strpos(checkFirst, '#')+1) > 0
    null=strlen(strcompress(bufferString, /REMOVE)) eq 0
    ; #, ; is a comment
    ; void string string is discarded
    if (check1+check2) gt 0 or null then begin
    ;      print, 'Discard row', i
    ;      print, bufferString
    endif else begin
      info=strsplit(bufferString, ';', /EXTRACT)
      if n_elements(info) eq 3 then begin
        thisParameter=getFMParameterFile()
        thisParameter.name=info[0]
        thisParameter.typeCode=info[1]
        thisParameter.measureUnit=info[2]
        parameters=[parameters, thisParameter]
      endif else begin
        print, 'Bad conf file at line', i, bufferString
      endelse
    endelse
  endwhile
  return, parameters[1:*]
  
END

PRO FMFileSystemManager::writeConfigurationFile, monitData, monitSummary, runData, runSummary, parametersSummary

  ; Build up configuration file from user ini at every startup of application [UPDATELOOKUP setting]
  ;testdir must change in final version
  ;test setting
  ;testDir="E:\mirko\develop\FAIR_MODE\test_resource\" ; self->getConfigurationDir(WITH)
  ;distribution setting
  testDir=self->getConfigurationDir(/WITH)
  
  names=strlowcase(["CATEGORY","OBSERVED_CATEGORY","MODE_CONFIG",$
    "MODEL","RUN","SCENARIO","OBSERVED","OBSERVED_PARAMETER","PARAMETER",$
    "RUN_PARAMETER","IDL_ROUTINE","ELABORATION","GOALS_CRITERIA_OC",$
    "GROUP_BY_TIME","GROUP_BY_STAT","DIAGRAM_TYPE",$
    "PERIOD", "PARAMETER_TYPE"])
    
  units=indgen(n_elements(names))+1
  ;test internal parameters
  WRITE=1
  TEST=1
  
  ; Build up Category from [Monitoring] section:all values found in startup.ini file (6th, 7th, 8th and 9th columns)
  categoryObsList=monitSummary.titles
  catNumber=n_elements(categoryObsList)
  self->buildCategory, units[0], testDir+names[0]+'.dat', categoryObsList, WRITE=WRITE
  
  ; Build up Observed from [Monitoring] section:station values found in startup.ini file (First to 5th column)
  self->buildObserved, units[6], testDir+names[6]+'.dat', monitData, WRITE=WRITE
  
  ; Build up Observed_Category from [Monitoring] section: station values found in startup.ini file
  self->buildObservedCategory, units[1], testDir+names[1]+'.dat', monitData, WRITE=WRITE
  
  ; Build up parameter from [parameters] section in found in startup.ini file
  self->buildParameter, units[8], testDir+names[8]+'.dat', parametersSummary, WRITE=WRITE
  
  ; Build up parameter from [parameters] section in found in startup.ini file
  self->buildParameterType, units[17], testDir+names[17]+'.dat', parametersSummary.typecode, WRITE=WRITE
  
  ; Build up Model from all files *.cdf in $HOME$/data/models dir ($MODELNAME$_$SCENARIONAME$_2D.cdf or $MODELNAME$_$SCENARIONAME$_TIME.cdf
  ; ****************** Model section
  self->buildModel, units[3], testDir+names[3]+'.dat', runSummary.models, WRITE=WRITE
  
  ; Build up Scenario from all files *.cdf in $HOME$/data/models dir ($MODELNAME$_$SCENARIONAME$_2D.cdf or $MODELNAME$_$SCENARIONAME$_TIME.cdf
  self->buildScenario, units[5], testDir+names[5]+'.dat', runSummary.scenarios, WRITE=WRITE
  
  self->buildRun, units[4], testDir+names[4]+'.dat', runData, WRITE=WRITE
  
; **Only for test** Build up Elaboration ('cause dynamic parameters list)
;elabs=self->test_GetElaboration()
;if keyword_set(TEST) then self->buildElaboration, units[11], testDir+names[11]+'.dat', elabs, parametersSummary.name, WRITE=WRITE
  
END
; *****************************
; save/load internal file
; *****************************
PRO FMFileSystemManager::saveObservationList, fileName, singleCodes, groupNames, groupCodes, groupStatCode

  ;save list of observation in entity GUI!!!
  ERROR=0
  catch, error_status
  
  if error_status NE 0 THEN BEGIN
    ERROR=1
    catch, /CANCEL
    errMsg=dialog_message('problem with file '+fileName+' check existence or read permission.', /ERROR)
  endif
  
  openw, unit, fileName, /GET_LUN
  headerString1='[Singles section]'
  headerString2='[Groups section]'
  
  filename=''
  bufferString=''
  
  printf, unit, headerString1
  for i=0, n_elements(singleCodes)-1 do begin
    line=self.singlePrefix+strcompress(i, /REMOVE)+'='+strcompress(singleCodes[i], /REMOVE)
    printf, unit, line
  endfor
  
  printf, unit, headerString2
  line=self.groupStatPrefix+strcompress(0, /REMOVE)+'='+strcompress(groupStatCode, /REMOVE)
  printf, unit, line
  for i=0, n_elements(groupNames)-1 do begin
    fillType=size(groupCodes, /TYPE)
    if fillType ne 2 then begin
      line=self.groupNamePrefix+strcompress(i, /REMOVE)+'='+groupNames[i]
      printf, unit, line
      codes=*groupCodes[i]
      cc=''
      for j=0, n_elements(codes)-1 do cc=cc+strcompress(codes[j], /REMOVE)+'*'
      line=self.groupCodesPrefix+strcompress(i, /REMOVE)+'='+cc
      printf, unit, line
    endif else begin
      line=self.groupNamePrefix+strcompress(i, /REMOVE)+'='+'-1'
      printf, unit, line
      line=self.groupCodesPrefix+strcompress(i, /REMOVE)+'='+'-1'
      printf, unit, line
    endelse
  endfor
  close, unit & free_lun, unit
  
END

PRO FMFileSystemManager::loadObservationList, fileName, singleCodes, groupNames, groupCodes, groupStatCode, VALIDSINGLE=VALIDSINGLE, VALIDGROUP=VALIDGROUP

  ;load a previous saved (or hand made one) list of observations in entity GUI!!!
  ERROR=0
  catch, error_status
  
  if error_status NE 0 THEN BEGIN
    ERROR=1
    catch, /CANCEL
    errMsg=dialog_message('problem with file '+fileName+' check contents, existence or read permission.', /ERROR)
  endif
  
  openr, unit, fileName, /GET_LUN
  headerString1='[Singles section]'
  headerString2='[Groups section]'
  
  VALIDSINGLE=1
  VALIDGROUP=1
  
  bufferString=''
  
  ;singleParameterValuePrefix=''
  singleCodes=['-1']
  groupNames=['']
  groupCodes=ptr_new()
  i=0
  parameterNames=['']
  parametervalues=['']
  
  while not(eof(unit)) do begin
    readf, unit, bufferString
    i++
    bufferString=strcompress(bufferString, /REMOVE)
    checkFirst=strmid(bufferString, 0,1)
    check1=(strpos(checkFirst, '[')+1) > 0
    check2=(strpos(checkFirst, ';')+1) > 0
    check3=(strpos(checkFirst, '#')+1) > 0
    null=strlen(checkFirst) eq 0
    if (check1+check2+check3) gt 0 or null then begin
    ;      print, 'Discard row', i
    ;      print, bufferString
    endif else begin
      info=strsplit(bufferString, '=', /EXTRACT)
      parameterNames=[parameterNames, info[0]]
      parametervalues=[parametervalues, info[1]]
    endelse
  endwhile
  close, unit & free_lun, unit
  parameterNames=parameterNames[1:*]
  parameterValues=parameterValues[1:*]
  
  for i=0, n_elements(parameterNames)-1 do begin
    if strpos(parameterNames[i], self.singlePrefix) ne -1 then begin
      if parametervalues[i] ne '-1' then singleCodes=[singleCodes, parametervalues[i]]
      continue
    endif
    if strpos(parameterNames[i], self.groupStatPrefix) ne -1 then begin
      if parametervalues[i] ne '-1' then groupStatCode=parametervalues[i]
      continue
    endif
    if strpos(parameterNames[i], self.groupNamePrefix) ne -1 then begin
      if parametervalues[i] ne '-1' then groupNames=[groupNames, parametervalues[i]]
      continue
    endif
    if strpos(parameterNames[i], self.groupCodesPrefix) ne -1 then begin
      grCodes=strsplit(parametervalues[i], '*', /EXTRACT)
      groupCodes=[groupCodes, ptr_new(grCodes, /NO_COPY)]
      continue
    endif
  endfor
  if n_elements(singleCodes) gt 1 then singleCodes=singleCodes[1:*] else VALIDSINGLE=0
  if n_elements(groupNames) gt 1 then groupNames=groupNames[1:*] else VALIDGROUP=0
  if n_elements(groupCodes) gt 1 then groupCodes=groupCodes[1:*] else VALIDGROUP=0
  if n_elements(groupStatCode) ne 1 then VALIDGROUP=0
  
END
;***********************
; directory management
;***********************
FUNCTION FMFileSystemManager::getHomeDir, WITHSEPARATOR=WITHSEPARATOR

  cd,current=dir
  ;dir=strcompress(direct,/remove_all)
  ;dir="C:\work\informatica\sviluppo\idl_projects\POMI"
  ;dir="D:\FairModeApp"
  dir=self.applicationRoot
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator

  return, dir
END


FUNCTION FMFileSystemManager::getDataDir, WITHSEPARATOR=WITHSEPARATOR

  dir=self->getHomeDir(/WITH)
  dir=dir+"data"
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END

FUNCTION FMFileSystemManager::getConfigurationDir, WITHSEPARATOR=WITHSEPARATOR

  dir=self->getHomeDir(/WITH)
  dir=dir+"configuration"
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END

FUNCTION FMFileSystemManager::getLogDir, WITHSEPARATOR=WITHSEPARATOR

  dir=self->getHomeDir(/WITH)
  dir=dir+"log"
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END

FUNCTION FMFileSystemManager::getTempDir, WITHSEPARATOR=WITHSEPARATOR

  dir=self->getHomeDir(/WITH)
  dir=dir+"temp"
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END

FUNCTION FMFileSystemManager::getResourceDir, WITHSEPARATOR=WITHSEPARATOR

  dir=self->getHomeDir(/WITH)
  dir=dir+"resource"
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END

FUNCTION FMFileSystemManager::getDocsDir, WITHSEPARATOR=WITHSEPARATOR

  dir=self->getHomeDir(/WITH)
  dir=dir+"documents"
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END

FUNCTION FMFileSystemManager::getHelpDir, WITHSEPARATOR=WITHSEPARATOR

  dir=self->getHomeDir(/WITH)
  dir=dir+"help"
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END

FUNCTION FMFileSystemManager::getSaveDir, WITHSEPARATOR=WITHSEPARATOR

  dir=self->getHomeDir(/WITH)
  dir=dir+"save"
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END

FUNCTION FMFileSystemManager::getDumpDir, WITHSEPARATOR=WITHSEPARATOR

  dir=self->getHomeDir(/WITH)
  dir=dir+"dump"
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END

;FUNCTION FMFileSystemManager::getDumpDir, WITHSEPARATOR=WITHSEPARATOR
;
;  dir=self->getHomeDir(/WITH)
;  dir=dir+"dump"
;  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
;  return, dir
;
;END

FUNCTION FMFileSystemManager::getObservedDataDir, WITHSEPARATOR=WITHSEPARATOR
;KeesC 30JAN2014
  dir=self->getDataDir(/WITH)
  dirResource=self->getResourceDir(/WITH)  
  fileHlp='monitoring'
  openr,unit,dirResource+'\MyDeltaInput.dat',/get_lun,error=err
  if err eq 0 then begin
    txt=' '
    readf,unit,txt
    readf,unit,txt
    readf,unit,txt
    fileHlp=strcompress(txt,/remove_all)
    close,unit
    free_lun, unit
  endif
  dir=dir+fileHlp
  res=file_test(dir,/directory)
  if res eq 0 then begin
    rsult=dialog_message(['Input OBS directory',' ',dir,' ','does not exist'],/error)
    stop
  endif
  ;  dir=dir+"monitoring"
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END

FUNCTION FMFileSystemManager::getRunDataDir, WITHSEPARATOR=WITHSEPARATOR

;KeesC 30JAN2014
  dir=self->getDataDir(/WITH)
  dirResource=self->getResourceDir(/WITH)  
  fileHlp='modeling'
  openr,unit,dirResource+'\MyDeltaInput.dat',/get_lun,error=err
  if err eq 0 then begin
    txt=' '
    readf,unit,txt
    readf,unit,txt
    fileHlp=strcompress(txt,/remove_all)
    close,unit
    free_lun, unit
  endif
  dir=dir+fileHlp
  res=file_test(dir,/directory)
  if res eq 0 then begin
    rsult=dialog_message(['Input MOD directory',' ',dir,' ','does not exist'],/error)
    stop
  endif
;  dir=dir+"modeling"
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END
;**************************
; file/extension management
;**************************
FUNCTION FMFileSystemManager::getPSExtension

  return, '.ps'
  
END

FUNCTION FMFileSystemManager::getObservedListExtension

  return, '.obl'
  
END

FUNCTION FMFileSystemManager::getBatchExtension

  return, '.btc'
  
END

FUNCTION FMFileSystemManager::getRequestExtension

  return, '.rqs'
  
END

FUNCTION FMFileSystemManager::getEntityExtension

  return, '.ent'
  
END

FUNCTION FMFileSystemManager::getElaborationExtension

  return, '.elb'
  
END

FUNCTION FMFileSystemManager::getMonitoringFileExtension

  return, '.csv'
  
END

FUNCTION FMFileSystemManager::getRunFileTimeSuffix

  return, '_TIME'
  
END

FUNCTION FMFileSystemManager::getRunFile2DSuffix

  return, '_2D'
  
END

FUNCTION FMFileSystemManager::getRunFileExtension

  return, '.cdf'
  
END

FUNCTION FMFileSystemManager::getAvailableRunFileExtension

  return, ['.cdf', '.csv']
  
END

FUNCTION FMFileSystemManager::getSplashLogoFileName

  fileName=self->getResourceDir(/WITH)
  fileName=fileName+"fairmode_logo.bmp"
  return, fileName
  
END

FUNCTION FMFileSystemManager::getLogoFileName

  fileName=self->getResourceDir(/WITH)
  fileName=fileName+"inside_logo.bmp"
  return, fileName
  
END

FUNCTION FMFileSystemManager::getPomiLogoFileName

  fileName=self->getResourceDir(/WITH)
  fileName=fileName+"pomi_logo.jpg"
  return, fileName
  
END

FUNCTION FMFileSystemManager::getJRCLogoFileName

  fileName=self->getResourceDir(/WITH)
  fileName=fileName+"jrc_logo.jpg"
  return, fileName
  
END

FUNCTION FMFileSystemManager::getIesLogoFileName

  fileName=self->getResourceDir(/WITH)
  fileName=fileName+"ies_logo.jpg"
  return, fileName
  
END

FUNCTION FMFileSystemManager::getStartUpFileName
;KeesC 30JAN2014
  dirResource=self->getResourceDir(/WITH)
  fileHlp='startup.ini'
  openr,unit,dirResource+'MyDeltaInput.dat',/get_lun,error=err
  if err eq 0 then begin
    txt=' '
    readf,unit,txt
    fileHlp=strcompress(txt,/remove_all)
    close, unit
    free_lun, unit
  endif
  fileName=dirResource+fileHlp
  openr,unit,fileName,/get_lun,error=err
  if err eq 0 then begin
    close, unit
    free_lun, unit
  endif else begin
    rsult=dialog_message(['Input file',' ',fileName,' ','does not exist'],/error)
    stop
  endelse
;  fileName=fileName+"startup.ini"
  return, fileName
  
END

FUNCTION FMFileSystemManager::getTempDataFile

  fileName=self->getTempDir(/WITH)
  fileName=fileName+"values.dat"
  return, fileName
  
END


FUNCTION FMFileSystemManager::getInitFileName

  fileName=self->getResourceDir(/WITH)
  fileName=fileName+"init.ini"
  return, fileName
  
END

;**************************
; get/set
;**************************
FUNCTION FMFileSystemManager::getLastUsedFileName

  return, self.lastUsedFileName
  
END

PRO FMFileSystemManager::setLastUsedFileName, fileName

  self.lastUsedFileName=fileName
  
END

FUNCTION FMFileSystemManager::getSystemDirSeparator

  return, self.oSDirSeparator
  
END

;from init file
PRO FMFileSystemManager::streamPrint

  print, '***********************'
  print, '**Start of<',OBJ_CLASS(self),'>**'
  print, 'oSDirSeparator:', self.oSDirSeparator
  print, 'lastUsedFileName:', self.lastUsedFileName
  print, 'singlePrefix:', self.singlePrefix
  print, 'groupNamePrefix:', self.groupNamePrefix
  print, 'groupCodesPrefix:', self.groupCodesPrefix
  print, 'browserApplication:', self.browserApplication
  print, '***********************'
  print, '**End of:<',OBJ_CLASS(self),'>**'
  
END

;***********************
; constructor/destructor
;***********************

FUNCTION FMFileSystemManager::init

  if not self -> Object :: init() then return , 0
  self.oSDirSeparator=path_sep()
  IDLDir=!DIR
  openr, unit, IDLDir+self.oSDirSeparator+'init.ini', /GET_LUN
  bufferString=''
  readf, unit, bufferString
  mainDir=strsplit(bufferString, '=', /EXTRACT)
  self.applicationRoot=mainDir[1]
  close, unit
  free_lun, unit
  
  self.utility=obj_new('FMUtility')
  self.parameterHeader='[PARAMETERS]'
  self.monitoringHeader='[MONITORING]'
  self.scaleHeader='[SCALE]'
  ;self.modelTypeHeader='[MODELTYPE]'
  self.modelHeader='[MODEL]'
  self.singlePrefix='SINGLE*'
  self.groupNamePrefix='GROUP*'
  self.groupCodesPrefix='GROUPCODES*'
  self.groupStatPrefix='GROUPSTAT*'
  return , 1
  
END

PRO FMFileSystemManager::cleanUp

  self -> Object :: cleanUp
  obj_destroy, self.utility
  
END

;****************************************************************************************

PRO FMFileSystemManager__Define

  Struct = { FMFileSystemManager , $
    utility: obj_new(), $
    applicationRoot: '', $
    oSDirSeparator: '', $
    lastUsedFileName: '', $
    singlePrefix: '', $
    parameterHeader: '', $
    monitoringHeader:'', $
    ;MM summer 2012 Start
    scaleHeader:'', $
    ;    modelTypeHeader:'', $
    modelHeader: '', $
    ;MM summer 2012 End
    groupStatPrefix: '', $
    groupNamePrefix: '', $
    groupCodesPrefix: '', $
    Inherits Object $
    }
    
END

;****************************************************************************************
