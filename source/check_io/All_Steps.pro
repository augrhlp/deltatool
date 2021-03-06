@DeltaCheck_io_definecommon
pro All_Steps, DOCDFCONVERSION=DOCDFCONVERSION, deltaMgr=deltaMgr
  common keesc1
  
  conversionStruct={startUpFile:'', $
    startHour:0, endHour:0, inputDir:'', outputDir:'', $
    prefixId:'', modelName:'', fulloutFileName:'', stringStartHour:'', stringEndHour:'', $
    logWin:0l, PROGRESSBAR:0b}
  csv2cdfInfo=replicate(conversionStruct, 1)
  
  ;STEP 01: Check on existence of directories
  ;STEP 02: Check on existence of STARTUPfile
  ;STEP 03: MODEL/PARAMETERS/MONITORING sections in STARTUPfile
  ;STEP 04: Read Species from PARAMETERS section in STARTUPfile
  ;STEP 05: Read Stations from MONITORING section in STARTUPfile
  ;KeesC 18JAN2015 1 line
  ;STEP 06: Check redundant station-Codes/Names/Abbr in STARTUPfile
  ;STEP 07: Check Nb of stations in STARTUPfile and MONITORING_DIR
  ;STEP 08: Consistency of statnames and OBSfiles
  ;STEP 09: Check consistency of species in STARTUPfile and OBSfile
  ;STEP 10: TimeLength OBSfiles [< 8760|8784 (Hourly), =1 (Yearly)]
  ;STEP 11: Conversion csv to cdf
  ;STEP 12: OBS data availability at stations (%); Extreme values
  ;STEP 13: Check OBS equal to zero (real or novalue ?)
  ;STEP 14: Existence of MODfile
  ;STEP 15: Existence of stations/species/attribute in MODfile
  ;STEP 16: TimeLength of MOD/species files [< 8760|8784 (hourly), =1 (yearly)]
  ;STEP 17: Check on MOD NaN/Inf/Extreme values
  ;STEP 18: MOD availability at stations for STARTUP species (%)
  ;STEP 19: Basic Statistics
  
  
  ; *******************************************************************************************************
  ; Counter
  ;nrlist=['01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19',$
  ;        '20','21','22','23','24','25']
  ;step='STEP'+nrlist
  
  ; *******************************************************************************************************
  atxt=' '
  atxt1=' '
  spec=strarr(100)
  types=strarr(100)
  units=strarr(100)
  statnames=strarr(6000)
  ;KeesC 18JAN2015  2 lines
  statcodes=strarr(6000)
  statabbr=strarr(6000)
  staterror=strarr(6000)
  spec_stations=strarr(6000)
  ierror=0
  
  ; *******************************************************************************************************
  ;*******************************************************************************************************
  ; Test on existence of directories
  widget_control,labprog_txt,set_value='STEP 01'
  res=file_test(dir,/directory)
  if res eq 0 then begin
    txt='STEP 01 WARNING! Directory HOME_DIR does not exist: See STARTUPfile'
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
    hlp='HOME_DIR does not exist'
    answ=dialog_message([hlp,' ','CONTINUE ?'],/question,title='STEP 01 WARNING')
    if answ eq 'No' then begin
      close,11
      close,12
      txtall=['STEP 01 WARNING ** STOP',txtall]
      widget_control,labcom_txt,set_value=txtall
      ierror=1
      return
    endif else begin
      txtall=['STEP 01 WARNING ** CONTINUE',txtall]
      widget_control,labcom_txt,set_value=txtall
    endelse
  endif else begin
    printf,11,' '
    printf,11,'*********************************************'
    printf,11,'***         STEP 01                         *'
    printf,11,'*** Check on existence of directories       *'
    printf,11,'*********************************************'
    print,'STEP 01'
    printf,11,'STEP 01 OK: HOME_DIR exists'
    txt='STEP 01 OK: '+dir+' exists'
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
  endelse
  res=file_test(dir_res,/directory)
  if res eq 0 then begin
    txt='STEP 01 STOP! Directory RESOURCE_DIR does not exist: See STARTUPfile'
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
    txtall=['STOP',txtall]
    widget_control,labcom_txt,set_value=txtall
    close,11
    close,12
    ierror=1
    return
  endif else begin
    printf,11,'STEP 01 OK: RESOURCE_DIR exists'
    txt='STEP 01 OK: RESOURCE_DIR exists'
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
  endelse
  if itobs eq 1 or itobsmod eq 1 then begin
    res=file_test(dir_obs,/directory)
    if res eq 0 then begin
      txt='STEP 01 STOP! Directory MONITORING_DIR does not exist: See STARTUPfile'
      txtall=[txt,txtall]
      widget_control,labcom_txt,set_value=txtall
      txtall=['STOP',txtall]
      widget_control,labcom_txt,set_value=txtall
      close,11
      close,12
      ierror=1
      return
    endif else begin
      csv2cdfInfo.inputDir=dir_obs
      csv2cdfInfo.outputDir=dir_obs
      printf,11,'STEP 01 OK: MONITORING_DIR exists'
      txt='STEP 01 OK: MONITORING_DIR exists'
      txtall=[txt,txtall]
      widget_control,labcom_txt,set_value=txtall
    endelse
  endif
  if itmod eq 1 or itobsmod eq 1 then begin
    res=file_test(dir_mod,/directory)
    if res eq 0 then begin
      txt='STEP 01 STOP! Directory MODELING_DIR does not exist: See STARTUPfile'
      txtall=[txt,txtall]
      widget_control,labcom_txt,set_value=txtall
      txtall=['STOP',txtall]
      widget_control,labcom_txt,set_value=txtall
      close,11
      close,12
      ierror=1
      return
    endif else begin
      printf,11,'STEP 01 OK: MODELING_DIR exists'
      txt='STEP 01 OK: MODELING_DIR exists'
      txtall=[txt,txtall]
      widget_control,labcom_txt,set_value=txtall
    endelse
  endif
  printf,11,' '
  widget_control,labok(1),set_value=' *OK* '
  
  leapyear=0
  if 4*(fix(year)/4) eq fix(year) then leapyear=1
  YearCheckWindow=year
  YearStartup=fix(year)
  
  ; *******************************************************************************************************
  if itgen eq 1 then begin  ;startup
  
    widget_control,labprog_txt,set_value='STEP 02'
    ; Test on existence of startup.ini
    printf,11,'***********************************************'
    printf,11,'***         STEP 02                           *'
    printf,11,'*** Check on existence of STARTUPfile         *'
    printf,11,'***********************************************'
    print,'STEP 02'
    ;res=file_test(dir_res+startup)
    res=file_test(startup)
    if res eq 0 then begin
      txt='STEP 02 STOP! STARTUPfile file not found in RESOURCE_DIR'
      txtall=[txt,txtall]
      widget_control,labcom_txt,set_value=txtall
      txtall=['STOP',txtall]
      widget_control,labcom_txt,set_value=txtall
      close,11
      close,12
      ierror=1
      return
    endif else begin
      printf,11,'STEP 02 OK: STARTUPfile file exists in RESOURCE_DIR'
      csv2cdfInfo.startUpFile=startup
      txt='STEP 02 OK: STARTUPfile file exists in RESOURCE_DIR'
      txtall=[txt,txtall]
      widget_control,labcom_txt,set_value=txtall
    endelse
    printf,11,' '
    widget_control,labok(2),set_value=' *OK* '
    
    ; *******************************************************************************************************
    
    widget_control,labprog_txt,set_value='STEP 03'
    ; test on existence of sections SCALE/PARAMETERS/MONITORING in startup.ini
    printf,11,'*********************************************************'
    printf,11,'***         STEP 03                                     *'
    printf,11,'*** MODEL/PARAMETERS/MONITORING sections in STARTUPfile *'
    printf,11,'*********************************************************'
    print,'STEP 03'
    close,1 & openr,1,startup
    check_MODEL=0
    check_PARAMETERS=0
    check_MONITORING=0
    
    while ~eof(1) do begin
      atxt=discardComments(1)
      if atxt eq '[MODEL]' then begin
        check_MODEL=1
        atxt=discardComments(1)
        fileYear=atxt
        YearStartup=fix(atxt)
        utility=obj_new('FMUtility')
        if not(utility->IsNumber(fileYear)) then begin
          txt='STEP 03: STOP! MODEL first line is NE a year : See MODEL section in STARTUPfile'
          txtall=[txt,txtall]
          widget_control,labcom_txt,set_value=txtall
          txtall=['STOP',txtall]
          widget_control,labcom_txt,set_value=txtall
          close,11
          close,12
          ierror=1
          return
        endif
        if YearCheckWindow ne YearStartup then begin
          txt='STEP 03: WARNING! YearCheckWindow ('+strcompress(YearCheckWindow,/remove_all)+') NE YearStartup ('+$
            strcompress(YearStartup,/remove_all)+')'
          txtall=[txt,txtall]
          widget_control,labcom_txt,set_value=txtall
        endif
        atxt=discardComments(1)
        frequency=strlowcase(atxt)
        if strupcase(frequency) ne 'HOUR' and strupcase(frequency) ne 'YEAR' then begin
          txt='STEP 03: STOP! MODEL second line is NE to hour and NE to year : See MODEL section in STARTUPfile'
          txtall=[txt,txtall]
          widget_control,labcom_txt,set_value=txtall
          txtall=['STOP',txtall]
          widget_control,labcom_txt,set_value=txtall
          close,11
          close,12
          ierror=1
          return
        endif
        atxt=discardComments(1)
        scale=strlowcase(atxt)
        if (scale ne 'local' and scale ne 'urban' and scale ne 'regional') then begin
          txt='STEP 03: STOP! MODEL third line is NE local/urban/regional: See MODEL section in STARTUPfile'
          txtall=[txt,txtall]
          widget_control,labcom_txt,set_value=txtall
          txtall=['STOP',txtall]
          widget_control,labcom_txt,set_value=txtall
          close,11
          close,12
          ierror=1
          return
        endif
        txt='STEP 03 OK: MODEL / '+fileYear+' '+frequency+' '+scale+' section exists in STARTUPfile'
        printf,11,txt
        txtall=[txt,txtall]
        widget_control,labcom_txt,set_value=txtall
        if fix(fileYear) lt 1900 or fix(fileYear) gt 2100 then begin
          txt='STEP 03: WARNING! YEAR LT 1900 or YEAR GT 2100. MODEL section in STARTUPfile'
          txtall=[txt,txtall]
          widget_control,labcom_txt,set_value=txtall
          txtall=['WARNING',txtall]
          widget_control,labcom_txt,set_value=txtall
        endif else begin
          printf,11,'STEP 03 OK: 1900 < YEAR < 2100'
          txt='STEP 03 OK: 1900 < YEAR < 2100'
          txtall=[txt,txtall]
          widget_control,labcom_txt,set_value=txtall
        endelse
      endif
      if atxt eq '[PARAMETERS]' then begin
        check_PARAMETERS=1
        readf,1,atxt
        if strmid(atxt,0,1) ne ';' then begin
        txt='STEP 03: STOP! Line after [PARAMETERS] in STARTUPfile does not start with ;'
        txtall=[txt,txtall]
        widget_control,labcom_txt,set_value=txtall
        txtall=['STOP',txtall]
        widget_control,labcom_txt,set_value=txtall
        close,11
        close,12
        ierror=1
        return
      endif else begin
        printf,11,'STEP 03 OK: PARAMETERS section exists in STARTUPfile'
        txt='STEP 03 OK: PARAMETERS section exists in STARTUPfile'
        txtall=[txt,txtall]
        widget_control,labcom_txt,set_value=txtall
      endelse
    endif
    if atxt eq '[MONITORING]' then begin
      check_MONITORING=1
      atxt=discardComments(1)
      if strmid(atxt,0,4) ne 'Stat' then begin
        txt='STEP 03: STOP! Line after [MONITORING] in STARTUPfile does not start with Stat'
        txtall=[txt,txtall]
        widget_control,labcom_txt,set_value=txtall
        txtall=['STOP',txtall]
        widget_control,labcom_txt,set_value=txtall
        close,11
        close,12
        ierror=1
        return
      endif else begin
        printf,11,'STEP 03 OK: MONITORING section exists in STARTUPfile'
        txt='STEP 03 OK: MONITORING section exists in STARTUPfile'
        txtall=[txt,txtall]
        widget_control,labcom_txt,set_value=txtall
      endelse
    endif
    
  endwhile
  if check_MODEL eq 0 then begin
    txt='STEP 03: STOP! No [MODEL] section in STARTUPfile file or check spelling'
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
    txtall=['STOP',txtall]
    widget_control,labcom_txt,set_value=txtall
    close,11
    close,12
    ierror=1
    return
  endif
  if check_PARAMETERS eq 0 then begin
    txt='STEP 03: STOP! No [PARAMETERS] section in STARTUPfile file or check spelling'
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
    txtall=['STOP',txtall]
    widget_control,labcom_txt,set_value=txtall
    close,11
    close,12
    ierror=1
    return
  endif
  if check_MONITORING eq 0 then begin
    txt='STEP 03: STOP! No [MONITORING] section in STARTUPfile file or check spelling'
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
    txtall=['STOP',txtall]
    widget_control,labcom_txt,set_value=txtall
    close,11
    close,12
    ierror=1
    return
  endif
  printf,11,' '
  widget_control,labok(3),set_value=' *OK* '
  ;
  ;  ; *******************************************************************************************************
  ;
  widget_control,labprog_txt,set_value='STEP 04'
  ; Start read section PARAMETERS from startup.ini
  
  readSpec
  
  printf,11,'*************************************************************'
  printf,11,'***         STEP 04                                         *'
  printf,11,'*** Read Species from PARAMETERS section in STARTUPfile     *'
  printf,11,'*************************************************************'
  printf,12,'*************************************************************'
  printf,12,'***         STEP 04                                         *'
  printf,12,'*** Read Species from PARAMETERS section in STARTUPfile     *'
  printf,12,'*************************************************************'
  print,'STEP 04'
  txt='STEP 04 (Info): Species from PARAMETER section:'
  txtall=[txt,txtall]
  widget_control,labcom_txt,set_value=txtall
  hlp=''
  for i=0,n_elements(spec)-1 do hlp=hlp+spec(i)+'  '
  txt='STEP 04 (Info): '+hlp
  txtall=[txt,txtall]
  widget_control,labcom_txt,set_value=txtall
  txt='STEP 04 (Info): Types from PARAMETER section:'
  txtall=[txt,txtall]
  widget_control,labcom_txt,set_value=txtall
  hlp=''
  for i=0,n_elements(types)-1 do hlp=hlp+types(i)+'  '
  txt='STEP 04 (Info): '+hlp
  txtall=[txt,txtall]
  widget_control,labcom_txt,set_value=txtall
  txt='STEP 04 (Info): Units from PARAMETER section:'
  txtall=[txt,txtall]
  widget_control,labcom_txt,set_value=txtall
  hlp=''
  for i=0,n_elements(units)-1 do hlp=hlp+units(i)+'  '
  txt='STEP 04 (Info): '+hlp
  txtall=[txt,txtall]
  widget_control,labcom_txt,set_value=txtall
  iprob=0
  for i=0,n_elements(spec)-1 do begin
    ertxt=''
    if staterror(i) eq 1 then begin
      ertxt='             ==> Information not complete in STARTUPfile'
      iprob=1
    endif
    printf,12,fix(i+1),spec(i)+ertxt,form='(i5,5x,a)'
  endfor
  if iprob eq 0 then begin
    printf,11,' '
    printf,12,' '
    widget_control,labok(4),set_value=' *OK* '
    txt='STEP 04 (Info): Number of species from PARAMETERS is '+strtrim(fix(n_elements(spec)),2)
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
  endif else begin
    txt='STEP 04: STOP! Information in PARAMETERS not complete (See SummaryFile) '
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
    txtall=['STOP',txtall]
    widget_control,labcom_txt,set_value=txtall
    close,11
    close,12
    ierror=1
    return
  endelse
  widget_control,labok(4),set_value=' *OK* '
  
  ; *******************************************************************************************************
  
  widget_control,labprog_txt,set_value='STEP 05'
  ;Start read section MONITORING from startupfile
  
  readstatspec
  
  printf,11,'*************************************************************'
  printf,11,'***         STEP 05                                         *'
  printf,11,'*** Read Stations from MONITORING section in STARTUPfile    *'
  printf,11,'*************************************************************'
  printf,11,'See SummaryFile'
  printf,12,'*************************************************************'
  printf,12,'***         STEP 05                                         *'
  printf,12,'*** Read Stations from MONITORING section in STARTUPfile    *'
  printf,12,'*************************************************************'
  print,'STEP 05'
  iprob=0
  for i=0,n_elements(statnames)-1 do begin
    widget_control,labprog_txt,set_value='STEP 05: '+string(fix(i+1))+'  / '+string(fix(n_elements(statnames)))
    ertxt=''
    if staterror(i) eq 1 then begin
      ertxt='                  ==> Information not complete in STARTUPfile'
      iprob=1
    endif
    printf,12,fix(i+1),statnames(i)+ertxt,form='(i5,5x,a)'
  endfor
  if iprob eq 0 then begin
    printf,11,' '
    printf,12,' '
    widget_control,labok(5),set_value=' *OK* '
    txt='STEP 05 (Info): Number of stations from MONITORING is '+strtrim(fix(n_elements(statnames)),2)
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
  endif else begin
    txt='STEP 05: STOP! Information in MONITORING not complete (See SummaryFile) '
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
    txtall=['STOP',txtall]
    widget_control,labcom_txt,set_value=txtall
    close,11
    close,12
    ierror=1
    return
  endelse
  
endif   ; end itgen general

; *******************************************************************************************************
; *******************************************************************************************************
;
if itobs eq 1 then begin

  ReadSpec
  
  ReadStatSpec
  
  widget_control,labprog_txt,set_value='STEP 06'
  ;KeesC 18JAN2015 12 lines
  Nres0=sort(strupcase(statnames))
  Nres1=strupcase(statnames(Nres0))
  Nres=uniq(Nres1)
  Ncc=where(indgen(n_elements(statnames)) eq Nres)
  Cres0=sort(strupcase(statcodes))
  Cres1=strupcase(statcodes(Cres0))
  Cres=uniq(Cres1)
  Ccc=where(indgen(n_elements(statcodes)) eq Cres)
  Ares0=sort(strupcase(statabbr))
  Ares1=strupcase(statabbr(Ares0))
  Ares=uniq(Ares1)
  Acc=where(indgen(n_elements(statabbr)) eq Ares)
  printf,11,'*************************************************************'
  printf,11,'***         STEP 06                                         *'
  printf,11,'*** Check redundant station-Codes/Names/Abbr in STARTUPfile        *'
  printf,11,'*************************************************************'
  ; KeesC 18JAN2015 5 lines
  printf,11,'See SummaryFile'
  printf,12,'*************************************************************'
  printf,12,'***         STEP 06                                         *'
  printf,12,'*** Check redundant station-Codes/Names/Abbr in STARTUPfile        *'
  printf,12,'*************************************************************'
  print,'STEP 06'
  ; KeesC 18JAN2015 1 line
  if n_elements(Nres) eq n_elements(statnames) and n_elements(Cres) eq n_elements(statcodes) and $
    n_elements(Ares) eq n_elements(statabbr) then begin
    printf,11,'STEP 06 OK: No redundant filenames in STARTUPfile'
    txt='STEP 06 OK: No redundant filenames in STARTUPfile'
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
  endif else begin
    ;KeesC 18JAN2015 18 lines
    if n_elements(Nres) ne n_elements(statnames) then begin
      txt='STEP 06: STOP! Redundancy in STARTUPfile: '+Nres1(n_elements(Ncc))+' station to be checked '
      txtall=[txt,txtall]
      widget_control,labcom_txt,set_value=txtall
      printf,12,'Redundancy in STARTUPfile: '+Nres1(n_elements(Ncc))+' station to be checked '
    endif
    if n_elements(Cres) ne n_elements(statcodes) then begin
      txt='STEP 06: STOP! Redundancy in STARTUPfile: '+Cres1(n_elements(Ccc))+' station to be checked '
      txtall=[txt,txtall]
      widget_control,labcom_txt,set_value=txtall
      printf,12,'Redundancy in STARTUPfile: '+Cres1(n_elements(Ccc))+' station to be checked '
    endif
    if n_elements(Ares) ne n_elements(statabbr) then begin
      txt='STEP 06: STOP! Redundancy in STARTUPfile: '+Ares1(n_elements(Acc))+' station to be checked '
      txtall=[txt,txtall]
      widget_control,labcom_txt,set_value=txtall
      printf,12,'Redundancy in STARTUPfile: '+Ares1(n_elements(Acc))+' station to be checked '
    endif
    txtall=['STOP',txtall]
    widget_control,labcom_txt,set_value=txtall
    close,11
    close,12
    ierror=1
    return
  endelse
  printf,11,' '
  ;KeesC 18JAN2015 1 line
  printf,12,' '
  widget_control,labok(6),set_value=' *OK* '
  
  ; *******************************************************************************************************
  
  widget_control,labprog_txt,set_value='STEP 07'
  printf,11,'***************************************************************'
  printf,11,'***         STEP 07                                           *'
  printf,11,'*** Check Nb of stations in STARTUPfile and MONITORING_DIR    *'
  printf,11,'***************************************************************'
  print,'STEP 07'
  dir_obs=dir_obs+path_sep()
  fileYearly=file_search(dir_obs+'OBS_Yearly.csv',count=count_fileY)
  if count_fileY eq 0 then begin   ; not yearly
    filenames=file_search(dir_obs+'*.csv',count=count_filenames)
    slashPos=strpos(filenames, path_sep(), /REVERSE_SEARCH)
    for i=0,count_filenames-1 do begin
      filenames[i]=strmid(filenames[i],slashPos[i]+1,100)
      res=strsplit(filenames(i),'.',/extract)
      filenames(i)=res(0)
    endfor
  endif else begin
    atxt=' '
    get_lun,lunY
    openr,lunY,dir_obs+'OBS_Yearly.csv'
    readf,lunY,atxt
    res=strsplit(atxt,';',/extract)
    if strlowcase(res[0]) ne 'yearlyavg' or n_elements(res) le 2 then begin
      txt='STEP 07: STOP! Incorrect first line in OBS_Yearly.csv'
      txtall=[txt,txtall]
      widget_control,labcom_txt,set_value=txtall
      txtall=['STOP',txtall]
      widget_control,labcom_txt,set_value=txtall
      close,lunY
      free_lun,lunY
      close,11
      close,12
      ierror=1
      return
    endif
    ;paramsYearlyObs=res[2:n_elements(res)-1]
    ;MM feb 2015
    paramsYearlyObs=res[2:*]
    YearOBS=fix(res(1))
    itel=0
    filenames=strarr(2000)
    while not(eof(lunY)) do begin
      readf,lunY,atxt
      res=strsplit(atxt,';',/extract)
      filenames(itel)=res(0)
      itel=itel+1
    endwhile
    filenames=filenames(0:itel-1)
    close,lunY
    free_lun,lunY
    count_filenames=itel
  endelse
  printf,11,'Nb of stations listed in STARTUPfile = ',nstat
  printf,11,'Nb of OBS-stations listed in STARTUPfile = ',numb_OBS
  printf,11,'Nb of NoOBS-stations listed in STARTUPfile = ',numb_NoOBS
  if count_fileY eq 0 then printf,11,'Nb of files in MONITORING_DIR  = ', count_filenames
  if count_fileY eq 1 then printf,11,'Nb of stations in OBS_Yearly.csv  = ', count_filenames
  if numb_OBS le count_filenames then begin
    if count_fileY eq 0 then begin
      printf,11,'STEP 07 OK: Nb of OBS-stations in STARTUPfile **LE** Nb of station files in MONITORING_DIR'
      txt='STEP 07 OK: Nb of OBS-stations in STARTUPfile **LE** Nb of station files in MONITORING_DIR'
    endif
    if count_fileY eq 1 then begin
      printf,11,'STEP 07 OK: Nb of OBS-stations in STARTUPfile **LE** Nb of stations in OBS_Yearly.csv'
      txt='STEP 07 OK: Nb of OBS-stations in STARTUPfile **LE** Nb of stations in OBS_Yearly.csv'
    endif
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
  endif else begin
    if count_fileY eq 0 then txt='STEP 07: STOP! Nb of OBS-stations in STARTUPfile **GT** Nb of station files in MONITORING_DIR'
    if count_fileY eq 1 then txt='STEP 07: STOP! Nb of OBS-stations in STARTUPfile **GT** Nb of stations in OBS_Yearly.csv'
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
    txtall=['STOP',txtall]
    widget_control,labcom_txt,set_value=txtall
    close,11
    close,12
    ierror=1
    return
  endelse
  printf,11,' '
  widget_control,labok(7),set_value=' *OK* '
  
  ; *******************************************************************************************************
  ; consistency between station names and csv files
  
  widget_control,labprog_txt,set_value='STEP 08'
  printf,11,'****************************************************'
  printf,11,'***         STEP 08                                *'
  printf,11,'***  Consistency of statnames and OBSfiles         *'
  printf,11,'****************************************************'
  print,'STEP 08'
  count_file=0
  inconsistent_files=' '
  fileYearly=file_search(dir_obs+'OBS_Yearly.csv',count=count_fileY)
  for i=0,n_elements(statnames)-1 do begin
    if strupcase(spec_stations(i)) ne 'NOOBS' then begin
      cc=where(strupcase(statnames(i)) eq strupcase(filenames), count)
      if count eq 0 then inconsistent_files=[inconsistent_files,statnames(i)]
      count_file=count_file+count
    endif
  endfor
  if count_file ne numb_OBS then begin
    txt='STEP 08: STOP! Inconsistent naming in STARTUPfile and OBSfiles'
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
    if count_fileY eq 0 then printf,11,'NO consistency in OBS-statnames and OBSfiles - Check the following stations:'
    if count_fileY eq 1 then printf,11,'NO consistency in OBS-statnames and OBS_Yearly.csv - Check the following stations:'
    printf,11,inconsistent_files
    txtall=['STOP',txtall]
    widget_control,labcom_txt,set_value=txtall
    close,11
    close,12
    ierror=1
    return
  endif else begin
    if count_fileY eq 0 then begin
      printf,11,'STEP 08 OK: Consistency in OBS-statnames and OBSfiles'
      txt='STEP 08 OK: Consistency in statnames and OBSfiles'
    endif
    if count_fileY eq 1 then begin
      printf,11,'STEP 08 OK: Consistency in OBS-statnames and OBS_Yearly.csv'
      txt='STEP 08 OK: Consistency in statnames and OBS_Yearly.csv'
    endif
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
  endelse
  printf,11,' '
  widget_control,labok(8),set_value=' *OK* '
  
  ; *******************************************************************************************************
  ; species consistency startup.ini and OBSfiles
  
  widget_control,labprog_txt,set_value='STEP 09'
  printf,11,'**************************************************************'
  printf,11,'***         STEP 09                                          *'
  printf,11,'***  Check consistency of species in STARTUPfile and OBSfile *'
  printf,11,'**************************************************************'
  print,'STEP 09'
  iprob=0
  fileYearly=file_search(dir_obs+'OBS_Yearly.csv',count=count_fileY)
  if count_fileY eq 1 then begin
    for is=0,n_elements(speclist)-1 do begin
      cc=where(speclist(is) eq paramsYearlyObs,count)
      if count eq 0 then begin
        iprob=1
        printf,11,'Inconsistent species: '+speclist(is)+' in STARTUPfile & NOT in OBS_Yearly.csv
      endif
    endfor
  endif
  if count_fileY eq 0 then begin
    for i=0,n_elements(statnames)-1 do begin
      widget_control,labprog_txt,set_value='STEP 09: '+string(fix(i+1))+'  / '+string(fix(n_elements(statnames)))
      if strupcase(spec_stations(i)) ne 'NOOBS' then begin
        fn=file_search(dir_obs+statnames[i]+'.csv',count=count)
        if count eq 1 then begin
          close,1 & openr,1,fn
          speclist=strsplit(spec_stations(i),'*',/extract) ; speclist is list of specs at station i from startup.ini
          readf,1,atxt  ; first line in obsfile
          res=strsplit(atxt,';',/extract) ; yyyy mm dd hh PM10 PM25- OR - YearlyAvg 2009 Param1 Param2
          res=strcompress(res,/remove_all)
          nres=n_elements(res)
          if strlowcase(res[0]) eq 'yearlyavg' then begin       ;if 'YearlyAvg'   YearlyAvg obs values
            res2=strsplit(atxt,';',/extract)  ; YearlyAvg 2009 PM10 PM25
            nb_specstat=n_elements(res2)-2
            spec_station=res2(2:nb_specstat+1) ; PM10 PM25
          endif else begin                                   ; hourly obs values
            nb_specstat=n_elements(res)-4
            spec_station=res(4:4+nb_specstat-1) ; PM10 PM25
          endelse
          ;if statnames[i] eq '42R801BORGERHOUT' then stop
          for is=0,n_elements(speclist)-1 do begin
            cc=where(speclist(is) eq spec_station,count)
            if count eq 0 then begin
              iprob=1
              printf,11,'Inconsistent species: '+speclist(is)+' in STARTUPfile & NOT in ',statnames(i)
            endif
          endfor
          close,1
        endif
      endif
      nextstat09:
    endfor
  endif
  if iprob eq 1 then begin
    if count_fileY eq 0 then txt='STEP 09: STOP! Inconsistency of species in STARTUPfile (station lines) and OBSfile'
    if count_fileY eq 1 then txt='STEP 09: STOP! Inconsistency of species in STARTUPfile (station lines) and OBS_Yearly.csv'
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
    txtall=['STOP',txtall]
    widget_control,labcom_txt,set_value=txtall
    close,11
    close,12
    ierror=1
    return
  endif else begin
    if count_fileY eq 0 then begin
      printf,11,'STEP 09 OK: Species at stations (from STARTUPfile) are also species in OBSfile for each station'
      txt='STEP 09 OK: Species at station (from STARTUPfile) are also species in OBSfile for each station'
    endif
    if count_fileY eq 1 then begin
      printf,11,'STEP 09 OK: Species at stations (from STARTUPfile) are also species in OBS_Yearly.csv'
      txt='STEP 09 OK: Species at station (from STARTUPfile) are also species in OBS_Yearly.csv'
    endif
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
  endelse
  printf,11,' '
  widget_control,labok(9),set_value=' *OK* '
  
  ; *******************************************************************************************************
  
  widget_control,labprog_txt,set_value='STEP 10'
  printf,11,'*****************************************************************'
  printf,11,'***         STEP 10                                             *'
  
  printf,11,'***  TimeLength OBSfiles [< 8760|8784 (Hourly), =1 (Yearly)]    *'
  printf,11,'*****************************************************************'
  print,'STEP 10'
  iprob=0
  fileYearly=file_search(dir_obs+'OBS_Yearly.csv',count=count_fileY)
  if count_fileY eq 1 then begin
    if YearOBS lt 1900 or YearOBS gt 2100 then begin
      printf,11,'Year may be out of range '
      txt='STEP 10: WARNING! Year may be out of range '
      txtall=[txt,txtall]
      widget_control,labcom_txt,set_value=txtall
    endif
    txt='STEP 10 OK'
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
  endif
  if count_fileY eq 0 then begin
    for i=0,n_elements(statnames)-1 do begin
      if strupcase(spec_stations(i)) ne 'NOOBS' then begin
        fn=dir_obs+statnames[i]+'.csv'
        fns=strmid(fn,strlen(dir_obs),100)
        widget_control,labprog_txt,set_value='STEP 10: '+string(fix(i+1))+'  / '+string(fix(n_elements(statnames)))
        nlines=file_lines(fn)
        close,1 & openr,1,fn
        readf,1,atxt  ; first line in obsfile
        res=strsplit(atxt,';',/extract) ; yyyy mm dd hh PM10 PM25- OR - year PM10 PM25
        res=strcompress(res,/remove_all)
        nres=n_elements(res)
        ;      if strupcase(res(nres-1)) eq 'NOOBS' then goto,nextstat10
        if strlowcase(res[0]) eq 'yearlyavg' then YearObs=fix(res(1))
        if strlowcase(res[0]) eq 'yearlyavg' and nlines ne 2 then begin
          printf,11,'TimeLines OBSfile (yearly) '+fns+' NE 1 -----',nlines-2
          iprob=1
        endif
        if strlowcase(res[0]) ne 'yearlyavg' then begin
          if nlines eq 1 then begin
            printf,11,'TimeLines OBSfile '+fns+' EQ 1 -----'
            iprob=1
          endif
          while not(eof(1)) do begin
            readf,1,atxt
            res1=strsplit(atxt,';',/extract)
            if n_elements(res1) le 3 then begin
              printf,11,'Date format not correct '+fns
              iprob=1
            endif
            datum=res1(0:3)
            YearOBS=fix(datum(0))
            if (fix(datum(0)) lt 1900 or fix(datum(0)) gt 2100) or (fix(datum(1)) le 0 or fix(datum(1)) ge 13) or $
              (fix(datum(2)) le 0 or fix(datum(2)) ge 32) or (fix(datum(3)) le -1 or fix(datum(3)) ge 25) then begin
              printf,11,'Date format not correct '+fns
              iprob=1
            endif
          endwhile
        endif
        nextstat10:
        close,1
      endif
    endfor
  endif
  if YearOBS ne YearStartup then begin
    txt='STEP 10: WARNING! YearOBS ('+strcompress(YearOBS,/remove_all)+') NE YearStartup ('+$
      strcompress(YearStartup,/remove_all)+')'
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
  endif
  if iprob eq 1 then begin
    if count_fileY eq 0 then txt='STEP 10: STOP! TimeLines OBSfile EQ 1 or Date format not correct'
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
    txtall=['STOP',txtall]
    widget_control,labcom_txt,set_value=txtall
    close,11
    close,12
    ierror=1
    return
  endif
  printf,11,'STEP 10 OK: TimeLength OBSfiles'
  txt='STEP 10 OK: TimeLength OBSfiles'
  txtall=[txt,txtall]
  widget_control,labcom_txt,set_value=txtall
  printf,11,' '
  widget_control,labok(10),set_value=' *OK* '
  
  ; *******************************************************************************************************
  
  widget_control,labprog_txt,set_value='STEP 11'
  printf,11,'*****************************************************************'
  printf,11,'***         STEP 11                                             *'
  printf,11,'***  OBS vsc to cdf conversion                                  *'
  printf,11,'*****************************************************************'
  print,'STEP 11'
  iprob=0
  if strupcase(frequency) ne 'HOUR' then begin
    txt='STEP 11: skip csv to cdf conversion (only on hourly data set)'
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
  endif

  if count_fileY eq 0 then begin
    csv2cdfInfo.inputDir=dir_obs
    csv2cdfInfo.startHour=0
    ;KEES OCT 2015
    csv2cdfInfo.endHour=8759
    if (fix(year) mod 4) eq 0 then csv2cdfInfo.endHour=8783
    csv2cdfInfo.prefixId=''
    csv2cdfInfo.modelName=''
    csv2cdfInfo.fulloutFileName=dir_obs+'OBS_TIME.cdf'
    csv2cdfInfo.stringStartHour=strcompress(year, /REMOVE)+'0101'
    csv2cdfInfo.stringEndHour=strcompress(year, /REMOVE)+'1231'
    csv2cdfInfo.PROGRESSBAR=0
    txt='STEP 11 (Info): Init OBS csv to cdf conversion'
    
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
  endif
  
  if count_fileY eq 0 and getenv('DO_OBS_CDF_CONVERSION') eq '1' and strupcase(frequency) eq 'HOUR' then begin
    widget_control,labprog_txt,set_value='STEP 11'
    a=dialog_message('Now convert to cdf format the observations...', title='CDF conversion')
    ; PHIL 14/02/15 Block below seems to belong to Mirko (3 lines)
    fm=deltaMgr->getFileSystemMgr()
    csv2cdfInfo.inputDir=fm->cleanPath(csv2cdfInfo.inputDir)
    csv2cdfInfo.outputDir=fm->cleanPath(csv2cdfInfo.outputDir)
    convRes=csv2cdf(csv2cdfInfo.startUpFile, $
      csv2cdfInfo.startHour, csv2cdfInfo.endHour, csv2cdfInfo.inputDir, csv2cdfInfo.outputDir, $
      csv2cdfInfo.prefixId, csv2cdfInfo.modelName, csv2cdfInfo.fulloutFileName, csv2cdfInfo.stringStartHour, $
      csv2cdfInfo.stringEndHour, $
      logWin=labcom_txt, PROGRESSBAR=csv2cdfInfo.PROGRESSBAR,ProgWin=labprog_txt)
    if convRes eq 1 then begin
      txt='STEP 11 OK: convert to cdf format the observations'
      txtall=[txt,txtall]
      widget_control,labcom_txt,set_value=txtall
      printf,11,' '
      widget_control,labok(11),set_value=' *OK* '
    endif else begin
      txt='STEP 11 OK: conversion to cdf format the observations failed'
      txtall=[txt,txtall]
      widget_control,labcom_txt,set_value=txtall
      printf,11,' '
      widget_control,labok(11),set_value=' *KO* '
      ierror=1
      return
    endelse
  endif
  ; *******************************************************************************************************
  widget_control,labprog_txt,set_value='STEP 12'
  printf,11,'***********************************************************'
  printf,11,'***           STEP 12                                     *'
  
  printf,11,'*** OBS availability at stations (%), and Extreme Values  *'
  printf,11,'***********************************************************'
  printf,11,'See SummaryFile'
  ;  printf,11,' '
  printf,12,'***********************************************************'
  printf,12,'***           STEP 12                                     *'
  
  printf,12,'*** OBS availability at stations (%), and Extreme Values  *'
  printf,12,'***********************************************************'
  print,'STEP 11'
  sumfile=fltarr(n_elements(statnames),n_elements(spec),6) & sumfile(*,*,*)=-999
  if count_fileY eq 1 then begin
    Values0=intarr(count_filenames) & values0(*)=0
    atxt=' '
    get_lun,lunY
    openr,lunY,dir_obs+'OBS_Yearly.csv'
    readf,lunY,atxt
    npYO=n_elements(paramsYearlyObs)
    iprob=0
    iprob1=0
    itel=0
    itelP=0
    itelE=0
    while not(eof(lunY)) do begin
      readf,lunY,atxt
      itel=itel+1
      res=strsplit(atxt,';',/extract)
      fname=res(0)
      nres=n_elements(res)
      if nres-1 ne npYO then begin
        iprob=1
        itelP=itelP+1
        printf,12,'In OBS_Yearly.csv: '+strtrim(fname,2)+' -- Number of Yearly values NOT equal to number of species'
      endif
      if nres-1 eq npYO then begin
        fres=float(res(1:npYO))
        for ip=0,npYO-1 do begin
          if fres(ip) eq 0. then values0(itel)=1
          if fres(ip) gt 1000. or (fres(ip) lt -100 and fres(ip) ne -999) then begin
            iprob1=1
            itelE=itelE+1
            printf,12,'In OBS_Yearly.csv: '+strtrim(fname,2)+' -- Extreme value'
          endif
        endfor
      endif
    endwhile
    if itelP ge 1 then printf,11,itelP,' Stations in OBS_Yearly.csv NOT correct'
    if itelE ge 1 then printf,11,itelE,' Stations in OBS_Yearly.csv with extreme values'
    close,lunY
    free_lun,lunY
    printf,11,' '
    printf,12,' '
    if iprob eq 0 and iprob1 eq 0 then begin
      widget_control,labok(12),set_value=' *OK* '
      txt='STEP 12 (Info) OK: Yearly OBS values, No extreme values'
      txtall=[txt,txtall]
      widget_control,labcom_txt,set_value=txtall
    endif
    if iprob eq 1 then begin
      txt='STEP 12: STOP! Stations in OBS_Yearly.csv NOT correct'
      txtall=[txt,txtall]
      widget_control,labcom_txt,set_value=txtall
      txtall=['STOP',txtall]
      widget_control,labcom_txt,set_value=txtall
      close,11
      close,12
      ierror=1
      return
    endif
    if iprob1 eq 1 then begin
      txt='STEP 12: WARNING! Extreme values in OBS_Yearly.csv'
      txtall=[txt,txtall]
      widget_control,labcom_txt,set_value=txtall
    endif
  endif
  if count_fileY eq 0 then begin
    poll=fltarr(n_elements(statnames),n_elements(spec),8784)
    poll(*,*,*)=-999.
    ipoll=strarr(n_elements(statnames),n_elements(spec))
    ipoll(*,*)='.'
    sumfile=fltarr(n_elements(statnames),n_elements(spec),6) & sumfile(*,*,*)=-999
    extvalues=0
    day_sum=[0,31,60,91,121,152,182,213,244,274,305,335,365]
    for i=0,n_elements(statnames)-1 do begin
      widget_control,labprog_txt,set_value='STEP 12: '+string(fix(i+1))+'  / '+string(fix(n_elements(statnames)))
      if strupcase(spec_stations(i)) ne 'NOOBS' then begin
        fn=file_search(dir_obs+statnames(i)+'.csv',count=count)
        if count eq 1 then begin
          close,1 & openr,1,fn
          readf,1,atxt
          res=strsplit(atxt,';',/extract) ; yyyy mm dd hh PM10 PM25- OR - year Param1 Param2
          res=strcompress(res,/remove_all)
          nres=n_elements(res)
          ;        if strupcase(res(nres-1)) eq 'NOOBS' then goto,nextstat11
          if strlowcase(res[0]) eq 'yearlyavg' then begin       ;if 'YearlyAvg'   YearlyAvg obs values
            res2=strsplit(atxt,';',/extract)  ; YearlyAvg 2009 PM10 PM25
            nb_specstat=n_elements(res2)-2
            spec_station=res2(2:nb_specstat+1) ; PM10 PM25
          endif else begin                                   ; hourly obs values
            nb_specstat=n_elements(res)-4
            spec_station=res(4:4+nb_specstat-1) ; PM10 PM25
          endelse
          if strlowcase(res[0]) ne 'yearlyavg' then begin
            while not(eof(1)) do begin
              readf,1,atxt
              res1=strsplit(atxt,';',/extract, /PRESERVE_NULL)
              datum=res1(0:3)
              k1=day_sum(fix(datum(1))-1)*24
              k2=(fix(datum(2))-1)*24
              k3=fix(datum(3))
              iline=k1+k2+k3
              if 4*(fix(datum(0))/4) ne fix(datum(0)) and iline ge 60*24 then iline=iline-24
              ph=reform(float(res1(4:4+nb_specstat-1)))
              for ip=0,nb_specstat-1 do begin
                shlp=where(spec_station(ip) eq spec,nc)
                if nc ge 1 then begin
                  if ph(ip) gt 1000. or (ph(ip) lt -100 and ph(ip) ne -999) then begin
                    ipoll(i,shlp[0])='X'
                    extvalues=1
                    poll(i,shlp[0],iline)=-999
                  endif else begin
                    poll(i,shlp[0],iline)=ph(ip)
                  endelse
                endif
              endfor
            endwhile
          endif
          if strlowcase(res[0]) eq 'yearlyavg' then begin
            readf,1,atxt
            res2=strsplit(atxt,';',/extract)
            ph=reform(float(res2(0:nb_specstat-1)))
            for ip=0,nb_specstat-1 do begin
              shlp=where(spec_station(ip) eq spec,nc)
              if nc ge 1 then begin
                if ph(ip) gt 1000. or (ph(ip) lt -100 and ph(ip) ne -999) then begin
                  ipoll(i,shlp[0])='X'
                  extvalues=1
                  poll(i,shlp[0],*)=-999
                endif else begin
                  poll(i,shlp[0],*)=ph(ip)
                endelse
              endif
            endfor
          endif
          hlp=where(poll lt -100,nc)
          if nc ge 1 then poll(hlp)=!values.f_nan
          for ip=0,nb_specstat-1 do begin
            shlp=where(spec_station(ip) eq spec,nc)
            if nc ge 1 then begin
              minv=min(poll(i,shlp[0],0:8759),/nan)
              maxv=max(poll(i,shlp[0],0:8759),/nan)
              meanv=mean(poll(i,shlp[0],0:8759),/nan)
              sumfile(i,shlp[0],0:2)=[minv,maxv,meanv]
            endif
          endfor
        endif
        nextstat11:
        close,1
      endif
    endfor
    apoll=finite(poll)
    kc=where(apoll eq 0,nkc)
    if nkc ge 1 then poll(kc)=-999
    avail=fltarr(n_elements(statnames),n_elements(spec))
    for istat=0,n_elements(statnames)-1 do begin
      for is=0,n_elements(spec)-1 do begin
        cc=where(poll(istat,is,0:8759) gt -800. ,count)
        avail(istat,is)=count/8760.*100.
      endfor
    endfor
    printf,12,spec,'   Extr.Val.X',format='(18x,25a10)'
    for istat=0,n_elements(statnames)-1 do begin
      stravail=strtrim(string(reform(avail(istat,*))),2)
      Av=''
      for is=0,n_elements(spec)-1 do begin
        if avail(istat,is) eq 0. then stravail(is)='-'
        Av=Av+stravail(is)+' '
      endfor
      txt=string(' ',format='(a100)')
      strput,txt,fix(istat+1),0
      strput,txt,statnames(istat),5
      strput,txt,Av,20
      printf,12,txt
    endfor
    widget_control,labprog_txt,set_value=''
    printf,12,' '
    widget_control,labok(12),set_value=' *OK* '
    txt='STEP 12 (Info) OK: OBS availability: see LogFile'
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
    if extvalues eq 0 then txt='STEP 12 OK: No Extreme OBS values'
    if extvalues eq 1 then txt='STEP 12 (Info): Extreme OBS values: see LogFile'
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
  endif
  
  ; *******************************************************************************************************
  ; Check OBS equal to zero (real or novalue ?)
  ;test 12
  
  widget_control,labprog_txt,set_value='STEP 13'
  printf,11,'********************************************************'
  printf,11,'***           STEP 13                                  *'
  printf,11,'*** Check OBS equal to zero (real or novalue ?)        *'
  printf,11,'********************************************************'
  printf,11,'See SummaryFile'
  ;  printf,11,' '
  printf,12,'********************************************************'
  printf,12,'***           STEP 13                                  *'
  printf,12,'*** Check OBS equal to zero (real or novalue ?)        *'
  printf,12,'********************************************************'
  print,'STEP 13'
  poll=fltarr(n_elements(statnames),n_elements(spec),8784)
  poll(*,*,*)=-999.
  ipoll=strarr(n_elements(statnames),n_elements(spec))
  ipoll(*,*)='.'
  extvalues=0
  if count_fileY eq 1 then begin
    printf,12,' '
    for iv=0,count_filenames-1 do begin
      if values0(iv) eq 1 then printf,12,'In OBS_Yearly.csv: '+strtrim(fname,2)+' -- OBS=0'
    endfor
    if fix(total(values0)) ge 1 then begin
      txt='STEP 13 WARNING: Check OBS = 0: see LogFile'
      txtall=[txt,txtall]
      widget_control,labcom_txt,set_value=txtall
      printf,11,fix(total(values0)),' Stations in OBS_Yearly.csv with OBS=0'
    endif else begin
      txt='STEP 13 OK: No OBS = 0'
      txtall=[txt,txtall]
      widget_control,labcom_txt,set_value=txtall
      widget_control,labok(13),set_value=' *OK* '
    endelse
  endif
  if count_fileY eq 0 then begin
    day_sum=[0,31,60,91,121,152,182,213,244,274,305,335,365]
    for i=0,n_elements(statnames)-1 do begin
      widget_control,labprog_txt,set_value='STEP 13: '+string(fix(i+1))+'  / '+string(fix(n_elements(statnames)))
      if strupcase(spec_stations(i)) ne 'NOOBS' then begin
        fn=file_search(dir_obs+statnames(i)+'.csv',count=count)
        if count eq 1 then begin
          close,1 & openr,1,fn
          readf,1,atxt
          res=strsplit(atxt,';',/extract) ; yyyy mm dd hh PM10 PM25- OR - year Param1 Param2
          res=strcompress(res,/remove_all)
          nres=n_elements(res)
          ;        if strupcase(res(nres-1)) eq 'NOOBS' then goto,nextstat12
          if strlowcase(res[0]) eq 'yearlyavg' then begin       ;if 'YearlyAvg'   YearlyAvg obs values
            res2=strsplit(atxt,';',/extract)  ; YearlyAvg 2009 PM10 PM25
            nb_specstat=n_elements(res2)-2
            spec_station=res2(2:nb_specstat+1) ; PM10 PM25
          endif else begin                                   ; hourly obs values
            nb_specstat=n_elements(res)-4
            spec_station=res(4:4+nb_specstat-1) ; PM10 PM25
          endelse
          if strlowcase(res[0]) ne 'yearlyavg' then begin
            while not(eof(1)) do begin
              readf,1,atxt
              res1=strsplit(atxt,';',/extract,  /PRESERVE_NULL)
              datum=res1(0:3)
              k1=day_sum(fix(datum(1))-1)*24
              k2=(fix(datum(2))-1)*24
              k3=fix(datum(3))
              iline=k1+k2+k3
              if 4*(fix(datum(0))/4) ne fix(datum(0)) and iline ge 60*24 then iline=iline-24
              ph=reform(float(res1(4:4+nb_specstat-1)))
              for ip=0,nb_specstat-1 do begin
                shlp=where(spec_station(ip) eq spec,nc)
                if nc ge 1 then begin
                  if abs(ph(ip)) lt 0.00001 then begin
                    ipoll(i,shlp[0])='X'
                    extvalues=1
                    poll(i,shlp[0],iline)=-999
                  endif else begin
                    poll(i,shlp[0],iline)=0
                  endelse
                endif
              endfor
            endwhile
          endif
          if strlowcase(res[0]) eq 'yearlyavg' then begin
            readf,1,atxt
            res2=strsplit(atxt,';',/extract)
            ph=reform(float(res2(0:nb_specstat-1)))
            for ip=0,nb_specstat-1 do begin
              shlp=where(spec_station(ip) eq spec,nc)
              if nc ge 1 then begin
                if abs(ph(ip)) lt 0.00001 then begin
                  ipoll(i,shlp[0])='X'
                  extvalues=1
                  poll(i,shlp[0],*)=-999
                endif else begin
                  poll(i,shlp[0],*)=0
                endelse
              endif
            endfor
          endif
          close,1
        endif
        nextstat12:
        close,1
      endif
    endfor
    apoll=finite(poll)
    kc=where(apoll eq 0,nkc)
    if nkc ge 1 then poll(kc)=-999
    printf,12,spec,'   Value=0',format='(18x,25a10)'
    for istat=0,n_elements(statnames)-1 do begin
      txt=string(' ',format='(a100)')
      ExV=''
      for ip=0,n_elements(spec)-1 do begin
        ExV=ExV+ipoll(istat,ip)
      endfor
      strput,txt,fix(istat+1),0
      strput,txt,statnames(istat),5
      strput,txt,exV,20
      printf,12,txt
    endfor
    widget_control,labprog_txt,set_value=''
    printf,12,' '
    ;PHIL 14/02/15 Mirko has labok=12
    widget_control,labok(13),set_value=' *OK* '
    txt='STEP 13 (Info) OK: Check OBS = 0: see LogFile'
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
    if extvalues eq 0 then txt='STEP 13 OK: No OBS = 0'
    if extvalues eq 1 then txt='STEP 13 (Info): OBS = 0: Make -999, see LogFile'
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
  endif
  printf,11,' '
endif   ; itobs
; *******************************************************************************************************
; *******************************************************************************************************

if itmod eq 1 then begin
  ;test 13

  widget_control,labprog_txt,set_value='STEP 14'
  printf,11,'****************************************'
  printf,11,'***         STEP 14                    *'
  printf,11,'***  Existence of MODfile              *'
  printf,11,'****************************************'
  print,'STEP 14'
  dir_mod=dir_mod+path_sep()
  res=file_test(dir_mod+model)
  point=strpos(model, '.', /REVERSE_SEARCH)
  bFName=strmid(model, 0, point)
  extension=strmid(model, point+1, 3)
  if res eq 0 then begin
    extension='csv'
    FName=dir_mod+bFName+'.'+extension
    res=file_test(FName)
    ;modelFile=FName
    model=bFName+'.'+extension
    if res eq 0 then begin
      txt='STEP 14 STOP! Modfile '+model+' does not exist in MODELING_DIR'
      txtall=[txt,txtall]
      widget_control,labcom_txt,set_value=txtall
      txtall=['STOP',txtall]
      widget_control,labcom_txt,set_value=txtall
      close,11
      close,12
      ierror=1
      return
    endif
  endif else begin
    printf,11,'STEP 14 OK: MODfile '+model+' found in MODELING_DIR'
    txt='STEP 14 OK: MODfile '+model+' found in MODELING_DIR'
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
  endelse
  printf,11,' '
  widget_control,labok(14),set_value=' *OK* '
  
  ; *******************************************************************************************************
  ;test 14
  
  readSpec
  readStatSpec
  
  widget_control,labprog_txt,set_value='STEP 15'
  printf,11,'*****************************************************************'
  printf,11,'***         STEP 15                                             *'
  printf,11,'***  Existence of stations/species/attribute in MODfile         *'
  printf,11,'*****************************************************************'
  print,'STEP 15'
  ;lmod=strlen(dir_mod+model)
  ;extension=strmid(dir_mod+model,lmod-3,3)
  if extension eq 'cdf' then begin
    fileName=dir_mod+model
    fName=file_search(fileName)
    id=ncdf_open(strcompress(fName, /remove)) ;id=ncdf_open('C:\data\work\DeltaTool_V2.5\data\modeling\2005_CAMX_TIME.cdf')
    res=ncdf_inquire(id)
    icase=0  ; old version
    attnum=res.ngatts
    if attnum ge 1 then begin
      for i=0,attnum-1 do begin
        attname=ncdf_attname(id,i,/global)
        if attname eq 'Parameters' then icase=1 ; new version
      endfor
    endif
    if icase eq 0 then begin
      iprob=0
      for i=0,n_elements(statnames)-1 do begin
        for is=0,n_elements(spec)-1 do begin
          idname=statnames[i]+'_'+spec(is)
          !quiet=1
          Result = NCDF_VARID(id, idName)
          !quiet=0
          if result eq -1 then begin
            printf,11,'Missing '+spec[is]+' at station '+statnames[i]
            iprob=1
          endif
        endfor
      endfor
    endif
    if icase eq 1 then begin
      iprob=0
      ncdf_attget,id,'Parameters',params,/global
      params=string(params)
      params=strsplit(params,' ',/extract)
      params=strcompress(params,/remove_all)
      nparams=n_elements(params)
      if nparams lt n_elements(spec) then begin
        printf,11,'Number of species ('+strcompress(nparams,/remove_all)+') in MODfile LT to number of species ('+ $
          strcompress(n_elements(spec),/remove_all)+') in STARTUPfile'
        iprob=1
      endif
      for ispec=0,n_elements(spec)-1 do begin
        cc=where(spec(ispec) eq params,ncc)
        if ncc eq 0 then begin
          printf,11,spec(ispec)+' in STARTUPfile - but NOT in MODfile'
          iprob=1
        endif
      endfor
    endif
    ncdf_close,id
  endif
  if extension eq 'csv' then begin
    iprob=0
    close,1 & openr,1,dir_mod+model
    readf,1,atxt
    atxt=strcompress(atxt,/remove_all)
    res=strsplit(atxt,';',/extract)
    params=res(2:n_elements(res)-1)
    nparams=n_elements(params)
    if nparams lt n_elements(spec) then begin
      printf,11,'Number of species ('+strcompress(nparams,/remove_all)+') in MODfile LT to number of species ('+ $
        strcompress(n_elements(spec),/remove_all)+') in STARTUPfile'
      iprob=1
    endif
    for ispec=0,n_elements(spec)-1 do begin
      cc=where(spec(ispec) eq params,ncc)
      if ncc eq 0 then begin
        printf,11,spec(ispec)+' in STARTUPfile - but NOT in MODfile'
        iprob=1
      endif
    endfor
    statvals=strarr(5000,nparams+1) & statvals(*,*)=!values.f_nan
    itel=0
    ;readf,1,atxt
    while ~eof(1) do begin
      readf,1,atxt
      atxt=strcompress(atxt,/remove_all)
      res1=strsplit(atxt,';',/extract)
      statvals(itel,0:n_elements(res1)-1)=res1
      itel=itel+1
    endwhile
    statvals=statvals(0:itel-1,*)
    for i=0,n_elements(statnames)-1 do begin
      for is=0,n_elements(spec)-1 do begin
        statv1=reform(statvals[*,0])
        cc=where(statnames[i] eq statv1,ncc)
        if ncc eq 0 then begin
          printf,11,'Missing station '+statnames(i)+' in MODfile
          iprob=1
        endif
        if ncc eq 1 then begin
          statv2=reform(statvals(cc[0],1:nparams))
          cc2=where(finite(float(statv2)) eq 0,ncc2)
          if ncc2 ge 1 then begin
            printf,11,'Missing species at station '+statnames[i]
            iprob=1
          endif
        endif
      endfor
    endfor
    close,1
  endif
  if iprob eq 1 then begin
    txt='STEP 15: STOP! Inconsistent speclist in STARTUPfile and MODfile'
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
    txtall=['STOP',txtall]
    widget_control,labcom_txt,set_value=txtall
    close,11
    close,12
    ierror=1
    return
  endif
  if iprob eq 0 then begin
    printf,11,'STEP 15 OK: Species consistent in STARTUPfile and MODfile'
    txt='STEP 15 OK: Species consistent in STARTUPfile and MODfile'
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
  endif
  printf,11,' '
  widget_control,labok(15),set_value=' *OK* '
  
  ; *******************************************************************************************************
  ;check on length modeling data
  
  widget_control,labprog_txt,set_value='STEP 16'
  printf,11,'**********************************************************************'
  printf,11,'***         STEP 16                                                  *'
  printf,11,'***  TimeLength of MOD/species files [< 8760|8784 (hourly), =1 (yearly)]   *'
  printf,11,'**********************************************************************'
  print,'STEP 16'
  lmod=strlen(dir_mod+model)
  iprob=0
  extension=strmid(dir_mod+model,lmod-3,3)
  YearMOD=-1
  if extension eq 'cdf' then begin
    id=ncdf_open(dir_mod+model)
    !quiet=1
    inqStHr=ncdf_attinq(Id,'StartHour',/global)
    inqEnHr=ncdf_attinq(Id,'EndHour',/global)
    !quiet=0
    inqYr=ncdf_attinq(Id,'Year',/global)
    if inqYr.datatype ne 'UNKNOWN' then begin
      ncdf_attget,id,'Year',YearMOD,/global
    endif
    if icase eq 0 then begin  ; variable is Station_PM10 ( ntimes) 
      for i=0,n_elements(statnames)-1 do begin
        for is=0,n_elements(spec)-1 do begin
          idname=statnames(i)+'_'+spec(is)
          Result = NCDF_VARID(id, idName)
          if result ne -1 then begin
            if inqStHr.datatype eq 'UNKNOWN' and inqEnHr.datatype eq 'UNKNOWN' then begin
              ncdf_varget, Id, idname, var
              dimVar=n_elements(var)
              if dimvar ne 8760 and dimvar ne 8784 then begin
                printf,11,'Incorrect nb of time elements in variable '+idname+' in MODfile'
                iprob=1
              endif
            endif else begin
              ncdf_attget,id,'StartHour',StartHour,/global
              ncdf_attget,id,'EndHour',EndHour,/global
              ncdf_varget,Id,idname,var  ;,count=[1,8760],offset=[cc(0),0]
              dimVar=n_elements(var)
              if dimvar ne EndHour-StartHour+1 then begin
                printf,11,'Incorrect nb of time elements in variable '+idname+' in MODfile'
                iprob=1
              endif
            endelse
          endif else begin
            printf,11,'Variable '+idName+' does NOT exist in MODfile'
            iprob=1
          endelse
        endfor
      endfor
    endif
    if icase eq 1 then begin
      for i=0,n_elements(statnames)-1 do begin
        idname=statnames(i)
        Result = NCDF_VARID(id, idName)
        if result ne -1 then begin
          ncdf_varget,id,idname,var
          dimensionVar=size(var,/dimensions)
          if dimensionVar(0) ne nparams then begin
            printf,11,'Number of params ('+strcompress(nparams,/remove_all)+ $
              ') in MODfile NE Number of parameters ('+strcompress(dimensionVar(1),/remove_all)+ $
              ') in Data for station '+idName
            iprob=1
          endif
          if inqStHr.datatype eq 'UNKNOWN' and inqEnHr.datatype eq 'UNKNOWN' then begin
            dimVar=n_elements(var)
            if dimvar ne 8760 and dimvar ne 8784 then begin
              printf,11,'Incorrect nb of time elements in variable '+idname+' in MODfile'
              iprob=1
            endif
          endif else begin
            ncdf_attget,id,'StartHour',StartHour,/global
            ncdf_attget,id,'EndHour',EndHour,/global
            ncdf_varget,Id,idname,var    ;,count=[1,8760],offset=[cc(0),0]
            dimVar=size(var,/dimensions)
            if dimvar[1] ne EndHour-StartHour+1 then begin
              printf,11,'Incorrect nb of time elements in variable '+idname+' in MODfile'
              iprob=1
            endif
          endelse
        endif else begin
          printf,11,'Variable '+idName+' does NOT exist in MODfile'
          iprob=1
        endelse
      endfor
    endif
    ncdf_close,id
  endif
  if YearMOD ne YearStartup and YearMOD ne -1 then begin
    txt='STEP 16: WARNING! YearMOD ('+strcompress(YearMOD,/remove_all)+') NE YearStartup ('+$
      strcompress(YearStartup,/remove_all)+')'
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
  endif
  if iprob eq 1 then begin
    txt='STEP 16: STOP! TimeLength of MOD/species files [< 8760|8784 (hourly), =1 (yearly)]'
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
    txtall=['STOP',txtall]
    widget_control,labcom_txt,set_value=txtall
    close,11
    close,12
    ierror=1
    return
  endif
  if iprob eq 0 then begin
    printf,11,'STEP 16 OK: TimeLength all MODfile/species eq 8760|EndHour-StartHour+1 (hourly), or 1 (yearly)'
    txt='STEP 16 OK: TimeLength all MODfile/species eq 8760|EndHour-StartHour+1 (hourly), or 1 (yearly)'
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
  endif
  printf,11,' '
  widget_control,labok(16),set_value=' *OK* '
  
  ; *******************************************************************************************************
  ; checking mod extreme values
  ;test 16
  
  widget_control,labprog_txt,set_value='STEP 17'
  printf,11,'**************************************************'
  printf,11,'***         STEP 17                              *'
  printf,11,'***  Check on MOD NaN/Inf/Extreme values         *'
  printf,11,'**************************************************'
  printf,12,'**************************************************'
  printf,12,'***         STEP 17                              *'
  printf,12,'***  Check on MOD NaN/Inf/Extreme values         *'
  printf,12,'**************************************************'
  print,'STEP 17'
  lmod=strlen(dir_mod+model)
  extension=strmid(dir_mod+model,lmod-3,3)
  iprob=0
  if extension eq 'cdf' then begin
    id=ncdf_open(dir_mod+model)
    for i=0,n_elements(statnames)-1 do begin
      for is=0,n_elements(spec)-1 do begin
        if icase eq 0 then begin
          idname=statnames(i)+'_'+spec(is)
          result=ncdf_varid(id, idName)
          if result eq -1 then goto,nosp
          ncdf_varget,id,idname,var
        endif
        if icase eq 1 then begin
          idname=statnames(i)
          cc=where(spec[is] eq params,ncc)
          if ncc ne 1 then goto,nosp
          !quiet=1
          ncdf_varget,id,idname,varall,count=[1,8760],offset=[cc[0],0]
          !quiet=0
          var=reform(varall)
        endif
        ivar=finite(var)
        kc=where(ivar eq 0,nkc)
        if nkc ge 1 then begin
          printf,12,'NaN or Inf values for '+spec(is)+' at station '+statnames(i)
          iprob=1
        endif
        if nkc ge 1 then var(kc)=-9999
        kc1=where(var gt -1000. and var lt -998,nkc1)
        if nkc1 ge 1 then begin
          printf,12,'-999 values for '+spec(is)+' at station '+statnames(i)
          iprob=1
        endif
        if nkc ge 1 then var(kc)=0.
        if nkc1 ge 1 then var(kc1)=0.
        if max(var) gt 800. or min(var) lt -800 then begin
          printf,12,'Extreme values for '+spec(is)+' at station '+statnames(i)+' [values < -800 or > +800]'
          iprob=1
        endif
        nosp:
      endfor
    endfor
    ncdf_close,id
    printf,12,' '
  endif
  if extension eq 'csv' then begin
    close,1 & openr,1,dir_mod+model
    readf,1,atxt
    atxt=strcompress(atxt,/remove_all)
    res=strsplit(atxt,';',/extract)
    params=res(2:n_elements(res)-1)
    statvals=strarr(5000,nparams+1) & statvals(*,*)=!values.f_nan
    itel=0
    readf,1,atxt
    while ~eof(1) do begin
      readf,1,atxt
      atxt=strcompress(atxt,/remove_all)
      res1=strsplit(atxt,';',/extract)
      statvals(itel,0:n_elements(res1)-1)=res1
      itel=itel+1
    endwhile
    statvals=statvals(0:itel-1,*)
    for i=0,n_elements(statnames)-1 do begin
      statv1=reform(statvals[*,0])
      cc=where(statnames[i] eq statv1,ncc)
      statv2=reform(statvals(cc[0],1:nparams))
      cc=where(finite(statv2) eq 0,ncc)
      if ncc ge 1 then begin
        for j=0,ncc-1 do printf,12,'NaN or Inf values for '+params(cc[j])+' at station '+statnames(i)
        iprob=1
      endif
      ivar=finite(statv2)
      kc=where(ivar eq 0,nkc)
      if nkc ge 1 then statv2(kc)=-9999
      cc=where(statv2 gt -1000. and statv2 lt -998,ncc)
      if ncc ge 1 then begin
        for j=0,ncc-1 do printf,12,'-999 values for '+params(cc[j])+' at station '+statnames(i)
        iprob=1
      endif
      if nkc ge 1 then statv2(kc)=0.
      if ncc ge 1 then statv2(cc)=0.
      cc=where(max(statv2) gt 800. or min(statv2) lt -800,ncc)
      if ncc ge 1 then begin
        for j=0,ncc-1 do printf,12,'Extreme values for '+params(cc[j])+' at station '+statnames(i)+' [values < -800 or > +800]'
        iprob=1
      endif
    endfor
    close,1
    printf,12,' '
  endif
  if iprob eq 1 then begin
    txt='STEP 17 (Info): NaN/Inf/Extreme MOD values at stations: see SummaryFile'
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
    printf,11,'See SummaryFile'
  endif else begin
    printf,11,'STEP 17 OK: No NaN/Inf/Extreme values in MOD results'
    txt='STEP 17 OK: No NaN/Inf/Extreme values in MOD results'
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
  endelse
  printf,11,' '
  widget_control,labok(17),set_value=' *OK* '
  
endif   ;itmod

; *******************************************************************************************************
; *******************************************************************************************************
; checking mod availability per species

if itobsmod eq 1 then begin

  ReadSpec
  
  ReadStatSpec
  
  widget_control,labprog_txt,set_value='STEP 18'
  printf,11,'****************************************************************'
  printf,11,'***           STEP 18                                          *'
  printf,11,'*** MOD availability at stations for STARTUP species (%)       *'
  printf,11,'****************************************************************'
  printf,11,'See SummaryFile'
  printf,11,' '
  printf,12,'****************************************************************'
  printf,12,'***           STEP 18                                          *'
  printf,12,'*** MOD availability at stations for STARTUP species (%)       *'
  printf,12,'****************************************************************'
  print,'STEP 18'
  iprob=0
  YearMOD=-1
  avail=fltarr(n_elements(statnames),n_elements(spec))
  poll=fltarr(n_elements(statnames),n_elements(spec),8760)
  if itobs ne 1 then begin
    sumfile=fltarr(n_elements(statnames),n_elements(spec),6)
    sumfile(*,*,*)=-999
  endif
  lmod=strlen(dir_mod+model)
  extension=strmid(dir_mod+model,lmod-3,3)
  StartHour=0
  EndHour=8760
  day_sum=[0,31,60,91,121,152,182,213,244,274,305,335,365]
  if extension eq 'cdf' then begin
    id=ncdf_open(dir_mod+model)
    res=ncdf_inquire(id)
    icase=0  ; old version
    attnum=res.ngatts
    if attnum ge 1 then begin
      for i=0,attnum-1 do begin
        attname=ncdf_attname(id,i,/global)
        if attname eq 'Parameters' then icase=1 ; new version
        if attname eq 'StartHour' then ncdf_attget,id,'StartHour',StartHour,/global
        if attname eq 'EndHour' then ncdf_attget,id,'EndHour',EndHour,/global
        if attname eq 'Year' then ncdf_attget,id,'Year',YearMOD,/global
        !quiet=1
      endfor
    endif
    ;KeesC 12JAN2015
    startHour=float(StartHour)
    endHour=float(EndHour)
    if icase eq 1 then begin
      ncdf_attget,id,'Parameters',params,/global
      params=string(params)
      params=strsplit(params,' ',/extract)
      params=strcompress(params,/remove_all)
      nparams=n_elements(params)
    endif
  endif
  if YearMOD ne YearStartup and YearMOD ne -1 then begin
    txt='STEP 18: WARNING! YearMOD ('+strcompress(YearMOD,/remove_all)+') NE YearStartup ('+$
      strcompress(YearStartup,/remove_all)+')'
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
  endif
  if extension eq 'csv' then begin
    close,1 & openr,1,dir_mod+model
    readf,1,atxt
    atxt=strcompress(atxt,/remove_all)
    res=strsplit(atxt,';',/extract)
    params=res(2:n_elements(res)-1)
    nparams=n_elements(params)
  endif
  
  if extension eq 'cdf' then begin
    for i=0,n_elements(statnames)-1 do begin
      widget_control,labprog_txt,set_value='STEP 18: '+string(fix(i+1))+'  / '+string(fix(n_elements(statnames)))
      for is=0,n_elements(spec)-1 do begin
        if icase eq 0 then begin
          idname=statnames(i)+'_'+spec(is)
          result=NCDF_VARID(id, idName)
        endif
        if icase eq 1 then begin
          idname=statnames(i)
          result = NCDF_VARID(id, idName)
          cc=where(spec[is] eq params,ncc)
          if ncc ne 1 then result=-1
        endif
        if result ne -1 then begin
          if icase eq 0 then begin
            ncdf_varget,id,idname,var
          endif
          if icase eq 1 then begin
            cc=where(spec[is] eq params,ncc)
            ncdf_varget,id,idname,varall,count=[1,8760],offset=[cc[0],0]
            var=reform(varall)
          endif
          ivar=finite(var)
          kc=where(ivar eq 0,nkc)
          if nkc ge 1 then var(kc)=-999
          cc=where(var gt -800.,count)   ;var(8760)
          ;KeesC 12JAN2015
          avail(i,is)=float(count)/(EndHour-StartHour+1.)*100.
          cc=where(var le -800.,count)   ;var(8760)
          if count ge 1 then var(cc)=!values.f_nan
          minv=min(var,/nan)
          maxv=max(var,/nan)
          meanv=mean(var,/nan)
          sumfile(i,is,3:5)=[minv,maxv,meanv]
        endif else begin
          if icase eq 0 then printf,11,'Variable '+idName+' does NOT exist in MODfile'
          if icase eq 1 then printf,11,'Variable '+idName+' for '+spec(is)+' does NOT exist in MODfile'
          iprob=1
        endelse
      endfor
    endfor
  endif
  if extension eq 'csv' then begin
    statvals=strarr(5000,nparams+1) & statvals(*,*)=!values.f_nan
    itel=0
    readf,1,atxt
    while ~eof(1) do begin
      readf,1,atxt
      atxt=strcompress(atxt,/remove_all)
      res1=strsplit(atxt,';',/extract)
      statvals(itel,0:n_elements(res1)-1)=res1
      itel=itel+1
    endwhile
    statvals=statvals(0:itel-1,*)
    for i=0,n_elements(statnames)-1 do begin
      widget_control,labprog_txt,set_value='STEP 18: '+string(fix(i+1))+'  / '+string(fix(n_elements(statnames)))
      statv1=reform(statvals[*,0])
      cc=where(statnames[i] eq statv1,ncc)
      statv2=reform(statvals(cc[0],1:nparams))
      for is=0,n_elements(spec)-1 do begin
        cc=where(spec(is) eq params,ncc)
        statv3=statv2[cc[0]]
        if statv3 ne -999 then begin
          avail(i,is)=100.
          sumfile(i,is,3:5)=[statv3,statv3,statv3]
        endif else begin
          avail(i,is)=0.
          printf,11,'Variable '+spec(is)+' at station '+statnames(i)+' does NOT exist in MODfile'
        endelse
      endfor
    endfor
  endif
  
  ; if itobs ne 1 then begin
  fileYearly=file_search(dir_obs+'OBS_Yearly.csv',count=count_fileY)
  if count_fileY eq 1 then begin
    atxt=' '
    get_lun,lunY
    openr,lunY,dir_obs+'OBS_Yearly.csv'
    readf,lunY,atxt
    res=strsplit(atxt,';',/extract)
    ; MM feb 2015
    ;paramsYearlyObs=res[2:n_elements(res)-3]
    paramsYearlyObs=res[2:*]
    npYO=n_elements(paramsYearlyObs)
    valuesOBS=fltarr(count_filenames,npYO)
    for i=0,count_filenames-1 do begin
      readf,lunY,atxt
      res=strsplit(atxt,';',/extract)
      filenames(i)=strtrim(res(0),2)
      valuesOBS(i,*)=float(res(1:npYO))
    endfor
    close,lunY
    free_lun,lunY
    nspec=n_elements(spec)
    for i=0,n_elements(statnames)-1 do begin
      cc=where(statnames(i) eq filenames,nc)
      if nc eq 1 then begin
        nr=cc[0]
        valStat=reform(valuesOBS(nr,*))
        for is=0,nspec-1 do begin
          kk=where(spec(is) eq paramsYearlyObs,nk)
          if nk eq 1 then begin
            sumfile(i,is,0:2)=valStat(kk[0])
          endif
        endfor
      endif
    endfor
  endif
  if itobs ne 1 and count_fileY eq 0 then begin
    day_sum=[0,31,60,91,121,152,182,213,244,274,305,335,365]
    for i=0,n_elements(statnames)-1 do begin
      widget_control,labprog_txt,set_value='STEP 18: '+string(fix(i+1))+'  / '+string(fix(n_elements(statnames)))
      if strupcase(spec_stations(i)) ne 'NOOBS' then begin
        fn=file_search(dir_obs+statnames(i)+'.csv',count=count)
        if count gt 0 then begin
          close,1 & openr,1,fn
          readf,1,atxt
          res=strsplit(atxt,';',/extract) ; yyyy mm dd hh PM10 PM25- OR - year Param1 Param2
          res=strcompress(res,/remove_all)
          nres=n_elements(res)
          ;          if strupcase(res(nres-1)) eq 'NOOBS' then goto,nextstat17
          if strlowcase(res[0]) eq 'yearlyavg' then begin       ;if 'YearlyAvg'   YearlyAvg obs values
            res2=strsplit(atxt,';',/extract)  ; YearlyAvg 2009 PM10 PM25
            nb_specstat=n_elements(res2)-2
            spec_station=res2(2:nb_specstat+1) ; PM10 PM25
          endif else begin                                   ; hourly obs values
            nb_specstat=n_elements(res)-4
            spec_station=res(4:4+nb_specstat-1) ; PM10 PM25
          endelse
          if strlowcase(res[0]) ne 'yearlyavg' then begin
            while not(eof(1)) do begin
              readf,1,atxt
              res1=strsplit(atxt,';',/extract)
              datum=res1(0:3)
              yearOBS=fix(datum(0))
              k1=day_sum(fix(datum(1))-1)*24
              k2=(fix(datum(2))-1)*24
              k3=fix(datum(3))
              iline=k1+k2+k3
              if 4*(fix(datum(0))/4) ne fix(datum(0)) and iline ge 60*24 then iline=iline-24
              ph=reform(float(res1(4:4+nb_specstat-1)))
              for ip=0,nb_specstat-1 do begin
                shlp=where(spec_station(ip) eq spec,nc)
                if nc ge 1 then begin
                  if ph(ip) gt 1000. or (ph(ip) lt -100 and ph(ip) ne -999) then begin
                    extvalues=1
                    poll(i,shlp[0],iline)=-999
                  endif else begin
                    poll(i,shlp[0],iline)=ph(ip)
                  endelse
                endif
              endfor
            endwhile
          endif
          if strlowcase(res[0]) eq 'yearlyavg' then begin
            yearOBS=fix(res(1))
            readf,1,atxt
            readf,1,atxt
            res2=strsplit(atxt,';',/extract)
            ph=reform(float(res2(0:nb_specstat-1)))
            for ip=0,nb_specstat-1 do begin
              shlp=where(spec_station(ip) eq spec,nc)
              if nc ge 1 then begin
                if ph(ip) gt 1000. or (ph(ip) lt -100 and ph(ip) ne -999) then begin
                  ipoll(i,shlp[0])='X'
                  extvalues=1
                  poll(i,shlp[0],0:8759)=-999
                endif else begin
                  poll(i,shlp[0],0:8759)=ph(ip)
                endelse
              endif
            endfor
          endif
          hlp=where(poll lt -100,nc)
          if nc ge 1 then poll(hlp)=!values.f_nan
          for ip=0,nb_specstat-1 do begin
            shlp=where(spec_station(ip) eq spec,nc)
            if nc ge 1 then begin
              minv=min(poll(i,shlp[0],0:8759),/nan)
              maxv=max(poll(i,shlp[0],0:8759),/nan)
              meanv=mean(poll(i,shlp[0],0:8759),/nan)
              sumfile(i,shlp[0],0:2)=[minv,maxv,meanv]
            endif
          endfor
        endif
        ; KeesC 11JAN2013
        nextstat17:
        close,1
      endif
    endfor
  endif  ; itobs ne 1
  
  if extension eq 'cdf' then ncdf_close,id
  if extension eq 'csv' then close,1
  
  printf,12,strtrim(spec,2),format='(20x,25a8)'
  hlp=where(finite(sumfile) eq 0,nc)
  if nc ge 1 then sumfile(hlp)=-999
  for i=0,n_elements(statnames)-1 do begin
    printf,12,fix(i+1),statnames(i),avail(i,*),format='(i5,3x,a15,25f8.2,1x)'
  endfor
  printf,12,' '
  txt='STEP 18 (Info) OK: MOD availability / BasicStatistics: see LogFile/SummaryFile'
  txtall=[txt,txtall]
  widget_control,labcom_txt,set_value=txtall
  widget_control,labok(18),set_value=' *OK* '
  
  ; *******************************************************************************************************
  ;
  widget_control,labprog_txt,set_value='STEP 19'
  printf,11,'***********************************'
  printf,11,'***         STEP 19               *'
  printf,11,'***     Basic Statistics          *'
  printf,11,'***********************************'
  printf,11,'See SummaryFile'
  printf,11,' '
  printf,12,'***********************************'
  printf,12,'***           STEP 19             *'
  printf,12,'***     Basic Statistics          *'
  printf,12,'***********************************'
  print,'STEP 19'
  printf,12,'BASIC STATISTICS '
  printf,12,'================'
  printf,12,'Station  **  OBS:Min/Max/Mean  **  MOD:Min/Max/Mean'
  printf,12,' '
  for i=0,n_elements(statnames)-1 do begin
    str=strcompress(fix(i+1),/remove_all)+'  '+strcompress(statnames(i),/remove_all)
    printf,12,str ;fix(i+1),statnames(i),form='(i4,2x,a15)'
    for ip=0,n_elements(spec)-1 do begin
      if sumfile(i,ip,0) ne -999 or sumfile(i,ip,1) ne -999 or sumfile(i,ip,2) ne -999 or $
        sumfile(i,ip,3) ne -999 or sumfile(i,ip,4) ne -999 or sumfile(i,ip,5) ne -999 then begin
        sumf1=reform(sumfile(i,ip,0:2))
        sumf2=reform(sumfile(i,ip,3:5))
        sep='   ** '
        printf,12,spec(ip)+': ',sumf1,sep,sumf2,form='(5x,a8,3f10.4,a6,3f10.4)'
      endif
    endfor
  endfor
  printf,12,' '
  txt='STEP 19 (Info) OK: BasicStatistics: see SummaryFile'
  txtall=[txt,txtall]
  widget_control,labcom_txt,set_value=txtall
  widget_control,labok(19),set_value=' *OK* '
  
endif  ; itobsmod

; *******************************************************************************************************
; ; *******************************************************************************************************
printf,11,'==> END DeltaCheck_IO <=='
close,11
close,12
widget_control,labprog_txt,set_value='==> END DeltaCheck_IO <=='
txtall=[' ',txtall]
widget_control,labcom_txt,set_value=txtall
txt='==> END DeltaCheck_IO <=='
txtall=[txt,txtall]
widget_control,labcom_txt,set_value=txtall
print,'==> END DeltaCheck_IO <=='

end
