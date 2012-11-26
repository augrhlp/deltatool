@DeltaCheck_io_definecommon
PRO updateRunFileName, ev, deltaMgr, model=model, scenario=scenario

  fsm=deltaMgr->getFileSystemMgr()
  extension=fsm->getAvailableRunFileExtension()
  suffix=fsm->getRunFileTimeSuffix()
  
  runFileTXT=widget_info(ev.top, FIND_BY_UNAME='RUNFILE_TEXT')
  yearTXT=widget_info(ev.top, FIND_BY_UNAME='YEAR_TEXT')
  
  modelCB=widget_info(ev.top, FIND_BY_UNAME='MODEL_COMBOBOX')
  scenarioCB=widget_info(ev.top, FIND_BY_UNAME='SCENARIO_COMBOBOX')
  
  
  modelText=widget_info(modelCB, /COMBOBOX_GETTEXT)
  modelIndex=widget_info(modelCB, /COMBOBOX_NUMBER)
  
  scenarioText=widget_info(scenarioCB, /COMBOBOX_GETTEXT)
  scenarioIndex=widget_info(scenarioCB, /COMBOBOX_NUMBER)
  
  filename=scenarioText+'_'+modelText+suffix+extension[0]
  utility=obj_new('FMUtility')
  if n_elements(scenario) then begin
    if utility->IsNumber(scenarioText) then widget_control, yearTXT, set_value=scenarioText
  endif
  obj_destroy, utility
  
  widget_control, runFileTXT, set_value=filename
  
END

PRO DeltaCheck_IO_event, ev
  common keesc1
  
  Widget_Control, ev.id,  GET_UVALUE=what
  Widget_Control, ev.top,  GET_UVALUE=deltaMgr
  if n_elements(what) eq 2 then begin
    ;what=what[0]
    names=*(what[0])
    codes=*what[1]
    code=names[ev.index]
    name=codes[ev.index]
    what=widget_info(ev.id, /UNAME)
  endif
  IF TAG_NAMES(ev, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN what='DONE'
  CASE what OF
    'MODEL_COMBOBOX':  begin
      updateRunFileName, ev, deltaMgr, model=name
    end
    'SCENARIO_COMBOBOX':  begin
      updateRunFileName, ev, deltaMgr, scenario=name
    end
    'DIR':  begin
      sens=1
      widget_control,labdir1_txt,get_value = dir
      dir=dir[0]
      dir=strcompress(dir,/remove_all)
      if strmid(dir,0,1,/reverse_offset) ne '\' then dir=dir+'\'
      dir_res=dir+'resource\'
      dir_obs=dir+'data\monitoring\'
      dir_mod=dir+'data\modeling\'
      dir_log=dir+'log\'
      widget_control,labdir2_txt,set_value=dir_res
      widget_control,labdir3_txt,set_value=dir_obs
      widget_control,labdir4_txt,set_value=dir_mod
      widget_control,labdir6_txt,set_value=dir_log
      print,dir
    end
    'DIR_RES':  begin
      widget_control,labdir2_txt,get_value = dir_res
      dir_res=dir_res[0]
      dir_res=strcompress(dir_res,/remove_all)
      if strmid(dir_res,0,1,/reverse_offset) ne '\' then dir_res=dir_res+'\'
      print,dir_res
    end
    'STARTUP':  begin
      widget_control,labdir2a_txt,get_value = startup
      print,startup
    end
    'DIR_OBS':  begin
      widget_control,labdir3_txt,get_value = dir_obs
      dir_obs=dir_obs[0]
      dir_obs=strcompress(dir_obs,/remove_all)
      if strmid(dir_obs,0,1,/reverse_offset) ne '\' then dir_obs=dir_obs+'\'
      print,dir_obs
    end
    'DIR_MOD':  begin
      widget_control,labdir4_txt,get_value = dir_mod
      dir_mod=dir_mod[0]
      dir_mod=strcompress(dir_mod,/remove_all)
      if strmid(dir_mod,0,1,/reverse_offset) ne '\' then dir_mod=dir_mod+'\'
      print,dir_mod
    end
    'DIR_MOD':  begin
      widget_control,labdir4_txt,get_value = dir_mod
      dir_mod=dir_mod[0]
      dir_mod=strcompress(dir_mod,/remove_all)
      if strmid(dir_mod,0,1,/reverse_offset) ne '\' then dir_mod=dir_mod+'\'
      print,dir_mod
    end
    'MODEL':  begin
      widget_control,labdir5_txt,get_value = model
      model=model[0]
      print,model
    end
    'YEAR':  begin
      widget_control,labdir7_txt,get_value = year
      print,year
    end
    'DIR_LOG':  begin
      widget_control,labdir6_txt,get_value = dir_log
      dir_log=dir_log[0]
      dir_log=strcompress(dir_log,/remove_all)
      if strmid(dir_log,0,1,/reverse_offset) ne '\' then dir_log=dir_log+'\'
      print,dir_log
    end
    'STEPG':begin
    Widget_Control, ev.id, GET_VALUE=itgen
  end
  'STEPO':begin
  Widget_Control, ev.id, GET_VALUE=itobs
end
'STEPM':begin
Widget_Control, ev.id, GET_VALUE=itmod
end
'STEPOM':begin
Widget_Control, ev.id, GET_VALUE=itobsmod
end
'GO': begin
  ;widget_control,/hourglass
  yearTXT=widget_info(ev.top, FIND_BY_UNAME='YEAR_TEXT')
  runFileTXT=widget_info(ev.top, FIND_BY_UNAME='RUNFILE_TEXT')
  widget_control, yearTXT, get_value=yearValue
  widget_control, runFileTXT, get_value=model
  yearValue=yearValue[0]
  model=model[0]
  print, model
  utility=obj_new('FMUtility')
  if not(utility->IsNumber(yearValue)) then begin
    a=dialog_message(['Year text field must be a valid numeric value'], DIALOG_PARENT=ev.top, TITLE=['Check Data Fields'], /CENTER, /ERROR)
    obj_destroy, utility
    return
  endif
  year=fix(yearValue)
  res=file_test(dir_log,/directory)
  if res eq 0 then begin
    txt='STEP 01 STOP! Directory LOG_DIR does not exist: See STARTUP FILE'
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
    txtall=['STOP',txtall]
    widget_control,labcom_txt,set_value=txtall
    close,11
    close,12
    return
  endif else begin
    close,11 & openw,11,dir_log+logfile
    close,12 & openw,12,dir_log+summaryfile
    printf,11,'STEP 01 OK: LOG_DIR exists'
    txt='STEP 01 OK: LOG_DIR exists'
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
    txt=['LOGFILE = '+dir_log+logfile,'=====================================']
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
    txt=['=====================================','SUMMARYFILE = '+dir_log+summaryfile,$
      '=====================================']
    txtall=[txt,txtall]
    widget_control,labcom_txt,set_value=txtall
  endelse
  for i=1,nsteps-1 do begin
    widget_control,labok(i),set_value='      '
  endfor
  txt=['=====================================',systime()]
  txtall=[txt,txtall]
  widget_control,labcom_txt,set_value=txtall
  txt=['=====================================','DeltaCheck_IO *** '+version]
  txtall=[txt,txtall]
  widget_control,labcom_txt,set_value=txtall
  
  All_steps
  txt=['DeltaCheck_IO --- DONE','=====================================']
  txtall=[txt,txtall]
  widget_control,labcom_txt,set_value=txtall
  txt=systime()
  txtall=[txt,txtall]
  widget_control,labcom_txt,set_value=txtall
  close,11
  close,12
end
'LOG': begin
  widget_control,/hourglass
  command=deltaMgr->getNotepadLocation()
  spawn,[command, dir_log+logfile],/noshell,/nowait
;spawn,['notepad.exe', dir_log+logfile],/noshell,/nowait
end
'SUM': begin
  widget_control,/hourglass
  command=deltaMgr->getNotepadLocation()
  spawn,[command, dir_log+summaryfile],/noshell,/nowait
;spawn,['notepad.exe', dir_log+summaryfile],/noshell,/nowait
end
'DONE':    BEGIN
  !p.position=0
  Widget_control,ev.top,/destroy
  close,11
  close,12
  print,'DeltaCheck_IO ---- EXIT'
  deltaMgr->checkDataIntegrityClose
  return
END
ENDCASE

;widget_control,base1212,sensitive=1

;if itobs eq 1 or itobsmod eq 1 then begin
;  widget_control,base1213,sensitive=1
;endif else begin
;  widget_control,base1213,sensitive=0
;endelse
;if itmod eq 1 or itobsmod eq 1 then begin
;  widget_control,base1214,sensitive=1
;  widget_control,base1215,sensitive=1
;endif else begin
;  widget_control,base1214,sensitive=0
;  widget_control,base1215,sensitive=0
;endelse
END
;*********************************************

;pro DeltaCheck_IO
pro DeltaCheck_IO, DeltaMgr
  common keesc1
  
  orig_P   = !P
  !P.Multi = 0
  Device, GET_SCREEN_SIZE=scr_size
  xw = 1250
  yw = 725
  days=[0,31,28,31,30,31,30,31,31,30,31,30,31]
  go=0
  next=0
  
  ; BEGIN USER PART ********************************
  
  ;version='VERSION 2.5'
  ;MM summer 2012
  ;Delta tool integration
  version='VERSION 3.0'
  fileMgr=deltaMgr->getFileSystemMgr()
  dir=fileMgr->getHomeDir(/WITH)
  dir_res=dir+'resource\'  ;fileMgr->getResourceDir()
  dir_obs=dir+'data\monitoring\'  ;fileMgr->getObservedDataDir()
  dir_mod=dir+'data\modeling\'  ;fileMgr->getRunDataDir()
  dir_log=dir+'log\'  ;fileMgr->getLogDir()
  
  modelInfo=deltaMgr->getModelList()
  modelNames=modelInfo->getDisplayNames()
  modelCodes=modelInfo->getCodes()
  
  scenarioInfo=deltaMgr->getScenarioList()
  scenarioNames=scenarioInfo->getDisplayNames()
  scenarioCodes=scenarioInfo->getCodes()
  
  ;model='2009_CHIM07BIL_TIME.cdf'
  
  year='2009'
  txtall=''
  oktxt='          '
  logfile='IO_check.log'
  summaryfile='IO_Summary.log'
  startup=fileMgr->getStartUpFileName()
  
  DeltaPol=['O3','PM10','PM25','NO2','NO','NOx','SO2']
  DeltaPolUnits=['ppb','ug/m3','ugm-3']
  DeltaMet=['WS','WD','TEMP']
  DeltaMetUnits=['m/s','ms-1','m/sec','deg','K','C']
  
  steps=strarr(18+1)
  STEPS(1)= 'STEP 01: Check on existence of directories'
  STEPS(2)= 'STEP 02: Check on existence of STARTUPfile'
  STEPS(3)= 'STEP 03: SCALE/PARAMETERS/MONITORING in STARTUPfile'
  STEPS(4)= 'STEP 04: Read species from PARAMETERS section in STARTUPfile'
  STEPS(5)= 'STEP 05: Read Stations from MONITORING section in STARTUPfile'
  STEPS(6)= 'STEP 06: Check redundant station-filenames in STARTUPfile'
  STEPS(7)= 'STEP 07: Check Nb of stations in STARTUPfile and MONITORING_DIR'
  STEPS(8)= 'STEP 08: Consistency of statnames and OBSfiles'
  STEPS(9)= 'STEP 09: Check consistency of species in STARTUPfile and OBSfile'
  STEPS(10)= 'STEP 10: TimeLength OBSfiles [=8760, =8784 (LeapYear), =1 (Yearly)]'
  STEPS(11)= 'STEP 11: OBS data availability at stations (%); Extreme values'
  STEPS(12)= 'STEP 12: Check OBS equal to zero (real or novalue ?)'
  STEPS(13)= 'STEP 13: Existence of MODfile'
  STEPS(14)= 'STEP 14: Existence of stations/species/attribute in MODfile'
  STEPS(15)= 'STEP 15: TimeLength of MOD/species [=8760 (hourly), =1 (yearly)]'
  STEPS(16)= 'STEP 16: Check on MOD NaN/Inf/Extreme values'
  STEPS(17)= 'STEP 17: MOD availability at stations for STARTUP species (%)'
  STEPS(18)= 'STEP 18: Basic Statistics'
  
  ;count=['01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16']
  nsteps=n_elements(STEPS)
  itgen=1 & itobs=1 & itmod=1 & itobsmod=1
  labok=lonarr(nsteps)
  base = WIDGET_BASE(/ROW,title='DELTACHECK_IO *** '+version, MBAR=WID_MENU, $
    xsize=xw,ysize=yw,TLB_FRAME_ATTR=1, uvalue=deltaMgr, /TLB_KILL_REQUEST_EVENTS, event_pro='DeltaCheck_IO_event')
  base1=WIDGET_BASE(base,/row,space=5)
  base110=widget_base(base1,/column)
  lab0=widget_label(base110,value='CHECK_STEPS',font='times Roman*18*bold')
  base11=widget_base(base110,/column,space=7,/frame)
  labGen=lonarr(5)
  baseG2=lonarr(5)
  baseG2(0)=widget_base(base11,/row,space=10)
  labGen(0)=widget_label(baseG2(0),value=steps(1),/align_left)
  labok(1)=widget_label(baseG2(0),value=oktxt,/align_left)
  butGen=cw_bgroup(base11,/column,/nonexclusive,'READ INFO from STARTUP File',uvalue='STEPG',set_value=[1])
  for i=1,4 do begin
    baseG2(i)=widget_base(base11,/row,space=10)
    labGen(i)=widget_label(baseG2(i),value=steps(i+1),/align_left)
    labok(i+1)=widget_label(baseG2(i),value=oktxt,/align_left)
  endfor
  butObs=cw_bgroup(base11,/column,/nonexclusive,'OBS',uvalue='STEPO',set_value=[1])
  labObs=lonarr(7)
  baseO2=lonarr(7)
  for i=0,6 do begin
    baseO2(i)=widget_base(base11,/row,space=10)
    labObs(i)=widget_label(baseO2(i),value=steps(i+6),/align_left)
    labok(i+6)=widget_label(baseO2(i),value=oktxt,/align_left)
  endfor
  butMod=cw_bgroup(base11,/column,/nonexclusive,'MOD',uvalue='STEPM',set_value=[1])
  labMod=lonarr(5)
  baseM2=lonarr(5)
  labelXsize=120
  textXsize=230
  for i=0,3 do begin
    baseM2(i)=widget_base(base11,/row,space=10)
    labMod(i)=widget_label(baseM2(i),value=steps(i+13),/align_left)
    labok(i+13)=widget_label(baseM2(i),value=oktxt,/align_left)
  endfor
  butOM=cw_bgroup(base11,/column,/nonexclusive,'OBS/MOD',uvalue='STEPOM',set_value=[1])
  labOM=lonarr(2)
  baseOM2=lonarr(3)
  for i=0,1 do begin
    baseOM2(i)=widget_base(base11,/row,space=10)
    labOM(i)=widget_label(baseOM2(i),value=steps(i+17),/align_left)
    labok(i+17)=widget_label(baseOM2(i),value=oktxt,/align_left)
  endfor
  base12=widget_base(base1,/column)
  lab121=widget_label(base12,value='FILE LOCATIONS',font='times Roman*18*bold', /ALIGN_RIGHT)
  base121=widget_base(base12,/column,/frame)
  base1211=widget_base(base121,/row)
  labdir1=widget_label(base1211,scr_XSIZE=labelXsize,value='HOME_DIR =',font='times Roman*14*bold')
  labdir1_txt=WIDGET_TEXT(base1211,scr_XSIZE=textXsize,ysize=0.3,uvalue='DIR',value=dir,/all_events)
  base1216=widget_base(base121,/row)
  labdir6=widget_label(base1216,scr_XSIZE=labelXsize,value='LOG_DIR =',font='times Roman*14*bold')
  labdir6_txt=WIDGET_TEXT(base1216,scr_XSIZE=textXsize,ysize=0.3,uvalue='DIR_LOG',value=dir_log,/all_events)
  base1212=widget_base(base121,/row)
  labdir2=widget_label(base1212,scr_XSIZE=labelXsize,value='RESOURCE_DIR =',font='times Roman*14*bold')
  labdir2_txt=WIDGET_TEXT(base1212,scr_XSIZE=textXsize,ysize=0.3,uvalue='DIR_RES',value=dir_res,/all_events)
  base1212a=widget_base(base121,/row)
  labdir2a=widget_label(base1212a,scr_XSIZE=labelXsize,value='STARTUP FILE =',font='times Roman*14*bold')
  labdir2a_txt=WIDGET_TEXT(base1212a,scr_XSIZE=textXsize,ysize=0.3,uvalue='STARTUP',value=startup,/all_events)
  base1213=widget_base(base121,/row)
  labdir3=widget_label(base1213,scr_XSIZE=labelXsize,value='MONITORING_DIR =',font='times Roman*14*bold')
  labdir3_txt=WIDGET_TEXT(base1213,scr_XSIZE=textXsize,ysize=0.3,uvalue='DIR_OBS',value=dir_obs,/all_events)
  base1214=widget_base(base121,/row)
  labdir4=widget_label(base1214,scr_XSIZE=labelXsize,value='MODELING_DIR =',font='times Roman*14*bold')
  labdir4_txt=WIDGET_TEXT(base1214,scr_XSIZE=textXsize,ysize=0.3,uvalue='DIR_MOD',value=dir_mod,/all_events)
  ;MM summer 2012 start
  modelBase=widget_base(base121,/row)
  modelLabel=widget_label(modelBase,scr_XSIZE=labelXsize, value='MODEL = ',font='times Roman*14*bold')
  modelDropList=widget_combobox(modelBase,scr_XSIZE=textXsize, uname='MODEL_COMBOBOX', value=modelNames, uvalue=[ptr_new(modelNames), ptr_new(modelCodes)],editable=0, $
    event_pro='DeltaCheck_IO_event')
  scenarioBase=widget_base(base121,/row)
  scenarioLabel=widget_label(scenarioBase,scr_XSIZE=labelXsize,value='SCENARIO = ',font='times Roman*14*bold')
  scenarioDropList=widget_combobox(scenarioBase, uname='SCENARIO_COMBOBOX', value=scenarioNames, uvalue=[ptr_new(scenarioNames), ptr_new(scenarioCodes)] , $
    event_pro='DeltaCheck_IO_event',scr_XSIZE=textXsize,editable=0)
  runFileBase=widget_base(base121,/row)
  runFileLabel=widget_label(runFileBase,scr_XSIZE=labelXsize, value='RUNFILE = ',font='times Roman*14*bold')
  runFileText=widget_text(runFileBase,scr_XSIZE=textXsize,ysize=0.3,uname='RUNFILE_TEXT',uvalue='RUNFILE_TEXT',value='',/editable)
  ;MM summer 2012 end
  ;  base1215=widget_base(base121,/row)
  ;  labdir5=widget_label(base1215,value='MODEL =                      ',font='times Roman*14*bold')
  ;  labdir5_txt=WIDGET_TEXT(base1215,XSIZE=30,ysize=0.3,uvalue='MODEL',value=model,/editable,/all_events)
  base1217=widget_base(base121,/row)
  labdir7=widget_label(base1217,scr_XSIZE=labelXsize,value='YEAR =',font='times Roman*14*bold')
  labdir7_txt=WIDGET_TEXT(base1217,scr_XSIZE=textXsize,ysize=0.3,uvalue='YEAR',value='',uname='YEAR_TEXT',/editable,/all_events)
  lab1x=widget_label(base12,value=' ',ysize=20)
  basegn=widget_base(base12,/row,space=10)
  butgo = WIDGET_BUTTON(basegn,VALUE='  Go  ', UVALUE='GO',ysize=50,$
    font='times Roman*18*bold', event_pro='DeltaCheck_IO_event')
  lab2x=widget_label(base12,value=' ',ysize=20)
  base122=widget_base(base12,/row)
  lab1222=widget_label(base122,value='PROGRESS: ',font='times Roman*14*bold')
  labprog_txt=WIDGET_TEXT(base122,XSIZE=44,ysize=0.3)
  widget_control,labprog_txt,set_value='---'
  lab5x=widget_label(base12,value=' ',ysize=20)
  baselg=widget_base(base12,/row,space=22)
  lab55x=widget_label(baselg,value='              ',ysize=20)
  butlog=WIDGET_BUTTON(baselg,VALUE='Edit LogFile', UVALUE='LOG',ysize=50,$
    font='times Roman*18*bold', event_pro='DeltaCheck_IO_event')
  butsum=WIDGET_BUTTON(baselg,VALUE='Edit SummaryFile', UVALUE='SUM',ysize=50,$
    font='times Roman*18*bold', event_pro='DeltaCheck_IO_event')
  lab4x=widget_label(base12,value=' ',ysize=20)
  baseex=widget_base(base12,/row,space=10)
  WID_EXIT=Widget_Button(baseex, VALUE='Exit',ysize=50,UVALUE='DONE',font='times Roman*18*bold', $
    event_pro='DeltaCheck_IO_event')
  base13=widget_base(base1,/column)
  lab13=widget_label(base13,value='             COMMENTS [See Log/Summary Files]',font='times Roman*18*bold')
  labcom_txt=WIDGET_TEXT(base13,XSIZE=75,ysize=40,/frame)
  Widget_Control, base, /REALIZE
  
  fakeEvent={top:base}
  updateRunFileName, fakeEvent, deltaMgr, scenario=scenarioNames[0]
  
  XMANAGER, 'DeltaCheck_IO', base, /JUST_REG, /CATCH
  !P = orig_P
  
end
