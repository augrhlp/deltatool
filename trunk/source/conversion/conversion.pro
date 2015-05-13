pro conversion, state, deltaMgr, NOVIEW=NOVIEW, AUTOCHECK=AUTOCHECK

  slash=path_sep()
  dims=getScreenSize()
  genericWidth=dims[0]/3-dims[0]/10
  
  ; MM fall 2014 Start
  startUpFile= ''
  initRun= ''
  endRun= ''
  convDir=''
  dirIn= ''
  dirOut= ''
  prefixId= ''
  observedFlag=0
  ;modelId= '', $
  year= ''
  modelName= ''
  postfix= ''
  startHour= ''
  endHour= ''
  dir_obs=''
  dir_mod=''
  if obj_valid(deltaMgr) then begin
    fileMgr=deltaMgr->getFileSystemMgr()
    deltaMgr->closeAllFIles
    dir_res=fileMgr->getResourceDir()
    dir_obs=fileMgr->getObservedDataDir()
    dir_mod_csv=fileMgr->getRunDataDir(/WITH)+'csv'
    dir_mod_cdf=fileMgr->getRunDataDir()
    dir_log=fileMgr->getLogDir()
    convDir=fileMgr->getConversionDir(/WITH);+'MODcsv2cdf'+slash
    dirIn=fileMgr->getRunDataDir(/WITH)
    dirOut=fileMgr->getRunDataDir(/WITH)
    helpDir=fileMgr->getHelpDir()
    
    modelInfo=deltaMgr->getModelList()
    modelNames=modelInfo->getDisplayNames()
    modelCodes=modelInfo->getCodes()
    
    scenarioInfo=deltaMgr->getScenarioList()
    scenarioNames=scenarioInfo->getDisplayNames()
    scenarioCodes=scenarioInfo->getCodes()

    startUpFile= fileMgr->getStartUpFileName()
    
    year= scenarioNames[0]
    initRun= year+'0101';strtrim(0, 1)
    endRun= year+'1231';strtrim(8760, 1)
    prefixId= scenarioNames[0]
    prefixId= ''
    observedFlag=0
    modelName= modelNames[0]
    postfix= 'TIME.cdf'
    startHour= strtrim(1, 1)
    endHour= strtrim(8760, 1)
    dmgr=deltaMgr
  endif else begin
    CD, CURRENT=convDir
    if strmid(dir,0,1,/reverse_offset) ne slash then dir=dir+slash
    dMgr=obj_new()
  endelse
  ; MM Fall 2014 End
  
  orig_P   = !P
  !P.Multi = 0
  device, GET_SCREEN_SIZE=scr_size
  xw = 1200
  yw = 625
  ;  days=[0,31,28,31,30,31,30,31,31,30,31,30,31]
  
  ; BEGIN USER PART ********************************
  Version='VERSION 1.1'
  ; END USER PART **********************************
  
  print,'OS_system = ',strupcase(!version.os_family)  ;WINDOWS, UNIX
  
  txtall=''
  YNlab121=0
  YNlabvars=0
  spa='              '
  
  base = WIDGET_BASE(/ROW,title=spa+'DELTATOOL_MODcsv2cdf *** '+version, $
    MBAR=WID_MENU,scr_xsize=xw,scr_ysize=yw,TLB_FRAME_ATTR=1,/TLB_KILL_REQUEST_EVENTS, NOTIFY_REALIZE='conversionConfig', /SCROLL)
  base1=WIDGET_BASE(base,/column,space=10)
  labxx=widget_label(base1,value=' ',ysize=9)
  base1211=widget_base(base1,/row)
  labdir1=widget_label(base1211,value='CONV_DIR =             ',font='times Roman*14*bold')
  labdir1_txt=WIDGET_text(base1211,XSIZE=91,ysize=0.3,value=convDir,font='times Roman*16*bold',$
    /editable,/all_events,uvalue='CONVDIR', sensitive=0)
  labxx=widget_label(base1211,value=' ',xsize=2)
  basegn=widget_base(base1,/row,space=20)
  butgo = WIDGET_BUTTON(basegn,VALUE=' ReadInfo >> ', UVALUE='READINFO',ysize=30,font='times Roman*18*bold', sensitive=1)
  ;butdef = WIDGET_BUTTON(basegn,VALUE=' Default Setting ', UVALUE='DEFAULT',ysize=30,font='times Roman*18*bold', sensitive=1)
  next02=widget_label(basegn,value=' ',xsize=250,font='times Roman*16*bold')
  baseleft=widget_base(base1)
  
  infoWids = buildInteractiveWids(baseLeft, labWids=labWids, bttWids=bttWids)
  
  labxx=widget_label(base1,value=' ',ysize=5)
  basegn2=widget_base(base1,/row)
  ;
  WID_save1=Widget_Button(basegn2, VALUE='SaveInfo',ysize=30,UVALUE='SAVE',font='times Roman*18*bold')
  wid_save2=widget_label(basegn2,value='  in File  ',font='times Roman*16*bold')
  savfile='InfoMODcsv2cdf*.txt'
  wid_save3=WIDGET_text(basegn2,XSIZE=76,ysize=0.3,value=savfile,font='times Roman*16*bold',$
    /editable,/all_events,uvalue='SAVFILE',uname='SAVFILE')
  widget_control,wid_save1,sensitive=0
  widget_control,wid_save2,sensitive=0
  widget_control,wid_save3,sensitive=0
  labxx=widget_label(basegn2,value=' ')
  labxx=widget_label(base1,value=' ',ysize=3)
  basepr=widget_base(base1,/row)
  labpr=widget_label(basepr,value='Progress ',font='times Roman*16*bold')
  labpr_txt=widget_text(basepr,xsize=85,ysize=0.3)
  widget_control,labpr_txt,set_value='--'
  labxx=widget_label(base1,value=' ',ysize=3)
  ; MM fall 2014 Start
  base2=widget_base(base,/column)
  basegn2=widget_base(base2,/COLUMN,ypad=2)
  buthelp = WIDGET_BUTTON(basegn2,VALUE='HELP', UVALUE='HELP',ysize=30,font='times Roman*18*bold')
  butgo2 = WIDGET_BUTTON(basegn2,VALUE='GO', UVALUE='GO',ysize=30,font='times Roman*18*bold')
  wid_exit2=Widget_Button(basegn2, VALUE='EXIT',ysize=30,UVALUE='DONE',font='times Roman*18*bold')
  lab2=widget_label(base2,value='COMMENTS',font='times Roman*18*bold',XSIZE=40)
  labcom_txt=WIDGET_TEXT(base2,XSIZE=40,ysize=48,/frame,/SCROLL)
  widget_control,butgo2,sensitive=0
  
  pState = {                     $
    ierror:0,                       $
    iread:0,                       $
    deltaMgr:dMgr, $
    convDir:convDir, $
    txtall:txtall, $
    labcom_txt:labcom_txt, $
    next02:next02, $
    wid_save1:wid_save1, $
    wid_save2:wid_save2, $
    wid_save3:wid_save3, $
    butgo: butgo, $
    butgo2: butgo2, $
    wid_exit2: wid_exit2, $
    modelBtt:bttWids[0], $
    ;observedBtt:bttWids[1], $
    startUpFile: startUpFile, $
    initRun: initRun, $
    endRun: endRun, $
    dirIn: dirIn, $
    dirOut: dirOut, $
    prefixId: prefixId, $
    observedFlag:observedFlag, $
    ;modelId: '', $
    year: year, $
    modelName: modelName, $
    postfix: postfix, $
    startHour: startHour, $
    endHour: endHour, $
    isave: 0, $
    nsavefile: 0, $
    infoWids : infoWids, $
    labWids:labWids, $
    version :version, $
    dir_obs: dir_obs, $
    dir_mod_csv: dir_mod_csv, $
    helpDir: helpDir, $
    dir_mod_cdf: dir_mod_cdf $
    }
    
  Widget_Control, base, SET_UVALUE=Ptr_New(pState, /NO_COPY)
  
  ; MM Fall 2014 End
  Widget_Control, base, /REALIZE
  
  JUST_REG=0
  if obj_valid(deltaMgr) then JUST_REG=deltaMgr->isRunning()
  
  XMANAGER, 'conversion', base, JUST_REG=JUST_REG
  ;XMANAGER, 'conversion', base, CATCH=0
  ; MM Fall 2014 End
  !P = orig_P
  
end
