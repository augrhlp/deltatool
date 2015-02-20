@conversion_cb
@loadConversionSetting
pro conversion_event, ev

  common conv1
  
  Widget_Control, ev.id,  GET_UVALUE=what
  Widget_Control, ev.top, GET_UVALUE=pState
  prefixWid=WIDGET_INFO( ev.top, find_by_uname='INPUT_PREFIX')
  saveWid=WIDGET_INFO( ev.top, find_by_uname='SAVFILE')
  
  IF TAG_NAMES(ev, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN what='DONE'
  
  pState=*(pstate)
  deltaMgr=pState.deltaMgr
  slash=path_sep()
  ;Widget_Control, ev.top,  GET_UVALUE=deltaMgr
  CASE what OF
    'CONVDIR':  begin
      widget_control,labdir1_txt,get_value = convDir
      dir=deltaMgr.fileSystem->cleanPath(convDir)
      pState.convDir=convDir
      ;dir=dir[0]
      ;dir=strcompress(dir,/remove_all)
      ;if strmid(dir,0,1,/reverse_offset) ne slash then dir=dir+slash
      ;print,dir
      fillConversionWids, pstate
    end
    'DEFAULT': begin   ;
      fillConversionWids, pstate
      widget_control,pstate.butgo2,sensitive=1
    end
    'READINFO': begin   ; Read Info from InfoXXXcsv2cdf file and display info
      if pState.observedFlag then typeFile='obs' else typeFile='mod'
      baseFileName='info'+typeFile+'csv2cdf'
      ;convDir=pState.dir+path_sep()+'conversion'+path_sep()
      convDir=pState.convDir
      if pState.iread eq 0 then begin   ; 1st time readinfo, check HomeDir, check InfoXXXcsv2cdf files
        res=file_test(convDir,/directory)
        if res eq 0 then begin
          txt='==> STOP! CONV_DIR does not exist'
          addLogText, pState.labcom_txt, txt
          pstate.ierror=1
          goto, whatnow
        endif
        res=file_search(convDir+baseFileName+'*.txt', count=count)
        resSave=strcompress(res)
        pstate.nsavefile=count
        if res[0] eq '' then pstate.nsavefile=0
        thisFile=res[pState.isave]
        if pstate.nsavefile ge 1 then begin
          txt=strtrim(pstate.nsavefile,2)+' '+baseFileName+'* files found:'
          for i=0,pstate.nsavefile-1 do begin
            hlp1=strsplit(resSave(i),slash,/extract)
            hlp2=hlp1(n_elements(hlp1)-1)
            txt=[txt,strtrim(hlp2,2)]
          endfor
          addLogText, pState.labcom_txt, txt
          ;savefile=convDir+baseFileName+'.txt'
          isave=0
          widget_control,pstate.next02,set_value=thisFile
        ;storeConversionSetting, 0, pstate.strHelp, pState.dir+'InfoMODcsv2cdf.txt'
        ;values=loadConversionSetting(convDir+baseFileName+'.txt')
        endif else begin
          txt=['=====================================',baseFileName+'*.txt NOT found in CONV_DIR']
          addLogText, pState.labcom_txt, txt
          pstate.ierror=1
          goto,whatnow
        endelse
        ;        widget_control,lab121,sensitive=0
        widget_control,pstate.butgo2,sensitive=1
        ;        widget_control,basevars,sensitive=1
        widget_control,pstate.next02,set_value=hlp2
      endif
      if pState.iread eq 1 then begin ; Display InfoMODcsv2cdf
        widget_control,pstate.butgo2,sensitive=1
        pState.isave=(pState.isave+1) mod pstate.nsavefile
        ;print,pState.isave,'  ',resSave
        thisfile=resSave(pState.isave)
        ;SaveInfo,1,savefile
        ;storeConversionSetting, buildStoredValues(pstate), thisfile
        hlp1=strsplit(thisfile,slash,/extract)
        hlp2=hlp1(n_elements(hlp1)-1)
        widget_control,pstate.next02,set_value=hlp2
        widget_control,pstate.wid_save3,set_value=hlp2
        savfile=hlp2
        if pstate.ierror eq 1 then goto,whatnow
        txt=['=====================================',hlp2+'   from CONV_DIR']
        addLogText, pState.labcom_txt, txt
      endif
      
      namePos=strpos(thisfile, path_sep(), /REVERSE_SEARCH)
      name=strmid(thisfile, namePos+1, strlen(thisfile))
      widget_control,saveWid, set_value = name
      
      values=loadConversionSetting(thisFile)
      pstate.startUpFile= values[0]
      pstate.initRun= values[1]
      pstate.endRun= values[2]
      pstate.dirIn= values[3]
      pstate.prefixId= values[4]
      pstate.dirOut= values[5]
      pstate.year= values[6]
      pstate.modelName= values[7]
      pstate.postfix= values[8]
      pstate.observedFlag=fix(values[9])
      pstate.startHour= values[10]
      pstate.endHour= values[11]
      fillConversionWids, pstate
      ;pstate.prefixId, pstate.dirOut, pstate.modelId]
      hlp1=strsplit(resSave(isave),slash,/extract)
      hlp2=hlp1(n_elements(hlp1)-1)
      ;widget_control,pstate.wid_save3,set_value=hlp2
      savfile=hlp2
      if pstate.ierror eq 1 then goto,whatnow
      txt=['=====================================',baseFileName+'.txt    from CONV_DIR']
      addLogText, pState.labcom_txt, txt
      pstate.ierror=0
      if pState.iread eq 0 then pState.iread=1
      widget_control,pstate.wid_save1,sensitive=1  ; save file 3x
      widget_control,pstate.wid_save2,sensitive=1
      widget_control,pstate.wid_save3,sensitive=1
      fillConversionWids, pstate
    end
    'startup': begin
      widget_control,pState.infowids[0],get_value=startup
      pstate.startUpFile=strcompress(startup[0], /REMOVE)
      print,startup
      fillConversionWids, pstate
    end
    'MODEL': begin
      pstate=conversionObsModelSetting(ev.top, pstate, /MODEL)
    end
    'OBSERVED': begin
      pstate=conversionObsModelSetting(ev.top, pstate, /OBS)
    end
    'initrun': begin
      widget_control,pState.infowids[1],get_value=initrun
      initrun=initrun[0]
      if strlen(initrun) ne 8 then goto, noinit
      print,initrun
      year=fix(strmid(initrun,0,4))
      leapyear=0
      if 4*(fix(year)/4) eq fix(year) then leapyear=1
      months=['01','02','03','04','05','06','07','08','09','10','11','12']
      if leapyear eq 0 then days=[31,28,31,30,31,30,31,31,30,31,30,31]
      if leapyear eq 1 then days=[31,29,31,30,31,30,31,31,30,31,30,31]
      mnthInit=fix(strmid(initrun,4,2))
      dayInit=fix(strmid(initrun,6,2))
      hrs=0
      if mnthInit ge 2 then begin
        for im=0,mnthInit-2 do hrs=hrs+days(im)*24
      endif
      hrs=hrs+(dayInit-1)*24
      hour0=hrs
      fillConversionWids, pstate
      noinit:
    end
    'endrun': begin
      widget_control,pState.infowids[2],get_value=endrun
      endrun=endrun[0]
      if strlen(endrun) ne 8 then goto, noend
      print,endrun
      year=fix(strmid(endrun,0,4))
      leapyear=0
      if 4*(fix(year)/4) eq fix(year) then leapyear=1
      months=['01','02','03','04','05','06','07','08','09','10','11','12']
      if leapyear eq 0 then days=[31,28,31,30,31,30,31,31,30,31,30,31]
      if leapyear eq 1 then days=[31,29,31,30,31,30,31,31,30,31,30,31]
      mnthEnd=fix(strmid(endrun,4,2))
      dayEnd=fix(strmid(endrun,6,2))
      hrs=0
      if mnthEnd ge 2 then begin
        for im=0,mnthEnd-2 do hrs=hrs+days(im)*24
      endif
      hrs=hrs+(dayEnd-1)*24 + 23
      hour1=hrs
      fillConversionWids, pstate
      noend:
    end
    'inputdir': begin
      widget_control,pState.infowids[3],get_value=dir_in
      dir_in=deltaMgr->cleanPath(dir_in)
      pState.dirIn=dir_in
      ;pstate.dirIn=strcompress(dir_in[0], /REMOVE)
      fillConversionWids, pstate
    end
    'prefixid': begin
      widget_control,pState.infowids[4],get_value=prefixId
      pstate.prefixId=strcompress(prefixId[0], /REMOVE)
      fillConversionWids, pstate
    end
    'outputdir': begin
      widget_control,pState.infowids[5],get_value=dir_out
      dir_out=deltaMgr->cleanPath(dir_out)
      pState.dirOut=dir_out
      ;pstate.dirOut=strcompress(dir_out[0], /REMOVE)
      fillConversionWids, pstate
    end
    ;    'outputfile': begin
    ;      widget_control,pState.infowids[4],get_value=model
    ;      pstate.modelId=strcompress(model[0], /REMOVE)
    ;      widget_control,pstate.labWids[1],set_value=pstate.dirOut+pstate.modelId
    ;    end
    'year': begin
      widget_control,pState.infowids[6],get_value=year
      pstate.year=strcompress(year[0], /REMOVE)
      fillConversionWids, pstate
    end
    'modelname': begin
      widget_control,pState.infowids[7],get_value=modelName
      pstate.modelName=strcompress(modelName[0], /REMOVE)
      fillConversionWids, pstate
    end
    'postfix': begin
      widget_control,pState.infowids[8],get_value=postfix
      pstate.postfix=strcompress(postfix[0], /REMOVE)
      fillConversionWids, pstate
    end
    'SAVFILE':  begin ; event on new InfoMODcsv2cdf file names
      widget_control,ev.id,get_value = savfile
      savfile=strcompress(savfile,/remove_all)
      print,savfile
    end
    'SAVE':    BEGIN  ;Save the InfoMODcsv2cdf file
      widget_control,saveWid,get_value = savfile
      savfile=strcompress(savfile,/remove_all)
      !p.position=0
      if savfile eq 'InfoMODcsv2cdf.txt' then begin
        txt='WARNING! This is the default Save File'
        addLogText, pState.labcom_txt, txt
        txt='InfoMODcsv2cdf NOT saved'
        addLogText, pState.labcom_txt, txt
        pstate.ierror=1
        goto,whatnow
      endif
      ;SaveInfo,0,dir+savfile
      ;convDir=pState.dir+path_sep()+'conversion'+path_sep()
      convDir=pState.convDir
      storeConversionSetting, buildStoredValues(pState), convDir+savfile
      if pstate.ierror eq 1 then goto,whatnow
      txt=savfile+'  saved'
      txt=['=====================================',txt]
      addLogText, pState.labcom_txt, txt
      ;      WidgetHelp
      if pstate.ierror eq 1 then goto,whatnow
      res=file_search(convDir+'InfoMODcsv2cdf*.txt')
      resSave=strcompress(res)
      pstate.nsavefile=n_elements(resSave)
      widget_control,pstate.butgo2,sensitive=1
      widget_control,pstate.next02,set_value=savfile[0]
    END
    'HELP': begin
      ;if obj_valid()
      spawn,['notepad.exe',pState.helpDir+path_sep()+'MODcsv2cdfHelp.txt'], /noshell,/nowait
    end
    'GO': begin
      if fix(strmid(pState.initRun,0,4)) ne fix(strmid(pState.endRun,0,4)) then begin
        txt=['STOP','INIT_YEAR # END_YEAR']
        addLogText, pState.labcom_txt, txt
        pstate.ierror=1
        goto,whatnow
      endif
      txt='Year from Input Window = '+strmid(pState.initRun,0,4)
      addLogText, pState.labcom_txt, txt
      if fix(pstate.startHour) ge fix(pstate.endHour) then begin
        txt=['STOP','INIT_RUN ge END_RUN']
        addLogText, pState.labcom_txt, txt
        pstate.ierror=1
        goto,whatnow
      endif
      ;widget_control,/hourglass
      widget_control,pstate.butgo,sensitive=0   ; readinfo button
      widget_control,pstate.wid_save1,sensitive=0  ; save file 3x
      widget_control,pstate.wid_save2,sensitive=0
      widget_control,pstate.wid_save3,sensitive=0
      widget_control,pstate.butgo2,sensitive=0  ; go button
      widget_control,pstate.wid_exit2,sensitive=0  ; exit button
      pstate.ierror=0
      txt=['=====================================',systime()]
      addLogText, pState.labcom_txt, txt
      txt=['=====================================','DELTATOOL_MODcsv2cdf *** '+pstate.version]
      addLogText, pState.labcom_txt, txt
      outFileName=buildOutputFileName(pstate.dirOut, pstate.year, pstate.modelName, pstate.postFix, MODELTYPE=1-pstate.observedFlag)
      fInfo=file_info(outFileName)
      if not(fInfo.write) and fInfo.exists eq 1 then begin
        a=dialog_message([[outFileName], ['Already in use, please change your output name or execute conversion as first action of the tool']], title='Error')
        break
      endif
      pstate.ierror=conversionFileTest(pstate.startupFile, pstate.dirIn, pstate.dirOut, $
        outFileName, pState.labcom_txt)
      ;pState.year, pState.modelName, pState.observedFlag, pState.labcom_txt)
      if pstate.ierror eq 1 then begin
        txt=['=====================================','conversion_Test ---- ERROR',$
          '=====================================']
        addLogText, pState.labcom_txt, txt
      endif else begin
        convRes=csv2cdf(pstate.startUpFile, $
          pstate.startHour, pstate.endHour, pstate.dirIn, pstate.dirOut,  $
          pstate.prefixId, pstate.modelName, outFileName, pstate.initRun, pstate.endRun, logWin= pState.labcom_txt,progwin=pState.labcom_txt)
          
        ;      if ierror eq 1 then txt=['=====================================','CDF_to_CDF ---- ERROR',$
        ;        '=====================================']
        if convRes eq 1 then begin
          txt=['conversion ---- DONE','=====================================']
          addLogText, pState.labcom_txt, txt
          print,txt
          txt=systime()
          addLogText, pState.labcom_txt, txt
          txt=strarr(4)
          txt[0]='A model file named '+outFileName+'was created or updated.'
          txt[1]='Please close the application and start again'
          txt[2]='new data set will be available at next launch of Delta'
          txt[3]='manually remove input files from'+pstate.dirIn
          a=dialog_message(txt, title='Model updating...', /INFO)
        endif
      endelse
      widget_control,pstate.wid_save1,sensitive=1  ; save file 3x
      widget_control,pstate.wid_save2,sensitive=1
      widget_control,pstate.wid_save3,sensitive=1
      widget_control,pstate.butgo,sensitive=1   ; readinfo button
      widget_control,pstate.butgo2,sensitive=1  ; go button
      widget_control,pstate.wid_exit2,sensitive=1  ; exit button
    end
    'DONE': BEGIN
      !p.position=0
      Widget_control,ev.top,/destroy
      print,'conversion ---- EXIT'
      if n_elements(deltaMgr) eq 1 then if obj_valid(deltaMgr) then deltaMgr->dataConversionClose
      close, /all
      return
    END
  ENDCASE
  Widget_Control, ev.top, SET_UVALUE=Ptr_New(pstate, /NO_COPY)
  
  return
  whatnow:
  if pstate.ierror eq 1 then begin
    widget_control,pstate.butgo2,sensitive=0  ; go button
    widget_control,pstate.butgo,sensitive=1   ; readinfo button
    widget_control,pstate.wid_exit2,sensitive=1  ; exit button
  endif
  pstate.ierror=0
  
END
;*********************************************
