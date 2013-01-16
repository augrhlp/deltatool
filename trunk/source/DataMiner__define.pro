;********************
@structure_definition
;********************
FUNCTION DataMiner::getRunFileInfo, fileName

  ;print, fileName
  name=self.fileSystemMgr->getBaseFileName(fileName, /PRESERVE_PATH)
  ;  print, 'DataMiner::getRunFileInfo'
  ;  print, 'fullefilename', fileName
  ;  print, 'remove extension... result->', name
  ;name=(strsplit(fileName, '.', /EXTRACT))[0]
  return, strsplit(name, '_', /EXTRACT)
  
END

FUNCTION DataMiner::getNanValue

  return, -999
  
END

FUNCTION DataMiner::getMissingValue

  return, -888
  
END

FUNCTION DataMiner::readCSVFile, filename, HEADER=HEADER

;if 4*(year/4) ne year then begin   ;normal year
;    day_nb= [31,28,31,30,31,30,31,31,30,31,30,31]
;    day_sum=[0,31,59,90,120,151,181,212,243,273,304,334,365]
;  endif
;  if 4*(year/4) eq year then begin   ;leapyear - no 31 dec
;    day_nb= [31,29,31,30,31,30,31,31,30,31,30,30]
;    day_sum=[0,31,60,91,121,152,182,213,244,274,305,335,365]
;  endif
; do as if leapyear; correct at end  
  day_nb= [31,29,31,30,31,30,31,31,30,31,30,30]
  day_sum=[0,31,60,91,121,152,182,213,244,274,305,335,365]
  ERROR=0
  catch, error_status
  ;print, systime()
  if error_status NE 0 THEN BEGIN
    ERROR=1
    catch, /CANCEL
    errMsg=dialog_message('problem with file: <'+fileName+'> check existence or read permission.', /ERROR)
  endif
  
  openr, unit, fileName, /GET_LUN
  bufferString=''
  firstRow=1
  iyear=0
  noOBS=0
  while not(eof(unit)) do begin
    readf, unit, bufferString
    checkFirst=strmid(bufferString, 0,1)
    check1=(strpos(checkFirst, '[')+1) > 0
    check2=(strpos(checkFirst, ';')+1) > 0
    check3=(strpos(checkFirst, '#')+1) > 0
    null=strlen(strcompress(bufferString, /REMOVE_all)) eq 0
    if (check1+check2+check3) gt 0 or null then begin
    ;      print, 'Discard row', i
    ;      print, bufferString
    endif else begin
      info=strsplit(bufferString, ';', /EXTRACT, count=count) ;, /PRESERVE_NULL)
      if firstRow eq 1 then begin
        firstRow=0     
        if strcompress(strlowcase(info[0]),/remove_all) eq 'yearlyavg' then begin
          info=['YearlyAvg','mm','dd','hh',info[2:n_elements(info)-1]]
          infoyr=info[0]
          iyear=1  ; yearlyavg
          storeYear=info(1)
        endif
        HEADER=info      
; KeesC 11DEC2012: Problem Ana, day = 1hr ... 24 hr !        
        storeData=strarr(n_elements(info),8785) & storeData(*,*)='-999'  ;8760
;        for im=0,11 do begin
;        for id=0,day_nb(im)-1 do begin
;        for ih=0,23 do begin
;          sim=strtrim(im,2)
;          if im le 9 then sim='0'+sim
;          sid=strtrim(id,2)
;          if id le 9 then sid='0'+sid
;          sih=strtrim(ih,2)
;          if ih le 9 then sih='0'+sih
;          storeData(1,*)=sim
;          storeData(2,*)=sid
;          storeData(3,*)=sih
;          storeData(4:n_elements(info)-1,*)='-999'
;        endfor
;        endfor
;        endfor
        infoNoOBS=strupcase(strcompress(info,/remove_all))
        NoHlp=where(infoNoOBS eq 'NOOBS',noObs)
        k=0
      endif else begin
        if iyear eq 1 then info=[infoyr,'mm','dd','hh',info]
        storeData[*, 0]=strcompress(info, /REMOVE_all)
        if iyear eq 0 then storeYear=info(0)
        if iyear eq 1 then goto,kIsOne
        ; year=info(0); mnth=info(1); day=info(2); hrs=info(3)
        ; calculate hour in year     
        k1=day_sum(fix(info(1))-1)*24
        k2=(fix(info(2))-1)*24
        k3=fix(info(3))
        k0=k1+k2+k3
        storeData[*, k0]=strcompress(info, /REMOVE_all)
        k++
      endelse
    endelse
  endwhile
  ; KeesC 31MAY2012  for Yearly OBS values
  kIsOne:
  if NoOBS eq 1 then goto, NoObserv
  storeData(0,*)=storeYear
  if 4*(fix(StoreYear)/4) ne fix(StoreYear) then begin   ;normal year: shift 24 hours back
    storeHlp=storeData(*,60*24:366*24-1)  ;01/03/year 0hr - 31/dec/year 23hr
    storeData(*,59*24:365*24-1)=storeHlp
  endif
  if k eq 0 then begin
    for kk=1,8783 do storeData(*,kk)=storeData(*,0)
  endif
  NoObserv:
  storeData=reform(storeData(*,0:8759))
  close, unit & free_lun, unit
  return, storeData
  
END

FUNCTION DataMiner::buildMonitoringFileName, monitoringCode

  ;fName=self.fileSystemMgr->getObservedDataDir(/WITH)+monitoringCode+self.fileSystemMgr->getMonitoringFileExtension()
  ;TODO: remove temp set 'station' prefix
  ; temp set to workaround filenames!!!
  ;fName=self.fileSystemMgr->getObservedDataDir(/WITH)+'station'+monitoringCode+self.fileSystemMgr->getMonitoringFileExtension()
  fName=self.fileSystemMgr->getObservedDataDir(/WITH)+monitoringCode+self.fileSystemMgr->getMonitoringFileExtension()
  return, fName
  
END

FUNCTION DataMiner::buildRunFileName, fileName, resultType; (TWOD=TWOD, TIME=TIME)

  ;allRunCodes=runInfoList->getCodes()
  ;idx=(where(runCode eq allRunCodes))[0]

  ;fName=runInfoList[idx].filename
  ;fName=runInfoList[idx].scenarioCode+'_'+runInfoList[idx].modelCode
  extensionPos=strpos(fileName, '.', /REVERSE_SEARCH)
  extension=strarr(2)
  extension[0]=strmid(fileName, 0, extensionPos)
  extension[1]=strmid(fileName, extensionPos+1, strlen(fileName)-extensionPos)
  
  fName=self.fileSystemMgr->getRunDataDir(/WITH)+extension[0]+'_'+resultType+'.'+extension[1]
  ;self.fileSystemMgr->getRunFileExtension()
  
  ;fName=fName+scenarioCode+'_'+modelCode+'_'+resultType
  
  ; if keyword_set(TWOD) then suffix=self.fileSystemMgr->getRunFile2DSuffix()
  ; if keyword_set(TIME) then suffix=self.fileSystemMgr->getRunFileTimeSuffix()
  ; if resultType eq '2D' then suffix=self.fileSystemMgr->getRunFile2DSuffix()
  ; if resultType eq 'TIME' then suffix=self.fileSystemMgr->getRunFileTimeSuffix()
  
  ; fName=fName+self.fileSystemMgr->getRunFileExtension()
  return, fName
  
END

FUNCTION DataMiner::buildRunDataBlockName, statCode, parameterCode

  cdfBlockName=statCode+'_'+parameterCode
  return, cdfBlockName
  
END

PRO DataMiner::readAllData, request, result, screensize=screensize

  result=obj_new('Result', '', self.fileSystemMgr->getTempDataFile())
  isSingleSelection=request->IsSingleObsPresent()
  isGroupSelection=request->IsGroupObsPresent()
  parameters=request->getParameterCodes()
  models=request->getModelCodes()
  scenarios=request->getScenarioCodes()
  runFiles=request->getRunFileNames()
  aData=getResultData()
  if isSingleSelection then begin
    singleMonits=request->getSingleObsNames()
    singleResultData=replicate(aData, n_elements(singleMonits)*n_elements(parameters)*n_elements(runFiles))
    singleMonMaxVal=-99999. & singleRunMaxVal=-99999.
    singleMonMinVal=99999. & singleRunMinVal=-99999.
    sl=0
  endif
  if isGroupSelection then begin
    groupMonits=request->buildAllGroupNames()
    groupResultData=replicate(aData, n_elements(groupMonits)*n_elements(parameters)*n_elements(runFiles))
    groupMonMaxVal=-99999. & groupRunMaxVal=-99999.
    groupMonMinVal=99999. & groupRunMinVal=-99999.
    gl=0
  endif
  
  ;progressbar = Obj_New('progressbar', Color='red',/noCancel,title='Diagram Elaboration',xsize=250,ysize=20)
  
  count=1
  numberUpdates=20
  ;nloop=n_elements(runFiles)*n_elements(singleMonits)
  nloop=n_elements(runFiles)*(n_elements(singleMonits)+n_elements(groupMonits))
  updateFreq=fix(nloop/float(numberUpdates))
  if nloop lt 20 then updateFreq=1
  ;stop
  progressbar = Obj_New('PROGRESSBARBTT',/noCancel,title='Diagram Elaboration',xsize=250,ysize=20,$
    /NODRAW,/TRUE,BUTTONSAMPLE=[250,0,0], BUTTONNUMBER=numberUpdates, screensize=screensize)
  progressbar -> Start
  
  ; split llops?
  for i=0, n_elements(runFiles)-1 do begin
    runFileName=self->buildRunFileName(runFiles[i], request->getRunResultType())
    ; single obj loop
    if isSingleSelection then begin
      for j=0, n_elements(singleMonits)-1 do begin
      
        updateOk=count mod updateFreq
        IF updateOk eq 0 THEN BEGIN
          percent=float(count)/nloop*100
          progressbar -> Update, percent
        ENDIF
        ;        print, count, '/', nloop
        count++
        
        singleMonitFileName=self->buildMonitoringFileName(singleMonits[j])
        for k=0, n_elements(parameters)-1 do begin
          singleResultData[sl].observedCode=singleMonits[j]
          singleResultData[sl].parameterCode=parameters[k]
          ;      print, parameters[k]
          ;      print, '--> Observed'
          mParData=self->readMonitoringData(singlemonitFileName, parameters[k], NOTPRESENT=NOTPRESENT)
          if NOTPRESENT eq 1 then begin
            ;print, 'NOT PRESENT'
            ptr_free, singleResultData[sl].observedData ; null pointer
          endif else begin
            nanIdxs=where ((mParData ne self->getNanValue()) and (mParData ne self->getMissingValue()), nans)
            if nans ne 0 then minVal=min(mParData[nanIdxs], /NAN, max=maxVal) else minVal=min(mParData, /NAN, max=maxVal)
            singleResultData[sl].observedMinVal=minVal
            singleResultData[sl].observedMaxVal=maxVal
            singleMonMaxVal=min([singleMonMinVal, singleResultData[sl].observedMinVal], /NAN)
            singleMonMinVal=max([singleMonMaxVal, singleResultData[sl].observedMaxVal], /NAN)
            singleResultData[sl].observedData=ptr_new(mParData, /NO_COPY)
          endelse
          ;print, '--> Run'
          rParData=self->readRunData(runFileName, singleMonits[j], parameters,k, NOTPRESENT=NOTPRESENT)
          if NOTPRESENT eq 1 then begin
            ;print, 'NOT PRESENT'
            ptr_free, singleResultData[sl].runData ; null pointer
          endif else begin
            singleResultData[sl].runCode=parameters[k]
            rFInfos=self->getRunFileInfo(runFiles[i])
            singleResultData[sl].scenarioCode=rFInfos[0]
            singleResultData[sl].modelCode=rFInfos[1]
            nanIdxs=where ((rParData ne self->getNanValue()) and (rParData ne self->getMissingValue()), nans)
            if nans ne 0 then minVal=min(rParData[nanIdxs], /NAN, max=maxVal) else minVal=min(rParData, /NAN, max=maxVal)
            singleResultData[sl].runMinVal=minVal
            singleResultData[sl].runMaxVal=maxVal
            singleRunMaxVal=min([singleRunMinVal, singleResultData[sl].runMinVal], /NAN)
            singleRunMinVal=max([singleRunMaxVal, singleResultData[sl].runMaxVal], /NAN)
            singleResultData[sl].runData=ptr_new(rParData, /NO_COPY)
          endelse
          ;help,  singleResultData[sl], /STR
          sl++
        endfor
      ;  print, '*********************'
      endfor
    endif
    
    if isGroupSelection then begin
      ; group obj loop
      for j=0, n_elements(groupMonits)-1 do begin
      
        updateOk=count mod updateFreq
        IF updateOk eq 0 THEN BEGIN
          percent=float(count)/nloop*100
          progressbar -> Update, percent
        ENDIF
        ;        print, count, '/', nloop
        count++
        
        monitFileName=self->buildMonitoringFileName(groupMonits[j])
        for k=0, n_elements(parameters)-1 do begin
          groupResultData[gl].observedCode=groupMonits[j]
          groupResultData[gl].parameterCode=parameters[k]
          ;      print, parameters[k]
          ;      print, '--> Observed'
          mParData=self->readMonitoringData(monitFileName, parameters[k], NOTPRESENT=NOTPRESENT)
          if NOTPRESENT eq 1 then begin
            ;print, 'NOT PRESENT'
            ptr_free, groupResultData[gl].observedData ; null pointer
          endif else begin
            nanIdxs=where ((mParData ne self->getNanValue()) and (mParData ne self->getMissingValue()), nans)
            if nans ne 0 then minVal=min(mParData[nanIdxs], /NAN, max=maxVal) else minVal=min(mParData, /NAN, max=maxVal)
            groupResultData[gl].observedMinVal=minVal
            groupResultData[gl].observedMaxVal=maxVal
            groupMonMaxVal=min([groupMonMinVal, groupResultData[gl].observedMinVal], /NAN)
            groupMonMinVal=max([groupMonMaxVal, groupResultData[gl].observedMaxVal], /NAN)
            groupResultData[gl].observedData=ptr_new(mParData, /NO_COPY)
          endelse
          ;print, '--> Run'
          rParData=self->readRunData(runFileName, groupMonits[j], parameters,k, NOTPRESENT=NOTPRESENT)
          if NOTPRESENT eq 1 then begin
            ;print, 'NOT PRESENT'
            ptr_free, groupResultData[gl].runData ; null pointer
          endif else begin
            groupResultData[gl].runCode=parameters[k]
            rFInfos=self->getRunFileInfo(runFiles[i])
            groupResultData[gl].scenarioCode=rFInfos[0]
            groupResultData[gl].modelCode=rFInfos[1]
            nanIdxs=where ((rParData ne self->getNanValue()) and (rParData ne self->getMissingValue()), nans)
            if nans ne 0 then minVal=min(rParData[nanIdxs], /NAN, max=maxVal) else minVal=min(rParData, /NAN, max=maxVal)
            groupResultData[gl].runMinVal=minVal
            groupResultData[gl].runMaxVal=maxVal
            groupRunMaxVal=min([groupRunMinVal, groupResultData[gl].runMinVal], /NAN)
            groupRunMinVal=max([groupRunMaxVal, groupResultData[gl].runMaxVal], /NAN)
            groupResultData[gl].runData=ptr_new(rParData, /NO_COPY)
          ;            print,'DataMiner',groupResultData
          endelse
          ;help,  groupResultData[gl], /STR
          gl++
        endfor
      ;  print, '*********************'
      endfor
    endif
    
  endfor
  ;  print, count, nloop
  
  progressbar -> Destroy
  
  result->setRawRunMinVal, singleRunMinVal, groupRunMinVal
  result->setRawRunMaxVal, singleRunMaxVal, groupRunMaxVal
  result->setRawMonMinVal, singleMonMinVal, groupMonMinVal
  result->setRawMonMaxVal, singleMonMaxVal, groupMonMaxVal
  result->setRawData, singleResultData, groupResultData
  
END

FUNCTION DataMiner::readParameter, parameterCode, data, header, NOTPRESENT=NOTPRESENT

  NOTPRESENT=0
  idx=where(header eq parameterCode, count)
  if count ne 1 then begin
    NOTPRESENT=1
    return, [-1]
  endif
  ;end
  return, reform(float(data[idx[0], *]))
  
END

FUNCTION DataMiner::readMonitoringData, fileName, parameterCode, NOTPRESENT=NOTPRESENT

  allData=self->readCSVFile(fileName, HEADER=HEADER)
  parData=self->readParameter(parameterCode, allData, HEADER, NOTPRESENT=NOTPRESENT)
  if NOTPRESENT then begin
;    print, '<',parameterCode, '> ', 'isn''t in :<', fileName, '>'
  endif
  allData=0
  return, parData
  
END

FUNCTION DataMiner::readMonitoringDataForAllParameters, fileName, parameterCodes

  allData=self.fileSystemMgr->readCSVFile(fileName, HEADER=HEADER)
  parNumber=n_elements(parameterCodes)
  pars=ptrarr(parNumber)
  for i=0, parNumber-1 do begin
    parData=self->readParameter(parameterCode, allData, HEADER, NOTPRESENT=NOTPRESENT)
    if NOTPRESENT then begin
;      print, '<',parameterCode, '> ', 'isn''t in :<', fileName, '>'
    ;      errMsg=dialog_message('<'+parameterCode+ '> isn''t in :<'+ fileName+ '>', /ERROR)
    endif else begin
      pars[i]=ptr_new(parData, /NO_COPY)
    endelse
  endfor
  
  return, pars
  
END

FUNCTION DataMiner::readRunData, fileName, statCode, parameterCodes,k, NOTPRESENT=NOTPRESENT
  ;KeesC 31MAY2012
  ;fsm=obj_new('FMFileSystemManager')
  ERROR=0
  catch, error_status
  
  if error_status NE 0 THEN BEGIN
    ERROR=1
    catch, /CANCEL
    NOTPRESENT=1
    return, -1
  endif
  NOTPRESENT=0
  
  extPos=strpos(filename, '.', /REVERSE_SEARCH)
  ext=strmid(filename,extPos+1,3)   ; = cdf  or   csv
   
  if ext eq 'cdf' then begin
    !quiet=1
    Id = ncdf_open(fileName)
    cdfBlockName=statCode+'_'+parameterCodes[k]
    checkName=ncdf_varid(Id,cdfBlockName)
    ;Old: variable = StationName_Parameter
    if checkName ne -1 then begin  
      ncdf_varget, Id, cdfBlockName, data
      ncdf_close, Id
      return, data
    endif else begin
      ;New: Variable = StationName
      ncdf_attget,Id,'Parameters',pollout,/global
      pollout=string(pollout)
      pollout=strsplit(pollout,' ',/extract)
      pollout=strcompress(pollout,/remove_all)
      cc=where(pollout eq parameterCodes[k],ncc)
      cdfBlockName=statCode
; KeesC 21NOV2012      
      inqStHr=ncdf_attinq(Id,'StartHour',/global)
      if inqStHr.dataType eq 'UNKNOWN' then begin
        ncdf_varget, Id, cdfBlockName, data,count=[1,8760],offset=[cc(0),0]
        data=reform(data)
      endif else begin
        ncdf_attget,id,'StartHour',StartHour,/global
        data=fltarr(8760) & data(*)=-999
        ncdf_varget,Id,cdfBlockName,dataShort,count=[1,8760],offset=[cc(0),0]
        dataShort=reform(dataShort)
        dimShort=n_elements(dataShort)
        data(StartHour:StartHour+dimShort-1)=dataShort
      endelse
      ncdf_close, Id
      return,data
    endelse
    !quiet=0
  endif
  if ext eq 'csv' then begin
    openr, unit, fileName, /GET_LUN
    bufferString=''
    RowNr=0
    kk=0
    iyear=0
    while not(eof(unit)) do begin
      readf, unit, bufferString
      checkFirst=strmid(bufferString, 0,1)
      check1=(strpos(checkFirst, '[')+1) > 0
      check2=(strpos(checkFirst, ';')+1) > 0
      check3=(strpos(checkFirst, '#')+1) > 0
      null=strlen(strcompress(bufferString, /REMOVE_all)) eq 0
      if (check1+check2+check3) gt 0 or null then begin
      endif else begin
        info=strsplit(bufferString, ';', /EXTRACT, count=count)
        if RowNr eq 0 then begin
          RowNr=1
          if strlowcase(info[0]) eq 'yearlyavg' then begin
            pollout=strcompress(info[2:n_elements(info)-1],/remove_all)
            infoyr=info[0]
            iyear=1           
          endif else begin  
            pollout=strcompress(info[4:n_elements(info)-1],/remove_all)
          endelse
          npol=n_elements(pollout)
          storeData=fltarr(8784)  ;8760
        endif else begin
          if strupcase(strcompress(info[0],/remove_all)) eq strupcase(statCode) then begin
            kpol=where(pollout eq parameterCodes[k],nc)
            nr=1+kpol[0]
            storeData[kk]=float(info(nr))
            kk=kk+1
            if iyear eq 1 then goto,kkIsOne
          endif
        endelse
      endelse
    endwhile
    kkIsOne:
    if kk eq 1 then begin
      for kkk=1,8783 do storeData(kkk)=storeData(0)
    endif
    close, unit & free_lun, unit
    data=reform(storeData(0:8759))
    return,data
  endif
  
END
;****************************************************************************************
; constructor/destructor
;****************************************************************************************
FUNCTION DataMiner :: init

  if not self -> Object :: init() then return , 0
  self.fileSystemMgr=obj_New('FMFileSystemManager')
  return, 1
  
END

PRO DataMiner :: cleanUp

  obj_destroy, self.fileSystemMgr
  self -> Object :: cleanUp
  
END

PRO DataMiner__Define

  Struct = { DataMiner , $
    fileSystemMgr: obj_new(), $
    Inherits Object $
    }
    
END

