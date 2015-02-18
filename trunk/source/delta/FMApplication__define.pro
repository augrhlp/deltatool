;********************
@../common/structure_definition
@../check_io/checkcriteria
;********************

FUNCTION FMApplication::cleanPath, path

  return, self.fileSystemMgr->cleanPath(path)
  
END

PRO FMApplication::setUserType, value

  self.mainConfig->setUserType, value
  
END

;PRO FMApplication::setLastUserType, value
;
; self.lastUserType=value
;
;END

;FUNCTION FMApplication::getLastUserType
;
; return, self.lastUserType
;
;END

PRO FMApplication::closeAllFiles

  self.dataMinerMgr->CloseAllDesc
  
END

PRO FMApplication::configureExecutable

  lines=''
  parList=strarr(6) & exeList=strarr(6)
  
  parList[0]='GOOGLEEARTH_LOCATION'
  exeList[0]=self.googleEarthLocation
  
  parList[1]='WORKSHEET_LOCATION'
  exeList[1]=self.workSheetLocation
  
  parList[2]='NOTEPAD_LOCATION'
  exeList[2]=self.notepadLocation
  
  parList[3]='DOCUMENTSREADER_LOCATION'
  exeList[3]=self.docReaderLocation
  
  parList[4]='PDFREADER_LOCATION'
  exeList[4]=self.pdfReaderLocation
  
  parList[5]='BROWSER_LOCATION'
  exeList[5]=self.browserLocation
  
  test=0
  textMessage='This operation will take a lot of time (more than one hour, in the worst case). Do you want to proceed anyway?'
  title='Find external applications'
  
  answ=dialog_Message(textMessage, title=title, /question)
  if strupcase(answ) eq 'YES' then begin
    if not(keyword_set(test)) then begin
    
    
      for i=0, n_elements(exeList)-1 do begin
        info=strsplit(exeList[i], path_sep(), /EXTRACT)
        if n_elements(info) gt 1 then begin
          print, systime(), 'path of', parList[i], ' looks already modified, skip'
          print, exeList[i]
        endif else begin
          print, 'try to search...', exeList[i]
          print, '... it will take long time, please wait...'
          widget_control, /HOURGLASS
          if strupcase(!VERSION.OS_FAMILY) eq 'WINDOWS' then begin
            ;deep search
            ;rootPaths=['C:\']
            ;fast search
            rootPaths=['C:\Program Files (x86)', 'C:\Program Files', 'C:\Windows']
          endif else begin
            rootPaths='/'
          endelse
          for j=0, n_elements(rootPaths)-1 do begin
            result=file_search(rootPaths[j], exeList[i], count=count)
            widget_control, HOURGLASS=0
            if count gt 0 then begin
              lines=[lines, 'replacing '+exeList[i]+'with :'+result[0]]
              print, ' found '+'replacing '+exeList[i]+'with :'+result[0]
              exeList[i]=result[0]
              break
            endif else begin
              lines=[lines, exeList[i]+' not found (root '+rootPaths[j]+')']
              print, exeList[i], ' not found (root '+rootPaths[j]+')'
            endelse
          endfor
        endelse
      endfor
    endif else begin
      exeList[0]='C:\Program Files (x86)\Google\Google Earth\client\googleearth.exe'
    endelse
    ;now replace init.ini
    fileContents=self.fileSystemMgr->readPlainTextFile(self.fileSystemMgr->getInitFileName())
    for j=0, n_elements(fileContents)-1 do begin
      for i=0, n_elements(parList)-1 do begin
        info=strsplit(fileContents[j], '=', /EXTRACT)
        if strupcase(parList[i]) eq strupcase(info[0]) then begin
          print, 'replace: ', parList[i], ' with:', exeList[i]
          lines=[lines, fileContents[j]]
          fileContents[j]=parList[i]+'='+exeList[i]
          self->setExecutableLocationKey, parList[i], exeList[i]
        endif
      endfor
    endfor
    lines=[lines, 'overwriting :'+self.fileSystemMgr->getInitFileName()]
    ;info=self.dialogMessage('Overwrite:'+self.fileSystemMgr->getInitFileName(), title='Replacing init.ini', /info)
    ;info=dialog_Message(lines, title='Infos', /info)
    self.fileSystemMgr->writePlainTextFile, self.fileSystemMgr->getInitFileName(), fileContents
    
    textMessage='Done.'
    title='Configuration file updated.'
    info=dialog_Message(textMessage, title=title, /info)
  endif else begin
    textMessage='No actions taken.'
    title='Find external applications'
    info=dialog_Message(textMessage, title=title, /info)
  endelse
END

PRO FMApplication::setExecutableLocationKey, keyName, value

  if keyName eq 'GOOGLEEARTH_LOCATION' then self.googleEarthLocation= value
  
  if keyName eq 'WORKSHEET_LOCATION' then self.workSheetLocation= value
  
  if keyName eq 'NOTEPAD_LOCATION' then self.notepadLocation= value
  
  if keyName eq 'DOCUMENTSREADER_LOCATION' then self.docReaderLocation= value
  
  if keyName eq 'PDFREADER_LOCATION' then self.pdfReaderLocation= value
  
  if keyName eq 'BROWSER_LOCATION' then self.browserLocation= value
  
END

PRO FMApplication::changeAllAvailableScenarioSelection, selection

  self.entityDisplay->setAllAvailableScenarioFlag, selection
  
END

PRO FMApplication::doTestBatch, batchFile, entFile, elabFile, wDir, resDir

  fm=self->getFileSystemMgr()
  
  testElab=fm->getElaborationTemplateFileName()
  testEnt=fm->getEntityTemplateFileName()
  testRequest=fm->getRequestTemplateFileName()
  testBatch=fm->getBatchTemplateFileName()
  
  ;reqs=strarr(1)
  reqs=strarr(2)
  postScriptRequest=fm->getRequestTemplateFileName(/POSTSCRIPT)
  rasterRequest=fm->getRequestTemplateFileName(/RASTER)
  
  ;copy all the templates in working dir
  templateFolder=fm->getTemplateDir(/WITH)
  
  fm->fileCopy, templateFolder+testElab, wDir+testElab, /OVERWRITE
  fm->fileCopy, templateFolder+testEnt, wDir+testEnt, /OVERWRITE
  fm->fileCopy, templateFolder+testRequest, wDir+testRequest, /OVERWRITE
  fm->fileCopy, templateFolder+testBatch, wDir+testBatch, /OVERWRITE
  fm->fileCopy, templateFolder+postScriptRequest, wDir+postScriptRequest, /OVERWRITE
  fm->fileCopy, templateFolder+rasterRequest, wDir+rasterRequest, /OVERWRITE
  
  reqs[0]=postScriptRequest & reqs[1]=rasterRequest
  ;reqs[0]=rasterRequest
  ;copy elabFile to templateElab
  ;rename entityFile to templateEntity
  for i=0, n_elements(reqs)-1 do begin
    print, '********start**********'
    print, '***********'+batchFile+'**************'
    print, 'elaboration: ', wDir+elabFile
    print, 'entity: ', wDir+entFile
    fm->fileCopy, wDir+elabFile, wDir+testElab, /OVERWRITE
    fm->fileCopy, wDir+entFile, wDir+testEnt, /OVERWRITE
    fm->fileCopy, wDir+testBatch, wDir+batchFile, /OVERWRITE
    fm->fileCopy, wDir+reqs[i], wDir+testRequest, /OVERWRITE
    ;self.mainView->startBenchMark, wDir+testBatch, WORKINGDIR=wDir, /BATCH
    self.mainView->startBenchMark, batchFile, WORKINGDIR=wDir, /BATCH, /SAMEFILENAME
    self->moveResultFile, batchFile, wDir, resDir
    print, '********end***********'
    print, '***********************'
  ;execute batch (auto output naming)
  endfor
  self->setLogMode, 1
  
END
;  self.lastUserType=self.mainConfig->getUserType()
;  self->restoreUserType

PRO FMApplication::moveResultFile, fileName, sourceDir, destinationDir

  fm=self->getFileSystemMgr()
  
  bFileName=fm->getBaseFileName(fileName, PRESERVE_PATH=PRESERVE_PATH, PRESERVE_EXTENSION=PRESERVE_EXTENSION)
  resFiles=file_search(sourceDir+bFileName+'*')
  elabFilesExt=['.ent', '.elb', '.rqs', '.btc']
  for i=0, n_elements(resFiles)-1 do begin
    ext=fm->getExtension(resFiles[i])
    idx=where(ext eq elabFilesExt, count)
    if count eq 0 then begin
      bFileName=fm->getBaseFileName(resFiles[i], PRESERVE_PATH=PRESERVE_PATH, /PRESERVE_EXTENSION)
      fm->fileRename, resFiles[i], destinationDir+bFileName, /OVERWRITE
    endif
  endfor
  
END

PRO FMApplication::convertObsFromCSVtoCDF

  fm=self->getFileSystemMgr()
  
  startUpFile=fm->getStartUpFileName()
  year=self.modelInfo.year
  startHOur=0
  endHour=8759
  inputDir=fm->getObservedDataDir()
  outputDir=fm->getObservedDataDir()
  prefixId=''
  modelName=''
  ;fulloutFileName=strcompress(year, /REMOVE)+'OBS_TIME.cdf'
  fulloutFileName=outputDir+path_sep()+'OBS_TIME.cdf'
  stringStartHour=strcompress(year, /REMOVE)+'0101'
  stringEndHour=strcompress(year, /REMOVE)+'1231'
  
  csv2cdf, startUpFile, $
    startHour, endHour, inputDir, outputDir, $
    prefixId, modelName, fulloutFileName, stringStartHour, stringEndHour, logWin=logWin, /PROGRESSBAR,progWIN=progWIN
  textMessage=[fulloutFileName+' created', 'please move csv files away']
  titel='Conversion done'
  a=self->dialogMessage(textMessage, title=title, /INFORMATION)
  
END

FUNCTION FMApplication::getMagicElaborationList

  fm=self->getFileSystemMgr()
  return, fm->getMagicElaborationList()
  
END

FUNCTION FMApplication::getMagicEntityList

  fm=self->getFileSystemMgr()
  return, fm->getMagicEntityList()
  
END

PRO FMApplication::logging, file=file, QUIET=QUIET, ON=ON, OFF=OFF

  fm=self->getFileSystemMgr()
  if keyword_set(OFF) then begin
    logMode=0
    textMessage=['***************','end logging on'+systime(), 'file: '+self.logFile,'***************']
    title='Log info'
  endif else begin
    logMode=1
    self.logFile=file
    textMessage=['***************','start logging on'+systime(), 'file: '+self.logFile,'***************']
    title='Log info'
  endelse
  if n_elements(file) ne 0 then logFile=file else logFile=self.logFile
  if file_test(logFile) eq 0 then APPEND=1 else APPEND=0
  fm->writePlainTextFile, logFile, [title,textMessage], APPEND=APPEND
  if keyword_set(QUIET) then self->setLogMode, 0
  a=self->dialogMessage(textMessage, title=title, /INFORMATION)
  self->setLogMode, logMode
  
END

PRO FMApplication::setLogMode, value

  common deltaLog, logMode
  
  self.logMode=value
  logMode=value
  
END

FUNCTION FMApplication::getLogMode, value

  return, self.logMode
  
END

PRO FMApplication::setLogFile, value

  self.logFile=value
  
END

FUNCTION FMApplication::getLogFile, value

  return, self.logFile
  
END

FUNCTION FMApplication::dialogMessage, textMessage, dialog_parent=dialog_parent, title=title, INFORMATION=INFORMATION, error=error, question=question, WARNING=WARNING, CENTER=CENTER, FORCELOG=FORCELOG

  if not(self.logMode) then return, dialog_message(textMessage, dialog_parent=dialog_parent, title=title, INFORMATION=INFORMATION, ERROR=ERROR, QUESTION=QUESTION, CENTER=CENTER)
  fm=self->getFileSystemMgr()
  ans=self->logDialog(textMessage, title=title, INFORMATION=INFORMATION, error=error, question=question, WARNING=WARNING, logText=logText)
  if file_test(self.logFile) then fm->writePlainTextFile, self.logFile, logText, /APPEND else print, logText
  return, ans
  
END

FUNCTION FMApplication::isRunning

  return, obj_valid(self.mainView)
  
END

FUNCTION FMApplication::getLittleLogoSize

  return, [101, 62]
  
END

FUNCTION FMApplication::getMediumLogoSize

  return, [170, 127]
  
END

FUNCTION FMApplication::logDialog, textMessage, title=title, INFORMATION=INFORMATION, error=error, question=question, WARNING=WARNING, logTEXT=logTExt

  logText='WARNING: ' ; The default
  ans=''
  titleLines=n_elements(title)
  if keyword_set(INFORMATION) then logText='INFORMATION: '
  ;if keyword_set(WARNING) then logText='WARNING: '
  if keyword_set(error) then logText='ERROR: '
  if keyword_set(question) then begin
    logText='QUESTION (YES): '
    ans='YES'
  endif
  if n_elements(titleLines) ne 0 then for i=0, titleLines-1 do logText=[logText, title[i]]
  logText=[logText, textMessage]
  ;for i=0, n_elements(logText)-1 do self->doLog, logText[i]
  for i=0, n_elements(logText)-1 do print, logText[i]
  return, ans
  
END

PRO FMApplication::setTestMode, value

  self.testMode=value
  
END

FUNCTION FMApplication::getTestMode

  return, self.testMode
  
END

FUNCTION FMApplication::isNewDataSet

  return, self.doDataCheck
  
END

FUNCTION FMApplication::isAutoCheckEnabled

  return, self.fileSystemMgr->isAutoCheckEnabled()
  
END

FUNCTION FMApplication::getLastStartUpFileTime

  return, self.fileSystemMgr->getStartUpFileTime()
  
  
  
END

FUNCTION FMApplication::getBenchmarkManagingEnabled

  return, self.mainConfig->getBenchmarkManagingEnabled()
  
END

FUNCTION FMApplication::IsAdvancedUser

  userType=self->getUserType()
  return, userType gt 0
  
END

FUNCTION FMApplication::IsDeveloperUser

  userType=self->getUserType()
  return, userType eq 2
  
END

FUNCTION FMApplication::IsStandardUser

  userType=self->getUserType()
  return, userType eq 0
  
END

FUNCTION FMApplication::getUserType

  return, self.mainConfig->getUserType()
  
END

FUNCTION FMApplication::getAvailableUserType

  return, self.availableUserType
  
END

FUNCTION FMApplication::getModelList

  return, self.modelList
  
END

FUNCTION FMApplication::getScenarioList

  return, self.scenarioList
  
END

FUNCTION FMApplication::getFileSystemMgr

  return, self.fileSystemMgr
  
END

FUNCTION FMApplication::getResourceDir, WITHSEPARATOR=WITHSEPARATOR

  return, self.fileSystemMgr->getResourceDir(WITHSEPARATOR=WITHSEPARATOR)
  
END

FUNCTION FMApplication::getLogDir, WITHSEPARATOR=WITHSEPARATOR

  return, self.fileSystemMgr->getLogDir(WITHSEPARATOR=WITHSEPARATOR)
  
END

PRO FMApplication::checkDataIntegrityClose, errorResult=errorResult, AUTOCHECK=AUTOCHECK

  if obj_valid(self.mainView) then begin
    self->enable
    if errorResult eq 1 then begin
      errorMessage=['Uhhmmm... You are working with an invalid data set.', 'Check detailed information in Check Integrity Tool log', 'You can ALWAYS run this tool under Help menu.']
      errorTitle='Wrong Data Set'
      a=self->dialogMessage(errorMessage, TITLE=errorTitle, /CENTER, /ERROR)
      self->closeApplication
    ; close Delta!!!
    endif
    if errorResult eq -1 and keyword_set(AUTOCHECK) then begin
      warnMessage=['No complete checks performed or error. Run Delta at your own risk.']
      warnTitle='Check Failed or skipped'
      a=self->dialogMessage(warnMessage, TITLE=warnTitle, /CENTER, /ERROR)
      self->updateVersionFile, fileTime=self.fileSystemMgr->getstartUpFileTime()
      self->enable
    ;self->checkDataIntegrity, /AUTOCHECK
    endif
    if errorResult eq 0 then begin
      infoMessage=['Test Passed']
      infoTitle='Test Passed'
      a=self->dialogMessage(infoMessage, TITLE=infoTitle, /CENTER, /INFO)
      ;if strupcase(sel) eq 'YES' then begin
      ;self.autoCheckEnabled=0b
      self->updateVersionFile, fileTime=self.fileSystemMgr->getstartUpFileTime()
      self->enable
    ;endif
    endif
  endif
  
END

PRO FMApplication::dataConversionClose, errorResult=errorResult, AUTOCHECK=AUTOCHECK

  self->enable
  
END

PRO FMApplication::checkDataIntegrity, NOVIEW=NOVIEW, AUTOCHECK=AUTOCHECK

  if not(keyword_set(NOVIEW)) then self->disable
  ;state is undefined for compatibility
  DeltaCheck_IO, state, self, NOVIEW=NOVIEW, AUTOCHECK=AUTOCHECK
  
END

PRO FMApplication::dataFormatConversionUtility, NOVIEW=NOVIEW, AUTOCHECK=AUTOCHECK

  if not(keyword_set(NOVIEW)) then self->disable
  conversion, state, self, NOVIEW=NOVIEW, AUTOCHECK=AUTOCHECK
  
  
  
  
END

PRO FMApplication::updateMenuBenchMarkInfoContents, fileName, displayName, fatherCode

  fileName=self.fileSystemMgr->getFileName(fileName)
  ;confDir=self.fileSystemMgr->getConfigurationDir(/WITH)
  self.benchMarkMenuList->addElement, fileName, displayName, fatherCode;, confDir
  self.mainView->buildBenchMarkMenuTree, /REBUILD
  
END

FUNCTION FMApplication::getGoogleEarthLocation
  return, self.googleEarthLocation
END

PRO FMApplication::testPlotQuality

  ERROR=0
  catch, error_status
  
  if error_status NE 0 THEN BEGIN
    ERROR=1
    self->setTestMode, 0
    self->setLogMode, 0
    catch, /CANCEL & close, /ALL
    if n_elements(applicationUserType) eq 1 then begin
      self->setUserType, applicationUserType
      self->setUserType, applicationUserType
      self->updateElaborationDisplay, saveElab
      self->updateEntityDisplay, saveEntity
    endif
    errMsg=self->dialogMessage(title='File(s) corrupted or missing', [['Something wrong with Magic files'], ['Repeat process using Magic button within Data selection and Analysis GUI']], /ERROR)
    return
  endif
  
  fm=self->getFileSystemMgr()
  elabMagicFile=self->getMagicElaborationList()
  entityMagicFile=self->getMagicEntityList()
  
  fInfo=file_info(elabMagicFile)
  if fInfo.read ne 1 then begin
    self->setTestMode, 0
    self->setLogMode, 0
    if n_elements(applicationUserType) eq 1 then begin
      self->setUserType, applicationUserType
      self->setUserType, applicationUserType
      self->updateElaborationDisplay, saveElab
      self->updateEntityDisplay, saveEntity
    endif
    aa=self->dialogMessage('Use Magic button on Analysis Diaolg GUI before compute this test', /WARN)
    return
  endif
  
  fInfo=file_info(entityMagicFile)
  if fInfo.read ne 1 then begin
    self->setTestMode, 0
    self->setLogMode, 0
    if n_elements(applicationUserType) eq 1 then begin
      self->setUserType, applicationUserType
      self->setUserType, applicationUserType
      self->updateElaborationDisplay, saveElab
      self->updateEntityDisplay, saveEntity
    endif
    aa=self->dialogMessage('Use Magic button on Entity Diaolg GUI before compute this test', /WARN)
    return
  endif
  
  elabList=fm->readPlainTextFile(elabMagicFile)
  entList=fm->readPlainTextFile(entityMagicFile)
  
  elabList=elabList[where(elabList ne '')]
  entList=entList[where(entList ne '')]
  
  amount=n_elements(elabList)*n_elements(entList)
  
  a=self->dialogMessage('Expected about '+strcompress(amount)+' files...', /WARN)
  self->setTestMode, 1
  self->setLogMode, 1
  
  ;wDir=self->getWorkingDir(/WITH, /ADD_DATE_FOLDER)
  wDir=self->getMagicDir(/WITH)
  resDir=self->getMagicDir(/WITH, /ADD)
  
  k=0
  ; setting for STANDARD user
  applicationUserType=self->getUserType()
  self->setUserType, 0
  saveEntity=self.entityDisplay
  saveElab=self.elaborationDisplay
  ent=obj_new('EntityDisplayInfo')
  elab=obj_new('ElaborationDisplayInfo')
  self->updateElaborationDisplay, elab
  self->updateEntityDisplay, ent
  ;; end
  
  for i=0, n_elements(entList)-1 do begin
    for j=0, n_elements(elabList)-1 do begin
      entId=(strsplit(entList[i], '_', /EXTRACT))[0]
      elabId=(strsplit(elabList[j], '_', /EXTRACT))[0]
      ;if k ge 335 then begin
      ;  print, '*****'
      ;  print, k
      ;  print, '*****'
      ;  a=0
      ;endif
      k++
      batchName=strcompress(k, /REMOVE)+'_ent_'+entId+'elab_'+elabId+fm->getBatchExtension()
      self->doTestBatch, batchName, entList[i], elabList[j], wDir, resDir
    endfor
  endfor
  ; restore setting for this user
  self->setTestMode, 0
  self->setLogMode, 0
  obj_destroy, elab
  obj_destroy, ent
  self->setUserType, applicationUserType
  self->updateElaborationDisplay, saveElab
  self->updateEntityDisplay, saveEntity
  ;end
  a=self->dialogMessage('Test done', /INFO)
  
END

FUNCTION FMApplication::getMagicDir, WITHSEPARATOR=WITHSEPARATOR, ADD_DATE_FOLDER=ADD_DATE_FOLDER;, RESULT=RESULT

  return, self.fileSystemMgr->getMagicDir(WITHSEPARATOR=WITHSEPARATOR, ADD_DATE_FOLDER=ADD_DATE_FOLDER);, RESULT=RESULT)
  
END

PRO FMApplication::setGoogleEarthLocation, value
  self.googleEarthLocation=value
END

FUNCTION FMApplication::getWorkSheetLocation

  return, self.workSheetLocation
  
END

PRO FMApplication::setWorkSheetLocation, value

  self.workSheetLocation=value
  
END

FUNCTION FMApplication::getDocReaderLocation

  return, self.docReaderLocation
  
END

PRO FMApplication::setDocReaderLocation, value

  self.docReaderLocation=value
  
END

FUNCTION FMApplication::getPdfReaderLocation

  return, self.pdfReaderLocation
  
END

PRO FMApplication::setPdfReaderLocation, value

  self.pdfReaderLocation=value
  
END

FUNCTION FMApplication::getBrowserLocation

  return, self.browserLocation
  
END

PRO FMApplication::setBrowserLocation, value

  self.browserLocation=value
  
END

FUNCTION FMApplication::getNotepadLocation

  return, self.notepadLocation
  
END

PRO FMApplication::setNotepadLocation, value

  self.notepadLocation=value
  
END
; Start Modified August 10th 2011 MM

FUNCTION FMApplication::getAllBenchMarkSaveModeNames

  return, ['Insert into menu', 'Only save file']
  
END

FUNCTION FMApplication::getAllBenchMarkSaveModeCodes

  return, ['MENU', 'FREE']
  
END

FUNCTION FMApplication::getAllGraphicTypeNames

  return, ['Raster (Image)', 'Vectorial (PostScript)']
  
END

FUNCTION FMApplication::getAllGraphicTypeCodes

  return, ['IMAGE', 'PS']
  
END

FUNCTION FMApplication::getAllSplitStyleCodes

  return, ['1', '2H', '2V', '4']
  
END

FUNCTION FMApplication::getAllSplitStyleNames

  return, ['1', '2 - Horizontal', '2 - Vertical', '4']
  
END

FUNCTION FMApplication::getAllPageModeNames

  return, ['Each graph in a single page/file', 'All graphs in the same page/file']
  
END

FUNCTION FMApplication::getAllPageModeCodes

  return, ['MP', 'SP']
  
END

FUNCTION FMApplication::getAllPrintOrientNames

  return, ['Landscape', 'Portrait']
  
END

FUNCTION FMApplication::getAllPrintOrientCodes

  return, ['LANDSCAPE', 'PORTRAIT']
  
END
;PrintOrient=LANDSCAPE

FUNCTION FMApplication::getSingleSplitInfoLocation

  sInfo=self->getSingleSplitInfo()
  location=[sInfo.borderCoords[0,0],sInfo.borderCoords[1,0],sInfo.borderCoords[0,2],sInfo.borderCoords[1,2]]
  return, location
  
END

FUNCTION FMApplication::getSingleSplitInfo

  ; Phil: setting postscript dimensions (1 element)
  splitInfo=replicate(getSplitInfoStruct(), 1)
  splitInfo.name='1'
  splitInfo.index=1
  borderCoords=fltarr(2,5)
  borderCoords[*,0]=[.0, .0]
  borderCoords[*,1]=[.98, .0]
  borderCoords[*,2]=[.98, .98]
  borderCoords[*,3]=[.0, .98]
  borderCoords[*,4]=[.0, .0]
  splitInfo.borderCoords=borderCoords
  splitInfo.labelPosition=[0.55,0.45]
  return, splitInfo
  
END

FUNCTION FMApplication::getDoubleHSplitInfo

  ; Phil: setting postscript dimensions (2 element H)
  splitInfo=replicate(getSplitInfoStruct(), 2)
  splitInfo[0].name='1'
  splitInfo[0].index=1
  borderCoords=fltarr(2,5)
  borderCoords[*,0]=[.0, .0]
  borderCoords[*,1]=[.5, .0]
  borderCoords[*,2]=[.5, .98]
  borderCoords[*,3]=[.0, .98]
  borderCoords[*,4]=[.0, .0]
  splitInfo[0].borderCoords=borderCoords
  splitInfo[0].labelPosition=[0.22,0.45]
  
  splitInfo[1].name='2'
  splitInfo[1].index=2
  borderCoords=fltarr(2,5)
  borderCoords[*,0]=[.5, .0]
  borderCoords[*,1]=[.98, .0]
  borderCoords[*,2]=[.98, .98]
  borderCoords[*,3]=[.5, .98]
  borderCoords[*,4]=[.5, .0]
  splitInfo[1].borderCoords=borderCoords
  splitInfo[1].labelPosition=[0.72,0.45]
  return, splitInfo
  
END

FUNCTION FMApplication::getDoubleVSplitInfo

  ; Phil: setting postscript dimensions (2 element V)
  splitInfo=replicate(getSplitInfoStruct(), 2)
  splitInfo[0].name='1'
  splitInfo[0].index=1
  borderCoords=fltarr(2,5)
  borderCoords[*,0]=[0.05, 0.0]
  borderCoords[*,1]=[.95, 0.0]
  borderCoords[*,2]=[.95, .48]
  borderCoords[*,3]=[0.05, .48]
  borderCoords[*,4]=[0.05, 0.0]
  splitInfo[0].labelPosition=[0.45,0.22]
  splitInfo[0].borderCoords=borderCoords
  
  splitInfo[1].name='2'
  splitInfo[1].index=2
  borderCoords=fltarr(2,5)
  borderCoords[*,0]=[0.05, .50]
  borderCoords[*,1]=[.95, .50]
  borderCoords[*,2]=[.95, .90]
  borderCoords[*,3]=[0.05, .90]
  borderCoords[*,4]=[0.05, .50]
  splitInfo[1].labelPosition=[0.45,0.72]
  splitInfo[1].borderCoords=borderCoords
  return, splitInfo
  
END

FUNCTION FMApplication::getFourSplitInfo

  ; Phil: setting postscript dimensions (4 elements)
  splitInfo=replicate(getSplitInfoStruct(), 4)
  
  ;upper left
  splitInfo[0].name='1'
  splitInfo[0].index=1
  borderCoords=fltarr(2,5)
  borderCoords[*,0]=[.0, .0]
  borderCoords[*,1]=[.5, .0]
  borderCoords[*,2]=[.5, .5]
  borderCoords[*,3]=[.0, .5]
  borderCoords[*,4]=[.0, .0]
  splitInfo[0].labelPosition=[0.22,0.22]
  splitInfo[0].borderCoords=borderCoords
  
  ;upper right
  splitInfo[1].name='2'
  splitInfo[1].index=2
  borderCoords=fltarr(2,5)
  borderCoords[*,0]=[.5, .0]
  borderCoords[*,1]=[.98, .0]
  borderCoords[*,2]=[.98, .5]
  borderCoords[*,3]=[.5, .5]
  borderCoords[*,4]=[.5, .0]
  splitInfo[1].labelPosition=[0.72,0.22]
  splitInfo[1].borderCoords=borderCoords
  
  ;lower left
  splitInfo[2].name='3'
  splitInfo[2].index=3
  borderCoords=fltarr(2,5)
  borderCoords[*,0]=[.0, .5]
  borderCoords[*,1]=[.5, .5]
  borderCoords[*,2]=[.5, .98]
  borderCoords[*,3]=[.0, .98]
  borderCoords[*,4]=[.0, .5]
  splitInfo[2].labelPosition=[0.22,0.72]
  splitInfo[2].borderCoords=borderCoords
  
  ;lower right
  splitInfo[3].name='4'
  splitInfo[3].index=4
  borderCoords=fltarr(2,5)
  borderCoords[*,0]=[.5, .5]
  borderCoords[*,1]=[.98, .5]
  borderCoords[*,2]=[.98, .98]
  borderCoords[*,3]=[.5, .98]
  borderCoords[*,4]=[.5, .5]
  splitInfo[3].labelPosition=[0.72,0.72]
  splitInfo[3].borderCoords=borderCoords
  ;sInfo=ptr_new(splitInfo, /NO_COPY)
  return, splitInfo
  
END

FUNCTION  FMApplication::isRecognizible

  return, obj_valid(self.recognizeInfo)
  
END

PRO FMApplication::switchRecognize

  self.mainView->switchRecognize
  
END

FUNCTION FMApplication::getRecognizeInfo

  return, self.recognizeInfo
  
END

PRO FMApplication::setRecognizeInfo, value

  self.recognizeInfo=value
  
END

;FUNCTION FMApplication::getScaleInfo
;
;  return, self.scaleInfo
;
;END

FUNCTION FMApplication::getModelInfo

  return, self.modelInfo
  
END

;PRO FMApplication::setScaleInfo, value
;
;  self.scaleInfo=value
;
;END

PRO FMApplication::setModelInfo, value

  self.modelInfo=value
  
END

FUNCTION FMApplication::getSplashLogoImage

  image=read_bmp(self.fileSystemMgr->getSplashLogoFileName())
  dims=size(image, /DIM)
  ; trueImage=bytarr(dims[0], dims[1], 3)
  ; trueImage[*,*,0]=r & trueImage[*,*,1]=g & trueImage[*,*,2]=b
  return, image
  
END

FUNCTION FMApplication::getLogoImage, true=true

  image=read_bmp(self.fileSystemMgr->getLogoFileName(),/RGB)
  dims=size(image, /DIM)
  ; trueImage=bytarr(dims[0], dims[1], 3)
  ; trueImage[*,*,0]=r & trueImage[*,*,1]=g & trueImage[*,*,2]=b
  true=(where(dims eq 3))[0]
  return, image
  
END

FUNCTION FMApplication::getPSCharSizeFactor

  return, self.PSCharSizeFactor
  
END

FUNCTION FMApplication::setPSCharSizeFactor, value

  self.PSCharSizeFactor=value
  
END

FUNCTION FMApplication::getReleaseDate

  return, self.fileSystemMgr->getReleaseDate()
  
END

FUNCTION FMApplication::getReleaseVersion

  return, self.fileSystemMgr->getReleaseVersion()
  
  
END
PRO FMApplication::streamPrint

  print, '***********************'
  print, '**Start of<',OBJ_CLASS(self),'>**'
  if obj_valid(self.mainConfig) then self.mainConfig->streamPrint else print, '**** mainConfig:', '<NULL_OBJECT>'
  if obj_valid(self.modeList) then self.modeList->streamPrint else print, '**** modeList:', '<NULL_OBJECT>'
  if obj_valid(self.categoryList) then self.categoryList->streamPrint else print, '**** categoryList:', '<NULL_OBJECT>'
  if obj_valid(self.modelList) then self.modelList->streamPrint else print, '**** modelList:', '<NULL_OBJECT>'
  if obj_valid(self.scenarioList) then self.scenarioList->streamPrint else print, '**** scenarioList:', '<NULL_OBJECT>'
  if obj_valid(self.iDLRoutineList) then self.iDLRoutineList->streamPrint else print, '**** iDLRoutineList:', '<NULL_OBJECT>'
  ;if obj_valid(self.axisTypeList) then self.axisTypeList->streamPrint else print, '**** axisTypeList:', '<NULL_OBJECT>'
  if obj_valid(self.groupByTimeList) then self.groupByTimeList->streamPrint else print, '**** groupByTimeList:', '<NULL_OBJECT>'
  if obj_valid(self.groupByStatList) then self.groupByStatList->streamPrint else print, '**** groupByStatList:', '<NULL_OBJECT>'
  if obj_valid(self.monitoringGroupStatList) then self.monitoringGroupStatList->streamPrint else print, '**** monitoringGroupStatList:', '<NULL_OBJECT>'
  if obj_valid(self.diagramList) then self.diagramList->streamPrint else print, '**** diagramList:', '<NULL_OBJECT>'
  if obj_valid(self.seasonList) then self.seasonList->streamPrint else print, '**** seasonList:', '<NULL_OBJECT>'
  if obj_valid(self.dayPeriodList) then self.dayPeriodList->streamPrint else print, '**** dayPeriodList:', '<NULL_OBJECT>'
  if obj_valid(self.parameterTypeList) then self.parameterTypeList->streamPrint else print, '**** parameterTypeList:', '<NULL_OBJECT>'
  if obj_valid(self.parameterList) then self.parameterList->streamPrint else print, '**** parameterList:', '<NULL_OBJECT>'
  if obj_valid(self.observedList) then self.observedList->streamPrint else print, '**** observedList:', '<NULL_OBJECT>'
  if obj_valid(self.fileSystemMgr) then self.fileSystemMgr->streamPrint else print, '**** fileSystemMgr:', '<NULL_OBJECT>'
  if obj_valid(self.benchMarkMenuList) then self.benchMarkMenuList->streamPrint else print, '**** benchMarkMenuList:', '<NULL_OBJECT>'
  print, '***********************'
  print, '**End of:<',OBJ_CLASS(self),'>**'
  
END
; *****************************************************
; main view related methods
; *****************************************************
FUNCTION FMApplication::getMainView

  return, self.mainView
  
END

PRO FMApplication::setMainView, view

  self.mainView=view
  
  
  
  
END
;FUNCTION FMApplication::dialogMessage, textMessage, title=title, info=info, error=error, question=question, CENTER=CENTER
;
;  return, self.mainView->dialogMessage(textMessage, title=title, INFO=INFO, ERROR=ERROR, QUESTION=QUESTION, CENTER=CENTER)
;
;END

PRO FMApplication::show

  self.mainView->show
  
END

PRO FMApplication::enable

  if self->isRunning() then self.mainView->enable
  
END

PRO FMApplication::disable

  if self->isRunning() then self.mainView->disable
  
END

PRO FMApplication::display

  ;self->streamPrint
  self.mainView=obj_new('FMMainGUI', self)
  self.mainView->realize
  self.plotter=obj_new('Plotter', self.mainView)
  if self->isSuggestedDataIntegrity() then begin
    self->checkDataIntegrity, /AUTOCHECK
  endif
;self->displayModeSelectionGUI
  
END

; *****************************************************
; child views related methods
; *****************************************************
; check here if there is a new data set or a new version of software
FUNCTION FMApplication::isSuggestedDataIntegrity

  if self->isNewDataSet() then begin
    ;warningMessage=['Uhhmmm... You seem to have a new version of FairMode...', 'To prevent problems, it is strongly raccomended to run Check Integrity Tool.', 'You can ALWAYS run this tool under Help menu.']
    ;warningTitle='New FairMode Version'
    warningMessage=[['We detected that you are using a new dataset.'], $
      ['We highly recomended that you perform an integrity check before proceeding to the delta tool'], $
      ['This might require 5 minutes.'], $
      ['Note that you will always have the possibility to perform this integrity checks later on through the help menu'], $
      ['YES: Run Check'], $
      ['NO: Skip check and run delta tool']]
    warningTitle='New Dataset'
    ans=self->dialogMessage(warningMessage, TITLE=warningTitle, /CENTER, /QUESTION)
    if strupcase(ans) eq 'YES' then return, 1 else return, 0
  endif
  return, 0
  
END

PRO FMApplication::updateRecognizeData, request, result, SILENT=SILENT

  if request->getPlotDeviceName() ne 'PS' and not(keyword_set(SILENT)) then begin
    obj_destroy, self.recognizeInfo
    pInfo = result->getPlotInfo()
    originalRecInfo=pInfo->getRecognizeInfo()
    ;temp...
    if obj_valid(originalRecInfo) then rInfo = originalRecInfo->Clone(/DEEP) else rInfo=originalRecInfo
    self.recognizeInfo=rInfo
    self.mainView->updateRecognizeStatus
  ;temp...
  endif
  
END

PRO FMApplication::Recognize, coord

  print, "do RecognizeJob"
  
  rInfo=self->getRecognizeInfo()
  rEdges=rInfo->getRegionEdges()
  rNames=rInfo->getNames()
  rValues=rInfo->getValues()
  foundName=''
  foundValue=''
  for i=n_elements(rEdges)-1, 0, -1 do begin
    thisRegion=*rEdges[i]
    ;print, 'thisRegion:', thisRegion
    ;print, 'coord:', coord
    ;print, 'coord[0] ge thisRegion[0,0]', coord[0], thisRegion[0,0]
    ;print, 'coord[0] le thisRegion[3,0]', coord[0], thisRegion[3,0]
    ;print, 'coord[1] ge thisRegion[0,1]', coord[1], thisRegion[0,1]
    ;print, 'coord[1] le thisRegion[1,1]', coord[1], thisRegion[1,1]
    if (coord[0] ge thisRegion[0,0]) and $
      (coord[0] le thisRegion[3,0]) and $
      (coord[1] ge thisRegion[0,1]) and $
      (coord[1] le thisRegion[1,1]) then begin
      ;print, "found", i
      ; MM fall 2014
      foundName=foundName+rNames[i]+'**'
      foundValue=foundValue+rValues[i]+'**'
    ;break
    endif
  endfor
  foundName=strmid(foundName, 0, strlen(foundName)-2)
  foundValue=strmid(foundValue, 0, strlen(foundValue)-2)
  self.mainView->updateRecognizer, foundName, foundValue
  
END

FUNCTION FMApplication::entityDisplayIsValid

  return, self.executeOk[0]
  
END

FUNCTION FMApplication::elaborationDisplayIsValid

  return, self.executeOk[1]
  
END

PRO FMApplication::doRecognize, x, y

  ; if (self.executeOk[0] eq 1 and self.executeOk[1] eq 1) then begin
  destCoord=convert_coord(x, y, /device, /TO_NORMAL)
  self->recognize, destCoord
;endif
  
END

;FUNCTION FMApplication::checkMultiple, multipleUserChoices=multipleUserChoices
;
;  ;fixed sequence: npar, nmod, nsce, nobs
;
;  ;;
;  maxMultipleSelection=self.elaborationDisplay->getCurrentDiagramMaxMultipleChoiceNumber()
;  specificMultipleSelection=self.elaborationDisplay->getCurrentElabMultipleChoiceFlags()
;  ;  print, '************'
;  ;  print, 'getCurrentDiagramMaxMultipleChoiceNumber->', maxMultipleSelection
;  ;  print, 'ElabMultipleChoiceFlags->', specificMultipleSelection
;  ;  print, '************'
;
;  parCodes=self.entityDisplay->getParametersSelectedCodes()
;  parNames=self.parameterList->getNamesByCodes(parCodes)
;  elabName=self.elaborationDisplay->getSelectedElabName()
;  diagramName=self.elaborationDisplay->getSelectedDiagramName()
;  if self.entityDisplay->isSingleObsSelected() then begin
;    singleObsList=self.entityDisplay->buildSinglesObsNames()
;  endif
;  if self.entityDisplay->isGroupObsSelected() then begin
;    groupObsList=self.entityDisplay->getObservedGroupTitles()
;  endif
;  if self.entityDisplay->isRunPresent() then begin
;    modsList=self.entityDisplay->buildModelNames()
;    scenList=self.entityDisplay->buildScenarioNames()
;  ;runCodes=self.entityDisplay->getRunQueryCodes()
;  ;runFileNames=runList->getFileNamesByCodes(runCodes)
;  endif
;  ; standard sequence!
;  ; parameters, models, scenarios, observations
;  multipleUserChoices=[n_elements(parCodes), n_elements(modsList), n_elements(scenList), n_elements(groupObsList) > n_elements(singleObsList)]
;  mChoiceIndexes=where(multipleUserChoices gt 1, mChoiceNumber, complement=sChoiceIndexes, ncomplement=sChoiceNumber)
;  multipleUserChoices[*]=0
;  ; no multiple choices -> no check
;  if mChoiceNumber ne 0 then begin
;    if mChoiceNumber gt maxMultipleSelection then begin
;      aa=self->dialogMessage(['K_TooManyMultipleChoices.', 'You may select up to *'+strcompress(maxMultipleSelection, /REMOVE)+'*.', 'Diagram: '+'*'+diagramName+'*.'], title=['Execute'])
;      return, 0
;    endif
;    multipleUserChoices[mChoiceIndexes]=1
;    if sChoiceNumber gt 0 then multipleUserChoices[sChoiceIndexes]=0
;    elabMultipleInfo=self.elaborationDisplay->getCurrentElabMultipleChoiceFlags()
;    elabMultipleInfoNum=self.elaborationDisplay->getCurrentElabMultipleChoiceFlagsNumber()
;    elabMultipleInfo=*elabMultipleInfo
;    i=0
;    found=0
;    while (not(found)) and (i ne elabMultipleInfoNum) do begin
;      testMultiple=reform(elabMultipleInfo[i, *])
;      i++
;      if multipleUserChoices[0] ne testMultiple[0] then continue
;      if multipleUserChoices[1] ne testMultiple[1] then continue
;      if multipleUserChoices[2] ne testMultiple[2] then continue
;      if multipleUserChoices[3] ne testMultiple[3] then continue
;      found=1
;    endwhile
;    if found eq 0 then begin
;      stdMsg=['K_MultipleChoiceNotAllowed.', 'Statistic name: '+'*'+elabName+'*.', 'Check multiple info box inside -Analysis- window.']
;      aa=self->dialogMessage(stdMsg, title=['Execute'])
;      return, 0b
;    endif
;  endif
;  ;  if testMultiple[0] eq 1 then begin
;  ;    aa=self->dialogMessage(['K_MultipleChoiceNotAllowed.', 'Statistic name: '+'*'+elabName+'*.', 'Only one parameter (meteo/pollutant).'], title=['Execute'])
;  ;    return, 0b
;  ;  endif
;  ;  if testMultiple[1] eq 1 then begin
;  ;    aa=self->dialogMessage(['K_MultipleChoiceNotAllowed.', 'Statistic name: '+'*'+elabName+'*.', 'Only one model.'], title=['Execute'])
;  ;    return, 0b
;  ;  endif
;  ;  if testMultiple[2] eq 1 then begin
;  ;    aa=self->dialogMessage(['K_MultipleChoiceNotAllowed.', 'Statistic name: '+'*'+elabName+'*.', 'Only one scenario.'], title=['Execute'])
;  ;    return, 0b
;  ;  endif
;  ;  if testMultiple[3] eq 1 then begin
;  ;    aa=self->dialogMessage(['K_MultipleChoiceNotAllowed.', 'Statistic name: '+'*'+elabName+'*.', 'Only one observation.'], title=['Execute'])
;  return, 1b
;;  print, 'self.elaborationDisplay->getCurrentDiagramMaxMultipleChoiceNumber()', self.elaborationDisplay->getCurrentDiagramMaxMultipleChoiceNumber()
;;  print, 'self.elaborationDisplay->getCurrentElabMultipleChoiceFlags()', self.elaborationDisplay->getCurrentElabMultipleChoiceFlags()
;
;END

FUNCTION FMApplication::checkMultiple, entityDisplay, elaborationDisplay, multipleUserChoices=multipleUserChoices

  ;fixed sequence: npar, nmod, nsce, nobs
  maxMultipleSelection=elaborationDisplay->getCurrentDiagramMaxMultipleChoiceNumber()
  specificMultipleSelection=elaborationDisplay->getCurrentElabMultipleChoiceFlags()
  ;  print, '************'
  ;  print, 'getCurrentDiagramMaxMultipleChoiceNumber->', maxMultipleSelection
  ;  print, 'ElabMultipleChoiceFlags->', specificMultipleSelection
  ;  print, '************'
  
  parCodes=entityDisplay->getParametersSelectedCodes()
  parNames=self.parameterList->getNamesByCodes(parCodes)
  elabName=elaborationDisplay->getSelectedElabName()
  diagramName=elaborationDisplay->getSelectedDiagramName()
  elabList=self.mainConfig->getElaborationList()
  elabCode=elaborationDisplay->getSelectedElabCode()
  elabIsObsSingleAllowed=elabList->getIsSingleObsAllowed(elabCode)
  elabIsObsGroupAllowed=elabList->getIsGroupObsAllowed(elabCode)
  
  if entityDisplay->isSingleObsSelected() and not(elabIsObsSingleAllowed) then begin
    aa=self->dialogMessage(['K_NotAllowedCombination.', 'You cannot select -single- observation(s) with selected elaboration'], title=['Execute'])
    return, 0
  endif
  if entityDisplay->isSingleObsSelected() then begin
    singleObsList=entityDisplay->buildSinglesObsNames()
  endif
  
  if entityDisplay->isGroupObsSelected() and not(elabIsObsGroupAllowed) then begin
    aa=self->dialogMessage(['K_NotAllowedCombination.', 'You cannot select -group- observation(s) with selected elaboration'], title=['Execute'])
    return, 0
  endif
  if entityDisplay->isGroupObsSelected() then begin
    groupObsList=self.entityDisplay->getObservedGroupTitles()
  endif
  
  if entityDisplay->isRunPresent() then begin
    modsList=entityDisplay->buildModelNames()
    scenList=entityDisplay->buildScenarioNames()
  ;runCodes=entityDisplay->getRunQueryCodes()
  ;runFileNames=runList->getFileNamesByCodes(runCodes)
  endif
  ; standard sequence!
  ; parameters, models, scenarios, observations
  obsTotal=n_elements(groupObsList) + n_elements(singleObsList)
  print, 'total:', obsTotal
  print, 'is multiple:', obsTotal gt 1
  multipleUserChoices=[n_elements(parCodes), n_elements(modsList), n_elements(scenList), obsTotal]
  mChoiceIndexes=where(multipleUserChoices gt 1, mChoiceNumber, complement=sChoiceIndexes, ncomplement=sChoiceNumber)
  multipleUserChoices[*]=0
  ; no multiple choices -> no check
  ; here to change check group selection
  ;  if n_elements(groupObsList) gt 1 and n_elements(mChoiceIndexes) gt 1 then begin
  ;      aa=self->dialogMessage(['K_UnallowedMultipleChoices.', 'When you select more than 1(one) group no others selections could be multiple'], title=['Execute'])
  ;      return, 0
  ;  endif
  if mChoiceNumber ne 0 then begin
    if mChoiceNumber gt maxMultipleSelection then begin
      aa=self->dialogMessage(['K_TooManyMultipleChoices.', 'You may select up to *'+strcompress(maxMultipleSelection, /REMOVE)+'*.', 'Diagram: '+'*'+diagramName+'*.'], title=['Execute'])
      return, 0
    endif
    multipleUserChoices[mChoiceIndexes]=1
    if sChoiceNumber gt 0 then multipleUserChoices[sChoiceIndexes]=0
    elabMultipleInfo=elaborationDisplay->getCurrentElabMultipleChoiceFlags()
    elabMultipleInfoNum=elaborationDisplay->getCurrentElabMultipleChoiceFlagsNumber()
    elabMultipleInfo=*elabMultipleInfo
    i=0
    found=0
    while (not(found)) and (i ne elabMultipleInfoNum) do begin
      testMultiple=reform(elabMultipleInfo[i, *])
      i++
      if multipleUserChoices[0] ne testMultiple[0] then continue
      if multipleUserChoices[1] ne testMultiple[1] then continue
      if multipleUserChoices[2] ne testMultiple[2] then continue
      if multipleUserChoices[3] ne testMultiple[3] then continue
      found=1
    endwhile
    if found eq 0 then begin
      stdMsg=['K_MultipleChoiceNotAllowed.', 'Statistic name: '+'*'+elabName+'*.', 'Check multiple info box inside -Analysis- window.']
      aa=self->dialogMessage(stdMsg, title=['Execute'])
      return, 0b
    endif
  endif
  ;  if testMultiple[0] eq 1 then begin
  ;    aa=self->dialogMessage(['K_MultipleChoiceNotAllowed.', 'Statistic name: '+'*'+elabName+'*.', 'Only one parameter (meteo/pollutant).'], title=['Execute'])
  ;    return, 0b
  ;  endif
  ;  if testMultiple[1] eq 1 then begin
  ;    aa=self->dialogMessage(['K_MultipleChoiceNotAllowed.', 'Statistic name: '+'*'+elabName+'*.', 'Only one model.'], title=['Execute'])
  ;    return, 0b
  ;  endif
  ;  if testMultiple[2] eq 1 then begin
  ;    aa=self->dialogMessage(['K_MultipleChoiceNotAllowed.', 'Statistic name: '+'*'+elabName+'*.', 'Only one scenario.'], title=['Execute'])
  ;    return, 0b
  ;  endif
  ;  if testMultiple[3] eq 1 then begin
  ;    aa=self->dialogMessage(['K_MultipleChoiceNotAllowed.', 'Statistic name: '+'*'+elabName+'*.', 'Only one observation.'], title=['Execute'])
  return, 1b
;  print, 'elaborationDisplay->getCurrentDiagramMaxMultipleChoiceNumber()', elaborationDisplay->getCurrentDiagramMaxMultipleChoiceNumber()
;  print, 'elaborationDisplay->getCurrentElabMultipleChoiceFlags()', elaborationDisplay->getCurrentElabMultipleChoiceFlags()
  
END

FUNCTION FMApplication::getBenchMarkMenuInfo

  return, self.benchMarkMenuList
  
END

FUNCTION FMApplication::buildGroupByTimeStamp, elaborationDisplay, index

  gtSel=elaborationDisplay->getGroupByTimeSelection()
  gBTValue=self.groupByTimeList->getValueByIndex(gtSel)
  gBTTimeStamp=self.groupByTimeList->getTimeStampByIndex(gtSel)
  info=getTimeStampStruct()
  info.value=gBTValue
  info.template=gBTTimeStamp
  return, info
  
END

PRO FMApplication::saveRequest


END

PRO FMApplication::doCompositeBatch, splitBatchInfo

  print, "********************"
  print, "doCompositeBatch data:"
  print, splitBatchInfo.splitStyle
  print, splitBatchInfo.graphicType
  for i=0, n_elements(splitBatchInfo.requests)-1 do begin
    print, 'request #', i+1
    print, splitBatchInfo.requests[i].entity
    print, splitBatchInfo.requests[i].elaboration
    print, splitBatchInfo.requests[i].location
  endfor
  print, "********************"
  batchInfo=obj_new("BatchInfo")
  batchInfo->saveData, fileName, path, splitBatchInfo
  
END

FUNCTION FMApplication::buildCurrentSplitBatchInfo

  splitBatchInfo=getSplitBatchStruct(1)
  splitBatchInfo.requests[0].entity=self->getEntityDisplay()
  splitBatchInfo.requests[0].elaboration=self->getElaborationDisplay()
  splitBatchInfo.requests[0].location=[0.,0.,.98,.98]
  splitBatchInfo.printOrient=(self->getAllPrintOrientCodes())[0]
  splitBatchInfo.pageMode=(self->getAllPageModeCodes())[0]
  splitBatchInfo.splitStyle=(self->getAllSplitStyleCodes())[0]
  ;splitBatchInfo.graphicType=(self->getAllGraphicTypeCodes())[0]
  splitBatchInfo.graphicType=!D.name
  ;(self->getAllBenchMarkSaveModeCodes())[1]
  return, splitBatchInfo
  
END

;PRO FMApplication::saveCurrentAsBatch, fileName, path
;
;  batchInfo=obj_new("BatchInfo")
;  batchInfo->saveData, fileName, path, splitBatchInfo
;
;END

FUNCTION FMApplication::saveBatch, fileName, path, splitBatchInfo, GUI=GUI

  ;updateBenchmarkTreeMenu
  ;self.benchMarkMenuList->writeDataFromFile, confDir+fileName
  ;self.benchMarkMenuList->fillDataFromFile, confDir+fileName
  ;self.mainView->updateBenchmarkTreeMenu, self.benchMarkMenuList
  if keyword_set(GUI) then begin
    return, self.mainView->saveBatch(splitBatchInfo)
  endif else begin
    batchInfo=obj_new("BatchInfo")
    batchInfo->saveData, fileName, path, splitBatchInfo
    return, fileName
  endelse
  
END

;PRO FMApplication::saveBatch, fileName, path, splitBatchInfo
;
;  batchInfo=obj_new("BatchInfo")
;  batchInfo->saveData, batchFileName, path, splitBatchInfo
;
;END

;PRO FMApplication::doBenchMark, benchmark
;
;  batchFileNamesSequence=benchMark->getBatchFileNames()
;  batchDir=self.fileSystemMgr->getSaveDir(/WITH)
;  for i=0, n_elements(batchFileNamesSequence)-1 do begin
;    self->restoreBatch, batchDir+batchFileNamesSequence[i]
;    self->saveImage, benchmark->getOutputFileNameDestination()+'_'+strcompress(i+1, /REMOVE), benchmark->getOutputFormat(), benchmark->getOutputFormatType()
;  endfor
;
;END

;PRO FMApplication::saveImage, fileName, format, formatType, index
;
;  imageToSave=self.mainView->getImageToSave()
;  writeFileName=self.fileSystemMgr->getSaveDir(/WITH)+fileName
;  if format eq 'IMAGE' then begin
;    writeFileName=writeFileName+'.'+formatType
;    if formatType eq 'TIFF' then imageToSave=reverse(imageToSave,2)
;    write_image, writeFileName, formatType, imageToSave
;    print, 'Image saved, filename:<', writeFileName, '>', 'type:', formatType
;    msg=self.mainView->dialogMessage(['Image saved in:', '<'+writeFileName+'> file.', 'format = '+formatType], title=['Image'], /INFORMATION)
;    return
;  endif
;  print, 'Format not supported'
;
;END

;PRO FMApplication::restoreBenchMark, fileName
;
;  benchMarkDir=self.fileSystemMgr->getSaveDir(/WITH)
;  fileName=benchMarkDir+fileName
;  benchMark=obj_new('BenchmarkInfo')
;  benchMark->restoreData, fileName
;  self->doBenchMark, benchMark
;  obj_destroy, benchMark
;
;END

FUNCTION FMApplication::buildParametersObsRelationship, parCodes, obsCodes, obsPars

  obsNo=n_elements(obsCodes)
  parNo=n_elements(parCodes)
  parObsRelationsShip=strarr(parNo)
  parObs=ptrarr(parNo)
  
  for i=0, obsNo-1 do begin
    testObsPars=*obsPars[i]
    for j=0, parNo-1 do begin
      testPar=parCodes[j]
      idx=where(testPar eq testObsPars, count)
      if count eq 1 then parObsRelationsShip[j]=parObsRelationsShip[j]+obsCodes[i]+'*'
    endfor
  endfor
  for i=0, parNo-1 do begin
    obsCodes=strsplit(parObsRelationsShip[i], '*', /EXTRACT)
    parObs[i]=ptr_new(obsCodes, /NO_COPY)
  endfor
  return, parObs
  
END

FUNCTION FMApplication::buildStationsStructs

  singles=self.entityDisplay->getObservedCodesSelections()
  groups=self.entityDisplay->getSelectedGroupObsNames()
  codesOfGroups=self.entityDisplay->getObservedCodesGroupSelections()
  
;KeesC 17FEB2015  
  statsInfo={code:'', name:'', abbr:'', group:'', single:'', altitude:0, longitude:0., latitude:0., $
    gmtLag: '', region:'', stationType:'', areaType: '', siting: ''}
  singleElements=n_elements(singles)
  groupElements=n_elements(groups)
  bigStructs=replicate(statsInfo, 100)
  obsCat=self.mainConfig->getObservedCategoryList()
  allCodes=self.observedList->getCodes()
  k=0
  
  for i=0, singleElements-1 do begin
    idx=where(allCodes eq singles[i])
    if idx[0] eq -1 then break
    thisObs=self.observedList->getRecord(idx[0])
    bigStructs[k].code=thisObs.code
    bigStructs[k].name=thisObs.displayName
;KeesC 17FEB2015 
    bigStructs[k].abbr=thisObs.shortname
    bigStructs[k].group='N/A'
    bigStructs[k].single='X'
    bigStructs[k].altitude=thisObs.heightAboveSea
    bigStructs[k].longitude=thisObs.xGeoLocation
    bigStructs[k].latitude=thisObs.yGeoLocation
    bigStructs[k].gmtLag=thisObs.countryGMT
    values=obsCat->getValuesByObserved(thisObs.code)
    bigStructs[k].region=values[0]
    bigStructs[k].stationType=values[1]
    bigStructs[k].areaType=values[2]
    bigStructs[k].siting=values[3]
    k++
  endfor
  
  for i=0, groupElements-1 do begin
    group=groups[i]
    codes=*(codesOfGroups[i])
    for j=0, n_elements(codes)-1 do begin
      idx=where(allCodes eq codes[j])
      thisObs=self.observedList->getRecord(idx[0])
      bigStructs[k].code=thisObs.code
      bigStructs[k].name=thisObs.displayName
;KeesC 17FEB2015
      bigStructs[k].abbr=thisObs.shortname      
      bigStructs[k].group=group
      bigStructs[k].single='N/A'
      bigStructs[k].altitude=thisObs.heightAboveSea
      bigStructs[k].longitude=thisObs.xGeoLocation
      bigStructs[k].latitude=thisObs.yGeoLocation
      bigStructs[k].gmtLag=thisObs.countryGMT
      values=obsCat->getValuesByObserved(thisObs.code)
      bigStructs[k].region=values[0]
      bigStructs[k].stationType=values[1]
      bigStructs[k].areaType=values[2]
      bigStructs[k].siting=values[3]
      k++
    endfor
  endfor
  
  return, bigStructs[0:k-1]
  
;colLabels=['Station Code', 'Station Name', 'Group', 'Single', 'Altitude(m)', 'Lon', 'Lat', 'GMTLag', 'Region', 'Station Type', 'AreaType', 'Siting']
  
END

PRO FMApplication::changeStartMonth, index

  self.elaborationDisplay->setStartMonthSelection, index
  
END

PRO FMApplication::changeStartDay, index

  self.elaborationDisplay->setStartDaySelection, index
  
END

PRO FMApplication::changeStartHour, index

  self.elaborationDisplay->setStartHourSelection, index
  
END

PRO FMApplication::changeEndMonth, index

  self.elaborationDisplay->setEndMonthSelection, index
  
END

PRO FMApplication::changeEndDay, index

  self.elaborationDisplay->setEndDaySelection, index
  
END

PRO FMApplication::changeEndHour, index

  self.elaborationDisplay->setEndHourSelection, index
  
END

PRO FMApplication::changeAllStationsSelection, selection

  self.entityBatchDisplay->setAllStationsFlag, selection
  
END

PRO FMApplication::changeAllModelsSelection, selection

  self.entityBatchDisplay->setAllModelsFlag, selection
  
END

PRO FMApplication::changeAllScenariosSelection, selection

  self.entityBatchDisplay->setAllScenariosFlag, selection
  
END

PRO FMApplication::changeAllParametersSelection, selection

  self.entityBatchDisplay->setAllParametersFlag, selection
  
END

PRO FMApplication::changeObsModSelection, selection

  self.entityDisplay->setUseObservedModelFlag, selection
  
END

FUNCTION FMApplication::alreadyOpen

  ; if self.openView then return, 1 else return, 0
  return, 0
  
END

PRO FMApplication::setBlockWindowControl, OFF=OFF

  self.lastview=obj_new()
; if keyword_set(OFF) then self.openViewCheck=0 else self.openViewCheck=1
  
END

PRO FMApplication::displayModeSelectionGUI

  if obj_valid(self.lastView) then begin
    self.lastView->show
  endif else begin
    self.modeView=obj_new('FMModeSelectionGUI', self.modeDisplay->Clone(/DEEP), self)
    self.modeView->realize
    self.lastView=self.modeView
  endelse
  self->disable
;print, 'Open displayModeSelectionGUI'
  
END

PRO FMApplication::displayEntitySelectionGUI

  if obj_valid(self.lastView) then begin
    self.lastView->show
  endif else begin
    self.entityView=obj_new('FMEntitySelectionGUI', self.entityDisplay->Clone(/DEEP), self)
    ;self.entityView=obj_new('FMEntitySelectionGUI', self.entityDisplay->Clone(/DEEP), self)
    self.entityView->realize
    self.lastView=self.entityView
  endelse
  ;print, 'Open displayEntitySelectionGUI'
  self->disable
  
END

; Oct 2 2011 MM End
;PRO FMApplication::displayBatchMarkGUI
;
;  if obj_valid(self.lastView) then begin
;    self.lastView->show
;  endif else begin
;    self.elaborationView=obj_new('FMElaborationSelectionGUI', self.elaborationDisplay->Clone(/DEEP), self)
;    self.elaborationView->realize
;    self.lastView=self.elaborationView
;  endelse
;  ;print, 'Open displayElaborationSelectionGUI'
;  self->disable
;
;END

PRO FMApplication::displayElaborationSelectionGUI

  if obj_valid(self.lastView) then begin
    self.lastView->show
  endif else begin
    self.elaborationView=obj_new('FMElaborationSelectionGUI', self.elaborationDisplay->Clone(/DEEP), self)
    self.elaborationView->realize
    self.lastView=self.elaborationView
  endelse
  ;print, 'Open displayElaborationSelectionGUI'
  self->disable
  
END

PRO FMApplication::displayCompositeBatchGUI

  ; benchmark tree...
  if obj_valid(self.lastView) then begin
    self.lastView->show
  endif else begin
    ;self.benchMarkView=obj_new('FMBenchMarkCreationGUI', self.benchMarkDisplay->Clone(/DEEP), self)
    applicationUserType=self->getUserType()
    ;standard (As request by JRC-Staff)
    self->setUserType, 0
    bEntityDI=obj_new('EntityDisplayInfo')
    self->initEntityDisplay, bEntityDI, self.mainConfig, self.categoryList, self.modelList, self.scenarioList, self.observedList, self.parameterTypeList, self.parameterList, self.monitoringGroupStatList
    bElabDI=obj_new('ElaborationDisplayInfo')
    ;self->initElaborationDisplay, self.mainConfig, self.diagramList, self.axisTypeList, self.groupByStatList, self.groupByTimeList, self.seasonList, self.dayPeriodList
    self->initElaborationDisplay, bElabDI, self.mainConfig, self.diagramList, self.groupByStatList, self.groupByTimeList, self.seasonList, self.dayPeriodList
    self.compositeBatchMgr= obj_new('CompositeBatchManager', self, bEntityDI, bElabDI, applicationUserType)
    self.compositeBatchMgr->realize
  ;    self.benchMarkView=obj_new('FMBenchMarkCreationGUI', obj_new(""), self, bEntityDI, bElabDI)
  ;    self.benchMarkView->realize
  ;    self.lastView=self.benchMarkView
  endelse
  ;print, 'displayBenchMarkCreationSelectionGUI'
  self->disable
  
END

PRO FMApplication::updateEntityDisplayInfo, entityDisplayInfo

  obj_destroy, self.entityDisplay
  self.entityDisplay=entityDisplayInfo
  self.mainView->updateInfo
  self.executeOk[0]=1
;entityDisplayInfo->printStream
  
END

PRO FMApplication::updateElaborationDisplayInfo, elaborationDisplayInfo

  obj_destroy, self.elaborationDisplay
  self.elaborationDisplay=elaborationDisplayInfo
  self.executeOk[1]=1
  self.mainView->updateInfo
  
END

PRO FMApplication::updateModeInfo, modeDisplay

  obj_destroy, self.modeDisplay
  self.modeDisplay=modeDisplay
  newMode=self.modeDisplay->getSelection()
  ;Batch selection
  case newMode of
    0 : self->updateMode,newMode
  else : self->startBenchMarkingMode
endcase
self.mainView->show
; self.view->UpdateModeInfoSection, modeDisplay

END
; *****************************************************
; core methods
; *****************************************************
PRO FMApplication::upDateMode, mode

  elabList=self.mainConfig->getElaborationList()
  elabList->setMode, mode
  ;self->initElaborationDisplay, self.mainConfig, self.diagramList, self.axisTypeList, self.groupByStatList, self.groupByTimeList, self.seasonList, self.dayPeriodList
  self->initElaborationDisplay, self.elaborationDisplay, self.mainConfig, self.diagramList, self.groupByStatList, self.groupByTimeList, self.seasonList, self.dayPeriodList
  
END

;FUNCTION FMApplication::isDayPeriodSelected
;
;  return, self.elaborationDisplay->isDayPeriodSelected()
;
;END

;FUNCTION FMApplication::isSeasonSelected
;
;  return, self.elaborationDisplay->isSeasonSelected()
;
;END

;FUNCTION FMApplication::isGroupByStatSelected
;
;  return, self.elaborationDisplay->isGroupByStatSelected()
;
;END

;FUNCTION FMApplication::isGroupByTimeSelected
;
;  return, self.elaborationDisplay->isGroupByTimeSelected()
;
;END

;FUNCTION FMApplication::IsSingleObsSelected
;
;  return, self.entityDisplay->IsSingleObsSelected()
;
;END

;FUNCTION FMApplication::IsGroupObsSelected
;
;  return, self.entityDisplay->IsGroupObsSelected()
;
;END

;FUNCTION FMApplication::isRunPresent
;
;  return, self.entityDisplay->isRunPresent()
;
;END

FUNCTION FMApplication::checkRequest, entityDisplayInfo, elaborationDisplayInfo, multipleUserChoices=multipleUserChoices, NODISPLAYCHECK=NODISPLAYCHECK

  if total(self.executeOk) ne 2 and not(keyword_set(NODISPLAYCHECK)) then begin
    aa=self->dialogMessage(['Fill -Data selection- and/or -Analysis- windows before execute!'], title=['Execute'])
    return, 0
  endif
  if n_elements(entityDisplayInfo) eq 0 then checkEntity=self.entityDisplay else checkEntity=entityDisplayInfo
  if n_elements(elaborationDisplayInfo) eq 0 then checkElaboration=self.elaborationDisplay else checkElaboration=elaborationDisplayInfo
  if checkEntity->checkIntegrity(self.mainView) ne 1 then return, 0
  if checkElaboration->checkIntegrity(self.mainView, /NODISPLAYCHECK) ne 1 then return, 0
  if not(self->checkMultiple(checkEntity, checkElaboration, multipleUserChoices=multipleUserChoices)) then return, 0b
  ;mChoiceI=self->getMultipleChoiceInfo()
  return, 1b
  
END

;FUNCTION FMApplication::buildGroupObsShortNames
;
;  obsList=self.observedList->getShortNames()
;  groups=self.entityDisplay->getObservedCodesGroupSelections()
;
;  groupNo=n_elements(groups)
;  groupNames=ptrarr(groupNo)
;  if size(groups, /TYPE) eq 2 then return, [-1]
;  for i=0, groupNo-1 do begin
;    idxs=*groups[i]
;    groupNames[i]=ptr_new(obsList[idxs], /NO_COPY)
;  endfor
;
;  return, groupNames
;
;END

;FUNCTION FMApplication::buildModelNames
;
;  return, self.entityDisplay->getSelectedModelCodes()
;
;END
;
;FUNCTION FMApplication::buildScenarioNames
;
;  return, self.entityDisplay->getSelectedScenarioCodes()
;
;END

;FUNCTION FMApplication::buildGroupsObsNames
;
;  codes=self.entityDisplay->getObservedGroupCodesSelections()
;  ;obsList=self.observedList->getDisplayNames()
;  nameList=self.entityDisplay->getObservedNamesByCodes(codes)
;  ;singleList=obsList[idxs]
;  return, nameList
;
;END
;
;FUNCTION FMApplication::buildSinglesObsNames
;
;  codes=self.entityDisplay->getObservedCodesSelections()
;  ;obsList=self.observedList->getDisplayNames()
;  nameList=self.entityDisplay->getObservedNamesByCodes(codes)
;  ;singleList=obsList[idxs]
;  return, nameList
;
;END
;
;FUNCTION FMApplication::buildSinglesObsCodes
;
;  codes=self.entityDisplay->getObservedCodesSelections()
;  return, codes
;
;END
;; march 15 MM
;FUNCTION FMApplication::buildSinglesObsGMTs
;
;  codes=self.entityDisplay->getObservedCodesSelections()
;  ;obsList=self.observedList->getDisplayNames()
;  nameList=self.entityDisplay->getObservedGMTsByCodes(codes)
;  ;singleList=obsList[idxs]
;  return, nameList
;
;END
;
;FUNCTION FMApplication::buildSinglesObsCountries
;
;  codes=self.entityDisplay->getObservedCodesSelections()
;  ;obsList=self.observedList->getDisplayNames()
;  nameList=self.entityDisplay->getObservedCountriesByCodes(codes)
;  ;singleList=obsList[idxs]
;  return, nameList
;
;END
;
;FUNCTION FMApplication::buildSinglesObsLongitudes
;
;  codes=self.entityDisplay->getObservedCodesSelections()
;  ;obsList=self.observedList->getDisplayNames()
;  nameList=self.entityDisplay->getObservedLongitudesByCodes(codes)
;  ;singleList=obsList[idxs]
;  return, nameList
;
;END
;
;FUNCTION FMApplication::buildSinglesObsLatitudes
;
;  codes=self.entityDisplay->getObservedCodesSelections()
;  ;obsList=self.observedList->getDisplayNames()
;  nameList=self.entityDisplay->getObservedLatitudesByCodes(codes)
;  ;singleList=obsList[idxs]
;  return, nameList
;
;END
;
;FUNCTION FMApplication::buildSinglesObsAltitudes
;
;  codes=self.entityDisplay->getObservedCodesSelections()
;  ;obsList=self.observedList->getDisplayNames()
;  nameList=self.entityDisplay->getObservedAltitudesByCodes(codes)
;  ;singleList=obsList[idxs]
;  return, nameList
;
;END
;
;FUNCTION FMApplication::buildSinglesObsShortNames
;
;  codes=self.entityDisplay->getObservedCodesSelections()
;  ;obsList=self.observedList->getDisplayNames()
;  nameList=self.entityDisplay->getObservedShortNamesByCodes(codes)
;  ;singleList=obsList[idxs]
;  return, nameList
;
;END

FUNCTION FMApplication::buildRequest, multipleUserChoices, location, entityDisplay, elaborationDisplay

  ; convert gui data in a 'Request' object
  ; save on a Request object data needed to perform a job.
  ; then "DoElaboration" with the request as parameter.
  ;if n_elements(location)
  if n_elements(location) eq 0 then location=[0.,0.,1.,1.]
  if n_elements(entityDisplay) eq 0 then entityDisplay=self.entityDisplay
  if n_elements(elaborationDisplay) eq 0 then elaborationDisplay=self.elaborationDisplay
  request=obj_new('Request', self.mainConfig->getGoalsCriteriaOCList(), self.mainConfig->GetObservedCategoryList())
  dtu=obj_new('DateTimeUtility')
  parCodes=entityDisplay->getParametersSelectedCodes()
  parNames=self.parameterList->getNamesByCodes(parCodes)
  parMeasureUnits=self.parameterList->getMeasureUnitsByCodes(parCodes)
  elabList=self.mainConfig->getElaborationList()
  elabCode=elaborationDisplay->getSelectedElabCode()
  elabName=elaborationDisplay->getSelectedElabName()
  ; MM summer 2012 start
  elabOCUse=elabList->getGoalsCriteriaOCFlagByCode(elabCode)
  elabExtraInfo1=elabList->getExtraInfosByCode(elabCode, index=0)
  elabExtraInfo2=elabList->getExtraInfosByCode(elabCode, index=1)
  ; MM summer 2012 start
  routineCode=elabList->getIDLRoutineByCode(elabCode)
  diagramCode=elaborationDisplay->getSelectedDiagramCode()
  diagramName=elaborationDisplay->getSelectedDiagramName()
  elabRoutineName=self.IDLRoutineList->getRoutineNameByCode(routineCode)
  runList=self.mainConfig->getRunList()
  if entityDisplay->isSingleObsSelected() then begin
    singlesObsShortNames=entityDisplay->buildSinglesObsShortNames()
    singlesObsNames=entityDisplay->buildSinglesObsNames()
    singlesObsCodes=entityDisplay->buildSinglesObsCodes()
    singlesObsAltitudes=entityDisplay->buildSinglesObsAltitudes()
    singlesObsLatitudes=entityDisplay->buildSinglesObsLatitudes()
    singlesObsLongitudes=entityDisplay->buildSinglesObsLongitudes()
    singlesObsCountries=entityDisplay->buildSinglesObsCountries()
    singlesObsGMTs=entityDisplay->buildSinglesObsGMTs()
    singlesObsCatInfos=entityDisplay->buildSingleObservedCatInfos()
    request->setSingleShortObsNames, singlesObsShortNames
    request->setSingleObsNames, singlesObsNames
    request->setSingleObsCodes, singlesObsCodes
    request->setSingleObsAltitudes, singlesObsAltitudes
    request->setSingleObsLatitudes, singlesObsLatitudes
    request->setSingleObsLongitudes, singlesObsLongitudes
    request->setSingleObsCountries, singlesObsCountries
    request->setSingleObsGMTs, singlesObsGMTs
    request->setSingleObsCatInfos, singlesObsCatInfos
  endif
  if entityDisplay->isGroupObsSelected() then begin
    groupStatCode=entityDisplay->getObservedGroupStatCodeSelection()
    groupStatName=(entityDisplay->getObservedGroupStatNames())[groupStatCode]
    groupCodesSelections=entityDisplay->getObservedCodesGroupSelections()
    ;groupNamesSelections=self->buildGroupObsNames()
    groupNamesSelections=entityDisplay->buildGroupObsNames()
    groupTitles=entityDisplay->getObservedGroupTitles()
    request->setGroupTitles, groupTitles
    request->setGroupCodes, groupCodesSelections
    request->setGroupNames, groupNamesSelections
    request->setGroupStatToApplyCode, groupStatCode
    request->setGroupStatToApplyName, groupStatName
  endif
  ;self.info->buildRunList(scens, mods, RUNINDEXES=RUNINDEXES)
  if entityDisplay->isRunPresent() then begin
    modsList=entityDisplay->buildModelNames()
    scenList=entityDisplay->buildScenarioNames()
    runCodes=entityDisplay->getRunQueryCodes()
    runFileNames=runList->getFileNamesByCodes(runCodes)
  endif
  dayCode=elaborationDisplay->getSelectedDayPeriodCode()
  seasonCode=elaborationDisplay->getSelectedSeasonCode()
  dayPeriodInfo=self.dayPeriodList->getInfoByCode(dayCode)
  seasonInfo=self.seasonList->getInfoByCode(seasonCode)
  thresholdFlag=elaborationDisplay->getThresholdFlag()
  
  modCopy=modsList
  scenCopy=scenList
  request->setMultipleChoiceUserSelectionFlags, multipleUserChoices
  request->setModelCodes, modsList
  request->setModelNames, modCopy
  request->setScenarioCodes, scenList
  request->setScenarioNames, scenCopy
  request->setRunCodes, entityDisplay->getSelectedRunCodes()
  request->setRunNames, entityDisplay->getSelectedRunNames()
  request->setRunFileNames, runFileNames
  request->setRunResultType, 'TIME' ; or '2D'; read type
  request->setUseObservedModel, entityDisplay->getUseObservedModelFlag()
  request->setElaborationCode, elabCode
  request->setElaborationName, elabName
  request->setElaborationRoutine, elabRoutineName
  request->setElaborationOCUse, elabOCUse
  request->setElaborationOCStat, elabExtraInfo1
  request->setElaborationOCTimeAvgName, elabExtraInfo2
  request->setDiagramCode, diagramCode
  request->setDiagramName, diagramName
  plotRoutine=self.diagramList->getPlotRoutineByCode(diagramCode)
  request->setPlotRoutine, plotRoutine
  request->setParameterCodes, entityDisplay->getParametersSelectedCodes()
  request->setParameterNames, parNames
  request->setParameterMeasureUnits, parMeasureUnits
  if thresholdFlag then begin
    eV=self.elaborationDisplay->getThresholdValues()
    request->setExtraValues, eV
  endif
  if elaborationDisplay->isGroupByStatSelected() then request->setGroupByStatInfo, elaborationDisplay->getGroupByStatSelection()
  if elaborationDisplay->isGroupByTimeSelected() then request->setGroupByTimeInfo, self->buildGroupByTimeStamp(elaborationDisplay)
  if elaborationDisplay->isSeasonSelected() then begin
    request->setSeasonInfo, seasonInfo
    request->setSeasonType, elaborationDisplay->getSeasonSelection()
  endif
  if elaborationDisplay->isDayPeriodSelected() then begin
    request->setHourInfo, dayPeriodInfo
    request->setHourType, elaborationDisplay->getDayPeriodSelection()
  endif
  request->setStartDate, elaborationDisplay->getStartDateInfo(/REAL)
  request->setEndDate, elaborationDisplay->getEndDateInfo(/REAL)
  startIndex=dtu->getAbsoluteHours(request->getStartDate(), /STRUCT)
  endIndex=dtu->getAbsoluteHours(request->getEndDate(), /STRUCT)
  request->setStartIndex, startIndex
  request->setEndIndex, endIndex
  request->setStartPlotIndex, startIndex
  request->setEndPlotIndex, endIndex
  ; MM summer 2012 Start
  ;request->setScaleInfo, self->getScaleInfo()
  request->setModelInfo, self->getModelInfo()
  ; MM summer 2012 End
  ;print, plotCode, elaborationName
  request->setLocation, location
  request->setPlotDeviceName, !D.name
  ;  request->setPrintOrient, 'LANDSCAPE'
  request->setPageBreak, 'CLOSE'
  return, request
  
END

PRO FMApplication::doElaboration, request, multipleUserChoices, BATCH=BATCH, WORKINGDIR=WORKINGDIR, outFileName=outFileName

  ;request->setGoogleEarthLocation,self.fileSystemMgr->getGoogleEarthLocation()
  ;get data (fill result)
  ;  ERROR=0
  ;  catch, error_status
  ;  ;print, systime()
  ;  if error_status NE 0 THEN BEGIN
  ;    ERROR=1
  ;    catch, /CANCEL
  ;    errMsg=dialog_message(['Error occured with this Analysis/Data selection.', 'Check log file (you need to close application).','Hint: make sure that parameter is selected before stations'], /ERROR)
  ;    return
  ;  endif

  widget_control, /HOURGLASS
  startTime=systime(/SECONDS)
  self.dataMinerMgr->readAllData, request, result, screensize=self.mainView->getScreenSize(), ONLYMODEL=request->getUseObservedModel(), SILENT=BATCH
  endTime=systime(/SECONDS)
  ;print, '-->', endTime-startTime
  widget_control, HOURGLASS=0
  ;elab data (fill result, fill .plotInfo result to manage plot behaviour)
  request->setGoogleEarthLocation, self->getGoogleEarthLocation()
  call_procedure, request->getElaborationRoutine(), request, result
  
  targetInfo=result->getGenericPlotInfo()
  checkXY=targetInfo->getXYS()
  if ~keyword_set(outFileName) then outFileName=request->getFileName()
  self.plotter->setSilentMode, keyword_set(BATCH)
  self.plotter->opendevice, request->getPlotDeviceName(), outFileName, $
    request->getPrintOrient(),request->getPageBreak(), request->getLocation(), BATCH=BATCH, WORKINGDIR=WORKINGDIR
  self.plotter->wsetMainDataDraw, /PROGRESS
  self.plotter->plotAll, request, result
  self.plotter->closedevice, request->getPageBreak(), outFileName, request->getPrintOrient(), WORKINGDIR=WORKINGDIR
  request->closeDataDumpFile
  self->updateRecognizeData, request, result, SILENT=BATCH
  ;  endif
  
  obj_destroy, request
  obj_destroy, result
  
END

FUNCTION FMApplication::restoreEntity, fileName, WORKINGDIR=WORKINGDIR

  ;phil batch 18/03
  ;if n_elements(WORKINGDIR) ne 1 then benchMarkDir=self.fileSystemMgr->getSaveDir(/WITH) else benchMarkDir=WORKINGDIR
  ;fileName=benchMarkDir+fileName
  ;end phil batch
  if n_elements(WORKINGDIR) eq 1 then wDir=WORKINGDIR else wDir=self.fileSystemMgr->getSaveDir(/WITH)
  fm=self->getFileSystemMgr()
  fName=fm->getBaseFileName(fileName, EXTENSION=EXTENSION, FOLDER=FOLDER, /PRESERVE_EXT)
  if n_elements(FOLDER) eq 1 then wDir=FOLDER
  wDir=fm->cleanPath(wDir)
  fileName=wDir+path_sep()+fName
  entityD=self->getEntityDisplay()
  print, 'Data selection restored, filename:<', fileName, '>'
  ; confirm nice restore
  if entityD->restoreData(fileName) then begin
    self->updateViewAppearence, [0]
    return, 1
  endif
  return, 0
  
END

PRO FMApplication::updateViewAppearence, updateElements

  self.executeOk[updateElements]=1
  self.mainView->updateInfo
  
END

FUNCTION FMApplication::restoreElaboration, fileName, WORKINGDIR=WORKINGDIR

  if keyword_set(WORKINGDIR) then wDir=WORKINGDIR else wDir=self.fileSystemMgr->getSaveDir(/WITH)
  fm=self->getFileSystemMgr()
  fName=fm->getBaseFileName(fileName, EXTENSION=EXTENSION, FOLDER=FOLDER, /PRESERVE_EXT)
  if n_elements(FOLDER) eq 1 then wDir=FOLDER
  wDir=fm->cleanPath(wDir)
  fileName=wDir+path_sep()+fName
  elabD=self->getElaborationDisplay()
  print, 'Analysis restored, filename:<', fileName, '>'
  ; confirm nice restore
  if elabD->restoreData(fileName) then begin
    self->updateViewAppearence, [1]
    return, 1
  endif
  return, 0
  
END

;PRO FMApplication::startBenchMarkingMode
;
;  print, 'Benchmarking...'
;
;END
FUNCTION FMApplication::restoreRequest, fileName, WORKINGDIR=WORKINGDIR, BATCH=BATCH, outFileName=outFileName

  request=obj_new('Request')
  ;print, self.lastUserType
  applicationUT=self->getUserType()
  if (request->restoreData(fileName)) then begin
    ; MM summer 2012 Start
    ;request->setScaleInfo, self->getScaleInfo()
    request->setModelInfo, self->getModelInfo()
    ;MM summer 2012 End
    if ~self->restoreEntity(request->getEntityFileName(), WORKINGDIR=WORKINGDIR) then return, 0
    if ~self->restoreElaboration(request->getElaborationFileName(), WORKINGDIR=WORKINGDIR) then return, 0
    ;msg=self.mainView->dialogMessage(['Batch restored from:', '<'+fileName+'> file.'], title=['Request'], /INFORMATION)
    self->execRequest, request, WORKINGDIR=WORKINGDIR, BATCH=BATCH, outFileName=outFileName
    self->updateViewAppearence, [0,1]
  endif else begin
    msg=self->dialogMessage(['Batch restored failed from:', '<'+fileName+'> file.'], title=['Request'], /INFORMATION)
  endelse
  
END

;PRO FMApplication::restoreRequest, fileName
;
;  batch=obj_new('BatchInfo')
;  batch->restoreFromFile, fileName
;  requestInfos=batch->getRequestInfos()
;  for i=0, n_elements(requests) do begin
;    self->restoreElaboration, requestInfos->getElaborationFileName()
;    self->restoreEntity, requests[i]->getEntityFileName()
;    newRequest=self->buildRequest()
;    ;request=obj_new('Request')
;    ;requests[i]->restoreFromFile
;    newRequest->setScaleInfo, self->getScaleInfo()
;    msg=self.mainView->dialogMessage(['Request'+strcompress(i+1, /REMOVE)+ ' restored from:', '<'+requests[i]->getFileName()+'> file.'], title=['Request data'], /INFORMATION)
;    self->restoreElaboration, requests[i]->getElaborationFileName()
;    self->restoreEntity, requests[i]->getEntityFileName()
;    self->setRequest, newRequest
;    self->execRequest
;  endfor
;
;END

PRO FMApplication::execRequest, passedRequest, BATCH=BATCH, WORKINGDIR=WORKINGDIR, NODISPLAYCHECK=NODISPLAYCHECK, outFileName=outFileName

  if self->checkRequest(multipleUserChoices=multipleUserChoices, /NODISPLAYCHECK) then begin
    mUC=multipleUserChoices
    ;build
    request=self->buildRequest(mUC)
    ;dummy=request->getGoalsCriteriaValues(parameter=parCodes[0], scalename=scalename, statname=statistics, timeAvgName='ALL', NOVALUES=NOVALUES)
    ; Modified summer 2012 MM Start
    if request->getElaborationOCUse() then begin
      if Check_Criteria(request, result) eq 0 then begin
        aa=self->dialogMessage(['No criteria available. Check your selections and/or -elaboration.dat- file and/or', '-goals_criteria_oc.dat- file.'], title=['Execution not possible'])
        obj_destroy, request
        return
      endif
      print, 'GC needed...'
    ;      insert here Philippe checkCriteriaRoutine(request, result...)
    ;      dummy=request->getGoalsCriteriaValues(/CONTENTS, NOVALUES=NOVALUES)
    ;      if keyword_set(NOVALUES) then begin
    ;        aa=self->dialogMessage(['No criteria available. Check your selections and/or -elaboration.dat- file and/or', '-goals_criteria_oc.dat- file.'], title=['Execution not possible'])
    ;        print, '*******'
    ;        print, 'GC not founds'
    ;        print, '*******'
    ;        return
    ;      endif
    ;      print, '*******'
    ;      print, 'GC founds'
    ;      print, dummy
    ;      print, '*******'
    endif
    ; Modified summer 2012 MM End
    if n_elements(passedRequest) ne 0 then begin
      request->setFileName, passedRequest->getFileName()
      request->setPlotDeviceName, passedRequest->getPlotDeviceName()
      request->setLocation, passedRequest->getLocation()
      request->setPrintOrient, passedRequest->getPrintOrient()
      request->setPageBreak, passedRequest->getPageBreak()
    endif
    self->doElaboration, request, mUC, WORKINGDIR=WORKINGDIR, BATCH=BATCH, outFileName=outFileName
  endif else begin
    print, 'Bad request'
  ;aa=self.view->dialogMessage(['Check the request!'], title=['Request'], /error)
  endelse
  
END


PRO FMApplication::startBatchMode

  filter=['*'+self.fileSystemMgr->getBatchExtension()]
  fix_filter='*'+self.fileSystemMgr->getBatchExtension()
  batchFile=dialog_pickfile(DEFAULT_EXTENSION=self.fileSystemMgr->getBatchExtension(), $
    DIALOG_PARENT=self.mainView->getTopBase(), $
    FILE=self.fileSystemMgr->getLastUsedFileName(), $
    FILTER=filter, $FIX_FILTER=fix_filter, $
    GET_PATH=path, /MUST_EXIST, PATH=self.fileSystemMgr->getSaveDir(), $
    TITLE='Load a previous saved elaboration'  )
  if batchFile ne '' then begin
    self.fileSystemMgr->setLastUsedFileName, batchFile
  ;fill request
  ;launch an elaboration
  ;display result
  endif
;print, 'selected was:', batchFile
;print, 'last is:', self.fileSystemMgr->getLastUsedFileName()
  
END

PRO FMApplication::exitRequest

  exitMessage='Do you really want to quit?'
  if self->dialogMessage(exitMessage, /QUESTION) eq 'Yes' then self->closeApplication
  
END

PRO FMApplication::closeApplication

  obj_destroy, self
  
END

FUNCTION FMApplication::getApplicationName

  return, 'JRC - Fair Mode'
  
END
;****************************************************************************************
; start up methods
;****************************************************************************************
PRO FMApplication::loadUpdateStatusFileData, parameterNames=parameterNames, parameterValues=parameterValues

  self.fileSystemMgr->loadUpdateStatusFileData, parameterNames=parameterNames, parameterValues=parameterValues
  
END

PRO FMApplication::loadInitFileData, parameterNames=parameterNames, parameterValues=parameterValues, parsToBeRemoved=parsToBeRemoved

  self.fileSystemMgr->loadInitFileData, parameterNames=parameterNames, parameterValues=parameterValues, parsToBeRemoved=parsToBeRemoved
  
END

PRO FMApplication::startJournaling

  util=obj_new('FMUtility')
  journalFullFileName=self.fileSystemMgr->getLogDir(/WITH)
  journalFullFileName=journalFullFileName+util->getSysTime(/FILECOMPATIBILITY)+'.txt'
  journal, journalFullFileName
  obj_destroy, util
  
END

FUNCTION FMApplication::checkApplicationIntegrity, errorTitle, errorMessage

  if not(file_test(self.fileSystemMgr->getHomeDir(),/directory)) then begin
    errorMessage='HomeDir: ' + self.fileSystemMgr->getHomeDir()+ ' doesn''t exist. Check your installation!'
    errorTitle='Home Dir Error'
    return, 0
  endif
  
  if not(file_test(self.fileSystemMgr->getDataDir(),/directory)) then begin
    errorMessage='DataDir: ' + self.fileSystemMgr->getDataDir()+ ' doesn''t exist. Check your installation!'
    errorTitle='Data Dir Error'
    return, 0
  endif
  
  if not(file_test(self.fileSystemMgr->getObservedDataDir(),/directory)) then begin
    errorMessage='Monitoring Dir '+self.fileSystemMgr->getObservedDataDir()+ ' doesn''t exist. Check your installation!'
    errorTitle='Monitoring Dir Error'
    return, 0
  endif
  
  if not(file_test(self.fileSystemMgr->getRunDataDir(),/directory)) then begin
    errorMessage='Modeling Dir :'+self.fileSystemMgr->getRunDataDir()+ ' doesn''t exist. Check your installation!'
    errorTitle='Modeling Dir Error'
    return, 0
  endif
  
  if not(file_test(self.fileSystemMgr->getResourceDir(),/directory)) then begin
    errorMessage='Resource Dir ' + self.fileSystemMgr->getResourceDir()+ ' doesn''t exist. Check your installation!'
    errorTitle='Resource Dir Error'
    return, 0
  endif
  
  if not(file_test(self.fileSystemMgr->getStartUpFileName())) then begin
    errorMessage='StartupFile: ' + self.fileSystemMgr->getStartUpFileName()+ ' doesn''t exist. Check your installation!'
    errorTitle='StartupFile existence Error'
    return, 0
  endif
  
  ;  if not(self.fileSystemMgr->checkStartupFileContents(txt=txt, alltxt=alltxt)) then begin
  ;
  ;    errorMessage='StartupFile: ' + self.fileSystemMgr->getStartUpFileName()+ ' isn''t consistent. Check your installation!'
  ;    ;errorMessage=[errorMessage, txt]
  ;    errorMessage=[errorMessage, alltxt]
  ;    errorTitle='StartupFile consistency Error'
  ;    return, 0
  ;  endif
  
  return, 1
  
END

PRO FMApplication::updateVersionFile, fileTime=fileTime

  fm=self->getFileSystemMgr()
  parNames=['FORCE_CHECK_DATA', 'LAST_STARTUPFILE_TIME']
  parValues=strarr(n_elements(parNames))
  if self->isAutoCheckEnabled() then parValues[0]='TRUE' else parValues[0]='FALSE'
  if n_elements(fileTime) eq 0 then $
    parValues[1]=strcompress(self->getLastStartUpFileTime()-10, /REMOVE) $
  else $
    parValues[1]=strcompress(fileTime, /REMOVE)
  ;parValues[4]=fm->getConversionDir()
  ;parValues[5]=fm->getPlanningDir()
  ;parValues[6]=fm->getNonLinearDir()
  self.fileSystemMgr->setUpdateStatusInfoText, parNames, parValues
  self.fileSystemMgr->buildUpdateStatusFileData, /FORCE
  
END

PRO FMApplication::addVersionInfo, varName, varValue

  self.fileSystemMgr->addVersionInfo, varName, varValue
  
END

PRO FMApplication::startUp

  ;versionParCodes=*self.versionInfoCodes
  if self->checkApplicationIntegrity(errorTitle, errorMessage) then begin
    self->startJournaling
    confDir=self.fileSystemMgr->getConfigurationDir(/WITH)
    self->loadInitFileData, parameterName=parameterName, parameterValue=parameterValue
    
    lookUpIdx=(where(parameterName eq 'STARTUP_LOOKUP'))[0]
    if lookUpIdx[0] eq -1 then doLookUp=1 else doLookUp=fix(parameterValue[lookUpIdx])
    if doLookUp then self.fileSystemMgr->lookUpSystemData, modelInfo=modelInfo
    
    if n_elements(modelInfo) ne 0 then self->setModelInfo, modelInfo
    extraParameterNames=[''] & extraParameterValues=['']
    for i=0, n_elements(parameterName)-1 do begin
      thisPar=strupCase(parameterName[i])
      if strmid(thisPar, 0, 4) eq 'TXT_' then varname=parameterValue[i] else fileName=parameterValue[i]
      ;verParCheck=(where(versionParCodes eq thisPar))[0]
      ;if verParCheck ne -1 then begin
      ;  self->addVersionInfo, thisPar, parameterValue[i]
      ;  addVersionDefaultParameter=1
      ;  continue
      ;endif
      case strupCase(thisPar) of
        'STARTUP_LOOKUP' : ;do nothing
        ;'ELABORATION_FILE' : self->configElaboration, confDir, fileName, (self->getModelInfo()).frequency
        'BENCHMARK_FILE' : self->configBenchmark, confDir, fileName, (self->getModelInfo()).frequency
        'MODE_FILE' : self.modeList->fillDataFromFile, confDir+fileName
        'CATEGORY_FILE' : self.categoryList->fillDataFromFile, confDir+fileName
        'MODEL_FILE' : self.modelList->fillDataFromFile, confDir+fileName
        'SCENARIO_FILE' : self.scenarioList->fillDataFromFile, confDir+fileName
        'IDLROUTINE_FILE' : self.iDLRoutineList->fillDataFromFile, confDir+fileName
        'PARAMETERTYPE_FILE' : self.parameterTypeList->fillDataFromFile, confDir+fileName
        'GROUPBYTIME_FILE' : self.groupByTimeList->fillDataFromFile, confDir+fileName
        'MONITORINGGROUPSTAT_FILE' : self.monitoringGroupStatList->fillDataFromFile, confDir+fileName
        'GROUPBYSTAT_FILE' : self.groupByStatList->fillDataFromFile, confDir+fileName
        'DIAGRAM_FILE' : self->configDiagram, confDir, fileName, (self->getModelInfo()).frequency
        'SEASON_FILE' : self.seasonList->fillDataFromFile, confDir+fileName
        'DAYPERIOD_FILE' : self.dayPeriodList->fillDataFromFile, confDir+fileName
        'PARAMETER_FILE' : parameterFileName=confDir+fileName ; Save parameter File Name for later use
        'OBSERVED_FILE' : observedFileName=confDir+fileName ; Save observed File Name for later use
        'TXT_PS_CHARSIZE_FACTOR' : self.psCharSizeFactor=float(varName) ; save
        ;'TXT_VERSION_DATE' : self->addVersionInfo, 'TXT_VERSION_DATE', varName  ;varName ; save
        ;'TXT_VERSION_CODE' : self->addVersionInfo, 'TXT_VERSION_CODE', varName  ;varName ; save
        'BROWSER_LOCATION' : self->setBrowserLocation, fileName ; save location of browser application
        'NOTEPAD_LOCATION' : self->setNotePadLocation, fileName ; save location of notepad application
        'DOCUMENTSREADER_LOCATION' : self->setDocReaderLocation, fileName ; Save location of doc reader
        'WORKSHEET_LOCATION' : self->setWorkSheetLocation, fileName ; save location of worksheet reader
        'PDFREADER_LOCATION' : self->setPdfReaderLocation, fileName ; Save location of pdf reader
        'GOOGLEEARTH_LOCATION' : self->setGoogleEarthLocation, filename ; Save location of Google Earth
        'CONVERSION_DIR' : begin
          ;self->addVersionInfo, 'CONVERSION_DIR', filename
          self.fileSystemMgr->setConversionDir, filename ; Save location of Conversion Stuff
        end
        'PLANNING_DIR' : begin
          ;self->addVersionInfo, 'PLANNING_DIR', filename
          self.fileSystemMgr->setPlanningDir, filename ; Save location of Conversion Stuff
        end
        'NON_LINEAR_DIR' : begin
          ;self->addVersionInfo, 'NON_LINEAR_DIR', filename
          self.fileSystemMgr->setNonLinearDir, filename ; Save location of Conversion Stuff
        end
      ;'PLOTINTEGRITY_FILETESTLIST' : self->setPlotIntegrityFileList, filename ; Save location of Google Earth
      else :begin
      extraParameterNames=[extraParameterNames, thisPar]
      extraParameterValues=[extraParameterValues, parameterValue[i]]
      setenv, thisPar+'='+parameterValue[i]
      print, 'setting...'+thisPar+'='+parameterValue[i]
      print, 'getting...'+getenv(thisPar)
    end
  endcase
endfor
self->loadUpdateStatusFileData, parameterName=parameterName, parameterValue=parameterValue
forceCheckData=self.fileSystemMgr->isAutoCheckEnabled()
for i=0, n_elements(parameterName)-1 do begin
  thisPar=strupCase(parameterName[i])
  ;if strmid(thisPar, 0, 4) eq 'TXT_' then varname=parameterValue[i] else fileName=parameterValue[i]
  varname=parameterValue[i]
  case strupCase(thisPar) of
    ;'CONVERSION_DIR' : self.fileSystemMgr->setConversionDir, filename ; Save location of Conversion Stuff
    ;'NON_LINEAR_DIR' : self.fileSystemMgr->setNonLinearDir, filename ; Save location of Conversion Stuff
    ;'PLANNING_DIR' : self.fileSystemMgr->setPlanningDir, filename ; Save location of Conversion Stuff
    ;'TXT_VERSION_DATE' : self.versionDate=varName
    ;'TXT_VERSION_CODE' : self.versionCode=varName
    'FORCE_CHECK_DATA' : begin
      firstVersion=0
      varName=strupcase(varName)
      forceCheckData=varName eq '1' or varName eq 'TRUE' or varName eq 'YES'
    end
    'LAST_STARTUPFILE_TIME' : begin
      firstVersion=0
      varName=strupcase(varName)
      storedCheckTime=long(varName)
    end
  else :begin
;    do nothing
end
endcase
endfor
self.doDataCheck=forceCheckData and not(self.fileSystemMgr->isStartUpFileVerified(storedCheckTime)) and self.fileSystemMgr->isRecentlyUpdated()
self->updateVersionFile, fileTime=storedCheckTime
; Now Load Config Resources (may depend from first block)
self.parameterList->fillDataFromFile, parameterFileName
self.observedList->fillDataFromFile, observedFileName
self->fillMainConfigFromFile, confDir, parameterName=extraParameterNames[1:*], parameterValue=extraParameterValues[1:*]
self->initViewConfiguration
endif else begin
  a=dialog_message(errorMessage, TITLE=errorTitle, /ERROR, /CENTER)
  message, 'Application could not be started'
endelse
;fairmode_checkDataIntegrityMenuSelection

END

PRO FMApplication::modelFrequencyRename, confDir, fileName, frequencyType

  self.fileSystemMgr->modelFrequencyRename, confDir, fileName, frequencyType
  
END

PRO FMApplication::configBenchmark, confDir, fileName, frequencyType

  self->modelFrequencyRename, confDir, fileName, frequencyType
  self.benchMarkMenuList->fillDataFromFile, confDir+fileName
  
END

PRO FMApplication::configElaboration, confDir, fileName, frequencyType

  self->modelFrequencyRename, confDir, fileName, frequencyType
  self.benchMarkMenuList->fillDataFromFile, confDir+fileName
  
END

PRO FMApplication::configDiagram, confDir, fileName, frequencyType

  self->modelFrequencyRename, confDir, fileName, frequencyType
  self.diagramList->fillDataFromFile, confDir+fileName
  
END

PRO FMApplication::initModeDisplay, modeList

  codes=modeList->GetCodeList()
  self.modeDisplay->setNames, modeList->getDisplayNameList()
  self.modeDisplay->setCodes, codes
  self.modeDisplay->setDescriptions, modeList->getDescriptionList()
  self.modeDisplay->setSelection, 0
  
END

;PRO FMApplication::initEntityDisplay, mainConfig, categoryList, modelList, scenarioList, observedList, parameterTypeList, parameterList
PRO FMApplication::initEntityDisplay, entityDisplay, mainConfig, categoryList, modelList, scenarioList, observedList, parameterTypeList, parameterList, monitoringGroupStatList

  ;categoryList, modelList, scenarioList, observedList
  runList=mainConfig->getRunList()
  
  entityDisplay->setScenarioNames, scenarioList->getDisplayNames()
  entityDisplay->setScenarioCodes, scenarioList->getCodes()
  ;entityDisplay->setScenarioIsBases, scenarioList->getIsBases()
  entityDisplay->setScenarioDescriptions, scenarioList->getDescriptions()
  entityDisplay->setScenarioSelections, [-1]
  
  entityDisplay->setModelNames, modelList->getDisplayNames()
  entityDisplay->setModelCodes, modelList->getCodes()
  entityDisplay->setModelDescriptions, modelList->getDescriptions()
  entityDisplay->setModelSelections, [-1]
  
  entityDisplay->setRunNames, runList->getDisplayNames()
  entityDisplay->setRunCodes, runList->getCodes()
  entityDisplay->setRunDescriptions, runList->getDescriptions()
  entityDisplay->setRunScenarioCodes, runList->getScenarioCodes()
  entityDisplay->setRunModelCodes, runList->getModelCodes()
  entityDisplay->setRunSelections, [-1]
  
  
  obsCat=mainConfig->getObservedCategoryList()
  categoryValues=obsCat->getValuesByCategory(CATEGORYCODES=CATEGORYCODES)
  allPresetForCategories=intarr(4)
  allPresetForCategories[0:n_elements(CATEGORYCODES)-1]=-1
  
  entityDisplay->setCategoryCodes, CATEGORYCODES
  entityDisplay->setCategoryValues, categoryValues
  entityDisplay->setCategoryTitles, categoryList->getDisplayNames(CODES=CATEGORYCODES)
  entityDisplay->setCategorySelections, allPresetForCategories
  presenceFlagsNo=n_elements(CATEGORYCODES) & presenceFlags=bytarr(4) & presenceFlags[0:presenceFlagsNo-1]=1b
  entityDisplay->setCategoryPresenceFlags, presenceFlags
  
  entityDisplay->setObservedNames, observedList->getDisplayNames()
  entityDisplay->setObservedShortNames, observedList->getShortNames()
  entityDisplay->setObservedLatitudes, observedList->getYGeoLocations()
  entityDisplay->setObservedLongitudes, observedList->getXGeoLocations()
  entityDisplay->setObservedAltitudes, observedList->getHeightAboveSeas()
  entityDisplay->setObservedCountries, observedList->getCountries()
  entityDisplay->setObservedGMTs, observedList->getCountryGMTs()
  entityDisplay->setObservedCodes, observedList->getCodes()
  ;entityDisplay->setObservedCodesSelections, allSetForCategory
  entityDisplay->setObsCatValues, obsCat->getValues()
  entityDisplay->setObsCatObservedCodes, obsCat->getObservedCodes()
  entityDisplay->setObsCatCategoryCodes, obsCat->getCategoryCodes()
  
  ;if self->IsAdvancedFilter() then entityDisplay->setUseObservedModelFlag, 1 else entityDisplay->setUseObservedModelFlag, 0
  entityDisplay->setUseObservedModelFlag, 0
  
  entityDisplay->setParameterTypeNames, parameterTypeList->getDisplayNames()
  entityDisplay->setParameterTypeCodes, parameterTypeList->getCodes()
  entityDisplay->setParameterTypeDescriptions, parameterTypeList->getDescriptions()
  entityDisplay->setParameterTypeSelections, [0]
  
  entityDisplay->setParameterNames, parameterList->getDisplayNames()
  entityDisplay->setParameterCodes, parameterList->getCodes()
  entityDisplay->setParameterTypes, parameterList->getTypeCodes()
  entityDisplay->setParameterDescriptions, parameterList->getDescriptions()
  entityDisplay->setParameterMeasureUnits, parameterList->getDescriptions()
  
  allParsCodes=parameterList->getCodes()
  allObsCodes=observedList->getCodes()
  allObsPars=observedList->getParameters()
  
  parameterObs=self->buildParametersObsRelationship(allParsCodes, allObsCodes, allObsPars)
  
  entityDisplay->setParameterObservedCodes, parameterObs
  
  allMonGroupStatCodes=monitoringGroupStatList->getCodes()
  firstSelected=allMonGroupStatCodes[0]
  entityDisplay->setObservedGroupStatNames, monitoringGroupStatList->getDisplayNames()
  entityDisplay->setObservedGroupStatCodes, allMonGroupStatCodes
  entityDisplay->setObservedGroupStatCodeSelection, firstSelected
  
END

;PRO FMApplication::initElaborationDisplay, mainConfig, diagramList, axisTypeList, groupByStatList, groupByTimeList, seasonList, dayperiodList, parameterTypeList, parameterList
PRO FMApplication::updateElaborationDisplay, elabInfo

  self.mainConfig->setUserType, self->getUserType()
  self->initElaborationDisplay, elabInfo, self.mainConfig, self.diagramList, self.groupByStatList, self.groupByTimeList, self.seasonList, self.dayPeriodList
  
END

PRO FMApplication::updateEntityDisplay, entityInfo

  self.mainConfig->setUserType, self->getUserType()
  self->initEntityDisplay, entityInfo, self.mainConfig, self.categoryList, self.modelList, self.scenarioList, self.observedList, self.parameterTypeList, self.parameterList, self.monitoringGroupStatList
  
END

PRO FMApplication::initElaborationDisplay, elabDisplay, mainConfig, diagramList, groupByStatList, groupByTimeList, seasonList, dayperiodList, parameterTypeList, parameterList

  elabList=mainConfig->getElaborationList()
  goalsList=mainConfig->getGoalsCriteriaOCList()
  diagramList=self.diagramList
  
  elabDisplay->setDiagramNames, diagramList->getDisplayNames()
  elabDisplay->setDiagramCodes, diagramList->getCodes()
  elabDisplay->setDiagramMaxMultipleChoice, diagramList->getMaxMultipleChoice()
  elabDisplay->setDiagramDescriptions, diagramList->getDescriptions()
  ;elabDisplay->setDiagramAxisCodes, diagramList->getAxisCodes()
  elabDisplay->setDiagramSelection, [0]
  
  elabDisplay->setElabCodes, elabList->getCodes()
  elabDisplay->setElabNames, elabList->getDisplayNames()
  elabDisplay->setElabDescriptions, elabList->getDescriptions()
  elabDisplay->setElabMultipleChoiceFlags, elabList->getMultipleChoiceFlags()
  elabDisplay->setElabDiagramCodes, elabList->getDiagramCodes()
  ;elabDisplay->setElabParameters, elabList->getParameterCodes()
  elabDisplay->setElabNumberRefValues, elabList->getNumberRefValues()
  elabDisplay->setElabGCOC, elabList->getGoalsCriteriaOCFlags()
  elabDisplay->setElabGroupByTimeCodes, elabList->getGroupByTimeCodes()
  elabDisplay->setElabGroupByStatCodes, elabList->getGroupByStatCodes()
  elabDisplay->setElabDayPeriodCodes, elabList->getDayPeriodCodes()
  elabDisplay->setElabSeasonCodes, elabList->getSeasonCodes()
  elabDisplay->setElabSelection, [0]
  ;elabDisplay->setElabSelection, (elabList->getGoalsCriteriaOCFlags())[0]
  
  ;elabDisplay->setAxisCodes, axisTypeList->getCodes()
  ;elabDisplay->setAxisNames, axisTypeList->getNames()
  ;elabDisplay->setAxisDescriptions, axisTypeList->getDescriptions()
  ;elabDisplay->setAxisSelection, [0]
  
  ;  elabDisplay->setParameterNames, parameterList->getDisplayNames()
  ;  elabDisplay->setParameterCodes, parameterList->getCodes()
  ;  elabDisplay->setParameterDescriptions, parameterList->getDescriptions()
  ;  elabDisplay->setParameterMeasureUnits, parameterList->getDescriptions()
  ;  elabDisplay->setParametersSelection, [0]
  
  elabDisplay->setGroupByTitles, [groupByTimeList->getTitle(), groupByStatList->getTitle()]
  elabDisplay->setGroupByStatNames, groupByStatList->getDisplayNames()
  elabDisplay->setGroupByStatCodes, groupByStatList->getCodes()
  elabDisplay->setGroupByTimeNames, groupByTimeList->getDisplayNames()
  elabDisplay->setGroupByTimeCodes, groupByTimeList->getCodes()
  
  elabDisplay->setPeriodTitles, ['Season', 'Day']
  seasonNames=seasonList->getDisplayNames()
  seasonCodes=seasonList->getCodes()
  elabDisplay->setSeasonNames, seasonNames
  elabDisplay->setSeasonCodes, seasonCodes
  dayPeriodNames=dayPeriodList->getDisplayNames()
  dayPeriodCodes=dayPeriodList->getCodes()
  elabDisplay->setDayPeriodNames, dayPeriodNames
  elabDisplay->setDayPeriodCodes, dayPeriodCodes
  
END

PRO FMApplication::initViewConfiguration

  self->initModeDisplay, self.modeList
  self->initEntityDisplay, self.entityDisplay, self.mainConfig, self.categoryList, self.modelList, self.scenarioList, self.observedList, self.parameterTypeList, self.parameterList, self.monitoringGroupStatList
  ;self->initElaborationDisplay, self.mainConfig, self.diagramList, self.axisTypeList, self.groupByStatList, self.groupByTimeList, self.seasonList, self.dayPeriodList
  self->initElaborationDisplay, self.elaborationDisplay, self.mainConfig, self.diagramList, self.groupByStatList, self.groupByTimeList, self.seasonList, self.dayPeriodList
  
END

PRO FMApplication::fillMainConfigFromFile, confDir, parameterName=parameterName, parameterValue=parameterValue

  self.mainConfig->fillFlexyData, confDir, self.fileSystemMgr, (self->getModelInfo()).frequency, parameterName=parameterName, parameterValue=parameterValue
  
  userTypeIdx=(where(parameterName eq 'USER_TYPE'))[0]
  ; Available type: 0: Standard (Filtered elabs), 1: Advanced (All Elabs), 2: Developer (Benchmark+Magic)
  if userTypeIdx[0] eq -1 then userType=0 else userType=(where(parameterValue[userTypeIdx] eq self.availableUserType))[0]
  
  ;benchmarkManagingEnableIdx=(where(parameterName eq 'BENCHMARK_ENABLE_MGR'))[0]
  ; TRUE or 1 --> Enabling Benchmark related menu voices
  ; not present or other values --> Benchmark related menu voices are disabled
  benchmarkManagingEnable=0
  if userType ge 2 then benchmarkManagingEnable=1
  ;if benchmarkManagingEnableIdx[0] ne -1 then benchmarkManagingEnable=parameterValue[benchmarkManagingEnableIdx] eq '1' or strupcase(parameterValue[benchmarkManagingEnableIdx]) eq 'TRUE'
  
  self->setUserType, userType
  self.mainConfig->setBenchmarkManagingEnabled, benchmarkManagingEnable
  
END
;****************************************************************************************
; get/set
;****************************************************************************************
FUNCTION FMApplication::getEntityDisplay

  return, self.entityDisplay
  
END

PRO FMApplication::setEntityDisplay, entity

  self.entityDisplay=entity
  
END

FUNCTION FMApplication::getElaborationDisplay

  return, self.elaborationDisplay
  
END

PRO FMApplication::setElaborationDisplay, elab

  self.elaborationDisplay=elab
  
END

;****************************************************************************************
; constructor/destructor
;****************************************************************************************
FUNCTION FMApplication :: init

  if not self -> Object :: init() then return , 0
  self.fileSystemMgr=obj_New('FMFileSystemManager')
  self.mainConfig=obj_new('FMMainConfig')
  self.modeList=obj_new('Mode')
  self.categoryList=obj_new('Category')
  self.diagramList=obj_new('Diagram')
  self.monitoringGroupStatList=obj_new('MonitoringGroupStat')
  self.groupByStatList=obj_new('GroupByStat')
  self.groupByTimeList=obj_new('GroupByTime')
  self.IDLRoutineList=obj_new('IDLRoutine')
  self.modelList=obj_new('Model')
  self.scenarioList=obj_new('Scenario')
  self.seasonList=obj_new('TimePeriod')
  self.dayPeriodList=obj_new('TimePeriod')
  self.parameterTypeList=obj_new('ParameterType')
  self.parameterList=obj_new('Parameter')
  self.scenarioList=obj_new('Scenario')
  self.observedList=obj_new('Observed')
  self.modeDisplay=obj_new('ModeDisplayInfo')
  self.entityDisplay=obj_new('EntityDisplayInfo')
  self.elaborationDisplay=obj_new('ElaborationDisplayInfo')
  ;self.request=obj_new('Request')
  self.dataMinerMgr=obj_new('DataMiner')
  self.benchMarkMenuList=obj_new('MenuInfo')
  self.availableUserType=["STANDARD", "ADVANCED", "DEVELOPER"];, "BENCHMARK"]
  ;self.versionInfoCodes=ptr_new(['TXT_VERSION_DATE', 'TXT_VERSION_CODE'], /NO_COPY);, 'CONVERSION_DIR', 'PLANNING_DIR', 'NON_LINEAR_DIR']
  self.testMode=0
  self->setLogMode, 0
  ;self.magicFile='magic'
  return, 1
  
END

PRO FMApplication :: cleanUp

  obj_destroy, self.mainConfig
  obj_destroy, self.categoryList
  obj_destroy, self.modelList
  obj_destroy, self.modeList
  obj_destroy, self.scenarioList
  obj_destroy, self.IDLRoutineList
  obj_destroy, self.parameterList
  obj_destroy, self.parameterTypeList
  obj_destroy, self.monitoringGroupStatList
  obj_destroy, self.groupByTimeList
  obj_destroy, self.groupByStatList
  obj_destroy, self.diagramList
  obj_destroy, self.seasonList
  obj_destroy, self.dayPeriodList
  obj_destroy, self.observedList
  obj_destroy, self.fileSystemMgr
  obj_destroy, self.mainView
  obj_destroy, self.entityView
  obj_destroy, self.elaborationView
  obj_destroy, self.modeView
  obj_destroy, self.lastView
  obj_destroy, self.modeDisplay
  obj_destroy, self.entityDisplay
  obj_destroy, self.elaborationDisplay
  obj_destroy, self.request
  obj_destroy, self.dataMinerMgr
  obj_destroy, self.plotter
  obj_destroy, self.benchMarkMenuList
  obj_destroy, self.recognizeInfo
  obj_destroy, self.benchmarkView
  ptr_free, self.versionInfoCodes
  self -> Object :: cleanUp
  journal
  
END

PRO FMApplication__Define

  Struct = { FMApplication , $
    ;lastUserType: 0, $
    mainView : obj_new(), $
    benchmarkView: obj_new(), $
    entityView: obj_new(), $
    elaborationView: obj_new(), $
    entityBatchView: obj_new(), $
    elaborationBatchView: obj_new(), $
    modeView: obj_new(), $
    lastView: obj_new(), $
    mainConfig : obj_new(), $
    modeList : obj_new(), $
    categoryList : obj_new(), $
    modelList : obj_new(), $
    scenarioList : obj_new(), $
    IDLRoutineList : obj_new(), $
    parameterList : obj_new(), $
    parameterTypeList : obj_new(), $
    groupByTimeList : obj_new(), $
    monitoringGroupStatList : obj_new(), $
    groupByStatList : obj_new(), $
    diagramList : obj_new(), $
    dayPeriodList : obj_new(), $
    seasonList : obj_new(), $
    observedList : obj_new(), $
    modeDisplay: obj_new(), $
    entityDisplay: obj_new(), $
    elaborationDisplay: obj_new(), $
    elaborationBatchDisplay: obj_new(), $
    entityBatchDisplay: obj_new(), $
    compositeBatchMgr: obj_new(), $
    executeOk: intarr(2), $
    request: obj_new(), $
    plotter: obj_new(), $
    fileSystemMgr: obj_new(), $
    dataMinerMgr: obj_new(), $
    benchMarkMenuList: obj_new(), $
    recognizeInfo: obj_new(), $
    ;MM fall 2014 Start
    versionInfoCodes: ptr_new(), $
    doDataCheck: 0b, $
    ;MM fall 2014 End
    ;MM summer 2012 Start
    ;scaleInfo: '', $
    modelInfo: getFMModelInfoStruct(), $
    ;MM summer 2012 End
    availableUserType: strarr(3), $
    ;versionDate : '', $
    ;versionCode : '', $
    psCharSizeFactor: 0., $
    browserLocation: '', $
    googleEarthLocation: '', $
    workSheetLocation: '', $
    notepadLocation: '', $
    docReaderLocation: '', $
    pdfReaderLocation: '', $
    testMode: 0, $
    logMode: 0, $
    logFile: '', $
    Inherits Object $
    }
    
END

;****************************************************************************************
