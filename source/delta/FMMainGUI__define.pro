FUNCTION FMMainGUI::dialogMessage, textMessage, title=title, INFORMATION=INFORMATION, error=error, question=question, WARNING=WARNING, CENTER=CENER

  if self.mgr->getTestMode() then return, self.mgr->logDialog(textMessage, title=title, INFORMATION=INFORMATION, error=error, question=question, WARNING=WARNING)
  return, self->GUI::dialogMessage(textMessage, title=title, INFORMATION=INFORMATION, error=error, question=question, WARNING=WARNING, CENTER=CENTER)
  
END

FUNCTION FMMainGUI::getFontMgr

  return, self.mgr->getFontMgr()
  
END

PRO FMMainGUI::checkDataIntegrity

  self.mgr->checkDataIntegrity
; DeltaCheck_IO
  
END

PRO FMMainGUI::dataFormatConversionUtility

  self.mgr->dataFormatConversionUtility
; DeltaCheck_IO
  
END

FUNCTION  FMMainGUI::getMainMgr

  return, self->getMgr()
  
END

FUNCTION FMMainGUI::getPSCharSizeFactor

  return, self.mgr->getPSCharSizeFactor()
  
END

PRO FMMainGUI::updateRecognizer, dataName, dataValue

  self.recognizerGUI->update, dataName, dataValue
  if self.recognizerVisibleStatus then self.recognizerGUI->show
  
END

PRO FMMainGUI::updateRecognizeStatus

  self->displayRecognize
  if self.mgr->isRecognizible() then begin
    self.recognizerVisibleStatus=1b
    self.recognizerGUI->show
  endif else begin
    self.recognizerVisibleStatus=0b
    self.recognizerGUI->hide
  endelse
  self.recognizerGUI->update, "", ""
  
END

PRO FMMainGUI::recognize, x, y

  print, "Recognize point on device coord", x, y
  self->wsetMainDataDraw
  if self.mgr->isRecognizible() then self.mgr->DoRecognize, x, y
  
END

PRO FMMainGUI::destroyBenchMarkSubMenus

  if ptr_valid(self.benchMarkSubMenus) then subMenus=*self.benchMarkSubMenus else return
  for i=0, n_elements(subMenus)-1 do widget_control, subMenus[i], /DESTROY
  ptr_free, self.benchMarkSubMenus
  
END

PRO FMMainGUI::buildBenchMarkMenuTree, REBUILD=REBUILD

  if keyword_set(REBUILD) then self->destroyBenchMarkSubMenus
  
  bMD=self.mgr->getBenchMarkMenuInfo()
  topLevels=bMD->getSubMenuList(1, 999, NELEM=NELEM)
  eventPro=self.eventPrefix+'benchmarkBtt'
  root=self.benchMarkRootButton
  
  ;topLevElemNo=n_elements(topLevels)
  if NELEM gt 0 then thisMenuBttGFs=lonarr(NELEM)
  for i=0, NELEM-1 do begin
    thisMenuInfo=topLevels[i]
    thisMenuBttGFs[i]=widget_button(root, value=thisMenuInfo.displayName, UNAME=thisMenuInfo.fileName, MENU=thisMenuInfo.isMenu, event_pro=eventPro)
    subLevels=bMD->getSubMenuList(2, thisMenuInfo.code, NELEM=NELEM)
    ;print, thisMenuInfo.code
    for j=0, NELEM-1 do begin
      thisSubMenuInfo=subLevels[j]
      ;      thisMenuBttF=widget_button(thisMenuBttGF, value=thisSubMenuInfo.displayName, UNAME=thisSubMenuInfo.fileName, MENU=thisSubMenuInfo.isMenu, event_pro=eventPro)
      thisMenuBttF=widget_button(thisMenuBttGFs[i], value=thisSubMenuInfo.displayName, UNAME=thisSubMenuInfo.fileName, event_pro=eventPro)
    ;print, thisSubMenuInfo.code
    ;      subsubLevels=bMD->getSubMenuList(3, thisSubMenuInfo.code)
    ;      for k=0, n_elements(subsubLevels)-1 do begin
    ;        thisSubSubMenuInfo=subsubLevels[k]
    ;        thisMenuBttS=widget_button(thisMenuBttF, value=thisSubSubMenuInfo.displayName, UNAME=thisSubSubMenuInfo.fileName, event_pro=eventPro)
    ;      endfor
    endfor
  endfor
  self.benchMarkSubMenus=ptr_new(thisMenuBttGFs, /NO_COPY)
  if keyword_set(REBUILD) then widget_control, self.benchMarkRootButton, /realize
  
END

FUNCTION FMMainGUI::getImageToSave, BACKGROUNDCOLOR=BACKGROUNDCOLOR

  self->wsetMainDataDraw
  ;getmaindatadraw
  mainSectionImg=tvrd(/TRUE)
  ;getinfodatadraw
  self->wsetInfoDataDraw
  legendSectionImg=tvrd(/TRUE)
  legDim=size(legendSectionImg, /DIM)
  mainDim=size(mainSectionImg, /DIM)
  backDefault=[200,200,200]
  compositeImg=bytarr(3, legDim[1], legDim[2]+mainDim[2])
  compositeImg[*,*,legDim[2]:*]=mainSectionImg
  compositeImg[*,*,0:legDim[2]-1]=legendSectionImg
  ;backIdxs=where(compositeImg[0,*,*] eq backDefault[0] and compositeImg[1,*,*] eq backDefault[1] and compositeImg[2,*,*] eq backDefault[2], count)
  ;if count ne 0 then compositeImg[backIdxs]=BACKGROUNDCOLOR
  ;tvrd
  ;compose images
  return, compositeImg
  
END

PRO FMMainGUI::saveImage, filename, format, NO_CONFIRM=NO_CONFIRM, WORKINGDIR=WORKINGDIR

  if n_elements(filename) eq 1 then imageFileName=filename else imageFileName='image.ext'
  res=query_image(filename, SUPPORTED_WRITE=SUPPLIST)
  SUPPLIST=strlowcase(SUPPLIST)
  count=0
  if n_elements(format) ne 0 then idx=where(strlowcase(format) eq strlowcase(SUPPLIST), count)
  if count eq 1 then predefFormat=SUPPLIST[idx] else predefFormat=suppList[0]
  ;if n_elements(format) eq 0 then predefFormat=strlowcase(suppList[0]) else predefFormat=strlowcase(format)
  fsm=obj_new('FMFileSystemManager')
  imageFileNamePos=strpos(imageFileName, '.', /REVERSE_SEARCH)
  imageFileNameRes=strarr(2)
  imageFileNameRes[0]=strmid(imageFileName, 0, imageFileNamePos)
  imageFileNameRes[1]=strmid(imageFileName, imageFileNamePos, strlen(imageFileName)-imageFileNamePos)
  imageFileName=imageFileNameRes[0]+'.'+strlowcase(predefFormat)
  ;  imageFileName=strsplit(imageFileName, '.', /EXTRACT)
  ;  imageFileName=imageFileName[0]+'.'+strlowcase(suppList[0])
  ;imageToSave=self.mgr->getImageToSave(BACKGROUND=0)
  imageToSave=self->getImageToSave()
  ;
  ;MM 16/02/2011 Here save an image (asking or batch mode)
  if ~keyword_set(NO_CONFIRM) then begin
    result=Dialog_Write_Image(imageToSave, title=['Save image'], PATH=self.fsm->getSaveDir(), filename=imageFileName, options=info, $
      type=predefFormat)
    if result eq 1 then begin
      filename=info.filename
      print, 'Image saved, filename:<', info.filename, '>', 'type:', info.type
      msg=self->dialogMessage(['Image saved in:', '<'+filename+'> file.', 'format = '+info.type], title=['Image'], /INFORMATION)
    endif
  endif else begin
    fsm=obj_new('FMFileSystemManager')
    onlyFileName=fsm->getFileName(imageFileName)
    obj_destroy, fsm
    if keyword_set(WORKINGDIR) then fullFileName=WORKINGDIR+onlyFileName else fullFileName=imageFileName
    WRITE_IMAGE, fullFileName, predefFormat, imageToSave
  endelse
  
END

PRO FMMainGUI::SaveImageBlack

  fsm=obj_new('FMFileSystemManager')
  
  ;imageToSave=self.mgr->getImageToSave(BACKGROUND=0)
  imageToSave=self->getImageToSave(BACKGROUNDCOLOR=0b) ; 0 black -> 255 white
  ;imageToSave=DIST(100)
  imageFileName='image.ext'
  result=Dialog_Write_Image(imageToSave, title=['Save image'], PATH=self.fsm->getSaveDir(), filename=imageFileName, options=info)
  if result eq 1 then begin
    filename=info.filename
    print, 'Image saved, filename:<', info.filename, '>'
    msg=self->dialogMessage(['Image saved in:', '<'+filename+'> file.'], title=['Image'], /INFORMATION)
  endif
  
END

PRO FMMainGUI::SaveImageWhite

  self.mgr->saveImage, BACKGROUND=0
  
END

PRO FMMainGUI::showDisclaimer

  ;getFairModeFileName
  if obj_valid(self.disclaimerGUI) then obj_destroy, self.disclaimerGUI
  logo=self.mgr->getSplashLogoImage()
  self.disclaimerGUI=obj_new('FMDisclaimerGUI', mgr, logo, self.mgr->getReleaseVersion(), self.mgr->getVersionDate())
  self.disclaimerGUI->realize
  self.disclaimerGUI->moveToCenterScreen
  self.disclaimerGUI->show
  
END

PRO FMMainGUI::configureExecutable

  ;getFairModeFileName
  self.mgr->configureExecutable
  
END

;PRO FMMainGUI::showLicense
;
;  ;getFairModeFileName
;  self.mgr->showLicense
;;  if obj_valid(self.disclaimerGUI) then obj_destroy, self.disclaimerGUI
;;  logo=self.mgr->getSplashLogoImage()
;;  self.disclaimerGUI=obj_new('FMDisclaimerGUI', mgr, logo, self.mgr->getVersionCode(), self.mgr->getVersionDate())
;;  self.disclaimerGUI->realize
;;  self.disclaimerGUI->moveToCenterScreen
;;  self.disclaimerGUI->show
;
;END
PRO FMMainGUI::switchRecognize

  if self.recognizerVisibleStatus then self.recognizerGUI->hide else self.recognizerGUI->show
  self.recognizerVisibleStatus=1b-self.recognizerVisibleStatus
  
END

PRO FMMainGUI::showAboutSplash

  ;getFairModeFileName
  if obj_valid(self.aboutGUI) then obj_destroy, self.aboutGUI
  logo=self.mgr->getSplashLogoImage()
  self.aboutGUI=obj_new('FMLogoGUI', mgr, logo, self.mgr->getReleaseVersion(), self.mgr->getReleaseDate())
  self.aboutGUI->realize
  self.aboutGUI->moveToCenterScreen
  self.aboutGUI->showAboutSplash
  
END

PRO FMMainGUI::displayRecognize

  ;getFairModeFileName
  if obj_valid(self.recognizerGUI) then obj_destroy, self.recognizerGUI
  self.recognizerGUI=obj_new('FMRecognizeGUI', self.mgr, "Name", "Value", [250, 66])
  self.recognizerGUI->realize
  self.recognizerGUI->moveToLowerCenterScreen
  
END

FUNCTION FMMainGUI::saveBatch, splitBatchInfo

  batchFileName=''
  fsm=obj_new('FMFileSystemManager')
  if n_elements(splitBatchInfo) eq 0 then splitBatchInfo=self.mgr->buildCurrentSplitBatchInfo()
  
  ;if n_elements(requests) eq 0 then requests=self.mgr->buildRequest([0,0,0,0], [0.0,0.0,1.0,1.0], self.mgr->getEntityDisplay(), self.mgr->getElaborationDisplay())
  ;  if n_elements(entities) eq 0 then begin
  ;    entities=self.mgr->getEntityDisplay()
  ;    elaborations=self.mgr->getElaborationDisplay()
  ;    locations=ptr_new([0.,0.,1.,1.], /NO_COPY)
  ;  endif
  
  filter=['*'+self.fsm->getBatchExtension()]
  fix_filter='*'+self.fsm->getBatchExtension()
  batchFileName=dialog_pickfile(DEFAULT_EXTENSION=self.fsm->getBatchExtension(), $
    DIALOG_PARENT=self->getTopBase(), $
    FILTER=filter, FIX_FILTER=fix_filter, $
    GET_PATH=path, PATH=self.fsm->getSaveDir(), $
    TITLE='Batch selection', /OVERWRITE_PROMPT, /WRITE)
  if batchFileName ne '' then begin
    batchFileName=self.mgr->saveBatch(batchFileName, path, splitBatchInfo)
    print, 'Single/Multiple Request saved, filename:<', batchFileName, '>'
    msg=self->dialogMessage(['Batch file saved to:', '<'+batchFileName+'> file.'], title=['Benchmark'], /INFORMATION)
  endif
  return, batchFileName
  
END

PRO FMMainGUI::convertObsFromCSVtoCDF

  self.mgr->convertObsFromCSVtoCDF
  
END

PRO FMMainGUI::testPlotQuality

  self.mgr->testPlotQuality
  
END

PRO FMMainGUI::startBenchMark, fileName, WORKINGDIR=WORKINGDIR, BATCH=BATCH, SAMEFILENAME=SAMEFILENAME

  applicationUT=self.mgr->getUserType()
  self->restoreBatch, fileName, WORKINGDIR=WORKINGDIR, BATCH=BATCH, SAMEFILENAME=SAMEFILENAME
  self.mgr->setUserType, applicationUT
  
END

;PRO FMMainGUI::loadBatch
;
;  filter=['*'+self.fsm->getBatchExtension()]
;  fix_filter='*'+self.fsm->getBatchExtension()
;  batchFileName=dialog_pickfile(DEFAULT_EXTENSION=self.fsm->getBatchExtension(), $
;    DIALOG_PARENT=self->getTopBase(), $
;    FILTER=filter, $FIX_FILTER=fix_filter, $
;    GET_PATH=path, /MUST_EXIST, PATH=self.fsm->getSaveDir(), $
;    TITLE='Restore a previous saved entity'  )
;  if batchFileName ne '' then begin
;    self.mgr->restoreBatch, batchFileName
;    self->updateInfo
;  ; Launch elaboration!!
;  endif
;
;END

PRO FMMainGUI::restoreBatch, fileName, WORKINGDIR=WORKINGDIR, BATCH=BATCH, SAMEFILENAME=SAMEFILENAME

  if n_elements(fileName) ne 1 then begin
    filter=['*'+self.fsm->getBatchExtension()]
    fix_filter='*'+self.fsm->getBatchExtension()
    batchFileName=dialog_pickfile(DEFAULT_EXTENSION=self.fsm->getBatchExtension(), $
      DIALOG_PARENT=self->getTopBase(), $
      FILTER=filter, $FIX_FILTER=fix_filter, $
      GET_PATH=path, /MUST_EXIST, PATH=self.fsm->getSaveDir(), $
      TITLE='Restore a previous saved entity'  )
  endif else begin
    if n_elements(WORKINGDIR) ne 1 then path=self.fsm->getSaveDir(/WITH) else path=WORKINGDIR
    batchFileName=path+fileName
  endelse
  if batchFileName ne '' then begin
    batchInfo=obj_new("BatchInfo")
    if batchInfo->restoreData(batchFileName, path, resRequests) ne 0 then begin
      requests=resRequests
      for i=0, n_elements(requests)-1 do begin
        ;self.mgr->restoreBatch, batchFileName
        if keyword_set(SAMEFILENAME) then outFileName=fileName else outFileName=requests[i]->getFileName()
        if ~self.mgr->restoreRequest(requests[i]->getFileName(), WORKINGDIR=path, BATCH=BATCH, outFileName=outFileName) then continue
      ;self.entityElaboration->restore
      ;self.mgr->buildRequest()
      ;self->updateInfo
      endfor
    endif else begin
    endelse
  endif
; Launch elaboration!!
  
END

PRO FMMainGUI::saveEntity

  filter=['*'+self.fsm->getEntityExtension()]
  fix_filter='*'+self.fsm->getEntityExtension()
  entityFileName=dialog_pickfile(DEFAULT_EXTENSION=self.fsm->getEntityExtension(), $
    DIALOG_PARENT=self->getTopBase(), $
    FILTER=filter, FIX_FILTER=fix_filter, $
    GET_PATH=path, PATH=self.fsm->getSaveDir(), $
    TITLE='Save data selection', /OVERWRITE_PROMPT, /WRITE)
  if entityFileName ne '' then begin
    ; print, '****'
    ; print, entityFile
    ; print, '****'
    entityD=self.mgr->getEntityDisplay()
    entityD->saveData, entityFileName
    print, 'Entity saved, filename:<', entityFileName, '>'
    msg=self->dialogMessage(['Data selection saved to:', '<'+entityFileName+'> file.'], title=['Save data selection'], /INFORMATION)
  endif
  
END

PRO FMMainGUI::restoreEntity

  filter=['*'+self.fsm->getEntityExtension()]
  fix_filter='*'+self.fsm->getEntityExtension()
  entityFileName=dialog_pickfile(DEFAULT_EXTENSION=self.fsm->getEntityExtension(), $
    DIALOG_PARENT=self->getTopBase(), $
    FILTER=filter, $FIX_FILTER=fix_filter, $
    GET_PATH=path, /MUST_EXIST, PATH=self.fsm->getSaveDir(),$
    TITLE='Restore a previous saved entity')
  ;phil batch 18/03
  ;entityFileName = FILE_BASENAME(entityFileName)
  ; end phil
  if entityFileName ne '' then begin
    if self.mgr->restoreEntity(entityFileName) then begin
      msg=self->dialogMessage(['Data selection restored from:', '<'+entityFileName+'> file.'], title=['Load data selection '], /INFORMATION)
      self->updateInfo
      self->displayEntitySelectionGUI
    endif else begin
      msg=self->dialogMessage(['Data selection restore failed from:', '<'+entityFileName+'> file.'], title=['Load data selection '], /WARNING)
    endelse
  endif
END

PRO FMMainGUI::saveElaboration

  filter=['*'+self.fsm->getElaborationExtension()]
  fix_filter='*'+self.fsm->getElaborationExtension()
  elabFileName=dialog_pickfile(DEFAULT_EXTENSION=self.fsm->getElaborationExtension(), $
    DIALOG_PARENT=self->getTopBase(), $
    FILTER=filter, FIX_FILTER=fix_filter, $
    GET_PATH=path, PATH=self.fsm->getSaveDir(), $
    TITLE='Save analysis selection', /OVERWRITE_PROMPT, /WRITE)
  if elabFileName ne '' then begin
    elabD=self.mgr->getElaborationDisplay()
    elabD->saveData, elabFileName
    print, 'Analysis saved, filename:<', elabFileName, '>'
    msg=self->dialogMessage(['Analysis selections saved to:', '<'+elabFileName+'> file.'], title=['Save analysis selection '], /INFORMATION)
  endif
  
END

PRO FMMainGUI::restoreElaboration

  filter=['*'+self.fsm->getElaborationExtension()]
  fix_filter='*'+self.fsm->getElaborationExtension()
  elabFileName=dialog_pickfile(DEFAULT_EXTENSION=self.fsm->getElaborationExtension(), $
    DIALOG_PARENT=self->getTopBase(), $
    FILTER=filter, $FIX_FILTER=fix_filter, $
    GET_PATH=path, /MUST_EXIST, PATH=self.fsm->getSaveDir(), $
    TITLE='Restore a previous saved analysis'  )
  ;phil batch 18/03
  ;elabFileName = FILE_BASENAME(elabFileName)
  ; end phil
  if elabFileName ne '' then begin
    if self.mgr->restoreElaboration(elabFileName) then begin
      msg=self->dialogMessage(['Analysis selections restored from:', '<'+elabFileName+'> file.'], title=['Analysis'], /INFORMATION)
      self->updateInfo
      self->displayElaborationSelectionGUI
    endif else begin
      msg=self->dialogMessage(['Analysis selections restored failed from:', '<'+elabFileName+'> file.'], title=['Analysis'], /WARNING)
    endelse
  endif
  
END

PRO FMMainGUI::wsetMainDataDraw, topBase

  MainDataDraw=widget_info(self->getTopBase(), FIND='MAINDRAW')
  widget_control, MainDataDraw, get_value=wsetId
  wset, wsetId
  device, /DECOMPOSE
;  if ptr_valid(mainRefStruct.controlWindow) then begin
;    controlWindow=*(mainRefStruct.controlWindow)
;    !P=controlWindow.systemP[0] & !X=controlWindow.systemX[0]
;    !Y=controlWindow.systemY[0] & !Z=controlWindow.systemZ[0]
;  endif
  
END

PRO FMMainGUI::wsetInfoDataDraw, topBase

  InfoDraw=widget_info(self->getTopBase(), FIND='GRAPHDRAW')
  widget_control, InfoDraw, get_value=wsetId
  wset, wsetId
  device, /DECOMPOSE
;  if ptr_valid(mainRefStruct.controlWindow) then begin
;    controlWindow=*(mainRefStruct.controlWindow)
;    !P=controlWindow.systemP[1] & !X=controlWindow.systemX[1]
;    !Y=controlWindow.systemY[1] & !Z=controlWindow.systemZ[1]
;  endif
  
END

PRO FMMainGUI::wsetLogoDraw, topBase

  logoDraw=widget_info(self->getTopBase(), FIND='LOGODRAW')
  widget_control, logoDraw, get_value=wsetId
  wset, wsetId
  device,/DECOMPOSE
;  if ptr_valid(mainRefStruct.controlWindow) then begin
;    controlWindow=*(mainRefStruct.controlWindow)
;    !P=controlWindow.systemP[1] & !X=controlWindow.systemX[1]
;    !Y=controlWindow.systemY[1] & !Z=controlWindow.systemZ[1]
;  endif
  
END

; *****************************************************************
; events
; *****************************************************************
PRO FMMainGUI::unlockObsTable

  self->enable
  
END

PRO FMMainGUI::changeStartDate, type, index

  ;print, type, index
  if type eq 'YEAR' then self.mgr->changeStartYear, index
  if type eq 'MONTH' then self.mgr->changeStartMonth, index
  if type eq 'DAY' then self.mgr->changeStartDay, index
  if type eq 'HOUR' then self.mgr->changeStartHour, index
  
END

PRO FMMainGUI::changeEndDate, type, index

  ;print, type, index
  if type eq 'YEAR' then self.mgr->changeEndYear, index
  if type eq 'MONTH' then self.mgr->changeEndMonth, index
  if type eq 'DAY' then self.mgr->changeEndDay, index
  if type eq 'HOUR' then self.mgr->changeEndHour, index
  
END

PRO FMMainGUI::userAllStationsButton, selection

  self.mgr->changeAllStationsSelection, selection
  
END

PRO FMMainGUI::userAllModelsButton, selection

  self.mgr->changeAllModelsSelection, selection
  
END

PRO FMMainGUI::userAllScenariosButton, selection

  self.mgr->changeAllScenariosSelection, selection
  
END

PRO FMMainGUI::userAllParametersButton, selection

  self.mgr->changeAllParametersSelection, selection
  
END

PRO FMMainGUI::userObsModSelection, selection

  self.mgr->changeObsModSelection, selection
  
END

PRO FMMainGUI::userAllAvailableScenarioSelection, selection

  self.mgr->changeAllAvailableScenarioSelection, selection
  
END

PRO FMMainGUI::execRequest, NODISPLAYCHECK=NODISPLAYCHECK

  if self->checkGUIChanges() then self.mgr->execRequest, NODISPLAYCHECK=NODISPLAYCHECK
  
END

PRO FMMainGUI::startBatchMode

  self.mgr->startBatchMode
  
END

FUNCTION FMMainGUI::checkGUIChanges

  ; extra values change from main gui
  elaborationDisp=self.mgr->getElaborationDisplay()
  ;extraValNoMax=3
  ;extraValsUNames='EXTRAVAL#'+strcompress(indgen(extraValNoMax)+1, /REMOVE)
  extraValsUName='EXTRAVAL'
  tFlag=elaborationDisp->getThresholdFlag()
  extraValsWid=widget_info(self->getTopBase(), FIND_BY_UNAME=extraValsUName)
  ; MM May 2015 to avoid user typo
  widget_control, extraValsWid, sensitive=0
  ;extraValsWid=lonarr(extraValNoMax)
  values=0.
  ;disable all
  if tFlag eq 1 then begin
    refValues=elaborationDisp->getThresholdValues()
    extraValNo=n_elements(refValues)
    widget_control, extraValsWid, get_value=refValue
    refValueList=strsplit(refValue, '#', /EXTRACT)
    if n_elements(refValueList) ne extraValNo then begin
      msg=self->dialogMessage(['Check number of requested thresholds!'], title=['Main GUI'])
      return, 0
    endif
    for i=0, extraValNo-1 do begin
      ;for i=0, extraValNo-1 do begin
      ;extraValsWid[i]=widget_info(self->getTopBase(), FIND_BY_UNAME=extraValsUNames[i])
      ;widget_control, extraValsWid[i], get_value=refValue
      refValue=refValueList[i]
      if self.utility->IsNumber(refValue) then begin
        values=[values, float(refValue)]
      endif else begin
        ;widget_control, extraValsWid[i], set_value='**'
        widget_control, extraValsWid, set_value='**'
        msg=self->dialogMessage(['Check thresholds values!'], title=['Main GUI'])
        return, 0
      endelse
    endfor
    values=values[1:*]
    elaborationDisp->setThresholdValues, values
  endif else begin
  endelse
  return, 1
  
END

PRO FMMainGUI::displayModeSelectionGUI

  self.mgr->displayModeSelectionGUI
  
END

PRO FMMainGUI::displayCompositeBatchGUI

  self.mgr->displayCompositeBatchGUI
  
END

PRO FMMainGUI::displayFontEditGUI

  self.mgr->displayFontEditGUI
  
END

PRO FMMainGUI::displayEntitySelectionGUI

  self.mgr->displayEntitySelectionGUI
  
END

PRO FMMainGUI::displayElaborationSelectionGUI

  if self->checkGUIChanges() then self.mgr->displayElaborationSelectionGUI
  
END
; *****************************************************************
; build
; *****************************************************************
PRO FMMainGUI::realize

  self->build
  self->GUI::realize
  self.mgr->changeEndHour, 23
  self->displayLogo
  self->updateDateSection
;self->displayRecognize
  
END

PRO FMMainGUI::build

  ;bestDimensions=self->getBestDimensions()
  title=self->getTitle()
  
  ;self.dimensions[0]=self.dimensions[0]/2
  ;  base = WIDGET_BASE(/COLUMN,scr_XSIZE=self.dimensions[0], $
  ;    scr_ysize=self.dimensions[1], $
  ;    uvalue=self, MBAR=fmMenuBar, $ ;, TLB_FRAME_ATTR=8, $
  ;    /SCROLL, title=title, /TLB_KILL_REQUEST_EVENTS, event_pro=self.eventprefix+'event') ;='fm_destroyWindow') ;KILL_NOTIFY='fm_destroyWindow');, $
  ;  ;TLB_KILL_REQUEST_EVENTS='fm_destroyWindow', )
  ;PHIL SIZE
  base = WIDGET_BASE(/COLUMN, $
    uvalue=self, MBAR=fmMenuBar,scr_XSIZE=self.dimensions[0]*1.05,scr_YSIZE=self.dimensions[1]*1.05,$ ;, TLB_FRAME_ATTR=8, $
    ;    uvalue=self, MBAR=fmMenuBar,$ ;, TLB_FRAME_ATTR=8, $
    /SCROLL, title=title, /TLB_KILL_REQUEST_EVENTS, event_pro=self.eventprefix+'event') ;='fm_destroyWindow') ;KILL_NOTIFY='fm_destroyWindow');, $
  ;TLB_KILL_REQUEST_EVENTS='fm_destroyWindow', )
    
  mainBase = WIDGET_BASE(base,/ROW)
  
  fileMenu=widget_button(fmMenuBar, value='File', UNAME='FILEMENU', /MENU)
  self.benchMarkRootButton=widget_button(fmMenuBar, UVALUE='BENCHMARKTREEROOT', value='Benchmark', UNAME='BENCHMARKSELECT_BTT', event_pro=self.eventPrefix+'benchmarkMenuSelection', /MENU)
  modeMenu=widget_button(fmMenuBar, value='Mode', UNAME='MODEMENU', /MENU, SENSITIVE=1)
  entityMenu=widget_button(fmMenuBar, value='Data selection', UNAME='ENTITYMENU', /MENU)
  elaborationMenu=widget_button(fmMenuBar, value='Analysis', UNAME='DISPLAYMENU', /MENU)
  helpMenu=Widget_Button(fmMenuBar, VALUE='Help', UNAME='HELPMENU', UVALUE='HELPMENU', /MENU)
  
  ;checkIntegrityBtt=widget_Button(fileMenu, VALUE='Data check integrity tool', UNAME='CHECKDATAINTEGRITY', UVALUE='CHECKDATAINTEGRITY_BTT', event_pro=self.eventPrefix+'checkDataIntegrityMenuSelection')
  saveAsImageButton=widget_button(fileMenu, value='Save image', UNAME='SAVEIMG', event_pro=self.eventprefix+'saveImage')
  saveAsImageBlackBackGroundButton=widget_button(fileMenu, value='Save as image (Black Back)', UNAME='SAVEIMGBLK', event_pro=self.eventprefix+'saveImageBlack', sensitive=0)
  saveAsImageWhiteBackGroundButton=widget_button(fileMenu, value='Save as image (White Back)', UNAME='SAVEIMGWHT', event_pro=self.eventprefix+'saveImageWhite', sensitive=0)
  if self.mgr->IsDeveloperUser() then createBatchButton=widget_button(fileMenu, value='BatchComposition', UNAME='CREATEBATCH', event_pro=self.eventprefix+'batchComposition', sensitive=self.mgr->getBenchmarkManagingEnabled())
  if self.mgr->IsDeveloperUser() then saveAsBatchButton=widget_button(fileMenu, value='BatchSave', UNAME='SAVEBATCH', event_pro=self.eventprefix+'saveBatch', sensitive=self.mgr->getBenchmarkManagingEnabled())
  if self.mgr->IsDeveloperUser() then restoreBatch=widget_button(fileMenu, value='BatchRestore', UNAME='RESTOREBATCH', event_pro=self.eventprefix+'restoreBatch', sensitive=self.mgr->getBenchmarkManagingEnabled())
  exitButton=widget_button(fileMenu, value='Exit', UNAME='EXIT', event_pro=self.eventprefix+'destroyWindow')
  
  modeBtt=widget_button(modeMenu, UVALUE=0, value='Select mode', UNAME='MODESELECT_BTT', event_pro=self.eventPrefix+'modeMenuSelection', sensitive=0)
  recognizerBtt=widget_Button(modeMenu, VALUE='Hide/Show Recognizer Info', UNAME='Recognizer', UVALUE='DISPREC', event_pro=self.eventprefix+'recognizeSWITCHBTT')
  
  entitySelectButton=widget_button(entityMenu, UVALUE=0, value='Select data', UNAME='ENTITYSELECT_BTT', event_pro=self.eventprefix+'displayEntity')
  entitySaveButton=widget_button(entityMenu, UVALUE=0, value='Save data', UNAME='ENTITYSAVE_BTT', event_pro=self.eventprefix+'saveEntity')
  entityRestoreButton=widget_button(entityMenu, UVALUE=0, value='Restore data', UNAME='ENTITYLOAD_BTT', event_pro=self.eventprefix+'restoreEntity')
  
  elaborationSelectButton=widget_button(elaborationMenu, UVALUE=0, value='Select Analysis', UNAME='ELABORATIONSELECT_BTT', event_pro=self.eventprefix+'displayElaboration')
  elaborationSaveButton=widget_button(elaborationMenu, UVALUE=0, value='Save Analysis', UNAME='ELABORATIONSAVE_BTT', event_pro=self.eventprefix+'saveElaboration')
  elaborationRestoreButton=widget_button(elaborationMenu, UVALUE=0, value='Restore Analysis', UNAME='ELABORATIONLOAD_BTT', event_pro=self.eventprefix+'restoreElaboration')
  
  helpDescBtt=widget_Button(helpMenu, VALUE='Help file', UNAME='HELPBTT', UVALUE='OPENHELP_BTT', event_pro=self.eventPrefix+'helpMenuSelection')
 ;KeesC 06FEB2016
  saveDumpFileButton=widget_button(helpMenu, value='Edit DumpFile.dat',UNAME='EDITDUMP',event_pro=self.eventprefix+'editDumpFile')
  
  mInfo=self.mgr->getModelInfo()
  if strlowcase(mInfo.frequency) eq 'hour' then interactiveFormatConversionBtt=widget_Button(helpMenu, VALUE='Interactive format conversion tool (Model)', UNAME='FORMATCONVERSION', UVALUE='FORMATCONVERSION_BTT', event_pro=self.eventPrefix+'runInteractiveMenuSelection', /SEPARATOR)
  ;interactiveFormatConversionBtt=widget_Button(helpMenu, VALUE='Interactive format conversion tool (Model)', UNAME='FORMATCONVERSION', UVALUE='FORMATCONVERSION_BTT', event_pro=self.eventPrefix+'runInteractiveMenuSelection', /SEPARATOR)
  ;autoFormatConversionBtt=widget_Button(helpMenu, VALUE='Convert observation (csv to cdf)', UNAME='FORMATCONVERSION', UVALUE='FORMATCONVERSION_BTT', event_pro=self.eventPrefix+'runBatchMenuSelection')
  
  if self.mgr->IsDeveloperUser() then testPlotIntegrityBtt=widget_Button(helpMenu, VALUE='Plot quality test integrity tool', UNAME='TESTPLOTQUALITYINTEGRITY', UVALUE='TESTQUALITYINTEGRITY_BTT', event_pro=self.eventPrefix+'testQualityMenuSelection', /SEPARATOR)
  ;if self.mgr->IsAdvancedUser() then checkIntegrityBtt=widget_Button(helpMenu, VALUE='Data check integrity tool', UNAME='CHECKDATAINTEGRITY', UVALUE='CHECKDATAINTEGRITY_BTT', event_pro=self.eventPrefix+'checkDataIntegrityMenuSelection')
  checkIntegrityBtt=widget_Button(helpMenu, VALUE='Data check integrity tool', UNAME='CHECKDATAINTEGRITY', UVALUE='CHECKDATAINTEGRITY_BTT', event_pro=self.eventPrefix+'checkDataIntegrityMenuSelection')
  
  wwwPageBtt=widget_Button(helpMenu, VALUE='DELTA WWW', UNAME='Download site...', UVALUE='OPENWWW_BTT', event_pro=self.eventPrefix+'downloadMenuSelection')
  aboutBtt=widget_Button(helpMenu, VALUE='About...', UNAME='About', UVALUE='DISPMAP', event_pro=self.eventprefix+'aboutSplash')
  ;disclaimerBtt=widget_Button(helpMenu, VALUE='Disclaimer...', UNAME='Disclaimer', UVALUE='DISPMAP', event_pro=self.eventprefix+'disclaimer')
  foundExeBtt=widget_Button(helpMenu, VALUE='Find external applications paths...', UNAME='UpdateExe', UVALUE='DISPMAP', event_pro=self.eventprefix+'configureExecutable')
  if self.mgr->IsDeveloperUser() then fontEditBtt=widget_Button(helpMenu, VALUE='Configure font(s)...', UNAME='ConfFonts', UVALUE='CONFFONTS', event_pro=self.eventprefix+'fontEdit')
  licenseBtt=widget_Button(helpMenu, VALUE='Licence...', UNAME='License', UVALUE='DISPMAP', event_pro=self.eventprefix+'license')
  mBase=WIDGET_BASE(mainbase,xpad=0, ypad=0,space=0,/COLUMN)
  logoBase= WIDGET_BASE(mBase,xpad=0, ypad=0,space=0,/ROW, /ALIGN_CENTER)
  subBase1 = WIDGET_BASE(mBase,xpad=0, ypad=0,space=0,/ROW)
  subBase11 = WIDGET_BASE(subBase1,xpad=0, ypad=0,space=0,/COLUMN)
  subBase12 = WIDGET_BASE(subBase1,xpad=0, ypad=0,space=0,/COLUMN)
  subBase121 = WIDGET_BASE(subBase12,xpad=0, ypad=0,space=0,/COLUMN)
  subBase122 = WIDGET_BASE(subBase12,xpad=0, ypad=0,space=0,/COLUMN)
  
  self->buildBenchMarkMenuTree
  self->buildLogoSection, logoBase, [self.dimensions[0], 50]
  self->buildExecSection, subBase11
  self->buildDataSummarySection, subBase11
  self->buildMainDrawSection, subBase121
  self->buildGraphInfoSection, subBase122
  
  statusBarMain = widget_label(base, UNAME='STATUSBARMAIN', $
    XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=self.dimensions[0]+5 ,SCR_YSIZE= self->getTitleYDim(),/ALIGN_LEFT, $
    VALUE='---Information about application---', /SUNKEN_FRAME, font=self.labelFont)
    
  statusBarMap=widget_info(base, FIND='STATUSBARMAP')
  statusBarGraph=widget_info(base, FIND='STATUSBARGRAPH')
  
  self->setTopBase, base
  xmanager, 'fairmode', base, /JUST_REG
  
END

PRO FMMainGUI::buildExecSection, base

  bttBase=widget_base(base, xpad=0, ypad=0, space=0, /ROW)
  
  entityBtt=widget_button(bttBase, value='Data selection', UNAME='ENTITYSELECT_BTT', event_pro=self.eventprefix+'displayEntity')
  analysisBtt=widget_button(bttBase, value='Analysis', UNAME='ELABORATIONSELECT_BTT', event_pro=self.eventprefix+'displayElaboration')
  execBtt=widget_button(bttBase, value='Execute', UNAME='EXEC', event_pro=self.eventprefix+'execRequest')
  
END

FUNCTION FMMainGUI::getSmallLabelXDim

  return, self.dimensions[0]-20
  
END

FUNCTION FMMainGUI::getSmallLabelYDim

  return, 20
  
END

FUNCTION FMMainGUI::getSubTitleYDim

  return, 20
  
END

FUNCTION FMMainGUI::getValueXDim

  return, (self->xSummarySize()-12)*.68
  
END

FUNCTION FMMainGUI::getValueYDim

  return, self->getLabelPixHeight(20)
  
END

FUNCTION FMMainGUI::getSubTitleXDim

  return, (self->xSummarySize()-12)-self->getValueXDim()
  
END

FUNCTION FMMainGUI::getTitleYDim

  return, 20
  
END

FUNCTION FMMainGUI::getTitleXDim

  return, self.dimensions[0]/3
  
END

FUNCTION FMMainGUI::xSummarySize


  return, self.dimensions[0]-self->xGraphSize()
  
END

FUNCTION FMMainGUI::ySummarySize

  return, self.dimensions[1]*1.
  
END

FUNCTION FMMainGUI::xGraphSize

  return, self.dimensions[0]*.7
  
END

FUNCTION FMMainGUI::yGraphSize

  return, self.dimensions[1]*.7
  
END

FUNCTION FMMainGUI::xInfoGraphSize

  return, self->xGraphSize()
  
END

; KeesC 04FEB2014
FUNCTION FMMainGUI::getResourceDir, WITHSEPARATOR=WITHSEPARATOR

  return, self.mgr->getResourceDir(WITHSEPARATOR=WITHSEPARATOR)
  
END

FUNCTION FMMainGUI::getHomeDir, WITHSEPARATOR=WITHSEPARATOR
  cd,current=dir
  ;dir=strcompress(direct,/remove_all)
  ;dir="C:\work\informatica\sviluppo\idl_projects\POMI"
  ;dir="D:\FairModeApp"
  dir=self.applicationRoot
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
END

FUNCTION FMMainGUI::yInfoGraphSize

  return, self.dimensions[1]-self->yGraphSize()
  
END

FUNCTION FMMainGUI::getBestDimensions

  return, get_screen_size(RESOLUTION=resolution)*0.85
  
END

PRO FMMainGUI::buildSelectDateCmp, base, totSize, label, id, eventPro=eventPro, senslist=senslist

  if n_elements(senslist) ne 4 then senslist=[1b,1,1,1]
  elements=total(senslist)
  ;print, 'xSize; ', totSize
  id=strupcase(id)
  labelXSize=100
  comboXSize=((totSize-50)/(elements*2))>33
  labelYSize=25
  
  compoundBase=widget_base(base, ypad=0, space=0, /ROW)
  
  text=widget_label(compoundBase, value=label, scr_xsize=40, /ALIGN_LEFT)
  
  ;hours=strtrim(indgen(24), 1)
  days=strtrim(indgen(31)+1, 1)
  months=strtrim(indgen(12)+1, 1)
  ;years=strtrim(indgen(20)+1990, 1)
  
  selectDateBase=widget_base(compoundBase, /ROW)
  
  ;elementDateBase=widget_base(selectDateBase, /COLUMN, sensitive=sensList[0])
  ;hourL=widget_label(elementDateBase, value='Hour', SCR_XSIZE=comboXSize, SCR_YSIZE=labelYSize)
  ;hourCB=widget_combobox(elementDateBase, value=hours, UNAME='HOUR'+id, UVALUE='HOUR'+id, SCR_XSIZE=comboXSize, SCR_YSIZE=labelYSize, event_pro=eventPro)
  
  elementDateBase=widget_base(selectDateBase, /ROW, sensitive=sensList[1])
  dayL=widget_label(elementDateBase, value='Day', SCR_XSIZE=comboXSize, SCR_YSIZE=labelYSize)
  dayCB=widget_combobox(elementDateBase, value=days, UVALUE='DAY'+id, UNAME='DAY'+id, SCR_XSIZE=comboXSize, SCR_YSIZE=labelYSize, event_pro=eventPro)
  
  elementDateBase=widget_base(selectDateBase, /ROW, sensitive=sensList[2])
  monthL=widget_label(elementDateBase, value='Month', SCR_XSIZE=comboXSize, SCR_YSIZE=labelYSize)
  monthCB=widget_combobox(elementDateBase, value=months, UVALUE='MONTH'+id, UNAME='MONTH'+id, SCR_XSIZE=comboXSize, SCR_YSIZE=labelYSize, event_pro=eventPro)
  
;elementDateBase=widget_base(selectDateBase, /COLUMN, sensitive=sensList[3])
;yearL=widget_label(elementDateBase, value='Year', SCR_XSIZE=comboXSize, SCR_YSIZE=labelYSize)
;yearCB=widget_combobox(elementDateBase, value=years, UVALUE='YEAR'+id, UNAME='YEAR'+id, SCR_XSIZE=comboXSize+15, SCR_YSIZE=labelYSize, event_pro=eventPro)
  
END

PRO FMMainGUI::buildGraphInfoSection, base, dims

  graphDrawBase = Widget_Base(base, UNAME='mainDrawBase', $
    XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=self->xInfoGraphSize() ,SCR_YSIZE=self->yInfoGraphSize(), $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=2, /COLUMN, /ALIGN_CENTER)
    
  graphDraw = WIDGET_DRAW(graphDrawBase,RETAIN=2,SCR_XSIZE=self->xInfoGraphSize(),SCR_YSIZE=self->yInfoGraphSize()/2, $
    UNAME='GRAPHDRAW')
    
  ;KeesC 30JAN2014
  fileStr='startup.ini'
  dirResource=self->getResourceDir(/WITH)
  openr,unit,dirResource+'MyDeltaInput.dat',/get_lun,error=err
  if err eq 0 then begin
    txt=' '
    readf,unit,txt
    fileStr=strcompress(txt,/remove_all)
    close, unit
    free_lun, unit
  endif
  fileMod='modeling'
  openr,unit,dirResource+'MyDeltaInput.dat',/get_lun,error=err
  if err eq 0 then begin
    txt=' '
    readf,unit,txt
    readf,unit,txt
    fileMod=strcompress(txt,/remove_all)
    close,unit
    free_lun, unit
  endif
  fileMon='monitoring'
  openr,unit,dirResource+'MyDeltaInput.dat',/get_lun,error=err
  if err eq 0 then begin
    txt=' '
    readf,unit,txt
    readf,unit,txt
    readf,unit,txt
    fileMon=strcompress(txt,/remove_all)
    close,unit
    free_lun, unit
  endif
  
  infoSMM='['+fileStr+'   \\'+fileMod+'   \\'+fileMon+']'
  titleLabel = Widget_Label(graphDrawBase, UNAME='STATUSBARGRAPH', $
    XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=self->xInfoGraphSize() ,SCR_YSIZE=self->getTitleYDim() ,/ALIGN_LEFT, $
    VALUE='---Info input data =>  '+infoSMM, /SUNKEN_FRAME, font=self.labelFont)
    
END

PRO FMMainGUI::displayLogo

  logo=self.mgr->getLogoImage(true=true)
  logo=congrid(logo,3,self.dimensions[0],50,/interp)
  self->wsetLogoDraw
  tv, logo, true=true+1
  
END

PRO FMMainGUI::buildLogoSection, base, dims

  logoDrawBase = Widget_Base(base, UNAME='logoDrawBase', $
    XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=dims[0], SCR_YSIZE=50, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /COLUMN, /ALIGN_CENTER)
    
    
  logoDraw = WIDGET_DRAW(logoDrawBase,RETAIN=2,SCR_XSIZE=dims[0], SCR_YSIZE=dims[1], $
    UNAME='LOGODRAW')
    
END

PRO FMMainGUI::buildMainDrawSection, base, dims

  mainDrawBase = Widget_Base(base, UNAME='mainDrawBase', $
    XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=self->xGraphSize(), SCR_YSIZE=self->yGraphSize(), $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=2, /COLUMN, /ALIGN_CENTER)
    
    
  mainDraw = WIDGET_DRAW(mainDrawBase,RETAIN=2,SCR_XSIZE=self->xGraphSize(),SCR_YSIZE=self->yGraphSize()-self->getTitleYDim(), $
    /BUTTON_EVENTS, event_pro=self.eventPrefix+'mapDrawMouse', UNAME='MAINDRAW')
  ;/MOTION_EVENTS, /BUTTON_EVENTS, event_pro=self.eventPrefix+'mapDrawMouse', UNAME='MAINDRAW')
    
  titleLabel = Widget_Label(mainDrawBase, UNAME='STATUSBARMAP', $
    XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=self->xGraphSize() ,SCR_YSIZE=self->getTitleYDim() ,/ALIGN_LEFT, $
    VALUE='---Info about plot data---', /SUNKEN_FRAME, font=self.labelFont)
    
END

PRO FMMainGUI::buildModelSection, base

  titleBase = Widget_Base(base, UNAME='WID_BASE_1' , $
    XOFFSET=0, YOFFSET=0, /ALIGN_CENTER, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
    
  captionTitle = Widget_Label(titleBase, UNAME='WID_LABEL_0', $
    SCR_XSIZE=self->xSummarySize() ,SCR_YSIZE=labelY ,/ALIGN_CENTER, $
    VALUE='Run (Model/Scenario) Info', FONT=self.titleFont)
    
  baseFor5 = Widget_Base(base, UNAME='WID_BASE_2', $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=1, /COLUMN)
    
  base1of5 = Widget_Base(baseFor5, UNAME='WID_BASE_3' ,XOFFSET=7, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
    
    
  WID_LABEL_1 = Widget_Label(base1of5, UNAME='', $
    SCR_XSIZE=self->getSubTitleXDim() ,SCR_YSIZE=smallTextY ,/ALIGN_RIGHT, $
    VALUE='Models:', font=self.labelFont)
    
  modelsText = Widget_Text(base1of5, UNAME='MODELTXT' ,XOFFSET=29, $
    SCR_XSIZE=self->getValueXDim() ,SCR_YSIZE=self->getValueYDim() ,SENSITIVE=1 ,/ALL_EV $
    , font=self.textFont, /SCROLL, /WRAP)
    
  base2of5 = Widget_Base(baseFor5, UNAME='WID_BASE_3' ,XOFFSET=7, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
    
    
  WID_LABEL_1 = Widget_Label(base2of5, UNAME='', $
    SCR_XSIZE=self->getSubTitleXDim() ,SCR_YSIZE=smallTextY ,/ALIGN_RIGHT, $
    VALUE='Scenarios:', font=self.labelFont)
    
  scenariosText = Widget_Text(base2of5, UNAME='SCENARIOTXT' ,XOFFSET=29, $
    SCR_XSIZE=self->getValueXDim() ,SCR_YSIZE=self->getValueYDim() ,SENSITIVE=1 ,/ALL_EV $
    , font=self.textFont, /SCROLL, /WRAP)
    
  base3of5 = Widget_Base(baseFor5, UNAME='WID_BASE_3' ,XOFFSET=7, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
    
  WID_LABEL_1 = Widget_Label(base3of5, UNAME='', $
    SCR_XSIZE=self->getSubTitleXDim() ,SCR_YSIZE=self->getSubTitleYDim()*2 ,/ALIGN_RIGHT, $
    VALUE='Runs:', font=self.labelFont)
    
  runsText = Widget_Text(base3of5, UNAME='RUNTXT' ,XOFFSET=29, $
    SCR_XSIZE=self->getValueXDim() ,SCR_YSIZE=self->getValueYDim()*2 ,SENSITIVE=1 ,/ALL_EV $
    , font=self.textFont, /SCROLL, /WRAP)
    
  base4of5 = Widget_Base(baseFor5, UNAME='WID_BASE_4', $
    XOFFSET=0 ,YOFFSET=0, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
    
  observedModelFlagBase = Widget_Base(base4of5, UNAME='WID_BASE_4', $
    XOFFSET=0 ,YOFFSET=0, /NONEXCLUSIVE, event_pro=self.eventprefix+'obsModMainBtt', $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
    
  observedModelFlagButton= widget_button(observedModelFlagBase, $
    XOFFSET=0 ,YOFFSET=0, VALUE='MOD without OBS', UNAME='USEOBSMODBTT', event_pro=self.eventprefix+'obsModMainBtt', $$
    SCR_XSIZE=self->xSummarySize()-20 ,SCR_YSIZE=self->getSubTitleYDim(), sensitive=self.mgr->isAdvancedUser())
    
  ;widget_control, observedModelFlagButton, set_button=self.mgr->isAdvancedFilter()
  widget_control, observedModelFlagButton, set_button=0
  
  base5of5 = Widget_Base(baseFor5, UNAME='WID_BASE_5', $
    XOFFSET=0 ,YOFFSET=0, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
    
  scenarioBasicFlagBase = Widget_Base(base5of5, UNAME='WID_BASE_5', $
    XOFFSET=0 ,YOFFSET=0, /NONEXCLUSIVE, event_pro=self.eventprefix+'obsModMainBtt', $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
    
  scenarioBasicFlagButton= widget_button(scenarioBasicFlagBase, $
    XOFFSET=0 ,YOFFSET=0, VALUE='All available scenario(s)', UNAME='SHOWBASESCENBTT', event_pro=self.eventprefix+'allAvailableScenarioButton', $$
    SCR_XSIZE=self->xSummarySize()-20 ,SCR_YSIZE=self->getSubTitleYDim(), sensitive=self.mgr->isAdvancedUser())
    
  widget_control, scenarioBasicFlagButton, set_button=0
  
END

PRO FMMainGUI::buildObservedSection, base

  titleBase = Widget_Base(base, UNAME='WID_BASE_1' , $
    XOFFSET=0, YOFFSET=0, /ALIGN_CENTER, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
    
  captionTitle = Widget_Label(titleBase, UNAME='WID_LABEL_0', $
    SCR_XSIZE=self->xSummarySize() ,SCR_YSIZE=labelY ,/ALIGN_CENTER, $
    VALUE='Observation Info', FONT=self.titleFont)
    
  baseFor3 = Widget_Base(base, UNAME='WID_BASE_2', $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /COLUMN)
    
  base1of3 = Widget_Base(baseFor3, UNAME='WID_BASE_3' ,XOFFSET=7, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
    
  WID_LABEL_1 = Widget_Label(base1Of3, UNAME='WID_LABEL_1', $
    SCR_XSIZE=self->getSubTitleXDim() ,SCR_YSIZE=self->getSubTitleYDim()*2 ,/ALIGN_LEFT, $
    VALUE='single obs:', font=self.labelFont)
    
  singleObservationTxt = Widget_Text(base1Of3, UNAME='SINGLEOBSTXT' ,XOFFSET=29, $
    SCR_XSIZE=self->getValueXDim() ,SCR_YSIZE=self->getValueYDim()*2 ,SENSITIVE=1 ,/ALL_EV $
    , font=self.textFont, /SCROLL, /WRAP)
    
  base2of3 = Widget_Base(baseFor3, UNAME='WID_BASE_3' ,XOFFSET=7, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
    
  WID_LABEL_1 = Widget_Label(base2Of3, UNAME='WID_LABEL_1', $
    SCR_XSIZE=self->getSubTitleXDim() ,SCR_YSIZE=self->getSubTitleYDim()*2 ,/ALIGN_LEFT, $
    VALUE='group obs:', font=self.labelFont, sensitive=0)
    
  groupObservationTxt = Widget_Text(base2Of3, UNAME='GROUPOBSTXT' ,XOFFSET=29, $
    SCR_XSIZE=self->getValueXDim() ,SCR_YSIZE=self->getValueYDim()*2 ,/ALL_EV $
    , font=self.textFont, /SCROLL, /WRAP, sensitive=0)
    
  base3of3 = Widget_Base(baseFor3, UNAME='WID_BASE_3' ,XOFFSET=7, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0,/ALIGN_CENTER, /ROW)
    
  WID_BUTTON_1 = Widget_Button(base3of3, UNAME='EXPANDOBSBTT', $
    SCR_YSIZE=self->getValueYDim() ,SCR_XSIZE=self->getValueXDim() ,/ALIGN_CENTER, $
    VALUE='View Details', font=self.labelFont, event_pro=self.eventPrefix+'userViewObsDetails', SENSITIVE=0)
    
END

PRO FMMainGUI::buildExtraValuesSubSection, base, prefixlabel, title, elements

  ;smalltextX=self->xSummarySize()/(2*elements)
  smalltextX=self->xSummarySize()/2
  smallTextY=20
  
  captionSubTitle = Widget_Label(base, UNAME='WID_LABEL_0', $
    SCR_XSIZE=self->xSummarySize() ,SCR_YSIZE=labelY ,/ALIGN_LEFT, $
    VALUE=Title, FONT=self.textFont)
    
  baseForElements = Widget_Base(base, UNAME='EXTRAVALSBASE', $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=1, /ROW)
    
  ;  UNAME='EXTRAVAL#'
  ;  for i=1, elements do begin
  UNAME='EXTRAVAL'
  
  ;title=prefixlabel+strcompress(i, /REMOVE)
  title=prefixlabel
  aBase = Widget_Base(baseForElements, UNAME='WID_BASE_3' ,XOFFSET=7, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
    
    
  WID_LABEL_1 = Widget_Label(aBase, UNAME='', $
    SCR_XSIZE=self->getSubTitleXDim() ,SCR_YSIZE=self->getSubTitleYDim()*2, $
    ;SCR_XSIZE=smalltextX, /ALIGN_RIGHT, $
    VALUE=title+':', font=self.labelFont)
    
    
;  WID_TEXT_0 = Widget_Text(aBase, UNAME=UNAME+strcompress(i, /REMOVE) ,XOFFSET=29, $
;    SCR_XSIZE=smallTextX-3 ,SCR_YSIZE=smallTextY*2 ,SENSITIVE=1 ,/ALL_EV $
;    ;SCR_XSIZE=smallTextX-3, SENSITIVE=1 ,/ALL_EV $
;    , font=self.textFont, value='**')
  WID_TEXT_0 = Widget_Text(aBase, UNAME=UNAME, $
    SCR_XSIZE=self->getValueXDim() ,SCR_YSIZE=smallTextY*2 ,SENSITIVE=1 ,/ALL_EV $
    ;SCR_XSIZE=smallTextX-3, SENSITIVE=1 ,/ALL_EV $
    , font=self.textFont, value='**', /SCROLL, /WRAP)
;print, UNAME+strcompress(i, /REMOVE)
    
;endfor

END

PRO FMMainGUI::buildElaborationParameterSection, base

  thresholdPrefix='EXTRA' & thresholdTitle='Extra val#' & thresholdElements=3
  gcocPrefix='GCOC' & gcocTitle='Goals, criteria & OC' & gcocElements=2
  
  titleBase = Widget_Base(base, UNAME='WID_BASE_1' , $
    XOFFSET=0, YOFFSET=0, /ALIGN_CENTER, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
    
  captionTitle = Widget_Label(titleBase, UNAME='WID_LABEL_0', $
    SCR_XSIZE=self->xSummarySize() ,SCR_YSIZE=labelY ,/ALIGN_CENTER, $
    VALUE='Elaboration/Parameter Info', FONT=self.titleFont)
    
  baseFor5 = Widget_Base(base, UNAME='WID_BASE_2', $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=1, /COLUMN)
    
  base1of5 = Widget_Base(baseFor5, UNAME='WID_BASE_3' ,XOFFSET=7, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
    
    
  WID_LABEL_1 = Widget_Label(base1of5, UNAME='', $
    SCR_XSIZE=self->getSubTitleXDim() ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
    VALUE='Statistic:', font=self.labelFont)
    
  elabNameText = Widget_Text(base1of5, UNAME='ELABNAMETXT' ,XOFFSET=29, $
    SCR_XSIZE=self->getValueXDim() ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
    , font=self.textFont)
    
  base2of5 = Widget_Base(baseFor5, UNAME='WID_BASE_3' ,XOFFSET=7, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
    
  WID_LABEL_1 = Widget_Label(base2of5, UNAME='', $
    SCR_XSIZE=self->getSubTitleXDim() ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
    VALUE='Diagram:', font=self.labelFont)
    
  elabTypeText = Widget_Text(base2of5, UNAME='DIAGRAMTXT' ,XOFFSET=29, $
    SCR_XSIZE=self->getValueXDim() ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
    , font=self.textFont)
    
  ;  base3of5 = Widget_Base(baseFor5, UNAME='WID_BASE_3' ,XOFFSET=7, $
  ;    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
  ;
  ;  WID_LABEL_1 = Widget_Label(base3of5, UNAME='', $
  ;    SCR_XSIZE=self->getSubTitleXDim() ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
  ;    VALUE='Axis:', font=self.labelFont)
  ;
  ;  elabPlotText = Widget_Text(base3of5, UNAME='ELABAXISTXT' ,XOFFSET=29, $
  ;    SCR_XSIZE=self->getValueXDim() ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
  ;    , font=self.textFont)
    
  base4of5 = Widget_Base(baseFor5, UNAME='WID_BASE_3' ,XOFFSET=7, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
    
  WID_LABEL_1 = Widget_Label(base4of5, UNAME='', $
    SCR_XSIZE=self->getSubTitleXDim() ,SCR_YSIZE=smallTextY ,/ALIGN_RIGHT, $
    VALUE='Var(s):', font=self.labelFont)
    
  parametersText = Widget_Text(base4of5, UNAME='PARAMETERSTXT' ,XOFFSET=29, $
    SCR_XSIZE=self->getValueXDim() ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
    , font=self.textFont)
    
;  baseThresholds = Widget_Base(baseFor5, UNAME='WID_BASE_3' ,XOFFSET=7, $
;    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /COLUMN)
  baseThresholds = Widget_Base(baseFor5, UNAME='WID_BASE_3' ,XOFFSET=0, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /COLUMN)
    
  self->buildExtraValuesSubSection, baseThresholds, thresholdTitle, thresholdPrefix, thresholdElements
  
  ; base of gcoc inactive until explain his use...
  gcocBase = Widget_Base(baseFor5, UNAME='WID_BASE_4', $
    XOFFSET=0 ,YOFFSET=0, /NONEXCLUSIVE, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW, SENSITIVE=0)
    
  gcocFlagButton= widget_button(gcocBase, $
    XOFFSET=0 ,YOFFSET=0, VALUE='Goals,Criteria...', UNAME='GOALSBTT', $
    SCR_XSIZE=self->xSummarySize()-20 ,SCR_YSIZE=self->getSubTitleYDim())
    
  base5of4 = Widget_Base(baseFor5, UNAME='WID_BASE_3' ,XOFFSET=7, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
    
  base1of2 = Widget_Base(base5of4, UNAME='WID_BASE_3' ,XOFFSET=7, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
    
  WID_LABEL_1 = Widget_Label(base1of2, UNAME='', $
    SCR_XSIZE=self->getSubTitleXDim()/2 ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
    VALUE='Stat:', font=self.labelFont)
    
  statText = Widget_Text(base1of2, UNAME='GROUPBYSTATTXT' ,XOFFSET=29, $
    SCR_XSIZE=self->getValueXDim()/2 ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
    , font=self.textFont)
    
  base2of2 = Widget_Base(base5of4, UNAME='WID_BASE_3' ,XOFFSET=7, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
    
  WID_LABEL_1 = Widget_Label(base2of2, UNAME='', $
    SCR_XSIZE=self->getSubTitleXDim()/2 ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
    VALUE='Time:', font=self.labelFont)
    
  statText = Widget_Text(base2of2, UNAME='GROUPBYTIMETXT' ,XOFFSET=29, $
    SCR_XSIZE=self->getValueXDim()/2 ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
    , font=self.textFont)
    
END

PRO FMMainGUI::buildDayDateTimeSection, base

  senslist=[1b,1,1,1]
  presencelist=[1b,1,1,0]
  elements=total(presencelist)
  ;print, 'xSize; ', totSize
  id=''
  ;comboXSize=((self->getParameterAndGroupSectionXSize())/(elements*2))>33
  ;labelYSize=self->getLabelYSize()
  
  compoundBase=widget_base(base, ypad=0, space=0, /ROW)
  
  text=widget_label(compoundBase, value='Date', scr_xsize=40, /ALIGN_LEFT)
  
  hours=strtrim(indgen(24), 1)
  days=strtrim(indgen(31)+1, 1)
  months=strtrim(indgen(12)+1, 1)
  years=strtrim(indgen(20)+2000, 1)
  startPrefix='Start'
  endPrefix='End'
  startEventPro=self.eventPrefix+'userUpdateStartDate'
  endEventPro=self.eventPrefix+'userUpdateEndDate'
  ;postfix='Selection'
  
  selectDateBase=widget_base(compoundBase, /ROW)
  
  if presenceList[0] then begin
    id='Hour'
    elementDateBase=widget_base(selectDateBase, /COLUMN, sensitive=sensList[0])
    hourL=widget_label(elementDateBase, value='Hour', SCR_XSIZE=comboXSize, SCR_YSIZE=labelYSize)
    startHourCB=widget_combobox(elementDateBase, value=hours, UNAME=startPrefix+id, $
      SCR_XSIZE=comboXSize, SCR_YSIZE=labelYSize, UVALUE='HOUR', $
      event_pro=startEventPro)
    endHourCB=widget_combobox(elementDateBase, value=hours, UNAME=endPrefix+id, $
      SCR_XSIZE=comboXSize, SCR_YSIZE=labelYSize, UVALUE='HOUR', $
      event_pro=endEventPro)
  ;print, startPrefix+id, endPrefix+id
  endif
  
  if presenceList[1] then begin
    id='Day'
    elementDateBase=widget_base(selectDateBase, /COLUMN, sensitive=sensList[1])
    dayL=widget_label(elementDateBase, value='Day', SCR_XSIZE=comboXSize, SCR_YSIZE=labelYSize)
    dayCB=widget_combobox(elementDateBase, value=days, UNAME=startPrefix+id, $
      SCR_XSIZE=comboXSize, SCR_YSIZE=labelYSize, UVALUE='DAY', $
      event_pro=startEventPro)
    dayCB=widget_combobox(elementDateBase, value=days, UNAME=endPrefix+id, $
      SCR_XSIZE=comboXSize, SCR_YSIZE=labelYSize, UVALUE='DAY', $
      event_pro=endEventPro)
  ;print, startPrefix+id, endPrefix+id
  endif
  
  if presenceList[2] then begin
    id='Month'
    elementDateBase=widget_base(selectDateBase, /COLUMN, sensitive=sensList[2])
    monthL=widget_label(elementDateBase, value='Month', SCR_XSIZE=comboXSize, SCR_YSIZE=labelYSize)
    monthCB=widget_combobox(elementDateBase, value=months, UNAME=startPrefix+id, $
      SCR_XSIZE=comboXSize, SCR_YSIZE=labelYSize, UVALUE='MONTH', $
      event_pro=startEventPro)
    monthCB=widget_combobox(elementDateBase, value=months, UNAME=endPrefix+id, $
      SCR_XSIZE=comboXSize, SCR_YSIZE=labelYSize, UVALUE='MONTH', $
      event_pro=endEventPro)
  ;print, startPrefix+id, endPrefix+id
  endif
  
  if presenceList[3] then begin
    id='Year'
    elementDateBase=widget_base(selectDateBase, /COLUMN, sensitive=sensList[3])
    yearL=widget_label(elementDateBase, value='Year', SCR_XSIZE=comboXSize, SCR_YSIZE=labelYSize)
    yearCB=widget_combobox(elementDateBase, value=years, UNAME=startPrefix+id, $
      SCR_XSIZE=comboXSize+15, SCR_YSIZE=labelYSize, UVALUE='YEAR', $
      event_pro=startEventPro)
    yearCB=widget_combobox(elementDateBase, value=years, UNAME=endPrefix+id, $
      SCR_XSIZE=comboXSize+15, SCR_YSIZE=labelYSize, UVALUE='YEAR', $
      event_pro=endEventPro)
  ;print, startPrefix+id, endPrefix+id
  endif
  
END

PRO FMMainGUI::buildDateTimeSection, base

  titleBase = Widget_Base(base, UNAME='WID_BASE_1' , $
    XOFFSET=0, YOFFSET=0, /ALIGN_CENTER, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
    
  captionTitle = Widget_Label(titleBase, UNAME='WID_LABEL_0', $
    SCR_XSIZE=self->xSummarySize() ,SCR_YSIZE=labelY ,/ALIGN_CENTER, $
    VALUE='Date/Period Selection', FONT=self.titleFont)
    
    
  baseFor4 = Widget_Base(base, UNAME='WID_BASE_2', $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=1, /COLUMN)
    
  base1of4 = Widget_Base(baseFor4, UNAME='WID_BASE_3' ,XOFFSET=7, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
    
  WID_LABEL_1 = Widget_Label(base1of4, UNAME='', $
    SCR_XSIZE=self->getSubTitleXDim() ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
    VALUE='Season:', font=self.labelFont)
    
  seasonText = Widget_Text(base1of4, UNAME='SEASONTXT' ,XOFFSET=29, $
    SCR_XSIZE=self->getValueXDim() ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
    , font=self.textFont)
    
  base2of4 = Widget_Base(baseFor4, UNAME='WID_BASE_3' ,XOFFSET=7, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
    
  WID_LABEL_1 = Widget_Label(base2of4, UNAME='', $
    SCR_XSIZE=self->getSubTitleXDim() ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
    VALUE='Hour type:', font=self.labelFont)
    
  hourTypeText = Widget_Text(base2of4, UNAME='HOURTYPETXT' ,XOFFSET=29, $
    SCR_XSIZE=self->getValueXDim() ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
    , font=self.textFont)
    
  ; copy & paste start
  insideBase=widget_base(baseFor4, SCR_XSIZE=xSize, xpad=0, $
    ypad=0, XOFFSET=0 ,YOFFSET=0, /COLUMN, /ALIGN_CENTER)
    
  titleBase = Widget_Base(insideBase, UNAME='WID_BASE_1' , $
    XOFFSET=0, YOFFSET=0, /ALIGN_CENTER, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /COLUMN)
    
  label='Start'
  dateBase=widget_base(insideBase, /COLUMN, $
    ypad=0, XOFFSET=0 ,YOFFSET=0, xpad=0, /ALIGN_CENTER)
    
  self->buildDayDateTimeSection, dateBase
  
END

PRO FMMainGUI::buildStatsSection, base

  titleBase = Widget_Base(base, UNAME='WID_BASE_1' , $
    XOFFSET=0, YOFFSET=0, /ALIGN_CENTER, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
    
  captionTitle = Widget_Label(titleBase, UNAME='WID_LABEL_0', $
    SCR_XSIZE=self->xSummarySize() ,SCR_YSIZE=labelY ,/ALIGN_CENTER, $
    VALUE='Stats', FONT=self.titleFont)
    
END

PRO FMMainGUI::buildDataSummarySection, base, dims

  interactionBase = Widget_Base(base, UNAME='interactionBase', $
    XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=self->xSummarySize() ,SCR_YSIZE=self->ySummarySize(), $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=1, /COLUMN, /ALIGN_LEFT)
    
  modelInfoBase = Widget_Base(interactionBase, UNAME='modelInfoBase', $
    FRAME=1 ,XOFFSET=0 ,YOFFSET=0, /ALIGN_LEFT, $
    TITLE='IDL' ,SPACE=0 ,XPAD=2 ,YPAD=1, /COLUMN,SCR_XSIZE=self->xSummarySize())
    
  observedInfoBase = Widget_Base(interactionBase, $
    UNAME='observedInfoBase' ,FRAME=1 ,XOFFSET=0 ,YOFFSET=0, $
    SCR_XSIZE=self->xSummarySize() ,TITLE='IDL' ,SPACE=0 ,XPAD=2, $
    YPAD=1, /COLUMN)
    
  elaborationParameterInfoBase = Widget_Base(interactionBase, UNAME='elaborationparameterInfoBase', $
    FRAME=1 ,XOFFSET=0 ,SCR_XSIZE=self->xSummarySize(), $
    TITLE='IDL' ,SPACE=0 ,XPAD=2 ,YPAD=0, /ALIGN_LEFT, /COLUMN)
    
  dateTimeInfoBase = Widget_Base(interactionBase, UNAME='dateTimeInfoBase', $
    FRAME=1 ,XOFFSET=0 ,SCR_XSIZE=self->xSummarySize(), $
    TITLE='IDL' ,SPACE=0 ,XPAD=2 ,YPAD=0, /ALIGN_LEFT, /COLUMN)
    
  statsBase = Widget_Base(interactionBase, $
    UNAME='statsBase' ,FRAME=1 ,XOFFSET=0 ,YOFFSET=0, $
    SCR_XSIZE=self->xSummarySize() ,TITLE='IDL' ,SPACE=0 ,XPAD=2, $
    YPAD=0, /COLUMN, /ALIGN_LEFT)
    
  self->buildModelSection, modelInfoBase
  self->buildObservedSection, observedInfoBase
  self->buildElaborationParameterSection, elaborationParameterInfoBase
  self->buildDateTimeSection, dateTimeInfoBase
  self->buildStatsSection, statsBase
  
END
; *****************************************************************
; configure, update, display
; *****************************************************************
FUNCTION FMMainGUI::getReferenceValueContents

  elaborationDisp=self.mgr->getElaborationDisplay()
  extraValNoMax=3
  ;extraValsUNames='EXTRAVAL#'+strcompress(indgen(extraValNoMax)+1, /REMOVE)
  extraValsUName='EXTRAVAL'
  extraValsWid=widget_info(self->getTopBase(), FIND_BY_UNAME=extraValsUName)
  tFlag=elaborationDisp->getThresholdFlag()
  ;extraValsWid=lonarr(extraValNoMax)
  stringValues=''
  ;disable all
  if tFlag eq 1 then begin
    refValues=elaborationDisp->getThresholdValues()
    extraValNo=n_elements(refValues)
    widget_control, extraValsWid, get_value=stringValues
  ;    for i=0, extraValNo-1 do begin
  ;      extraValsWid[i]=widget_info(self->getTopBase(), FIND_BY_UNAME=extraValsUNames[i])
  ;      widget_control, extraValsWid[i], get_value=refValue
  ;      stringValues=stringValues+refValue+'#'
  ;    endfor
  endif
  return, stringValues
  
END

PRO FMMainGUI::updateInfo

  self->updateModelSection
  self->updateObservedSection
  self->updateElaborationSection
  self->updateDateSection
  
END

PRO FMMainGUI::updateModelSection

  entityDisp=self.mgr->getEntityDisplay()
  unames=['MODELTXT', 'SCENARIOTXT', 'RUNTXT', 'USEOBSMODBTT', 'SHOWBASESCENBTT']
  widgets=lonarr(n_elements(unames))
  for i=0, n_elements(widgets) -1 do widgets[i]=widget_info(self->getTopBase(), FIND_BY_UNAME=unames[i])
  
  modelNames=entityDisp->getSelectedModelNames()
  mods=''
  for i=0, n_elements(modelNames)-1 do mods=mods+modelNames[i]+'*'
  widget_control, widgets[0], set_value=mods
  
  scenarioNames=entityDisp->getSelectedScenarioNames()
  scens=''
  for i=0, n_elements(scenarioNames)-1 do scens=scens+scenarioNames[i]+'*'
  widget_control, widgets[1], set_value=scens
  
  runNames=entityDisp->getSelectedRunNames()
  runs=''
  for i=0, n_elements(runNames)-1 do runs=runs+runNames[i]+'*'
  widget_control, widgets[2], set_value=runs
  
  ;if entityDisp->getUseObservedModelFlag() then widget_control, widgets[3], /set_button else widget_control, widgets[3], set_button=0
  widget_control, widgets[3], set_button=entityDisp->getUseObservedModelFlag()
  widget_control, widgets[4], set_button=entityDisp->getAllAvailableScenarioFlag()
  
END

PRO FMMainGUI::updateObservedSection

  if self.mgr->entityDisplayIsValid() then begin
    entityDisp=self.mgr->getEntityDisplay()
    widgets=lonarr(4)
    unames=['SINGLEOBSTXT', 'GROUPOBSTXT', 'EXPANDOBSBTT', 'PARAMETERSTXT']
    for i=0, n_elements(widgets) -1 do widgets[i]=widget_info(self->getTopBase(), FIND_BY_UNAME=unames[i])
    
    singleNames=entityDisp->getSelectedSingleObsNames()
    singles=''
    for i=0, n_elements(singleNames)-1 do singles=singles+singleNames[i]+'*'
    widget_control, widgets[0], set_value=singleNames
    
    groupNames=entityDisp->getSelectedGroupObsNames()
    groups=''
    for i=0, n_elements(groupNames)-1 do groups=groups+groupNames[i]+'*'
    widget_control, widgets[1], set_value=groups
    
    widget_control, widgets[2], sensitive=entityDisp->isValidObservedGroupTitles()
    
    ; new october 19 2010
    parameterNames=entityDisp->getParametersSelectedNames()
    parameters=''
    for i=0, n_elements(parameterNames)-1 do parameters=parameters+parameterNames[i]+'*'
    widget_control, widgets[3], set_value=parameters
  endif
;end
  
END

PRO FMMainGUI::updateElaborationSection

  if self.mgr->elaborationDisplayIsValid() then begin
    elaborationDisp=self.mgr->getElaborationDisplay()
    ;unames=['ELABNAMETXT', 'DIAGRAMTXT', 'PARAMETERSTXT', 'GOALSBTT', 'GROUPBYSTATTXT', 'GROUPBYTIMETXT']
    unames=['DIAGRAMTXT', 'ELABNAMETXT',  'GOALSBTT', 'GROUPBYSTATTXT', 'GROUPBYTIMETXT', 'USEOBS']
    widgets=lonarr(n_elements(unames))
    for i=0, n_elements(widgets) -1 do widgets[i]=widget_info(self->getTopBase(), FIND_BY_UNAME=unames[i])
    
    ;  axisName=elaborationDisp->getSelectedAxisName()
    ;mods=''
    ;for i=0, n_elements(modelNames)-1 do mods=mods+modelNames[i]+'*'
    ;unames=['ELABAXISTXT', 'ELABNAMETXT', 'DIAGRAMTXT', 'PARAMETERSTXT', 'GOALSBTT', 'GROUPBYSTATTXT', 'GROUPBYTIMETXT']
    ;  widget_control, widgets[0], set_value=axisName
    
    diagramName=elaborationDisp->getSelectedDiagramName()
    ;mods=''
    ;for i=0, n_elements(modelNames)-1 do mods=mods+modelNames[i]+'*'
    ;unames=['ELABAXISTXT', 'ELABNAMETXT', 'DIAGRAMTXT', 'PARAMETERSTXT', 'GOALSBTT', 'GROUPBYSTATTXT', 'GROUPBYTIMETXT']
    widget_control, widgets[0], set_value=diagramName
    
    elabName=elaborationDisp->getSelectedElabName()
    ;scens=''
    ;for i=0, n_elements(scenarioNames)-1 do scens=scens+scenarioNames[i]+'*'
    widget_control, widgets[1], set_value=elabName
    
    
    if elaborationDisp->getGoalsCriteriaOCFlag() then widget_control, widgets[2], /set_button else widget_control, widgets[2], set_button=0
    ;  if elaborationDisp->getUseObservedModelFlag() then widget_control, widgets[2], /set_button else widget_control, widgets[3], set_button=0
    
    groupByStatName=elaborationDisp->getSelectedGroupByStatName()
    ;scens=''
    ;for i=0, n_elements(scenarioNames)-1 do scens=scens+scenarioNames[i]+'*'
    ;unames=['ELABAXISTXT', 'ELABNAMETXT', 'DIAGRAMTXT', 'PARAMETERSTXT', 'GOALSBTT', 'GROUPBYSTATTXT', 'GROUPBYTIMETXT']
    widget_control, widgets[3], set_value=groupByStatName
    
    groupByTimeName=elaborationDisp->getSelectedGroupByTimeName()
    ;scens=''
    ;for i=0, n_elements(scenarioNames)-1 do scens=scens+scenarioNames[i]+'*'
    ;unames=['ELABAXISTXT', 'ELABNAMETXT', 'DIAGRAMTXT', 'PARAMETERSTXT', 'GOALSBTT', 'GROUPBYSTATTXT', 'GROUPBYTIMETXT']
    widget_control, widgets[4], set_value=groupByTimeName
    
    extraValNoMax=3
    ;extraValsUNames='EXTRAVAL#'+strcompress(indgen(extraValNoMax)+1, /REMOVE)
    extraValsUName='EXTRAVAL'
    extraValsWid=widget_info(self->getTopBase(), FIND_BY_UNAME=extraValsUName)
    tFlag=elaborationDisp->getThresholdFlag()
    ;extraValsWid=lonarr(extraValNoMax)
    ;disable all
    ;    for i=0, extraValNoMax-1 do begin
    ;      extraValsWid[i]=widget_info(self->getTopBase(), FIND_BY_UNAME=extraValsUNames[i])
    ;    endfor
    widget_control, extraValsWid, sensitive=0, EDITABLE=0, set_value=''
    ;enable if ref values were set by user
    if tFlag eq 1 then begin
      refValues=strcompress(elaborationDisp->getThresholdValues(), /REMOVE)
      extraValNo=n_elements(refValues)
      stringValues=''
      for i=0, extraValNo-1 do stringValues=stringValues+'#'+refValues[i]
      stringValues=strmid(stringValues, 1, strlen(stringValues)-1)
      widget_control, extraValsWid, set_value=strcompress(stringValues, /REMOVE);, sensitive=1, /EDITABLE
    endif
  endif
  
END

PRO FMMainGUI::updateDateSection

  elaborationDisp=self.mgr->getElaborationDisplay()
  widgets=lonarr(8)
  unames=['SEASONTXT', 'HOURTYPETXT', 'StartHour', 'StartDay', 'StartMonth', $
    'EndHour', 'EndDay', 'EndMonth']
  for i=0, n_elements(widgets) -1 do widgets[i]=widget_info(self->getTopBase(), FIND_BY_UNAME=unames[i])
  
  seasonName=elaborationDisp->getSelectedSeasonName()
  ;scens=''
  ;for i=0, n_elements(scenarioNames)-1 do scens=scens+scenarioNames[i]+'*'
  widget_control, widgets[0], set_value=seasonName
  
  dayName=elaborationDisp->getSelectedDayPeriodName()
  ;scens=''
  ;for i=0, n_elements(scenarioNames)-1 do scens=scens+scenarioNames[i]+'*'
  widget_control, widgets[1], set_value=dayName
  
  widget_control,widgets[2] , SET_COMBOBOX_SELECT=elaborationDisp->getStartHourSelection()
  widget_control,widgets[3] , SET_COMBOBOX_SELECT=elaborationDisp->getStartDaySelection()
  widget_control,widgets[4] , SET_COMBOBOX_SELECT=elaborationDisp->getStartMonthSelection()
  widget_control,widgets[5] , SET_COMBOBOX_SELECT=elaborationDisp->getEndHourSelection()
  widget_control,widgets[6] , SET_COMBOBOX_SELECT=elaborationDisp->getEndDaySelection()
  widget_control,widgets[7] , SET_COMBOBOX_SELECT=elaborationDisp->getEndMonthSelection()
  
END

FUNCTION FMMainGUI::checkIntegrity, confInfo

  ; KeesC leapyear
  ;  scaleInfo=request->getScaleInfo()
  ;  res_scale=strsplit(scaleInfo,';',/extract)
  ;  year=fix(strcompress(res_scale[1],/remove_all))
  ;  print,'year',year
  ;year=2009
  ;MM summer 2012 Start
  mInfo=self.mgr->getModelInfo()
  year=mInfo.year
  ;MM summer 2012 Start
  dateExists=dtu->checkDate(year, month=self.info->getStartMonthSelection()+1, day=self.info->getStartDaySelection()+1, hour=self.info->getStartHourSelection(), julnumber=sDate)
  if dateExists ne 1 then begin
    msg=self->dialogMessage('Check start date existence',title='Check your data')
    return, 0
  endif
  dateExists=dtu->checkDate(year, month=self.info->getEndMonthSelection()+1, day=self.info->getEndDaySelection()+1, hour=self.info->getEndHourSelection(), julnumber=eDate)
  if dateExists ne 1 then begin
    msg=self->dialogMessage('Check end date existence',title='Check your data')
    return, 0
  endif
  if eDate-sDate le 0 then begin
    msg=self->dialogMessage('End date must be greater than start date',title='Check your data')
    return, 0
  endif
  return, 1
  
END

PRO FMMainGUI::configureMainStatusBar, topBase, statusMessage

  statusLabel=widget_info(topBase, FIND='STATUSBARMAIN')
  widget_control, statusLabel, set_value=statusMessage
  
END

FUNCTION FMMainGUI::getTitle

  return, self.mgr->getApplicationName()
  
END
;*************************************************************
; constructor / destructor
;*************************************************************
FUNCTION FMMainGUI::init, mgr, fonts=fonts

  if not self -> GUI :: init(mgr, fonts=fonts) then return , 0
  dims=get_screen_size(RESOLUTION=resolution)
  dims[0]=dims[0]*.85
  dims[1]=dims[1]*.92
  self.dimensions=dims
  self.recognizerVisibleStatus=0b
  self.utility=obj_new('FMUtility')
  self.fsm=obj_new('FMFileSystemManager')
  mgr->setMainView, self
  return , 1
  
END

PRO FMMainGUI::cleanUp

  obj_destroy, self.aboutGUI
  obj_destroy, self.disclaimerGUI
  obj_destroy, self.recognizerGUI
  obj_destroy, self.fsm
  obj_destroy, self.utility
  self -> GUI::cleanUp
  
END

;****************************************************************************************

PRO FMMainGUI__Define

  Struct = { FMMainGUI , $
    dimensions: lonarr(2), $
    benchMarkSubMenus: ptr_new(), $
    benchMarkRootButton: 0l, $
    recognizerVisibleStatus: 0b, $
    fsm: obj_new(), $
    utility: obj_new(), $
    aboutGUI: obj_new(), $
    disclaimerGUI: obj_new(), $
    recognizerGUI: obj_new(), $
    Inherits GUI $
    }
    
END

;****************************************************************************************

