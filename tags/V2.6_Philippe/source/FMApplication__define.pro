;********************
@structure_definition
;********************
PRO FMApplication::updateMenuBenchMarkInfoContents, fileName, displayName, fatherCode

  fileName=self.fileSystemMgr->getFileName(fileName)
  ;confDir=self.fileSystemMgr->getConfigurationDir(/WITH)
  self.benchMarkMenuList->addElement, fileName, displayName, fatherCode;, confDir
  self.mainView->buildBenchMarkMenuTree, /REBUILD

END

FUNCTION FMApplication::getGoogleEarthLocation
  return, self.googleEarthLocation
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

FUNCTION FMApplication::getScaleInfo

  return, self.scaleInfo

END

PRO FMApplication::setScaleInfo, value

  self.scaleInfo=value

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

FUNCTION FMApplication::getVersionDate

  return, self.versionDate

END

FUNCTION FMApplication::getVersionCode

  return, self.versionCode

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
FUNCTION FMApplication::dialogMessage, textMessage, title=title, info=info, error=error, question=question

  return, self.mainView->dialogMessage(textMessage, title=title, INFO=INFO, ERROR=ERROR, QUESTION=QUESTION)

END

PRO FMApplication::show

  self.mainView->show

END

PRO FMApplication::enable

  self.mainView->enable

END

PRO FMApplication::disable

  self.mainView->disable

END

PRO FMApplication::display

  ;self->streamPrint
  self.mainView=obj_new('FMMainGUI', self)
  self.mainView->realize
  self.plotter=obj_new('Plotter', self.mainView)
;self->displayModeSelectionGUI

END
; *****************************************************
; child views related methods
; *****************************************************
; get from views (data and analysis) multiple choice info related
;FUNCTION FMApplication::getMultipleChoiceInfo
PRO FMApplication::updateRecognizeData, request, result

  if request->getPlotDeviceName() ne 'PS' then begin
    obj_destroy, self.recognizeInfo
    pInfo = result->getPlotInfo()
    originalRecInfo=pInfo->getRecognizeInfo()
    if obj_valid(originalRecInfo) then rInfo = originalRecInfo->Clone(/DEEP) else rInfo=originalRecInfo
    self.recognizeInfo=rInfo
    self.mainView->updateRecognizeStatus
  endif

END

PRO FMApplication::Recognize, coord

  print, "do RecognizeJob"

  rInfo=self->getRecognizeInfo()
  rEdges=rInfo->getRegionEdges()
  rNames=rInfo->getNames()
  rValues=rInfo->getValues()
  foundName=""
  foundValue=""
  for i=n_elements(rEdges)-1, 0, -1 do begin
    thisRegion=*rEdges[i]
    print, 'thisRegion:', thisRegion
    print, 'coord:', coord
    print, 'coord[0] ge thisRegion[0,0]', coord[0], thisRegion[0,0]
    print, 'coord[0] le thisRegion[3,0]', coord[0], thisRegion[3,0]
    print, 'coord[1] ge thisRegion[0,1]', coord[1], thisRegion[0,1]
    print, 'coord[1] le thisRegion[1,1]', coord[1], thisRegion[1,1]
    if (coord[0] ge thisRegion[0,0]) and $
      (coord[0] le thisRegion[3,0]) and $
      (coord[1] ge thisRegion[0,1]) and $
      (coord[1] le thisRegion[1,1]) then begin
      print, "found", i
      foundName=rNames[i]
      foundValue=rValues[i]
      break
    endif
  endfor
  self.mainView->updateRecognizer, foundName, foundValue

END

PRO FMApplication::doRecognize, x, y

  if (self.executeOk[0] eq 1 and self.executeOk[1] eq 1) then begin
    destCoord=convert_coord(x, y, /DEVICE, /TO_NORMAL)
    self->recognize, destCoord
  endif

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
  if entityDisplay->isSingleObsSelected() then begin
    singleObsList=entityDisplay->buildSinglesObsNames()
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

  statsInfo={code:'', name:'', group:'', single:'', altitude:0, longitude:0., latitude:0., $
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
    bEntityDI=obj_new('EntityDisplayInfo')
    self->initEntityDisplay, bEntityDI, self.mainConfig, self.categoryList, self.modelList, self.scenarioList, self.observedList, self.parameterTypeList, self.parameterList, self.monitoringGroupStatList
    bElabDI=obj_new('ElaborationDisplayInfo')
    ;self->initElaborationDisplay, self.mainConfig, self.diagramList, self.axisTypeList, self.groupByStatList, self.groupByTimeList, self.seasonList, self.dayPeriodList
    self->initElaborationDisplay, bElabDI, self.mainConfig, self.diagramList, self.groupByStatList, self.groupByTimeList, self.seasonList, self.dayPeriodList
    self.compositeBatchMgr= obj_new('CompositeBatchManager', self, bEntityDI, bElabDI)
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
  self.mainView->updateInfo
  self.executeOk[1]=1

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
  ;  print, '****startIndex, endIndex***'
  ;  print, startIndex, endIndex
  ;  print, '***************************'
  request->setStartIndex, startIndex
  request->setEndIndex, endIndex
  request->setStartPlotIndex, startIndex
  request->setEndPlotIndex, endIndex
  request->setScaleInfo, self->getScaleInfo()
  ;print, plotCode, elaborationName
  request->setLocation, location
  request->setPlotDeviceName, !D.name
;  request->setPrintOrient, 'LANDSCAPE'
  request->setPageBreak, 'CLOSE'
  return, request

END

PRO FMApplication::doElaboration, request, multipleUserChoices

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
  self.dataMinerMgr->readAllData, request, result, screensize=self.mainView->getScreenSize()
  endTime=systime(/SECONDS)
  ;print, '-->', endTime-startTime
  widget_control, HOURGLASS=0
  ;elab data (fill result, fill .plotInfo result to manage plot behaviour)
  request->setGoogleEarthLocation, self->getGoogleEarthLocation()
  call_procedure, request->getElaborationRoutine(), request, result

; KeesC 19MAY2012  
  targetInfo=result->getGenericPlotInfo()
  checkXY=targetInfo->getXYS()
;  if checkXY[0] ne 'AllNaN' then begin 
    self.plotter->openDevice, request->getPlotDeviceName(), request->getFileName(), $
      request->getPrintOrient(),request->getPageBreak(), request->getLocation()
    self.plotter->plotAll, request, result
    self.plotter->closeDevice, request->getPageBreak(), request->getFileName(), request->getPrintOrient()
    self->updateRecognizeData, request, result
;  endif
    
  obj_destroy, request
  obj_destroy, result

END


FUNCTION FMApplication::restoreEntity, fileName

  ;phil batch 18/03
  benchMarkDir=self.fileSystemMgr->getSaveDir(/WITH)
  fileName=benchMarkDir+fileName
  ;end phil batch
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

FUNCTION FMApplication::restoreElaboration, fileName

  ;phil batch 18/03
  benchMarkDir=self.fileSystemMgr->getSaveDir(/WITH)
  fileName=benchMarkDir+fileName
  ;end phil batch
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
FUNCTION FMApplication::restoreRequest, fileName, path

  request=obj_new('Request')
  if (request->restoreData(fileName)) then begin
    request->setScaleInfo, self->getScaleInfo()
    if ~self->restoreEntity(request->getEntityFileName()) then return, 0
    if ~self->restoreElaboration(request->getElaborationFileName()) then return, 0
    ;msg=self.mainView->dialogMessage(['Batch restored from:', '<'+fileName+'> file.'], title=['Request'], /INFORMATION)
    self->execRequest, request
    self->updateViewAppearence, [0,1]
  endif else begin
    msg=self.mainView->dialogMessage(['Batch restored failed from:', '<'+fileName+'> file.'], title=['Request'], /INFORMATION)
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

PRO FMApplication::execRequest, passedRequest, NODISPLAYCHECK=NODISPLAYCHECK

  if self->checkRequest(multipleUserChoices=multipleUserChoices, /NODISPLAYCHECK) then begin
    mUC=multipleUserChoices
    ;build
    request=self->buildRequest(mUC)
    if n_elements(passedRequest) ne 0 then begin
      request->setFileName, passedRequest->getFileName()
      request->setPlotDeviceName, passedRequest->getPlotDeviceName()
      request->setLocation, passedRequest->getLocation()
      request->setPrintOrient, passedRequest->getPrintOrient()
      request->setPageBreak, passedRequest->getPageBreak()
    endif
    self->doElaboration, request, mUC
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
PRO FMApplication::loadInitFileData, parameterNames=parameterNames, parameterValues=parameterValues

  self.fileSystemMgr->LoadInitFileData, parameterNames=parameterNames, parameterValues=parameterValues

END

PRO FMApplication::startJournaling

  util=obj_new('FMUtility')
  journalFullFileName=self.fileSystemMgr->getLogDir(/WITH)
  journalFullFileName=journalFullFileName+util->getSysTime(/FILECOMPATIBILITY)+'.txt'
  journal, journalFullFileName
  obj_destroy, util

END

PRO FMApplication::startUp

  self->startJournaling
  confDir=self.fileSystemMgr->getConfigurationDir(/WITH)
  self->LoadInitFileData, parameterName=parameterName, parameterValue=parameterValue
  lookUpIdx=(where(parameterName eq 'STARTUP_LOOKUP'))[0]
  if lookUpIdx[0] eq -1 then doLookUp=1 else doLookUp=fix(parameterValue[lookUpIdx])
  if doLookUp then self.fileSystemMgr->lookUpSystemData, scaleInfo=scaleInfo
  self->setScaleInfo, scaleInfo
  ; Load Standard Resources first (fixed)
  extraParameterNames=[''] & extraParameterValues=['']
  for i=0, n_elements(parameterName)-1 do begin
    thisPar=strupCase(parameterName[i])
    if strmid(thisPar, 0, 4) eq 'TXT_' then varname=parameterValue[i] else fileName=parameterValue[i]
    case strupCase(thisPar) of
      'STARTUP_LOOKUP' : ;do nothing
      'BENCHMARK_FILE' : self.benchMarkMenuList->fillDataFromFile, confDir+fileName
      'MODE_FILE' : self.modeList->fillDataFromFile, confDir+fileName
      'CATEGORY_FILE' : self.categoryList->fillDataFromFile, confDir+fileName
      'MODEL_FILE' : self.modelList->fillDataFromFile, confDir+fileName
      'SCENARIO_FILE' : self.scenarioList->fillDataFromFile, confDir+fileName
      'IDLROUTINE_FILE' : self.iDLRoutineList->fillDataFromFile, confDir+fileName
      'PARAMETERTYPE_FILE' : self.parameterTypeList->fillDataFromFile, confDir+fileName
      'GROUPBYTIME_FILE' : self.groupByTimeList->fillDataFromFile, confDir+fileName
      'MONITORINGGROUPSTAT_FILE' : self.monitoringGroupStatList->fillDataFromFile, confDir+fileName
      'GROUPBYSTAT_FILE' : self.groupByStatList->fillDataFromFile, confDir+fileName
      'DIAGRAM_FILE' : self.diagramList->fillDataFromFile, confDir+fileName
      'SEASON_FILE' : self.seasonList->fillDataFromFile, confDir+fileName
      'DAYPERIOD_FILE' : self.dayPeriodList->fillDataFromFile, confDir+fileName
      'PARAMETER_FILE' : parameterFileName=confDir+fileName ; Save parameter File Name for later use
      'OBSERVED_FILE' : observedFileName=confDir+fileName ; Save observed File Name for later use
      'TXT_VERSION_DATE' : self.versionDate=varName ; save
      'TXT_PS_CHARSIZE_FACTOR' : self.psCharSizeFactor=float(varName) ; save
      'TXT_VERSION_CODE' : self.versionCode=varName ; save
      'BROWSER_LOCATION' : self->setBrowserLocation, fileName ; save location of browser application
      'DOCUMENTSREADER_LOCATION' : self->setDocReaderLocation, fileName ; Save location of doc reader
      'WORKSHEET_LOCATION' : self->setWorkSheetLocation, fileName ; save location of worksheet reader
      'PDFREADER_LOCATION' : self->setPdfReaderLocation, fileName ; Save location of pdf reader
      'GOOGLEEARTH_LOCATION' : self->setGoogleEarthLocation, filename ; Save location of Google Earth
    else :begin
    extraParameterNames=[extraParameterNames, thisPar]
    extraParameterValues=[extraParameterValues, parameterValue[i]]
  end
endcase
endfor
; Now Load Config Resources (may depend from first block)
self.parameterList->fillDataFromFile, parameterFileName
self.observedList->fillDataFromFile, observedFileName
self->fillMainConfigFromFile, confDir, parameterName=extraParameterNames[1:*], parameterValue=extraParameterValues[1:*]
self->initViewConfiguration

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

  entityDisplay->setUseObservedModelFlag, 0b

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

  elabList->updateMode
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

  self.mainConfig->fillFlexyData, confDir, parameterName=parameterName, parameterValue=parameterValue

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
  self -> Object :: cleanUp
  journal

END

PRO FMApplication__Define

  Struct = { FMApplication , $
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
    scaleInfo: '', $
    versionDate : '', $
    versionCode : '', $
    psCharSizeFactor: 0., $
    browserLocation: '', $
    googleEarthLocation: '', $
    workSheetLocation: '', $
    docReaderLocation: '', $
    pdfReaderLocation: '', $
    Inherits Object $
    }

END

;****************************************************************************************