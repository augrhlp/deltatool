; MM summer 2012 start
FUNCTION CompositeBatchManager::selectionIsFilled

  return, obj_valid(self.batchEntityInfos[self.batchIndex]) and $
    obj_valid(self.batchElaborationInfos[self.batchIndex])
    
END

FUNCTION CompositeBatchManager::getMainMgr

  return, self.mainMgr
  
END
; MM summer 2012 end

FUNCTION CompositeBatchManager::getBenchMarkTreeFatherLevel

  return, self.benchMarkTreeFatherLevel
  
END

FUNCTION CompositeBatchManager::getBenchMarkSaveMode

  return, self.benchMarkSaveMode
  
END

FUNCTION CompositeBatchManager::getBenchMarkTreeElementName

  return, self.benchMarkTreeElementName
  
END

FUNCTION CompositeBatchManager::getBenchMarkTreeFatherElementCode

  return, self.benchMarkTreeFatherElementCode
  
END

FUNCTION CompositeBatchManager::getTreeNamesAtSameLevel

  bmMI=self->getBenchMarkMenuInfo()
  father=bmMI->findParent(self.benchMarkTreeElementCode)
  fatherLevel=bmMI->getLevelByCode(father)
  destLevel=fatherLevel+1
  self.benchMarkTreeFatherLevel=destLevel
  self.benchMarkTreeFatherElementCode=father
  childs=bmMI->findChilds(father)
  names=bmMI->getNamesByCodes(childs)
  print, names
  return, names
  
END

FUNCTION CompositeBatchManager::checkTreeMenuNameIntegrity, testString

  if self->getBenchMarkSaveMode() eq 'MENU' then begin
    utility=obj_new('FMUtility')
    res=utility->IsFileNameCompatible(testString)
    obj_destroy, utility
    if ~res then begin
      aa=self.refView->dialogMessage(['Use file convention compatible display name.'], title=['Check your selections'])
      return, 0
    endif
    allNames=self->getTreeNamesAtSameLevel()
    aa=where(testString eq allNames, count)
    if count ne 0 then begin
      aa=self.refView->dialogMessage(['Bench mark display name already exists.'], title=['Check your selections'])
      return, 0
    endif
    self.benchMarkTreeElementName=testString
  endif
  return, 1
;return, count > 0 ? 0 : 1
  
END

FUNCTION CompositeBatchManager::getBenchMarkMenuInfo

  return, self.mainMgr->getBenchMarkMenuInfo()
  
END

FUNCTION CompositeBatchManager::getAllPrintOrientNames

  return, self.mainMgr->getAllPrintOrientNames()
  
END

FUNCTION CompositeBatchManager::getAllPrintOrientCodes

  return, self.mainMgr->getAllPrintOrientCodes()
  
END

FUNCTION CompositeBatchManager::getAllBenchMarkSaveModeCodes

  return, self.mainMgr->getAllBenchMarkSaveModeCodes()
  
END

FUNCTION CompositeBatchManager::getAllBenchMarkSaveModeNames

  return, self.mainMgr->getAllBenchMarkSaveModeNames()
  
END

FUNCTION CompositeBatchManager::getAllPageModeCodes

  return, self.mainMgr->getAllPageModeCodes()
  
END

FUNCTION CompositeBatchManager::getAllPageModeNames

  return, self.mainMgr->getAllPageModeNames()
  
END

FUNCTION CompositeBatchManager::getAllGraphicTypeNames

  return, self.mainMgr->getAllGraphicTypeNames()
  
END

FUNCTION CompositeBatchManager::getAllGraphicTypeCodes

  return, self.mainMgr->getAllGraphicTypeCodes()
  
END

PRO CompositeBatchManager::setBenchMarkTreeElementCode, value

  self.benchMarkTreeElementCode=value
  
END

FUNCTION CompositeBatchManager::getBenchMarkTreeElementCode

  return, self.benchMarkTreeElementCode
  
END

PRO CompositeBatchManager::setBenchMarkTreeElementName, value

  self.benchMarkTreeElementName=value
  
END

FUNCTION CompositeBatchManager::getBenchMarkTreeElementName

  return, self.benchMarkTreeElementName
  
END

PRO CompositeBatchManager::setBenchMarkSaveMode, value

  self.benchMarkSaveMode=value
  
END

FUNCTION CompositeBatchManager::getBenchMarkSaveMode

  return, self.benchMarkSaveMode
  
END

PRO CompositeBatchManager::setPageModeSelection, value

  self.pageModeSelection=value
  
END

FUNCTION CompositeBatchManager::getPageModeSelection

  return, self.pageModeSelection
  
END

PRO CompositeBatchManager::setPrintOrientSelection, value

  self.printOrientSelection=value
  
END

FUNCTION CompositeBatchManager::getPrintOrientSelection

  return, self.printOrientSelection
  
END

PRO CompositeBatchManager::setGraphCodeSelection, value

  self.graphCodeSelection=value
  
END

FUNCTION CompositeBatchManager::getGraphCodeSelection

  return, self.graphCodeSelection
  
END

PRO CompositeBatchManager::enable

  self.refView->enable
  
END

FUNCTION CompositeBatchManager::getAllSplitStyleNames

  return, self.mainMgr->getAllSplitStyleNames()
  
END

FUNCTION CompositeBatchManager::getAllSplitStyleCodes

  return, self.mainMgr->getAllSplitStyleCodes()
  
END

FUNCTION CompositeBatchManager::getSplitStyleCodeSelection

  return, self.splitStyleCodeSelection
  
END

PRO CompositeBatchManager::setSplitStyleCodeSelection, code

  self.splitStyleCodeSelection=code
  
END

FUNCTION CompositeBatchManager::getSplitInfo

  return, *self.splitInfo
  
END

PRO CompositeBatchManager::updateSplitInfo

  splitStyleCode=self->getSplitStyleCodeSelection()
  allSplitStyles=self->getAllSplitStyleCodes()
  if (splitStyleCode eq allSplitStyles[0]) then begin
    splitInfo=self.mainMgr->getSingleSplitInfo()
  endif
  if (splitStyleCode eq allSplitStyles[1]) then begin
    splitInfo=self.mainMgr->getDoubleHSplitInfo()
  endif
  if (splitStyleCode eq allSplitStyles[2]) then begin
    splitInfo=self.mainMgr->getDoubleVSplitInfo()
  endif
  if (splitStyleCode eq allSplitStyles[3]) then begin
    splitInfo=self.mainMgr->getFourSplitInfo()
  endif
  self->setSplitNumber, n_elements(splitInfo)
  sInfo=ptr_new(splitInfo, /NO_COPY)
  self.splitInfo=sInfo
  
END

FUNCTION CompositeBatchManager::getSelectedSplitInfoGraphNumber

  return, n_elements(self->getSplitInfo())
  
END

PRO CompositeBatchManager::setBatchIndex, value

  self.batchIndex=value
  
END

FUNCTION CompositeBatchManager::getBatchIndex

  return, self.batchIndex
  
END

FUNCTION CompositeBatchManager::checkIntegrity

  ; MM summer 2012 Start
  frameNumber=self->getSplitNumber()
  multipleUserChoices=[0,0,0,0]
  if self->checkTreeMenuNameIntegrity(self.refView->getTreeMenuTextName()) ne 1 then return, 0
  for i=0, frameNumber-1 do begin
    if self.modified[i] eq 0 then begin
      aa=self.refView->dialogMessage(['Request element #'+strcompress(i+1, /REMOVE)+' not filled or the selections are invalid.'], title=['Check your selections'])
      return, 0
    endif
    if obj_valid(self.batchEntityInfos[i]) and obj_valid(self.batchElaborationInfos[i]) then begin
      ; MM summer/fall 2012 start
      mainMgr=self->getMainMgr()
      tempReq=mainMgr->buildRequest(multipleUserChoices, location, self.batchEntityInfos[i], self.batchElaborationInfos[i])
      useGoalsAndCriteria=tempReq->getElaborationOCUse()
      dummy=tempReq->getGoalsCriteriaValues(/CONTENTS, NOVALUES=NOVALUES)
      if useGoalsAndCriteria then begin
        if Check_Criteria(request, result) eq 0 then begin
          aa=self.refView->dialogMessage(['Request element #'+strcompress(i+1, /REMOVE)+' K_CriteriaNotAvailable.', 'Check elaboration, parameter and/or configuration files.'], title=['Check your selections'])
          obj_destroy, tempReq
          return, 0b
        endif
      endif
      ;      insert here Philippe checkCriteriaRoutine(request, result...)
      ;      dummy=tempReq->getGoalsCriteriaValues(/CONTENTS, NOVALUES=NOVALUES)
      ;      if keyword_set(NOVALUES) then GC_EXISTS=0 else GC_EXISTS=1
      ;      ;if keyword_set(GC_EXISTS) and useGoalsAndCriteria then goalsAndCriteriaText[i]=strcompress(goalsAndCriteriaInfo, /REMOVE)
      ;      if not(keyword_set(GC_EXISTS)) and useGoalsAndCriteria then begin
      ;        aa=self.refView->dialogMessage(['Request element #'+strcompress(i+1, /REMOVE)+' K_CriteriaNotAvailable.', 'Check elaboration, parameter and/or configuration files.'], title=['Check your selections'])
      ;        obj_destroy, tempReq
      ;        return, 0b
      ;      endif
      obj_destroy, tempReq
      continue
    endif
    aa=self.refView->dialogMessage(['Request element no.:'+strcompress(i+1, /REMOVE)+' not filled.'], title=['Check your selections'])
    return, 0
  endfor
  ; MM summer 2012 end
  return, 1
  
END

PRO CompositeBatchManager::setSplitNumber, value

  self.splitNumber=value
  
END

FUNCTION CompositeBatchManager::getSplitNumber

  return, self.splitNumber
  
END

PRO CompositeBatchManager::updateToCaller

  splitBatchInfo=getSplitBatchStruct(self.splitNumber)
  splitInfos=self->getSplitInfo()
  splitBatchInfo.splitStyle=self->getSplitStyleCodeSelection()
  splitBatchInfo.graphicType=self->getGraphCodeSelection()
  splitBatchInfo.pageMode=self->getPageModeSelection()
  splitBatchInfo.fullPageLocation=self.mainMgr->getSingleSplitInfoLocation()
  splitBatchInfo.printOrient=self->getPrintOrientSelection()
  for i=0, self.splitNumber-1 do begin
    splitBatchInfo.requests[i].entity=self.batchEntityInfos[i]
    splitBatchInfo.requests[i].elaboration=self.batchElaborationInfos[i]
    splitBatchInfo.requests[i].location=[splitInfos[i].borderCoords[0,0],splitInfos[i].borderCoords[1,0],splitInfos[i].borderCoords[0,2],splitInfos[i].borderCoords[1,2]]
  endfor
  self.mainMgr->enable
  self.mainMgr->setBlockWindowControl, /OFF
  fileName=self.mainMgr->saveBatch("", "", splitBatchInfo, /GUI)
  if fileName ne '' and self->getBenchMarkSaveMode() eq 'MENU' then self.mainMgr->updateMenuBenchMarkInfoContents, fileName, self->getBenchMarkTreeElementName(), self->getBenchMarkTreeFatherElementCode()
  self.mainMgr->restoreElabFilterType
  
END

PRO CompositeBatchManager::exitRequest

  self.mainMgr->restoreElabFilterType
  self.mainMgr->enable
  
END

PRO CompositeBatchManager::realize

  self.refView=obj_new('FMBenchMarkCreationGUI', obj_new(""), self)
  self.refView->realize
  
END

PRO CompositeBatchManager::userEditBatch

  if obj_valid(self.batchEntityInfos[self.batchIndex]) then self.tempBatchEntityInfo=self.batchEntityInfos[self.batchIndex]->Clone(/DEEP)
  if obj_valid(self.batchElaborationInfos[self.batchIndex]) then self.tempBatchElaborationInfo=self.batchElaborationInfos[self.batchIndex]->Clone(/DEEP)
  self->displayEntity
  
END

PRO CompositeBatchManager::userShowRequestBatch

  if obj_valid(self.batchEntityInfos[self.batchIndex]) then self.tempBatchEntityInfo=self.batchEntityInfos[self.batchIndex]->Clone(/DEEP)
  if obj_valid(self.batchElaborationInfos[self.batchIndex]) then self.tempBatchElaborationInfo=self.batchElaborationInfos[self.batchIndex]->Clone(/DEEP)
  self->displayRequest
  
END

PRO CompositeBatchManager::displayEntity

  ;batchEntityView=obj_new("FMBatchEntitySelectionGUI", self.batchEntityInfo, self)
  ;October 2011 use clone and delegate obj_destroy inside
  batchEntityView=obj_new('FMBatchEntitySelectionGUI', self.tempBatchEntityInfo, self)
  batchEntityView->realize
  self.refView->disable
  
END

PRO CompositeBatchManager::displayRequest

  ;MM summer 2012 Start
  mainMgr=self->getMainMgr()
  multipleUserChoices=[0,0,0,0]
  fakeReq=mainMgr->buildRequest(multipleUserChoices, location, self.tempBatchEntityInfo, self.tempBatchElaborationInfo)
  batchShowView=obj_new('FMBatchShowRequestGUI', fakeReq, self)
  batchShowView->realize
  self.refView->disable
  
END

PRO CompositeBatchManager::setBatchInfo, index, entity, elaboration

  obj_destroy, self.batchElaborationInfos[index]
  obj_destroy, self.batchEntityInfos[index]
  self.batchElaborationInfos[index]=elaboration
  self.batchEntityInfos[index]=entity
  
END

PRO CompositeBatchManager::elaborationOK, elabInfo
  ; check integrity of request!!!
  ;if (self.mainMgr->checkRequest(self.tempBatchEntityInfo, self.tempBatchElaborationInfo, /NODISPLAYCHECK)) then begin
  if (self.mainMgr->checkRequest(self.tempBatchEntityInfo, elabInfo, /NODISPLAYCHECK)) then begin
    self.tempBatchElaborationInfo=elabInfo->Clone(/DEEP)
    self.modified[self.batchIndex]=1
    self->setBatchInfo, self.batchIndex, self.tempBatchEntityInfo->Clone(/DEEP), self.tempBatchElaborationInfo->Clone(/DEEP)
    self.refView->enable
  endif
; ask for filename and group name
; October 2011 saving memory
;if obj_valid(self.refView) then obj_destroy, self.refView
;self.refView=obj_new("")
  
END

PRO CompositeBatchManager::showRequestOK, requestView

  ;obj_destroy, requestView.info
  obj_destroy, requestView
  self.refView->enable
  
END

PRO CompositeBatchManager::entityOK, entityView

  self.tempBatchEntityInfo=entityView.info->Clone(/DEEP)
  obj_destroy, entityView
  self->displayElaboration
  self.refView->disable
  
END

PRO CompositeBatchManager::displayElaboration

  ;self.batchElaborationView=obj_new('FMElaborationSelectionGUI', self.batchElaborationDisplay->Clone(/DEEP), self)
  ;batchElaborationView=obj_new("FMBatchElaborationSelectionGUI", self.batchEntityInfo, self)
  ; October 2011 saving memory delegate obj_destroy inside
  batchElaborationView=obj_new("FMBatchElaborationSelectionGUI", self.tempBatchElaborationInfo->Clone(/DEEP), self)
  batchElaborationView->realize
  self.refView->disable
  
END

FUNCTION CompositeBatchManager::abort

  self.refView->enable
  obj_destroy, self
  
END

PRO CompositeBatchManager::configure

  ; MM October 2001: saving memory, better objects & pointers management
  if obj_valid(self.batchEntityView) then obj_destroy, self.batchEntityView
  if obj_valid(self.batchElaborationView) then obj_destroy, self.batchElaborationView
  ;end
  self.batchEntityView=obj_new("FMBatchEntitySelectionGUI")
  self.batchElaborationView=obj_new("FMBatchElaborationSelectionGUI")
  
END


FUNCTION CompositeBatchManager::getTitle

  return, 'Benchmark building'
  
END
;*************************************************************
; constructor / destructor
;*************************************************************
FUNCTION CompositeBatchManager::init, mainMgr, sampleEntity, sampleElaboration;, benchMarkTreeInfo

  self.mainMgr=mainMgr
  self.tempBatchElaborationInfo=sampleElaboration
  self.tempBatchEntityInfo=sampleEntity
  self.splitNumber=1
  self.modified=[0,0,0,0]
  ;self.benchMarkTreeInfo=benchMarkTreeInfo
  ;  if obj_valid(entityDisplayInfo) then self.tempBatchEntityInfo=entityDisplayInfo else self.tempBatchEntityInfo=obj_new("EntityDisplayInfo")
  ;  if obj_valid(elaborationDisplayInfo) then self.tempBatchElaborationInfo=elaborationDisplayInfo else self.tempBatchElaborationInfo=obj_new("ElaborationDisplayInfo")
  return , 1
  
END

PRO CompositeBatchManager::cleanUp

  ptr_free, self.splitInfo
  ; MM October 2001: saving memory, better objects & pointers management
  obj_destroy, self.tempBatchEntityInfo
  obj_destroy, self.tempBatchElaborationInfo
  for i=0, 3 do if obj_valid(self.batchEntityInfos[i]) then obj_destroy, self.batchEntityInfos[i]
  for i=0, 3 do if obj_valid(self.batchElaborationInfos[i]) then obj_destroy, self.batchElaborationInfos[i]
  self.refView=obj_new('')
  ;self.benchMarkTreeInfo=obj_new('')
  self.mainMgr=obj_new('')
; end
  
END

;****************************************************************************************

PRO CompositeBatchManager__Define

  Struct = { CompositeBatchManager , $
    tempBatchEntityInfo: obj_new(), $
    tempBatchElaborationInfo: obj_new(), $
    batchEntityInfos: objarr(4), $
    batchElaborationInfos: objarr(4), $
    batchEntityView: obj_new(), $
    batchElaborationView: obj_new(), $
    mainMgr: obj_new(), $
    refView: obj_new(), $
    ;benchMarkTreeInfo: obj_new(), $
    batchIndex: 0, $
    splitNumber: 0, $
    splitInfo: ptr_new(), $
    modified: fltarr(4), $
    splitStyleCodeSelection: '', $
    graphCodeSelection: '', $
    pageModeSelection: '', $
    benchMarkSaveMode: '', $
    printOrientSelection: '', $
    benchMarkTreeElementCode: 0, $
    benchMarkTreeFatherElementCode: 0, $
    benchMarkTreeElementName: '', $
    benchMarkTreeFatherLevel: 0, $
    Inherits Object $
    }
    
END

;****************************************************************************************

