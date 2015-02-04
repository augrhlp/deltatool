PRO FMEntitySelectionGUI::buildOKButton, base

  subBase=base
  okBtt=widget_button(subBase, value='OK', UNAME='DISPLAYOK', $
    event_pro=self.eventprefix+'OKRequest', SCR_XSIZE=70, SCR_YSIZE=35, /ALIGN_CENTER)
  if self.mgr->IsAdvancedFilter() then magicBtt=widget_button(subBase, value='Magic', UNAME='MAGIC', $
    event_pro=self.eventprefix+'magicEnt', SCR_XSIZE=70, SCR_YSIZE=35, /ALIGN_CENTER)
;okButton = widget_button(mainButtonBase, Value='OK', UNAME='APPLY', $
; XSIZE=70, YSIZE=35, event_pro=self.eventPrefix+'okModeBtt', /ALIGN_CENTER)
    
END

FUNCTION FMEntitySelectionGUI::buildTestObs, type, stats, $
    spec=spec, mods=mods, scen=scen
    
  util=obj_new('FMUtility')
  
  specName=''
  for i=0, n_elements(spec)-1 do specName=specName+'_'+string(spec[i])
  specName=strmid(specName, 1, strlen(specName)-1)
  
  modName=mods
  scenName=scen
  
  rName=util->removeSpecialChars('_run_'+modName+'_'+scenName, /SPACETOO)
  sName=util->removeSpecialChars('spec_'+specName, /SPACETOO)
  
  case type of
    ; test obs 1 - one single stations
    1:begin
    self->singleObsRadioButton
    self->addObservations, stats[0], '', /SINGLE
    oName='1single_stat'
  end
  ; test obs 2 - two single stations
  2:begin
  self->singleObsRadioButton
  self->addObservations, stats[0], '', /SINGLE
  self->addObservations, stats[1], '', /SINGLE
  oName='2single_stat'
end
; test obs 3 - one single stations and one group (of 2)
3:begin
self->singleObsRadioButton
self.info->addObservations, stats[0], '', /SINGLE
self->groupObsRadioButton
self.info->addObservations, [stats[2],stats[3]] , 'testG1'
;self.info->addObservations, , 'testG1'
oName='1single_1group_stat'
end
; test obs 4 - two single stations and one group (of 2)
4:begin
self->singleObsRadioButton
self->addObservations, stats[0], '', /SINGLE
self->addObservations, stats[1], '', /SINGLE
self->groupObsRadioButton
self->addObservations, [stats[2], stats[3]], 'testG2'
;self->addObservations, , 'testG2'
oName='2single_1group_stat'
end
; test obs 5 - two single stations and two groups (of 2)
5:begin
self->singleObsRadioButton
self->addObservations, stats[0], '', /SINGLE
self->addObservations, stats[1], '', /SINGLE
self->groupObsRadioButton
self->addObservations, [stats[2], stats[3]], 'testG3'
;self->addObservations, , 'testG3'
self->addObservations, [stats[4], stats[5]], 'testG4'
;self->addObservations, stats[5], 'testG4'
oName='2single_2group_stat'
end
; test obs 6 - one group (of 2)
6:begin
self->groupObsRadioButton
self->addObservations, [stats[2], stats[3]], 'testG5'
;self->addObservations, , 'testG5'
oName='1group_stat'
end
; test obs 7 - more groups (of 2)
7:begin
self->groupObsRadioButton
self->addObservations, [stats[2], stats[3]], 'testG6'
;self->addObservations, , 'testG6'
self->addObservations, [stats[4], stats[5]], 'testG7'
;self->addObservations, , 'testG7'
oName='2group_stat'
end
endcase

obj_destroy, util
return, rName+'_'+sName+'_'+oName

END

FUNCTION FMEntitySelectionGUI::getTestObs

  widget_control, self.queryResultList, get_uvalue=displayCodes
  ObsNo=n_elements(displayCodes)
  
  idx=randomu(seed)
  singleStat1=displayCodes[idx*ObsNo]
  singleStat2=displayCodes[(idx*ObsNo+1) mod ObsNo]
  
  idx=randomu(seed)
  groupStat1of1=displayCodes[idx*ObsNo]
  groupStat2of1=displayCodes[(idx*ObsNo+1) mod ObsNo]
  
  idx=randomu(seed)
  groupStat1of2=displayCodes[idx*ObsNo]
  groupStat2of2=displayCodes[(idx*ObsNo+1) mod ObsNo]
  
  return, [singleStat1, singleStat2, groupStat1of1, groupStat2of1, groupStat1of2, groupStat2of2]
  
END

PRO FMEntitySelectionGUI::buildAllAvailableCombination

  prevSilentMode=self.silentMode
  self.silentMode=1
  logFile=self.mgr->getTestDir(/WITH)+'entity.log'
  self.mgr->logging, file=logFile
  
  allScenarios=self->getAllScenarioCodes()
  allModels=self->getAllModelCodes()
  allpars=self->getAllParameterCodes()
  scenarioNo=n_elements(allScenarios)
  modelsNo=n_elements(allModels)
  parsNo=n_elements(allpars)
  sIndexes=indgen(scenarioNo)
  mIndexes=indgen(modelsNo)
  
  idx=randomu(seed)
  par1=allpars[idx*parsNo]
  par2=allpars[(idx*parsNo+1) mod parsNo]
  
  entFileList=''
  k=0
  fm=self.mgr->getFileSystemMgr()
  util=obj_new('FMUtility')
  ext=fm->getEntityExtension()
  
  done=0
  multipleToDo=1
  singleToDo=1
  sIdx=0
  mIdx=0
  k=0
  self->groupObsRadioButton
  self->removeAllObs
  self->singleObsRadioButton
  self->removeAllObs
  
  while (done ne 1) and ((multipleToDo) or (singleToDo)) do begin
  
    tempSIdx=sIndexes[sIdx]
    tempMIdx=mIndexes[mIdx]
    self->userScenarioSelections, tempSIdx
    self->userModelSelections, tempMIdx
    widget_control, self.runList, get_uvalue=RUNINDEXES
    valIdxs=where(RUNINDEXES ne -1, totRuns)
    if (totRuns eq 1) and singleToDo then begin
      scenNames='' & modelsNames=''
      for l=0, n_elements(sIndexes[sIdx])-1 do scenNames=scenNames+self->getScenarioNameAtIndex(sIndexes[l])
      for l=0, n_elements(mIndexes[mIdx])-1 do modelsNames=modelsNames+self->getModelNameAtIndex(mIndexes[l])
      for i=1, 7 do begin
        for j=0, 1 do begin
          if j eq 0 then parametersCodes=par1
          if j eq 1 then parametersCodes=[par1,par2]
          spec=parametersCodes
          self->userParameterCodeSelections, parametersCodes
          obsAreGood=0
          m=0
          while not(obsAreGood) do begin
            stats=self->getTestObs()
            fileName=self->buildTestObs( i, $
              stats, spec=spec, mods=modelsNames, scen=scenNames)
            obsAreGood=self->checkIntegrity()
            m++
            if m gt 1 then stop
          endwhile
          for l=0, 1 do begin
            self->userUseObsModButton, l
            k++
            thisFileName=strcompress(k, /REMOVE)+fileName+'_modobs'+strcompress(l, /REMOVE)+ext
            self.info->saveData, $
              self.mgr->getTestDir(/WITH)+thisFileName
            entFileList=[entFileList, thisFileName]
          endfor
          self->groupObsRadioButton
          self->removeAllObs
          self->singleObsRadioButton
          self->removeAllObs
        endfor
      endfor
      singleToDo=0
    endif
    
    if (totRuns gt 1) and multipleToDo then begin
      scenNames='' & modelsNames=''
      for l=0, n_elements(sIndexes[sIdx])-1 do scenNames=scenNames+'_'+self->getScenarioNameAtIndex(sIndexes[l])
      for l=0, n_elements(mIndexes[mIdx])-1 do modelsNames=modelsNames+'_'+self->getModelNameAtIndex(mIndexes[l])
      for i=1, 7 do begin
        for j=0, 1 do begin
          if j eq 0 then parametersCodes=par1
          if j eq 1 then parametersCodes=[par1,par2]
          spec=parametersCodes
          self->userParameterCodeSelections, parametersCodes
          stats=self->getTestObs()
          fileName=self->buildTestObs( i, $
            stats, spec=spec, mods=modelsNames, scen=scenNames)
          for l=0, 1 do begin
            self->userUseObsModButton, l
            k++
            thisFileName=strcompress(k, /REMOVE)+fileName+'_modobs'+strcompress(l, /REMOVE)+ext
            self.info->saveData, $
              self.mgr->getTestDir(/WITH)+thisFileName
            entFileList=[entFileList, thisFileName]
          endfor
          self->groupObsRadioButton
          self->removeAllObs
          self->singleObsRadioButton
          self->removeAllObs
        endfor
        multipleToDo=0
      endfor
    endif
    if n_elements(mIdx) le (modelsNo-1) then mIdx=[mIdx,mIdx+1] else done=1
  endwhile
  
  textFile=self.mgr->getMagicEntityList()
  fm->writePlainTextFile, textFile, entFileList[1:n_elements(entFileList)-1]
  self->groupObsRadioButton
  self->removeAllObs
  self->singleObsRadioButton
  self->removeAllObs
  self.silentMode=prevSilentMode
  self.mgr->logging, /OFF
  a=self.mgr->dialogMessage(['Magic done!','Look at '+textFile,'for details'], title='ENTITY MAGIC DONE', /INFORMATION )
  obj_destroy, util
  
END

PRO FMEntitySelectionGUI::setAllObservationsFlag, value

END

PRO FMEntitySelectionGUI::setObsGroupStatCode, code

  self.info->setObsGroupStatCode, code
  self.obsGroupStatIndex=self.info->getObsGroupStatIndex()
  
END

FUNCTION FMEntitySelectionGUI::getObsGroupStatCode

  return, self.info->getObsGroupStatCode()
  
END

FUNCTION FMEntitySelectionGUI::getGroupNameXSize

  return, self->getModelSectionXSize()+10
  
END

FUNCTION FMEntitySelectionGUI::checkGroupTitleIntegrity, groupName

  utility=obj_new("FMUtility")
  nameIntegrity=utility->IsFileNameCompatible(groupName)
  obj_destroy, utility
  if not(nameIntegrity) then return, 0
  alreadySel=self.info->getObservedGroupTitles()
  for i=0, n_elements(alreadySel)-1 do if alreadySel[i] eq groupName then return,0
  return, 1
  
END

PRO FMEntitySelectionGUI::groupNameOKRequest, topWidget, groupName, selectedCodes

  ;if checkIntegrity(groupName)
  if self->checkGroupTitleIntegrity(groupName) then begin
    widget_control, topWidget, /DESTROY
    self.info->setObservedGroupStatSelectionIndex, self.obsGroupStatIndex
    self->addObservations, selectedCodes, groupName
    self->enable
    return
  endif
  if ~self.silentMode then aa=self->dialogMessage(['Check group name (already in use or contain an invalid character)'], title=['Group name'], /WARNING)
  widget_control, topWidget, /SHOW
  
END

FUNCTION FMEntitySelectionGUI::getObservedGroupStatNames, CODES=CODES

  return, self.info->getObservedGroupStatNames(CODES=CODES)
  
END

FUNCTION FMEntitySelectionGUI::getMonitoringGroupStatCodes

  return, self.info->getMonitoringGroupStatCodes()
  
END

FUNCTION FMEntitySelectionGUI::getObservedGroupStatSelectionIndex

  return, self.info->getObservedGroupStatSelectionIndex()
  
END

PRO FMEntitySelectionGUI::createGroupNameGUI, suggestedName, selectedCodes

  groupStatNames=self->getObservedGroupStatNames(CODES=CODES)
  groupStatCodes=CODES
  groupStatSelectionIndex=self->getObservedGroupStatSelectionIndex()
  title="Observed Group Info"
  groupNameBase = widget_base(/COLUMN, $
    uvalue=self, TLB_FRAME_ATTR=9, $
    title=title, /ALIGN_CENTER)
  label = widget_label(groupNameBase, $
    XOFFSET=0 ,YOFFSET=0, VALUE='Write a name for this group:', $
    SCR_YSIZE=self->getLabelYSize(), SCR_XSIZE=self->getGroupNameXSize(), /ALIGN_CENTER)
  ;self.groupNameText = widget_text(groupNameBase, UNAME='GROUPNAMETXT', XOFFSET=0, $
  groupNameText = widget_text(groupNameBase, UNAME='GROUPNAMETXT', XOFFSET=0, $
    SCR_XSIZE=self->getGroupNameXSize(), SCR_YSIZE=self->getLabelYSize() ,SENSITIVE=1, /EDITABLE, $
    font=self.textFont, uvalue=selectedCodes)
  infoBase=widget_base(groupNameBase, xpad=0, ypad=0,space=0, /COLUMN, /ALIGN_CENTER)
  label = widget_label(infoBase, $
    XOFFSET=0 ,YOFFSET=0, VALUE='Select a statistic to apply:', $
    SCR_YSIZE=self->getLabelYSize(), SCR_XSIZE=self->getGroupNameXSize(), /ALIGN_CENTER)
  statSelectionBase=widget_base(groupNameBase, xpad=0, ypad=0,space=0, /ROW, /EXCLUSIVE, /ALIGN_CENTER)
  for i=0, n_elements(groupStatCodes)-1 do begin
    groupStatButton= widget_button(statSelectionBase, $
      XOFFSET=0 ,YOFFSET=0, VALUE=groupStatNames[i], $
      SCR_XSIZE=xSize, SCR_YSIZE=ySize, $
      UVALUE=groupStatCodes[i], $
      event_pro=self.eventPrefix+"groupStatSelection")
    widget_control, groupStatButton, set_button= i eq groupStatSelectionIndex
  endfor
  infoBase=widget_base(groupNameBase, xpad=0, ypad=0,space=0, /COLUMN, /ALIGN_CENTER)
  label = widget_label(infoBase, $
    XOFFSET=0 ,YOFFSET=0, VALUE='(Same statistic computation will be applied on ALL groups)', $
    SCR_YSIZE=self->getLabelYSize(), SCR_XSIZE=self->getGroupNameXSize(), /ALIGN_CENTER)
  ;  label = widget_label(infoBase, $
  ;    XOFFSET=0 ,YOFFSET=0, VALUE='applied on ALL groups)', $
  ;    SCR_YSIZE=self->getLabelYSize(), SCR_XSIZE=self->getGroupNameXSize(), /ALIGN_CENTER)
  okBttBase=widget_base(groupNameBase, xpad=0, ypad=0,space=0, /ROW, /ALIGN_CENTER)
  okBtt=widget_button(okBttBase, value='OK', UVALUE=groupNameText, UNAME='DISPLAYOK', $
    event_pro=self.eventprefix+'groupNameOKRequest', SCR_XSIZE=70, SCR_YSIZE=35, /ALIGN_CENTER)
  widget_control, groupNameText, set_value=suggestedName
  self->disable
  screendimension = get_screen_size()
  geom = widget_info(groupNameBase, /GEOMETRY)
  xoffset = (screendimension[0] - geom.scr_xsize)/2
  yoffset = (screendimension[1] - geom.scr_ysize)/2
  widget_control, groupNameBase, XOFFSET=xoffset ,YOFFSET=yoffset
  widget_control, groupNameBase, /REALIZE
  widget_control, groupNameText, SET_TEXT_SELECT=[0, strlen(suggestedName)]
  widget_control, groupNameText, /INPUT_FOCUS
  
  xmanager, 'fairmode', groupNameBase, /JUST_REG
  
END

;PRO FMEntitySelectionGUI::userParameterSelections, parametersIndexes
;
;  widget_control, self.parameterList, SET_LIST_SELECT=[-1]
;  ;self.info->setParameterSelections, [-1]
;  self.info->setParameterCodeSelections, ['-1']
;  if parametersIndexes[0] ne -1 then begin
;    widget_control, self.parameterList, SET_LIST_SELECT=parametersIndexes
;    parametersCodes=self->getParameterCodesByIndex(parametersIndexes)
;    self.info->setParameterSelections, parametersIndexes, /INDEXES
;  endif
;  self->fillObsQueryList
;; for each group...
;;  for i=0, n_elements(group) do begin
;;    self.info->filterStationsCodesByParameters, groupCodes, NO_MATCH=NO_MATCH
;;  endfor
;
;END

PRO FMEntitySelectionGUI::userParameterCodeSelections, parametersCodes

  ;  widget_control, self.parameterList, SET_LIST_SELECT=[-1]
  ;  self.info->setParameterCodeSelections, ['-1']
  if parametersCodes[0] ne '-1' then begin
    parametersIndexes=self.info->getParameterIndexesByCodes(parametersCodes)
    widget_control, self.parameterList, SET_LIST_SELECT=parametersIndexes
    self.info->setParameterCodeSelections, parametersCodes
    self->fillObsQueryList
  endif
; for each group...
;  for i=0, n_elements(group) do begin
;    self.info->filterStationsCodesByParameters, groupCodes, NO_MATCH=NO_MATCH
;  endfor
  
END

PRO FMEntitySelectionGUI::fillParameterList

  parameterTypeIndex=self.info->getParameterTypeSelections()
  parameterTypeCodes=(self.info->getParameterTypeCodes())[parameterTypeIndex]
  
  parameterNames=self.info->getParameterNamesBySelectedTypes()
  parameterCodes=self.info->getParameterCodesBySelectedTypes()
  
  widget_control, self.parameterList, set_value=parameterNames, set_uvalue=parameterCodes
  self->userParameterCodeSelections, parameterCodes[0]
;widget_control, self.parameterList, SET_VALUE=self->getAllParametersNames()
;widget_control, self.parameterList, SET_LIST_SELECT=names
;parameterNames=self.info->getParameterNamesBySelectedElab()
;names=self.info->getParameterNames()
  
END

PRO FMEntitySelectionGUI::userParameterTypeSelections, parameterTypesIndexes, ALL=ALL

  if keyword_set(ALL) then parameterTypesIndexes=indgen(self.info->getParameterTypeNumber())
  if parameterTypesIndexes[0] ne -1 then begin
    widget_control, self.parameterTypeList, SET_LIST_SELECT=parameterTypesIndexes
    self.info->setParameterTypeSelections, parameterTypesIndexes
    self->fillParameterList
    return
  endif
  widget_control, self.parameterTypeList, SET_LIST_SELECT=[-1]
; ToDo: update if it need parameter list
  
END

PRO FMEntitySelectionGUI::fillParameterTypeList

  parameterTypeNames=self.info->getParameterTypeNames()
  widget_control, self.parameterTypeList, set_value=parameterTypeNames
  self->userParameterTypeSelections, /ALL
  
END

;parameter section
FUNCTION FMEntitySelectionGUI::getAllParameterNames

  names=self.info->getParameterNames()
  return, names
  
END

FUNCTION FMEntitySelectionGUI::getAllParameterCodes

  codes=self.info->getParameterCodes()
  return, codes
  
END

FUNCTION FMEntitySelectionGUI::getParameterMeasureUnitAtIndex, index

  names=self.info->getParameterMeasureUnits()
  return, names[index]
  
END

FUNCTION FMEntitySelectionGUI::getCurrentParameterSelections

  return, self->getParameterMeasureUnitAtIndex(self.info->getParametersSelection())
  
END

FUNCTION FMEntitySelectionGUI::checkIntegrity

  self.info->setAllScenariosFlag, 0
  self.info->setAllModelsFlag, 0
  self.info->setAllObservationsFlag, 0
  return, self.info->checkIntegrity(self)
  
END

PRO FMEntitySelectionGUI::updateToCaller

  newInfo=self.info->clone(/DEEP)
  self.mgr->updateEntityDisplayInfo, newInfo
  self.mgr->enable
  
END
; events
PRO FMEntitySelectionGUI::removeAllObs

  widget_control, self.selectedObservedList, get_uvalue=displayCodes
  if n_elements(displayCodes) ne 0 then begin
    widget_control, self.selectedObservedList, SET_LIST_SELECT=indgen(n_elements(displayCodes))
    self->removeObsButton
  endif
  
END

PRO FMEntitySelectionGUI::addAllObs

  widget_control, self.queryResultList, get_uvalue=displayCodes
  widget_control, self.queryResultList, SET_LIST_SELECT=indgen(n_elements(displayCodes))
  self->addObsButton
  
END

PRO FMEntitySelectionGUI::addObservations, selectedCodes, groupName, SINGLE=SINGLE

  self.info->addObservations, selectedCodes, groupName, SINGLE=self.singleButtonFlag
  self->displaySelectedObservations
  
END

PRO FMEntitySelectionGUI::addObsButton

  ;self->addSelected, self.singleButtonFlag
  widget_control, self.queryResultList, get_uvalue=displayCodes
  idxes=widget_info(self.queryResultList, /LIST_SELECT)
  if idxes[0] ne -1 and displayCodes[0] ne '-1' then begin
    selectedCodes=displayCodes[idxes]
    ;if a group is filled with all stations build a good name for him
    ;print, '********'
    ;print, selectedCodes
    ;print, displayCodes
    ;print, '********'
    if array_equal(selectedCodes, displayCodes) then groupName=self->getLastGroupName() else groupName='UserGroup'+strcompress(long(systime(/SECONDS)), /REMOVE)
    if self.singleButtonFlag then begin
      self->addObservations, selectedCodes, groupName, SINGLE=self.singleButtonFlag
    endif else begin
      self->createGroupNameGUI, groupName, selectedCodes
    endelse
    return
  endif
  if ~self.silentMode then aa=self->dialogMessage(['No observations to add...'], title=['Obs query'], /info)
  
END

PRO FMEntitySelectionGUI::groupObsRadioButton

  ; Released version: Do Noting
  widget_control, self.singleRadioButton, SET_BUTTON=0
  widget_control, self.groupRadioButton, /SET_BUTTON
  self.singleButtonFlag=0b
  self->displaySelectedObservations
  
END

PRO FMEntitySelectionGUI::singleObsRadioButton

  widget_control, self.singleRadioButton, /SET_BUTTON
  widget_control, self.groupRadioButton, SET_BUTTON=0
  self.singleButtonFlag=1b
  self->displaySelectedObservations
  
END

PRO FMEntitySelectionGUI::userUseObsModButton, select

  widget_control, self.observedModelFlagButton, set_button=select
  self.info->setUseObservedModelFlag, select
  parCodes=self.info->getParameterCodeSelections()
  self->userParameterCodeSelections, parCodes
  
END

PRO FMEntitySelectionGUI::userAllAvailableScenarioSelection, select, NO_RESET=NO_RESET, modelSelections=modelselections

  widget_control, self.allAvailableScenarioFlagButton, set_button=select
  self.info->setAllAvailableScenarioFlag, select
  self->fillScenarioList
  if n_elements(modelselections) ne 0 then self->userModelSelections,modelselections else self->userModelSelections, 0 
  if not(keyword_set(NO_RESET)) then begin
    self->userScenarioSelections, 0
  endif
;mods=self.info->getModelSelections()
;self->userModelSelections, mods
;set here right.... ****
;scenarioCodes=self.info->getScenarioCodeSelections()
;self->userParameterCodeSelections, parCodes
  
END

PRO FMEntitySelectionGUI::userRunSelection, index

  widget_control, self.runList, get_uvalue=RUNINDEXES
  valIdxs=where(RUNINDEXES ne -1, count)
  widget_control, self.runDescrText, SET_VALUE=''
  if count ne -1 then begin
    RUNINDEXES=RUNINDEXES[valIdxs]
    self->setRunSelection, RUNINDEXES[index]
    widget_control, self.runDescrText, SET_VALUE=self->getCurrentRunDescription()
  endif
  
END

PRO FMEntitySelectionGUI::userModelSelections, indexes

  if indexes[0] ne -1 then begin
    list=indexes
    self.info->setModelSelections, list
    widget_control, self.modelList, SET_LIST_SELECT=self.info->getModelSelections()
    widget_control, self.scenarioList, SET_LIST_SELECT=self.info->getScenarioSelections()
    self->fillRunList
    return
  endif
  self->fillRunList, /VOID
  
END

PRO FMEntitySelectionGUI::userScenarioSelections, indexes

  hlp=strcompress(indexes,/remove_all)
  if hlp[0] ne '-1' then begin
    scenList=indexes
    self.info->setScenarioSelections, scenList
    widget_control, self.scenarioList, SET_LIST_SELECT=self.info->getScenarioSelections()
    widget_control, self.modelList, SET_LIST_SELECT=self.info->getModelSelections()
    self->fillRunList
    return
  endif
  self->fillRunList, /VOID
  
END

PRO FMEntitySelectionGUI::removeObsButton

  widget_control, self.selectedObservedList, get_uvalue=displayCodes
  idxes=widget_info(self.selectedObservedList, /LIST_SELECT)
  if strcompress(idxes[0], /REMOVE) ne '-1' and strcompress(displayCodes[0], /REMOVE) ne '-1' then begin
    ;if idxes[0] ne -1 and displayCodes[0] ne -1 then begin
    ;print, idxes
    if self.singleButtonFlag eq 1b then selectedCodes=displayCodes[idxes] else selectedCodes=idxes
    ;print, selectedCodes
    self.info->removeObservations, selectedCodes, SINGLE=self.singleButtonFlag
    self->displaySelectedObservations
    return
  endif
  if ~self.silentMode then aa=self->dialogMessage(['No observations to remove...'], title=['Obs selected'], /info)
  
END

;PRO FMEntitySelectionGUI::categoryValuesListSelection, id, categoryIndex, valueIndex
;
;  categorySelections=self.info->getCategorySelections()
;  categorySelections[categoryIndex]=valueIndex
;  self.info->setCategorySelections, categorySelections
;  widget_control, id, SET_LIST_SELECT=valueIndex+1
;  ;categorySelections=self.info->getCategorySelections()
;  ;print, categorySelections
;  self->fillObsQueryList
;
;END

PRO FMEntitySelectionGUI::obsQueryListSelection


END

PRO FMEntitySelectionGUI::saveObsButton

  fsm=obj_new('FMFileSystemManager')
  
  filter=['*'+fsm->getObservedListExtension()]
  fix_filter='*'+fsm->getObservedListExtension()
  obsFile=dialog_pickfile(DEFAULT_EXTENSION=fsm->getObservedListExtension(), $
    DIALOG_PARENT=self->getTopBase(), $
    FILTER=filter, FIX_FILTER=fix_filter, $
    GET_PATH=path, PATH=fsm->getSaveDir(), $
    TITLE='Save observations', /OVERWRITE_PROMPT, /WRITE)
  if obsFile ne '' then begin
    ;	print, '****'
    ;	print, obsFile
    ;	print, '****'
    fsm->saveObservationList, obsFile, self.info->getObservedCodesSelections(), self.info->getObservedGroupTitles(), self.info->getObservedCodesGroupSelections(), self.info->getObservedGroupStatCodeSelection()
    discard=self->dialogMessage([obsFile+'Saved'], title=['Save obsevations'], /INFO)
  endif
  obj_destroy, fsm
  
END

PRO FMEntitySelectionGUI::loadObsButton

  answer='YES'
  if ~self.silentMode then answer=self->dialogMessage(['This operation will ovewrite monitoring selections. Do you want to continue?'], title=['Load obsevations'], /QUESTION)
  fsm=obj_new('FMFileSystemManager')
  if strupcase(answer) eq 'YES' then begin
    self->setAllObservationsFlag, 0
    filter=['*'+fsm->getObservedListExtension()]
    fix_filter='*'+fsm->getObservedListExtension()
    obsFile=dialog_pickfile(DEFAULT_EXTENSION=fsm->getObservedListExtension(), $
      DIALOG_PARENT=self->getTopBase(), $
      FILTER=filter, $FIX_FILTER=fix_filter, $
      GET_PATH=path, /MUST_EXIST, PATH=fsm->getSaveDir(), $
      TITLE='Load a previous saved observation list'  )
    if obsFile ne '' then begin
    
      fsm->loadObservationList, obsFile, observedSingleCodes, observedGroupTitles, observedGroupCodes, observedGroupStatCode, VALIDSINGLE=VALIDSINGLE, VALIDGROUP=VALIDGROUP
      self.info->cancelObservedData
      if keyword_set(VALIDSINGLE) then self.info->setObservedCodesSelections, observedSingleCodes
      if keyword_set(VALIDGROUP) then begin
        self.info->setObservedGroupTitles, observedGroupTitles
        self.info->setObservedCodesGroupSelections, observedGroupCodes
        self.info->setObservedGroupStatCodeSelection, observedGroupStatCode
      endif
      self->singleObsRadioButton
      ;self->displayGroupObservations
      ;self->displaySingleObservations
      widget_control, self.singleRadioButton, /SET_BUTTON
      widget_control, self.groupRadioButton, SET_BUTTON=0
      if ~self.silentMode then discard=self->dialogMessage([obsFile+' : Restored'], title=['Save obsevations'], /INFO)
    endif
  endif
  obj_destroy, fsm
  
END

PRO FMEntitySelectionGUI::categoryValuesExclusiveSelection, id, categoryIndex, valueIndex

  categorySelections=self.info->getCategorySelections()
  categorySelections[categoryIndex]=valueIndex
  self.info->setCategorySelections, categorySelections
  widget_control, id, /SET_BUTTON
  self->fillObsQueryList
  
END

PRO FMEntitySelectionGUI::obsStoredListSelection

  widget_control, self.selectedObservedList, get_uvalue=displayCodes
  hlp=strcompress(displayCodes,/remove_all)
  if hlp[0] ne '-1' then begin
    if self.singleButtonFlag eq 1b then begin
      self->displaySingleDescr
    endif else begin
      self->displayGroupDescr
    endelse
    return
  endif
  widget_control, self.observedDescrText, set_value=text
  
END
; gui check/conf/fill/update...
PRO FMEntitySelectionGUI::configure

  ; Realize and MoveToCenter is done by superclass
  ; Here only fill & set internal widget
  ;self.mgr->streamPrintOfMode
  ; new october 19 2010
  prevSilentMode=self.silentMode
  self.silentMode=1
  ;savePreviousParams=self.info->getParameterSelections()
  savePreviousCodeParams=self.info->getParameterCodeSelections()
  savePreviousTypeParams=self.info->getParameterTypeSelections()
  
  if savePreviousCodeParams[0] eq '-1' then begin
    ;savePreviousIndexParams=[0]
    savePreviousCodeParams=(self.info->getParameterCodes())[0]
    savePreviousTypeParams=indgen(self.info->getParameterTypeNumber())
  endif
  self->fillParameterTypeList
  
  self->fillScenarioList
  self->fillModelList
  self->userAllAvailableScenarioSelection, self.info->getAllAvailableScenarioFlag(), /NO_RESET, modelSelections=self.info->getModelSelections() 
  
  self->userModelSelections, self.info->getModelSelections()
  self->userScenarioSelections, self.info->getScenarioSelections()
  
  self->fillCategoryObservationSelections, self.info->getCategorySelections()
  self->singleObsRadioButton
  self->userUseObsModButton, self.info->getUseObservedModelFlag()
  
  ;parametersIndexes=self.info->getParameterSelections()
  self->userParameterTypeSelections, savePreviousTypeParams
  ;self->userParameterSelections, savePreviousParams
  self->userParameterCodeSelections, savePreviousCodeParams
  
  self->setObsGroupStatIndex, self.info->getObservedGroupStatSelectionIndex()
  self.silentMode=prevSilentMode
  
END

FUNCTION FMEntitySelectionGUI::getObsGroupStatIndex

  return, self.obsGroupStatIndex
  
END

PRO FMEntitySelectionGUI::setObsGroupStatIndex, index

  self.obsGroupStatIndex=index
  
END
PRO FMEntitySelectionGUI::fillRunList, VOID=VOID

  ;get valid RUN combinations between model and scenario from
  ;self.info
  widget_control, self.runDescrText, SET_VALUE=''
  if not(keyword_set(VOID)) then begin
    mods=self.info->getModelSelections()
    scens=self.info->getScenarioSelections()
    if scens[0] ne -1 and mods ne [-1] then begin
      runList=self.info->buildRunList(scens, mods, RUNINDEXES=RUNINDEXES)
      if runList[0] ne '-1' then widget_control, self.runList, SET_VALUE=runList, set_uvalue=RUNINDEXES else widget_control, self.runList, SET_VALUE=''
      return
    endif
  endif
  widget_control, self.runList, SET_VALUE=''
  
END

PRO FMEntitySelectionGUI::fillScenarioList

  widget_control, self.scenarioList, SET_VALUE=self->getAllScenarioNames()
  
END

PRO FMEntitySelectionGUI::fillModelList

  widget_control, self.modelList, SET_VALUE=self->getAllModelNames()
  
END

PRO FMEntitySelectionGUI::fillCategoryObservationSelections, list

  prefix='CATEGORY**'
  widgetTypesList=self.info->getCategoryWidgetTypes()
  categorySelections=self.info->getCategorySelections()
  no=self.info->getCategoryNumber()
  ;catCodes=self.info->getCategoryCodes()
  ;catValues=self.info->getCategoryValues(catCodes[i-1])
  for i=0, no-1 do begin
    ;self->extractCategoryInfo, categoryIndex=categoryIndex, categoryValue=categoryValue
    idToFind=prefix+strcompress(i, /REMOVE)
    if widgetTypesList[i] eq 1 then begin
      check=widget_info(self->getTopBase(), FIND_BY_UNAME=idToFind)
      prevSilentMode=self.silentMode
      self.silentMode=1
      self->categoryValuesExclusiveSelection, check, i, list[i]
      self.silentMode=prevSilentMode
    endif else begin
      idToFind=idToFind+'**'+strcompress(list[i], /REMOVE)
      check=widget_info(self->getTopBase(), FIND_BY_UNAME=idToFind)
      self->categoryValuesExclusiveSelection, check, i, list[i]
    endelse
  endfor
  
END

PRO FMEntitySelectionGUI::fillObsQueryList

  self->executeObservationQuery, NORESULT=NORESULT
  ;print, '--> Void List?', keyword_set(NORESULT)
  self->displayObservationQueryResult
  
END
; gui building
PRO FMEntitySelectionGUI::displaySingleDescr

  ; idxs=widget_info(self.selectedObservedList, /LIST_SELECT)
  ; elems=n_elements(idxs)
  ; text=['Group details:']
  ; for i=0, elems-1 do begin
  ;	groupText=self.info->buildObsGroupDescription(idxs[i])
  ;	text=[text,groupText]
  ; endfor
  widget_control, self.observedDescrText, set_value=''
  
END

PRO FMEntitySelectionGUI::displayGroupDescr

  idxs=widget_info(self.selectedObservedList, /LIST_SELECT)
  elems=n_elements(idxs)
  if idxs[0] eq -1 then elems=0
  text=['Group details:']
  for i=0, elems-1 do begin
    groupText=self.info->buildObsGroupDescription(idxs[i])
    text=[text,groupText]
  endfor
  widget_control, self.observedDescrText, set_value=text
  
END

; set/get
PRO FMEntitySelectionGUI::setLastGroupName, name

  self.lastGroupName=name
  
END

FUNCTION FMEntitySelectionGUI::getLastGroupName

  return, self.lastGroupName
  
END

PRO FMEntitySelectionGUI::setRunSelection, index

  self.runSelection=index
  
END

FUNCTION FMEntitySelectionGUI::getRunSelection

  return, self.runSelection
  
END
; info interaction
PRO FMEntitySelectionGUI::displaySelectedObservations

  ;self.info->observedCodesSelections;pointer of pointers (to manage single and group)
  ;self.info->observedGroupNames;pointer of pointers (to manage single and group)
  if self.singleButtonFlag eq 1b then begin
    self->displaySingleObservations
  endif else begin
    self->displayGroupObservations
  endelse
  
END

PRO FMEntitySelectionGUI::displaySingleObservations

  displayCodes=self.info->getObservedCodesSelections()
  if displayCodes[0] ne '-1' then begin
    nameList=self.info->getObservedNamesSelections()
    widget_control, self.selectedObservedList, set_value=nameList, set_uvalue=displayCodes
    hintSingleLabel=widget_info(self->getTopBase(), FIND_BY_UNAME=self.eventPrefix+'SingleHint')
    widget_control, hintSingleLabel, set_value='S#: '+strcompress(n_elements(nameList), /REMOVE)
    return
  endif
  hintSingleLabel=widget_info(self->getTopBase(), FIND_BY_UNAME=self.eventPrefix+'SingleHint')
  widget_control, hintSingleLabel, set_value='No Single'
  widget_control, self.selectedObservedList, set_value='', set_uvalue=[-1]
  
END

PRO FMEntitySelectionGUI::displayGroupObservations

  displayCodes=self.info->getObservedCodesGroupSelections()
  if self.info->isValidObservedGroupTitles() then begin
    nameList=self.info->getObservedGroupTitles()
    widget_control, self.selectedObservedList, set_value=nameList, set_uvalue=indgen(n_elements(nameList))
    hintGroupLabel=widget_info(self->getTopBase(), FIND_BY_UNAME=self.eventPrefix+'GroupHint')
    widget_control, hintGroupLabel, set_value='G#: '+strcompress(n_elements(nameList), /REMOVE)
    ;print, (*displayCodes[0])[0]
    ;widget_control, self.groupDescr, set_value=
    return
  endif
  ;aa=self->dialogMessage(['No observations...'], title=['Obs query'], /info)
  hintGroupLabel=widget_info(self->getTopBase(), FIND_BY_UNAME=self.eventPrefix+'GroupHint')
  widget_control, hintGroupLabel, set_value='No group'
  widget_control, self.selectedObservedList, set_value='', set_uvalue=[-1]
  
END

PRO FMEntitySelectionGUI::displayObservationQueryResult

  displayCodes=self.info->getObservedQueryCodesSelections()
  if displayCodes[0] ne '-1' then begin
    nameList=self.info->getObservedQueryNames()
    widget_control, self.queryResultList, set_value=nameList, set_uvalue=displayCodes
    return
  endif
  if (~self.silentMode) then aa=self->dialogMessage(['No observations for types selected, try other one'], title=['Obs query'], /info)
  widget_control, self.queryResultList, set_value='', set_uvalue=[-1]
  
END

PRO FMEntitySelectionGUI::executeObservationQuery, groupName=groupName, NORESULT=NORESULT

  ; self.info->executeObservationParametersQuery, groupName=groupName
  ; self->executeObservationTypeQuery, groupName=groupName
  self.info->executeObservationQuery, groupName=groupName, NORESULT=NORESULT
  lastGroupName=groupName
  self->setLastGroupName, lastGroupName
  
END

FUNCTION FMEntitySelectionGUI::getTitle

  return, 'Data selection'
  
END

;; Scenario section - description
FUNCTION FMEntitySelectionGUI::getScenarioDescriptionAtIndex, index

  descrs=self.info->getScenarioDescriptions()
  return, descrs[index]
  
END

FUNCTION FMEntitySelectionGUI::getCurrentScenarioDescription

  return, self->getDescriptionAtIndex(self.info->getScenarioSelection())
  
END

FUNCTION FMEntitySelectionGUI::getAllScenarioDescriptions

  descrs=self.info->getScenarioDescriptions()
  return, descrs
  
END

;; Scenario section - name
FUNCTION FMEntitySelectionGUI::getScenarioNameAtIndex, index

  names=self.info->getScenarioNames()
  return, names[index]
  
END

FUNCTION FMEntitySelectionGUI::getCurrentScenarioName

  return, self->getNameAtIndex(self.info->getScenarioSelection())
  
END

FUNCTION FMEntitySelectionGUI::getAllScenarioNames

  names=self.info->getScenarioNames()
  return, names
  
END

; Run section - description
FUNCTION FMEntitySelectionGUI::getRunDescriptionAtIndex, index

  descrs=self.info->getRunDescriptions()
  return, descrs[index]
  
END

FUNCTION FMEntitySelectionGUI::getCurrentRunDescription

  return, self->getRunDescriptionAtIndex(self.runSelection)
  
END

FUNCTION FMEntitySelectionGUI::getAllRunDescriptions

  descrs=self.info->getRunDescription()
  return, descrs
  
END
; Run section - code
FUNCTION FMEntitySelectionGUI::getRunCodeAtIndex, index

  codes=self.info->getRunCodes()
  return, codes[index]
  
END

FUNCTION FMEntitySelectionGUI::getCurrentRunCode

  return, self->getRunCodeAtIndex(self.info->getRunSelection())
  
END

FUNCTION FMEntitySelectionGUI::getAllRunCodes

  codes=self.info->getRunCodes()
  return, codes
  
END
;; Scenario section - code
FUNCTION FMEntitySelectionGUI::getScenarioCodeAtIndex, index

  codes=self.info->getScenarioCodes()
  return, codes[index]
  
END

FUNCTION FMEntitySelectionGUI::getCurrentScenarioCode

  return, self->getScenarioCodeAtIndex(self.info->getScenarioSelection())
  
END

FUNCTION FMEntitySelectionGUI::getAllScenarioCodes

  codes=self.info->getScenarioCodes()
  return, codes
  
END

FUNCTION FMEntitySelectionGUI::getScenarioCodes

  codes=self.info->getScenarioCodes()
  return, codes
  
END
;Model section - description
FUNCTION FMEntitySelectionGUI::getModelDescriptionAtIndex, index

  descrs=self.info->getModelDescriptions()
  return, descrs[index]
  
END

FUNCTION FMEntitySelectionGUI::getCurrentModelDescription

  return, self->getModelDescriptionAtIndex(self.info->getModelSelection())
  
END

FUNCTION FMEntitySelectionGUI::getAllModelDescriptions

  descrs=self.info->getModelDescriptions()
  return, descrs
  
END
;Model section - name
FUNCTION FMEntitySelectionGUI::getModelNameAtIndex, index

  names=self.info->getModelNames()
  return, names[index]
  
END

FUNCTION FMEntitySelectionGUI::getCurrentModelName

  return, self->getModelNameAtIndex(self.info->getModelSelection())
  
END

FUNCTION FMEntitySelectionGUI::getAllModelNames

  names=self.info->getModelNames()
  return, names
  
END
;Model section - code
FUNCTION FMEntitySelectionGUI::getModelCodeAtIndex, index

  codes=self.info->getModelCodes()
  return, codes[index]
  
END

FUNCTION FMEntitySelectionGUI::getCurrentModelCode

  return, self->getModelCodeAtIndex(self.info->getModelSelection())
  
END

FUNCTION FMEntitySelectionGUI::getAllModelCodes

  codes=self.info->getModelCodes()
  return, codes
  
END
;category section
FUNCTION FMEntitySelectionGUI::getAllCategoryTitles

  titles=self.info->getCategoryTitles()
  return, titles
  
END

FUNCTION FMEntitySelectionGUI::getAllCategoryCodes

  titles=self.info->getCategoryCodes()
  return, titles
  
END
;build
PRO FMEntitySelectionGUI::build

  ;bestDimensions=self->getBestDimensions()
  title=self->getTitle()
  
  ; base = widget_base(/COLUMN,scr_XSIZE=self.dimensions[0], $
  ;  scr_ysize=self.dimensions[1], $
  ;  uvalue=self, TLB_FRAME_ATTR=17, $
  ;  title=title, /TLB_KILL_REQUEST_EVENTS, event_pro=self.eventprefix+'destroyWindow')
  base = widget_base(/COLUMN, $
    uvalue=self, TLB_FRAME_ATTR=1, $
    title=title, /TLB_KILL_REQUEST_EVENTS, event_pro=self.eventprefix+'destroyWindow')
    
  mainBase = widget_base(base, /COLUMN)
  
  subBase11 = widget_base(mainbase,xpad=self.xPad, ypad=self.yPad,space=0,/ROW)
  subBase12 = widget_base(mainbase,xpad=self.xPad, ypad=self.yPad,space=0,/ROW, /ALIGN_CENTER)
  subBase111 = widget_base(subBase11,xpad=1, ypad=1,space=1,/COLUMN, FRAME=1)
  subBase112 = widget_base(subBase11,xpad=1, ypad=1,space=1,/COLUMN, FRAME=1)
  
  self->buildModelSection, subBase111
  self->buildParameterSection, subBase112
  self->buildObservationSection, subBase112
  self->buildOKButton, subBase12
  
  self->SetTopBase, base
  xmanager, 'fairmode', base, /JUST_REG
  
END

FUNCTION FMEntitySelectionGUI::getModelSectionXSize

  return, self->getVisibleXSize()-self->getObservationSectionXSize()
  
END

FUNCTION FMEntitySelectionGUI::getModelSectionYSize

  ; Available Y space subtract titles spaces (3) and exclusive button space (1)
  return, self->getVisibleYSize()-(4*self->getLabelYSize())
  
END

FUNCTION FMEntitySelectionGUI::getObservationSectionXSize

  return, self->getVisibleXSize()*.60
  
END

FUNCTION FMEntitySelectionGUI::getParameterSectionXSize

  return, self->getVisibleXSize()*.31
  
END

FUNCTION FMEntitySelectionGUI::getLabelYSize

  return, 20
  
END

FUNCTION FMEntitySelectionGUI::getObservationSectionYSize

  return, self->getVisibleYSize()
  
END

FUNCTION FMEntitySelectionGUI::getModelListBoxXSize

  return, self->getModelSectionXSize()*.50
  
END

FUNCTION FMEntitySelectionGUI::getModelListBoxYSize

  return, self->getModelSectionYSize()*.40
  
END

FUNCTION FMEntitySelectionGUI::getObservationListBoxXSize

  return, self->getObservationSectionXSize()*.42
  
END

FUNCTION FMEntitySelectionGUI::getObservationListBoxYSize

  return, self->getObservationSectionYSize()*.33
  
END

FUNCTION FMEntitySelectionGUI::getModelDescrYSize

  return, self->getModelSectionYSize()*.20
  
END

FUNCTION FMEntitySelectionGUI::getButtonXSize

  return, self->getVisibleXSize()*.12
  
END

FUNCTION FMEntitySelectionGUI::getButtonYSize

  return, self->getVisibleYSize()*.08
  
END

FUNCTION FMEntitySelectionGUI::getCategoryXSize, catNo

  return, (self->getObservationSectionXSize())/catNo
  
END

FUNCTION FMEntitySelectionGUI::getCategoryYSize, valNo

  return, self->getObservationSectionYSize()*.30/valNo
  
END

PRO FMEntitySelectionGUI::buildModelSection, base

  sectionTitle=widget_label(base, value='Model & Scenario selection', SCR_YSIZE=self->getLabelYSize(), /ALIGN_LEFT, font=self.titleFont)
  
  doubleListBase=widget_base(base, xpad=0, ypad=0, space=0, /ROW)
  runsListBase=widget_base(base, xpad=0, ypad=0, space=0, /COLUMN)
  descrListBase=widget_base(base, xpad=0, ypad=0, space=0, /ROW)
  extraFlagBase=widget_base(base, xpad=0, ypad=0, space=0, /ROW)
  
  scenarioBase=widget_base(doubleListBase, xpad=0, ypad=0, space=0, /COLUMN)
  modelBase=widget_base(doubleListBase, xpad=0, ypad=0, space=0, /COLUMN)
  
  scenTitle=widget_label(scenarioBase, value='Scenario', scr_ysize=self->getLabelYSize(), /ALIGN_LEFT, font=self.titleFont)
  self.scenarioList=widget_list(scenarioBase, value=[''], $
    SCR_XSIZE=self->getModelListBoxXSize(), SCR_YSIZE=self->getModelListBoxYSize(), $
    event_pro=self.eventPrefix+'scenarioListSelection', /MULTIPLE)
    
    
  nameTitle=widget_label(modelBase, value='Model', scr_ysize=self->getLabelYSize(), /ALIGN_LEFT, font=self.titleFont)
  self.modelList=widget_list(modelBase, value=[''], $
    SCR_XSIZE=self->getModelListBoxXSize(), SCR_YSIZE=self->getModelListBoxYSize(), $
    event_pro=self.eventPrefix+'modelListSelection', /MULTIPLE)
    
  runTitle=widget_label(runsListBase, value='Run', scr_ysize=self->getLabelYSize(), /ALIGN_LEFT, font=self.titleFont)
  self.runList=widget_list(runsListBase, value=[''], $
    SCR_XSIZE=self->getModelListBoxXSize()*2, SCR_YSIZE=self->getModelListBoxYSize(), $
    event_pro=self.eventPrefix+'runListSelection')
    
  self.runDescrText = widget_Text(descrListBase, UNAME='RUNDESCRTXT', XOFFSET=0, $
    SCR_XSIZE=self->getModelListBoxXSize()*2 ,SCR_YSIZE=self->getModelDescrYSize() ,SENSITIVE=1 ,/ALL_EV, $
    font=self.textFont, /SCROLL, /WRAP)
    
  obsModelBase = widget_base(extraFlagBase, $
    XOFFSET=0 ,YOFFSET=0, /NONEXCLUSIVE, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /COLUMN)
  self.observedModelFlagButton = widget_button(obsModelBase, $
    XOFFSET=0 ,YOFFSET=0, VALUE='MOD without OBS', event_pro=self.eventPrefix+'useObsModButton', $
    SCR_YSIZE=self->getLabelYSize(), sensitive=self.mgr->isAdvancedFilter())
    
  allAvailableScenBase = widget_base(extraFlagBase, $
    XOFFSET=0 ,YOFFSET=0, /NONEXCLUSIVE, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /COLUMN)
  self.allAvailableScenarioFlagButton = widget_button(obsModelBase, $
    XOFFSET=0 ,YOFFSET=0, VALUE='All available scenario(s)', event_pro=self.eventPrefix+'allAvailableScenarioButton', $
    SCR_YSIZE=self->getLabelYSize(), sensitive=self.mgr->isAdvancedFilter())
    
END

; new october 19 2010
PRO FMEntitySelectionGUI::buildParameterSection, base

  parameterInfoBase=widget_base(base, xpad=0, ypad=0, space=0, /ROW)
  
  parameterTypeBase=widget_base(parameterInfoBase, xpad=0, ypad=0, space=0, /COLUMN)
  parameterBase=widget_base(parameterInfoBase, xpad=0, ypad=0, space=0, /COLUMN)
  
  parameterTypeTitle=widget_label(parameterTypeBase, value='Type', $
    scr_ysize=self->getLabelYSize(), /ALIGN_LEFT, font=self.titleFont)
    
  self.parameterTypeList=widget_list(parameterTypeBase, value=[''], $
    SCR_XSIZE=self->getParameterSectionXSize(), SCR_YSIZE=self->getObservationListBoxYSize(), $
    event_pro=self.eventPrefix+'parameterTypeListSelection', /MULTIPLE)
    
  parameterTypeTitle=widget_label(parameterBase, value='Parameter selection', $
    scr_ysize=self->getLabelYSize(), /ALIGN_LEFT, font=self.titleFont)
    
  self.parameterList=widget_list(parameterBase, value=[''], $
    SCR_XSIZE=self->getParameterSectionXSize(), SCR_YSIZE=self->getObservationListBoxYSize(), $
    event_pro=self.eventPrefix+'parameterListSelection', /MULTIPLE)
    
;  self.parameterDescriptionText= widget_text(parameterBase, UNAME='PARAMETERDESCTXT', XOFFSET=0, SENSITIVE=1 ,/ALL_EV, $
;    SCR_XSIZE=self->getParameterListBoxXSize(), SCR_YSIZE=self->getParameterBoxYSize(), $
;    font=self.textFont, /ALIGN_LEFT)
    
END
;end
PRO FMEntitySelectionGUI::buildObservationSection, base

  sectionTitle=widget_label(base, value='Stations: Filtering and selection', $
    scr_ysize=self->getLabelYSize(), /ALIGN_LEFT, font=self.titleFont)
    
  categorySelectionsBase=widget_base(base, xpad=0, ypad=0, space=0, /ROW)
  lowerSectionBase=widget_base(base, xpad=0, ypad=0, space=0, /ROW)
  queryAndDescrBase=widget_base(lowerSectionBase, xpad=0, ypad=0, space=0, /COLUMN)
  actionBase=widget_base(lowerSectionBase, xpad=0, ypad=0, space=0, /COLUMN)
  observationsSelectionsListBase=widget_base(lowerSectionBase, xpad=0, ypad=0, space=0, /COLUMN)
  
  self->buildCategoryAndValues, categorySelectionsBase
  
  fillFromQueryBase=widget_base(queryAndDescrBase, xpad=0, ypad=0, space=0, /COLUMN)
  
  queryResultTitle=widget_label(fillFromQueryBase, value='Available stations', $
    SCR_YSIZE=self->getLabelYSize(), /ALIGN_LEFT, font=self.titleFont)
  self.queryResultList=widget_list(fillFromQueryBase, value=[''], $
    SCR_XSIZE=self->getObservationListBoxXSize(), SCR_YSIZE=self->getObservationListBoxYSize(), $
    event_pro=self.eventPrefix+'obsQueryListSelection', /MULTIPLE)
    
  self.observedDescrText= widget_text(queryAndDescrBase, UNAME='OBSDESCTXT', XOFFSET=0, /SCROLL, $
    SCR_XSIZE=self->getObservationListBoxXSize(), SCR_YSIZE=self->getLabelYSize()*1.5 ,SENSITIVE=1 ,/ALL_EV, $
    font=self.textFont, /ALIGN_LEFT, /WRAP)
    
  optionBase=widget_base(actionBase, xpad=0, ypad=0, space=0, /COLUMN, /ALIGN_CENTER)
  
  geometricLabelTitle=widget_label(optionBase, value='', $
    SCR_YSIZE=self->getLabelYSize())
  singleOrGroupBase = widget_base(optionBase, UNAME='WID_BASE_4', $
    XOFFSET=0 ,YOFFSET=0, /EXCLUSIVE, /ALIGN_CENTER, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /COLUMN)
  actionButtonBase = widget_base(optionBase, UNAME='WID_BASE_4', $
    XOFFSET=0 ,YOFFSET=0, /ALIGN_CENTER, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /COLUMN)
  singleGroupBaseSintesys = widget_base(optionBase, UNAME='WID_BASE_4', $
    XOFFSET=0 ,YOFFSET=0, /ALIGN_CENTER, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /COLUMN)
    
  self.singleRadioButton= widget_button(singleOrGroupBase, /ALIGN_CENTER, $
    XOFFSET=0 ,YOFFSET=0, VALUE='Single mode', event_pro=self.eventPrefix+'singleObsRadioButton', $
    SCR_XSIZE=self->getButtonXSize() ,SCR_YSIZE=self->getButtonYSize(), /NO_RELEASE)
  self.groupRadioButton= widget_button(singleOrGroupBase, /ALIGN_CENTER, $
    XOFFSET=0 ,YOFFSET=0, VALUE='Group mode', event_pro=self.eventPrefix+'groupObsRadioButton', $
    SCR_XSIZE=self->getButtonXSize() ,SCR_YSIZE=self->getButtonYSize(), /NO_RELEASE) ;, sensitive=0)
  selectedSingleHint=widget_label(singleGroupBaseSintesys, value='S:  ', UNAME=self.eventPrefix+'SingleHint', $
    SCR_XSIZE=self->getButtonXSize(), SCR_YSIZE=self->getLabelYSize(), /ALIGN_LEFT)
  selectedGroupHint=widget_label(singleGroupBaseSintesys, value='G:  ', UNAME=self.eventPrefix+'GroupHint', $
    SCR_XSIZE=self->getButtonXSize(), SCR_YSIZE=self->getLabelYSize(), /ALIGN_LEFT, sensitive=0)
    
    
  self.addObservedButton= widget_button(actionButtonBase, /ALIGN_CENTER, $
    XOFFSET=0 ,YOFFSET=0, VALUE='Add', event_pro=self.eventPrefix+'addObsButton', $
    SCR_XSIZE=self->getButtonXSize() ,SCR_YSIZE=self->getButtonYSize())
  self.removeObservedButton= widget_button(actionButtonBase, /ALIGN_CENTER, $
    XOFFSET=0 ,YOFFSET=0, VALUE='Remove', event_pro=self.eventPrefix+'removeObsButton', $
    SCR_XSIZE=self->getButtonXSize() ,SCR_YSIZE=self->getButtonYSize())
    
  selectionsBase=widget_base(observationsSelectionsListBase, xpad=0, ypad=0, space=0, /COLUMN)
  
  selectedTitle=widget_label(selectionsBase, value='Selected stations', $
    SCR_YSIZE=self->getLabelYSize(), /ALIGN_LEFT, font=self.titleFont)
  self.selectedObservedList=widget_list(selectionsBase, value=[''], $
    SCR_XSIZE=self->getObservationListBoxXSize(), SCR_YSIZE=self->getObservationListBoxYSize(), $
    event_pro=self.eventPrefix+'obsStoredListSelection', /MULTIPLE)
    
  loadSaveButtonBase=widget_base(observationsSelectionsListBase, xpad=0, ypad=0, space=0, /ROW, /ALIGN_RIGHT)
  self.loadObservedButton= widget_button(loadSaveButtonBase, $
    XOFFSET=0 ,YOFFSET=0, VALUE='Load Obs', event_pro=self.eventPrefix+'loadObsButton', $
    SCR_XSIZE=self->getButtonXSize(), SCR_YSIZE=self->getButtonYSize())
  self.saveObservedButton= widget_button(loadSaveButtonBase, $
    XOFFSET=0 ,YOFFSET=0, VALUE='Save Obs', event_pro=self.eventPrefix+'saveObsButton', $
    SCR_XSIZE=self->getButtonXSize() ,SCR_YSIZE=self->getButtonYSize())
    
END

PRO FMEntitySelectionGUI::buildCategoryAndValues, base

  catNo=self.info->getCategoryNumber()
  catNames=self.info->getCategoryTitles()
  catCodes=self.info->getCategoryCodes()
  catWidgetTypes=self.info->getCategoryWidgetTypes()
  xSize=self->getCategoryXSize(catNo)
  ySize=self->getCategoryYSize(catNo)
  ;print, xSize, ySize
  for i=1, catNo do begin
    catValues=self.info->getCategoryValues(catCodes[i-1])
    ; Add "All" as "special one"
    catValues=['All', catValues]
    aCategoryBase = widget_base(base, UNAME='WID_BASE_4', $
      XOFFSET=0 ,YOFFSET=0, FRAME=1, $ ;SCR_XSIZE=xSize, SCR_YSIZE=ySize, $
      TITLE='IDL' ,SPACE=1 ,XPAD=0 ,YPAD=0, /COLUMN)
    aCatTitle=widget_label(aCategoryBase, value=catNames[i-1], $
      /ALIGN_LEFT, font=self.titleFont)
    ;Use find by uname!!!!
    ;uname='CATEGORY**'+catCodes[i-1]
    uname='CATEGORY**'+strcompress(i-1, /REMOVE)
    if catWidgetTypes[i-1] eq 1b then begin
      ;build widget_list
      ySize=self->getCategoryYSize(1)
      selectedListBox=widget_list(aCategoryBase, value=catValues, $
        event_pro=self.eventPrefix+'categoryValuesListSelection', $
        SCR_XSIZE=xSize, SCR_YSIZE=ySize, UVALUE={catIdx:i-1,catV:catValues}, $
        UNAME=uname)
    endif else begin
      ;build exclusive buttons
      ySize=self->getCategoryYSize(n_elements(catValues))
      catValuesExclusiveBase = Widget_Base(aCategoryBase, UNAME='WID_BASE_4', $
        XOFFSET=0 ,YOFFSET=0, /EXCLUSIVE, $
        TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /COLUMN)
      for j=0, n_elements(catValues)-1 do begin
        ;print, uname+'**'+strcompress(j-1, /REMOVE)
        aCatValuesButton= widget_button(catValuesExclusiveBase, $
          XOFFSET=0 ,YOFFSET=0, VALUE=catValues[j], UNAME=uname+'**'+strcompress(j-1, /REMOVE), $
          SCR_XSIZE=xSize, SCR_YSIZE=ySize, UVALUE={catIdx:i-1,catV:catValues, thisIdx:j-1}, $
          event_pro=self.eventPrefix+'categoryValuesExclusiveSelection', /NO_RELEASE)
      endfor
    endelse
  ;print, uname
  endfor
  
END

; constructor/destructor
FUNCTION FMEntitySelectionGUI::init, info, mgr, fonts=fonts

  if not self -> FMInfoSelectionGUI :: init(info, mgr, fonts=fonts) then return , 0
  return , 1
  
END

PRO FMEntitySelectionGUI::cleanUp

  ptr_free, self.categoryCodes
  ptr_free, self.categoryValues
  self -> FMInfoSelectionGUI::cleanUp
  
END

;****************************************************************************************

PRO FMEntitySelectionGUI__Define

  Struct = { FMEntitySelectionGUI , $
    runList: 0l, $
    runDescrText: 0l, $
    observedModelFlagButton: 0l, $
    allAvailableScenarioFlagButton: 0l, $
    modelList: 0l,$
    scenarioList: 0l,$
    parameterDescriptionText: 0l, $
    parameterList: 0l, $
    parameterTypeList: 0l, $
    queryResultList: 0l, $
    observedDescrText: 0l, $
    singleRadioButton: 0l, $
    groupRadioButton: 0l, $
    addObservedButton: 0l, $
    removeObservedButton: 0l, $
    selectedObservedList:0l, $
    loadObservedButton: 0l,$
    saveObservedButton: 0l, $
    runSelection: 0, $
    categoryCodes: ptr_new(), $
    categoryValues: ptr_new(), $
    singleButtonFlag: 0b, $
    lastGroupName: '', $
    obsGroupStatIndex: 0, $
    Inherits FMInfoSelectionGUI $
    }
    
END

;****************************************************************************************

