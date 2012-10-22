PRO FMBatchEntitySelectionGUI::categoryValuesExclusiveSelection, id, categoryIndex, valueIndex

  self->FMEntitySelectionGUI::categoryValuesExclusiveSelection, id, categoryIndex, valueIndex
  if self.info->getAllObservationsFlag() eq 1 then begin
    self.silentMode=1
    self->setAllObservationsFlag, 1
    self.silentMode=0
;    self.silentMode=1
;    self->removeAllObs
;    self->addAllObs
;    self.silentMode=prevSilentMode
  endif
  
END

PRO FMBatchEntitySelectionGUI::exitRequest

  ;print, 'Destroy Info gui'
  ;self.mgr->setBlockWindowControl, /OFF
  self.info=obj_new()
  obj_destroy, self
  
END

PRO FMBatchEntitySelectionGUI::OKRequest

  if self->checkIntegrity() then begin
    ;self.mgr->setBlockWindowControl, /OFF
    self->updateToCaller, self
  endif else begin
    print, 'Bad ', obj_class(self)
  ;exitMessage=['Something wrong in your selection', 'Check data']
  ;dlgResult=dialog_message(exitMessage, dialog_parent=self->getTopBase(), Title='Warning')
  endelse
  
END

PRO FMBatchEntitySelectionGUI::userAllObservationsButton, flag

  self->setAllObservationsFlag, flag
  
END

PRO FMBatchEntitySelectionGUI::userAllParametersButton, flag

  self->setAllParametersFlag, flag
  
END

PRO FMBatchEntitySelectionGUI::userAllScenariosButton, flag

  self->setAllScenariosFlag, flag
  
END

PRO FMBatchEntitySelectionGUI::userAllModelsButton, flag

  self->setAllModelsFlag, flag
  
END

PRO FMBatchEntitySelectionGUI::setAllScenariosFlag, selection

  self.info->setAllScenariosFlag, selection
  widget_control, self.scenarioList, sensitive=1-self.info->getAllScenariosFlag()
  widget_control, self.allScenariosFlagButton, SET_BUTTON=self.info->getAllScenariosFlag()
  if selection eq 1 then begin
    allScenarioIndexes=indgen(n_elements(self.info->getScenarioCodes()))
    self->userScenarioSelections, allScenarioIndexes
  endif
  
END

PRO FMBatchEntitySelectionGUI::setAllModelsFlag, selection

  self.info->setAllModelsFlag, selection
  widget_control, self.modelList, sensitive=1-self.info->getAllModelsFlag()
  widget_control, self.allModelsFlagButton, SET_BUTTON=self.info->getAllModelsFlag()
  if selection eq 1 then begin
    allModelIndexes=indgen(n_elements(self.info->getModelCodes()))
    self->userModelSelections, allModelIndexes
  endif
  
END

PRO FMBatchEntitySelectionGUI::setAllParametersFlag, selection

  self.info->setAllParametersFlag, selection
  widget_control, self.parameterList, sensitive=1-self.info->getAllParametersFlag()
  widget_control, self.parameterTypeList, sensitive=1-self.info->getAllParametersFlag()
  widget_control, self.allParametersFlagButton, SET_BUTTON=self.info->getAllParametersFlag()
  if selection eq 1 then begin
    self->userParameterTypeSelections, /ALL
    parameterNamesIndexes=indgen(n_elements(self.info->getParameterCodes()))
    self->userParameterSelections, parameterNamesIndexes
  endif
  
END

PRO FMBatchEntitySelectionGUI::userParameterSelections, parametersIndexes

  widget_control, self.parameterList, SET_LIST_SELECT=[-1]
  self.info->setParameterSelections, [-1]
  if parametersIndexes[0] ne -1 then begin
    widget_control, self.parameterList, SET_LIST_SELECT=parametersIndexes
    self.info->setParameterSelections, parametersIndexes
  endif
  self->fillObsQueryList
  allObsFlag=self.info->getAllObservationsFlag()
  self.silentMode=1
  if (allObsFlag) then self->setAllObservationsFlag, allObsFlag
  self.silentMode=0
  
;widget_control, self.descriptionLabel, set_value=self->getCurrentDescription()
;self->fillParameterList
  
END

PRO FMBatchEntitySelectionGUI::setAllObservationsFlag, selection

  self.info->setAllObservationsFlag, 0
  if selection eq 1 then begin
    if ~self.silentMode then answer=self->dialogMessage(['This operation will cancel all group monitoring selections and fix all category selections to "ALL". Do you want to continue?'], title=['All observations flag'], /QUESTION) else answer="Yes"
    if answer eq "Yes"  then begin
      ;self->
      self.info->setAllObservationsFlag, selection
      prevSilentMode=self.silentMode
      self.silentMode=1
      self->groupObsRadioButton
      self->removeAllObs
      self->singleObsRadioButton
      widget_control, self.removeObservedButton, sensitive=1-self.info->getAllObservationsFlag()
      self->removeAllObs
      self->addAllObs
      self.silentMode=prevSilentMode
    endif
  endif
  widget_control, self.selectedObservedList, sensitive=1-self.info->getAllObservationsFlag()
  widget_control, self.queryResultList, sensitive=1-self.info->getAllObservationsFlag()
  widget_control, self.allObservationsFlagButton, SET_BUTTON=self.info->getAllObservationsFlag()
  widget_control, self.addObservedButton, sensitive=1-self.info->getAllObservationsFlag()
  widget_control, self.removeObservedButton, sensitive=1-self.info->getAllObservationsFlag()
  widget_control, self.groupRadioButton, sensitive=1-self.info->getAllObservationsFlag()
  
END

; external
FUNCTION FMBatchEntitySelectionGUI::checkIntegrity

  return, self.info->checkIntegrity(self)
  
END

PRO FMBatchEntitySelectionGUI::updateToCaller, callingGUI

  self.mgr->entityOK, self
  
END

; gui check/conf/fill/update...
PRO FMBatchEntitySelectionGUI::configure

  self -> FMEntitySelectionGUI::configure
  self->setAllScenariosFlag, self.info->getAllScenariosFlag()
  self->setAllModelsFlag, self.info->getAllModelsFlag()
  previousSilentMode=self.silentMode
  self.silentMode=1
  self->setAllObservationsFlag, self.info->getAllObservationsFlag()
  self.silentMode=previousSilentMode
  
END

FUNCTION FMBatchEntitySelectionGUI::getTitle

  return, 'Batch Data selection'
  
END

PRO FMBatchEntitySelectionGUI::buildModelSection, base

  sectionTitle=widget_label(base, value='Model', SCR_YSIZE=self->getLabelYSize(), /ALIGN_LEFT, font=self.titleFont)
  
  doubleListBase=widget_base(base, xpad=0, ypad=0, space=0, /ROW)
  runsListBase=widget_base(base, xpad=0, ypad=0, space=0, /COLUMN)
  descrListBase=widget_base(base, xpad=0, ypad=0, space=0, /ROW)
  extraFlagBase=widget_base(base, xpad=0, ypad=0, space=0, /ROW)
  
  scenarioBase=widget_base(doubleListBase, xpad=0, ypad=0, space=0, /COLUMN)
  modelBase=widget_base(doubleListBase, xpad=0, ypad=0, space=0, /COLUMN)
  
  scenTitle=widget_label(scenarioBase, value='Scenario', scr_ysize=self->getLabelYSize(), /ALIGN_LEFT, font=self.titleFont)
  
  allFlagSelectionBase = widget_base(scenarioBase, $
    XOFFSET=0 ,YOFFSET=0, /NONEXCLUSIVE, $
    SPACE=0 ,XPAD=0 ,YPAD=0, /ROW, /ALIGN_CENTER)
    
  self.allScenariosFlagButton = widget_button(allFlagSelectionBase, $
    XOFFSET=0 ,YOFFSET=0, VALUE='All', event_pro=self.eventPrefix+'flagAllScenariosButton', $
    SCR_YSIZE=self->getLabelYSize(), sensitive=1)
    
  self.scenarioList=widget_list(scenarioBase, value=[''], $
    SCR_XSIZE=self->getModelListBoxXSize(), SCR_YSIZE=self->getModelListBoxYSize(), $
    event_pro=self.eventPrefix+'scenarioListSelection', /MULTIPLE)
    
    
  modelTitle=widget_label(modelBase, value='Model', scr_ysize=self->getLabelYSize(), /ALIGN_LEFT, font=self.titleFont)
  
  allFlagSelectionBase = widget_base(modelBase, $
    XOFFSET=0 ,YOFFSET=0, /NONEXCLUSIVE, $
    SPACE=0 ,XPAD=0 ,YPAD=0, /ROW, /ALIGN_CENTER)
    
  self.allModelsFlagButton = widget_button(allFlagSelectionBase, $
    XOFFSET=0 ,YOFFSET=0, VALUE='All', event_pro=self.eventPrefix+'flagAllModelsButton', $
    SCR_YSIZE=self->getLabelYSize(), sensitive=1)
    
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
    XOFFSET=0 ,YOFFSET=0, VALUE='Use OBS <-> MOD', event_pro=self.eventPrefix+'useObsModButton', $
    SCR_YSIZE=self->getLabelYSize(), sensitive=1)
    
END

PRO FMBatchEntitySelectionGUI::buildParameterSection, base

  ;  allFlagSelectionBase = widget_base(base, $
  ;    XOFFSET=0 ,YOFFSET=0, /NONEXCLUSIVE, $
  ;    SPACE=0 ,XPAD=0 ,YPAD=0, /ROW, /ALIGN_CENTER)
  ;
  ;  self.allParametersFlagButton = widget_button(allFlagSelectionBase, $
  ;    XOFFSET=0 ,YOFFSET=0, VALUE='All', event_pro=self.eventPrefix+'flagAllParametersButton', $
  ;    SCR_YSIZE=self->getLabelYSize(), sensitive=1)

  parameterInfoBase=widget_base(base, xpad=0, ypad=0, space=0, /ROW)
  
  parameterTypeBase=widget_base(parameterInfoBase, xpad=0, ypad=0, space=0, /COLUMN)
  parameterBase=widget_base(parameterInfoBase, xpad=0, ypad=0, space=0, /COLUMN)
  
  parameterTypeTitle=widget_label(parameterTypeBase, value='Type', $
    scr_ysize=self->getLabelYSize(), /ALIGN_LEFT, font=self.titleFont)
    
  self.parameterTypeList=widget_list(parameterTypeBase, value=[''], $
    SCR_XSIZE=self->getParameterSectionXSize(), SCR_YSIZE=self->getObservationListBoxYSize(), $
    event_pro=self.eventPrefix+'parameterTypeListSelection', /MULTIPLE)
    
  parameterTypeTitle=widget_label(parameterBase, value='Parameter', $
    scr_ysize=self->getLabelYSize(), /ALIGN_LEFT, font=self.titleFont)
    
  self.parameterList=widget_list(parameterBase, value=[''], $
    SCR_XSIZE=self->getParameterSectionXSize(), SCR_YSIZE=self->getObservationListBoxYSize(), $
    event_pro=self.eventPrefix+'parameterListSelection', /MULTIPLE)
    
END

PRO FMBatchEntitySelectionGUI::buildObservationSection, base

  ;self->FMEntitySelectionGUI :: buildObservationSection, base

  ;Add all flag only or rebuild whole widget hierarchy
  ;widget_button on non-exclusive base

  sectionTitle=widget_label(base, value='Observation', $
    scr_ysize=self->getLabelYSize(), /ALIGN_LEFT, font=self.titleFont)
    
  categorySelectionsBase=widget_base(base, xpad=0, ypad=0, space=0, /ROW)
  lowerSectionBase=widget_base(base, xpad=0, ypad=0, space=0, /ROW)
  queryAndDescrBase=widget_base(lowerSectionBase, xpad=0, ypad=0, space=0, /COLUMN)
  actionBase=widget_base(lowerSectionBase, xpad=0, ypad=0, space=0, /COLUMN)
  observationsSelectionsListBase=widget_base(lowerSectionBase, xpad=0, ypad=0, space=0, /COLUMN)
  
  self->buildCategoryAndValues, categorySelectionsBase
  
  fillFromQueryBase=widget_base(queryAndDescrBase, xpad=0, ypad=0, space=0, /COLUMN)
  
  allFlagSelectionBase = widget_base(queryAndDescrBase, $
    XOFFSET=0 ,YOFFSET=0, /NONEXCLUSIVE, $
    SPACE=0 ,XPAD=0 ,YPAD=0, /ROW, /ALIGN_CENTER)
    
  self.allObservationsFlagButton = widget_button(allFlagSelectionBase, $
    XOFFSET=0 ,YOFFSET=0, VALUE='All', event_pro=self.eventPrefix+'flagAllObservationsButton', $
    SCR_YSIZE=self->getLabelYSize(), sensitive=1)
    
  queryResultTitle=widget_label(fillFromQueryBase, value='Available', $
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
    SCR_XSIZE=self->getButtonXSize() ,SCR_YSIZE=self->getButtonYSize(), /NO_RELEASE, sensitive=1)
  selectedSingleHint=widget_label(singleGroupBaseSintesys, value='S:  ', UNAME=self.eventPrefix+'SingleHint', $
    SCR_XSIZE=self->getButtonXSize(), SCR_YSIZE=self->getLabelYSize(), /ALIGN_LEFT)
  selectedGroupHint=widget_label(singleGroupBaseSintesys, value='G:  ', UNAME=self.eventPrefix+'GroupHint', $
    SCR_XSIZE=self->getButtonXSize(), SCR_YSIZE=self->getLabelYSize(), /ALIGN_LEFT, sensitive=1)
    
    
  self.addObservedButton= widget_button(actionButtonBase, /ALIGN_CENTER, $
    XOFFSET=0 ,YOFFSET=0, VALUE='Add', event_pro=self.eventPrefix+'addObsButton', $
    SCR_XSIZE=self->getButtonXSize() ,SCR_YSIZE=self->getButtonYSize())
  self.removeObservedButton= widget_button(actionButtonBase, /ALIGN_CENTER, $
    XOFFSET=0 ,YOFFSET=0, VALUE='Remove', event_pro=self.eventPrefix+'removeObsButton', $
    SCR_XSIZE=self->getButtonXSize() ,SCR_YSIZE=self->getButtonYSize())
    
  selectionsBase=widget_base(observationsSelectionsListBase, xpad=0, ypad=0, space=0, /COLUMN)
  
  selectedTitle=widget_label(selectionsBase, value='Selected', $
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

; constructor/destructor
FUNCTION FMBatchEntitySelectionGUI::init, info, mgr, fonts=fonts

  if not self -> FMEntitySelectionGUI :: init(info, mgr, fonts=fonts) then return , 0
  return , 1
  
END

PRO FMBatchEntitySelectionGUI::cleanUp

  self ->FMEntitySelectionGUI::cleanUp
  
END

;****************************************************************************************

PRO FMBatchEntitySelectionGUI__Define

  Struct = { FMBatchEntitySelectionGUI , $
;    allObservationsFlag: 0b, $
;    allModelFlag: 0b, $
;    allParameterFlag: 0b, $
;    allScenarioFlag: 0b, $
    allScenariosFlagButton: 0l, $
    allModelsFlagButton: 0l, $
    allParametersFlagButton: 0l, $
    allObservationsFlagButton: 0l, $
    Inherits FMEntitySelectionGUI $
    }
    
END

;****************************************************************************************

