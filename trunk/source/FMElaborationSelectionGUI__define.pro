;getExtraFlagsLongLabelXSize+ getExtraFlagsLongLabelXSize+getExtraFlagsShortLabelXSize
; external

FUNCTION FMElaborationSelectionGUI::getMainMgr

 return, self->getMgr()

END

FUNCTION FMElaborationSelectionGUI::checkIntegrity

 return, self.info->checkIntegrity(self)

END

PRO FMElaborationSelectionGUI::updateToCaller

  newInfo=self.info->clone(/DEEP)
  self.mgr->updateElaborationDisplayInfo, newInfo
  self.mgr->enable
  
END

; events
PRO FMElaborationSelectionGUI::OKRequest

  self.info->setExclusives, self.radioSelections
  if self->checkIntegrity() then begin
    self.mgr->setBlockWindowControl, /OFF
    self->updateToCaller
    obj_destroy, self
  endif else begin
    print, 'Bad elaboration'
    ;exitMessage=['Something wrong in your selection', 'Check data']
    ;dlgResult=dialog_message(exitMessage, dialog_parent=self->getTopBase(), Title='Warning')
  endelse
  
END

PRO FMElaborationSelectionGUI::userStartHourSelection, index

  nameToSearch=self->getStartPrefix()+'Hour'
  wid=widget_info(self->getTopBase(), FIND_BY_UNAME=nameToSearch)
  widget_control, wid, SET_COMBOBOX_SELECT=index
  self.info->setStartHourSelection, index
  
END

PRO FMElaborationSelectionGUI::userStartDaySelection, index

  nameToSearch=self->getStartPrefix()+'Day'
  wid=widget_info(self->getTopBase(), FIND_BY_UNAME=nameToSearch)
  widget_control, wid, SET_COMBOBOX_SELECT=index
  self.info->setStartDaySelection, index
  
END

PRO FMElaborationSelectionGUI::userStartMonthSelection, index

  nameToSearch=self->getStartPrefix()+'Month'
  wid=widget_info(self->getTopBase(), FIND_BY_UNAME=nameToSearch)
  widget_control, wid, SET_COMBOBOX_SELECT=index
  self.info->setStartMonthSelection, index
  
END

PRO FMElaborationSelectionGUI::userEndHourSelection, index

  nameToSearch=self->getEndPrefix()+'Hour'
  wid=widget_info(self->getTopBase(), FIND_BY_UNAME=nameToSearch)
  widget_control, wid, SET_COMBOBOX_SELECT=index
  self.info->setEndHourSelection, index
  
END

PRO FMElaborationSelectionGUI::userEndDaySelection, index

  nameToSearch=self->getEndPrefix()+'Day'
  wid=widget_info(self->getTopBase(), FIND_BY_UNAME=nameToSearch)
  widget_control, wid, SET_COMBOBOX_SELECT=index
  self.info->setEndDaySelection, index
  
END

PRO FMElaborationSelectionGUI::userEndMonthSelection, index

  nameToSearch=self->getEndPrefix()+'Month'
  wid=widget_info(self->getTopBase(), FIND_BY_UNAME=nameToSearch)
  widget_control, wid, SET_COMBOBOX_SELECT=index
  self.info->setEndMonthSelection, index
  
END

PRO FMElaborationSelectionGUI::userGroupByTimeSelection, buttonIndex, NONE=NONE

  if not(keyword_set(NONE)) then begin
  nameToSearch=self->buildGroupUName(undef, buttonIndex, /TIME)
  wid=widget_info(self->getTopBase(), FIND_BY_UNAME=nameToSearch)
  widget_control, wid, /SET_BUTTON
  self.info->setGroupByTimeSelection, buttonIndex
  endif; else begin
  ;self.info->setGroupByTimeSelection, buttonIndex, NONE=NONE
  ;endelse 
  if n_elements(buttonIndex) ne 0 then self.radioSelections[0]=buttonIndex
  
END

PRO FMElaborationSelectionGUI::userGroupByStatSelection, buttonIndex, NONE=NONE

  if not(keyword_set(NONE)) then begin
  nameToSearch=self->buildGroupUName(undef, buttonIndex, /STAT)
  wid=widget_info(self->getTopBase(), FIND_BY_UNAME=nameToSearch)
  widget_control, wid, /SET_BUTTON
  self.info->setGroupByStatSelection, buttonIndex
  endif; else begin
  ;self.info->setGroupByStatSelection, buttonIndex, NONE=NONE
  ;endelse
  if n_elements(buttonIndex) ne 0 then self.radioSelections[1]=buttonIndex
  
END

PRO FMElaborationSelectionGUI::userSeasonSelection, buttonIndex, NONE=NONE

  if not(keyword_set(NONE)) then begin
  nameToSearch=self->buildPeriodUName(undef, buttonIndex, /SEASON)
  wid=widget_info(self->getTopBase(), FIND_BY_UNAME=nameToSearch)
  widget_control, wid, /SET_BUTTON
  self.info->setSeasonSelection, buttonIndex
  endif; else begin
  ;self.info->setSeasonSelection, buttonIndex, NONE=NONE
  ;endelse 
 if n_elements(buttonIndex) ne 0 then self.radioSelections[2]=buttonIndex
   
END

PRO FMElaborationSelectionGUI::userDayPeriodSelection, buttonIndex, NONE=NONE

  if not(keyword_set(NONE)) then begin
  nameToSearch=self->buildPeriodUName(undef, buttonIndex, /DAY)
  wid=widget_info(self->getTopBase(), FIND_BY_UNAME=nameToSearch)
  widget_control, wid, /SET_BUTTON
  self.info->setDayPeriodSelection, buttonIndex
  endif; else begin
  ;self.info->setDayPeriodSelection, buttonIndex, NONE=NONE
  ;endelse 
 if n_elements(buttonIndex) ne 0 then self.radioSelections[3]=buttonIndex
   
END

PRO FMElaborationSelectionGUI::userDiagramSelection, diagramIndex

  widget_control, self.elaborationDescriptionText, set_value=self->getCurrentDescription()
  widget_control, self.diagramList, SET_LIST_SELECT=diagramIndex
  self.info->setDiagramSelection, diagramIndex
  self->fillElaborationList
  
END

;PRO FMElaborationSelectionGUI::userAxisSelection, axisIndex
;
;  widget_control, self.axisList, SET_LIST_SELECT=axisIndex
;  self.info->setAxisSelection, axisIndex
;;ToDo: description area is a mix info text... (diagram, elab, axis, parameter... thresholds...)
;;widget_control, self.plotDescriptionText, set_value=self->getCurrentDescription()
;  
;END
FUNCTION FMElaborationSelectionGUI::getCurrentElabMultipleChoiceFlags

 return, sel.info->getCurrentElabMultipleChoiceFlags()

END

FUNCTION FMElaborationSelectionGUI::getCurrentElabMultipleChoiceFlagsNumber

 return, sel.info->getCurrentElabMultipleChoiceFlagsNumber()

END

PRO FMElaborationSelectionGUI::fillMultipleInfoText

  ;fixed sequence: npar, nmod, nsce, nobs
  diagramMultipleInfo=self.info->getCurrentDiagramMaxMultipleChoiceNumber()
  elabMultipleInfo=self.info->getCurrentElabMultipleChoiceFlags()
  elabMultipleInfoNum=self.info->getCurrentElabMultipleChoiceFlagsNumber()
  elabMultipleInfo=*elabMultipleInfo
  descText=strarr(elabMultipleInfoNum+4)
  for i=0, elabMultipleInfoNum-1 do descText[i+3]=strcompress(i+1, /REMOVE)+')'+self.info->buildTextMultiple(elabMultipleInfo[i, *])
  descText[0]='Max multiple choice for selected diagram is: '+strcompress(diagramMultipleInfo, /REMOVE)
  descText[1]=''
  descText[2]='For selected statistic you may select ('+strcompress(elabMultipleInfoNum, /REMOVE)+'): '
  widget_control, self.multipleInfoText, set_value=descText

END

PRO FMElaborationSelectionGUI::userElabSelection, elabIndex

  widget_control, self.elaborationList, SET_LIST_SELECT=elabIndex
  self.info->setElabSelection, elabIndex
  widget_control, self.elaborationDescriptionText, set_value=self->getCurrentDescription()
  ;self->fillParameterList
;  self->fillAxisList
  self->fillMultipleInfoText
  self->configureExclusiveListStatus
  self->configureReferenceValuesStatus
  self->configureGCOCStatus
  ; New!! to be tested
  self->configureExclusiveSelection
  ; End
  
END

;PRO FMElaborationSelectionGUI::userParameterSelections, parametersIndexes
;
;  if parametersIndexes[0] ne -1 then begin
;    widget_control, self.parameterList, SET_LIST_SELECT=parametersIndexes
;    self.info->setParametersSelection, parametersIndexes
;    return
;  endif
;  widget_control, self.parameterList, SET_LIST_SELECT=[-1]
;  
;;widget_control, self.descriptionLabel, set_value=self->getCurrentDescription()
;;self->fillParameterList
;  
;END

PRO FMElaborationSelectionGUI::useThresholdsButton

END

FUNCTION FMElaborationSelectionGUI::getGoalsCriteriaOCFlag

 return, self.info->getGoalsCriteriaOCFlag()

END
; gui check/conf...
PRO FMElaborationSelectionGUI::configureExclusiveSelection

  self.radioSelections[0]=self.info->getGroupByTimeSelection()
  self.radioSelections[1]=self.info->getGroupByStatSelection()
  self.radioSelections[2]=self.info->getSeasonSelection()
  self.radioSelections[3]=self.info->getDayPeriodSelection()

  groupExclInfo=self.info->getExclusiveInfo()

  if groupExclInfo.gbTime eq 'FREE' then begin
    self->userGroupByTimeSelection, self.radioSelections[0]>0
  ;    self.radioSelections[0]=usergbts>0
  endif

  if groupExclInfo.gbStat eq 'FREE' then begin
    self->userGroupByStatSelection, self.radioSelections[1]>0
  ;    self.radioSelections[1]=usergbss>0
  endif

  if groupExclInfo.season eq 'FREE' then begin 
  self->userSeasonSelection, self.radioSelections[2]>0
  ;      self.radioSelections[3]=userss>0
  endif

  if groupExclInfo.dayPeriod eq 'FREE'then begin
  self->userDayPeriodSelection, self.radioSelections[3]>0
  ;      self.radioSelections[2]=userdps>0
  endif
  

END

PRO FMElaborationSelectionGUI::Configure

  ; Realize and MoveToCenter is done by superclass
  ; Here only fill & set internal widget
  diagramIndex=self.info->getDiagramSelection()
  elabIndex=self.info->getElabSelection()
;  axisIndex=self.info->getAxisSelection()
  ;parametersIndexes=self.info->getParametersSelection()
  self->userStartHourSelection, self.info->getStartHourSelection()
  self->userStartDaySelection, self.info->getStartDaySelection()
  self->userStartMonthSelection, self.info->getStartMonthSelection()
  self->userEndHourSelection, self.info->getEndHourSelection()
  self->userEndDaySelection, self.info->getEndDaySelection()
  self->userEndMonthSelection, self.info->getEndMonthSelection()
  self->fillDiagramList
  self->userDiagramSelection, diagramIndex
  self->userElabSelection, elabIndex
;  self->userAxisSelection, axisIndex
  ;self->userParameterSelections, parametersIndexes
  self->cancelThresholdsData
  ;ToDo: Add parameter pre-selection!
  if self.info->getThresholdFlag() then self->fillReferencesValuesText, self.info->getThresholdValues()
  self->configureExclusiveSelection
  ;groupExclInfo=self.info->getExclusiveInfo()

  ;if groupExclInfo.gbTime eq 'FREE' then self->userGroupByTimeSelection, usergbts>0
  ;if groupExclInfo.gbStat eq 'FREE' then self->userGroupByStatSelection, usergbss>0
  ;if groupExclInfo.dayPeriod eq 'FREE'then self->userDayPeriodSelection, userdps>0
  ;if groupExclInfo.season eq 'FREE' then self->userSeasonSelection, userss>0
  
END

FUNCTION FMElaborationSelectionGUI::buildDemoReferenceText, numbers

  demoString=''
  for i=1, numbers do demoString=[demoString, 'Val', strcompress(i, /REMOVE), '#']
  
  return,  demoString
  
END

FUNCTION FMElaborationSelectionGUI::getReferenceValueContents

  widget_control, self.thresholdText, get_value=test
  return, test
  
END

PRO FMElaborationSelectionGUI::configureGCOCStatus

  gcocFlag=self.info->getCurrentGoalsCriteriaFlag()

  if gcocFlag eq 1 then begin
    widget_control, self.criteriaButton, sensitive=0, /set_button
  endif else begin
    widget_control, self.criteriaButton, sensitive=0, set_button=0
  endelse
  self.info->setGoalsCriteriaOCFlag, gcocFlag
  
END

PRO FMElaborationSelectionGUI::configureReferenceValuesStatus

  numberRefValues=self.info->getNumberReferenceValues()

  if numberRefValues ge 1 then begin
    widget_control, self.thresholdButton, sensitive=0, /set_button
    widget_control, self.thresholdText, /EDITABLE, sensitive=1, set_value=self->buildDemoReferenceText(numberRefValues)
    widget_control, self.measureUnitLabel, set_value=strcompress(numberRefValues, /REMOVE)
  endif else begin
    widget_control, self.thresholdButton, sensitive=0, set_button=0
    widget_control, self.thresholdText, EDITABLE=0, sensitive=0, set_value='';self->buildDemoReferenceText(numberRefValues)
    widget_control, self.measureUnitLabel, set_value=' N/A'
  endelse
  
END

PRO FMElaborationSelectionGUI::configureExclusiveListStatus

  groupExclInfo=self.info->getExclusiveInfo()
  conv=byte(groupExclInfo.gbStat)
  if conv[0] ge 48 and conv[0] le 57 then self->userGroupByStatSelection, fix(groupExclInfo.gbStat) else self->userGroupByStatSelection, /NONE
  conv=byte(groupExclInfo.gbTime)
  if conv[0] ge 48 and conv[0] le 57 then self->userGroupByTimeSelection, fix(groupExclInfo.gbTime) else self->userGroupByTimeSelection, /NONE
  conv=byte(groupExclInfo.season)
  if conv[0] ge 48 and conv[0] le 57 then self->userSeasonSelection, fix(groupExclInfo.season) else self->userSeasonSelection, /NONE
  conv=byte(groupExclInfo.dayPeriod)
  if conv[0] ge 48 and conv[0] le 57 then self->userDayPeriodSelection, fix(groupExclInfo.dayPeriod) else self->userDayPeriodSelection, /NONE
  
  ;print, '++++'
  ;print, groupExclInfo.gbStat
  ;print, groupExclInfo.gbTime
  ;print, groupExclInfo.season
  ;print, groupExclInfo.dayPeriod
  ;print, '++++'
  ;print, '++++'
  for i=0, 1 do begin
    groupNames=self.info->getGroupNamesByIndex(i+1)
    elems=n_elements(groupNames)+1
    if i eq 0 then conf=groupExclInfo.gbTime else conf=groupExclInfo.gbStat
    for j=0, elems-1 do begin
      self->configureGroupBySingleButton, i, j, conf, LAST=(elems-1) eq j
    ;if i eq 1 then self->configureGroupByStatSingleButton, j, groupExclInfo.gbStat
    endfor
  endfor
  
  sNames=self.info->getSeasonNames()
  sCodes=self.info->getSeasonCodes()
  pNames=self.info->getDayPeriodNames()
  pCodes=self.info->getDayPeriodCodes()
  names=[ptr_new(sNames, /NO_COPY), ptr_new(pNames, /NO_COPY)]
  codes=[ptr_new(sCodes, /NO_COPY), ptr_new(pCodes, /NO_COPY)]
  
  for i=0, 1 do begin
    groupNames=*names[i]
    elems=n_elements(groupNames)+1
    if i eq 0 then conf=groupExclInfo.season else conf=groupExclInfo.dayPeriod
    for j=0, elems-1 do begin
      self->configurePeriodSingleButton, i, j, conf, LAST=(elems-1) eq j
    ;if i eq 1 then self->configureGroupByStatSingleButton, j, groupExclInfo.gbStat
    endfor
  endfor
  
;widget_control, , sensitive=, set_button=
;self->buildGroupUName(i-1, j)
  
END

PRO FMElaborationSelectionGUI::configurePeriodSingleButton, typeIndex, bIndex, elabSetting, LAST=LAST

  wid=widget_info(self->getTopBase(), FIND_BY_UNAME=self->buildPeriodUName(typeIndex, bIndex))
  ;print, elabSetting, keyword_set(LAST)
  case elabSetting of
    'FREE':if not(keyword_set(LAST)) then widget_control, wid, /sensitive
    'NONE':begin
    if keyword_set(LAST) then set_button=1 else set_button=0
    widget_control, wid, sensitive=0, set_button=set_button
  end
  else:begin
  widget_control, wid, sensitive=0
  if bIndex eq fix(elabSetting) then widget_control, wid, /SET_BUTTON else widget_control, wid, SET_BUTTON=0
end
endcase

END

PRO FMElaborationSelectionGUI::configureGroupBySingleButton, typeIndex, bIndex, elabSetting, LAST=LAST

  wid=widget_info(self->getTopBase(), FIND_BY_UNAME=self->buildGroupUName(typeIndex, bIndex))
  ;print, elabSetting, keyword_set(LAST)
  case elabSetting of
    'FREE':if not(keyword_set(LAST)) then widget_control, wid, /sensitive
    'NONE':begin
    if keyword_set(LAST) then set_button=1 else set_button=0
    widget_control, wid, sensitive=0, set_button=set_button
  end
  else:begin
  widget_control, wid, sensitive=0
  if bIndex eq fix(elabSetting) then widget_control, wid, /SET_BUTTON else widget_control, wid, SET_BUTTON=0
end
endcase

END

PRO FMElaborationSelectionGUI::fillReferencesValuesText, values

  refVal=''
  for i=0, n_elements(values)-1 do refVal=[refVal, strcompress(values[i], /REMOVE), '#']
  
  ;print, refval
  widget_control, self.thresholdText, set_value=refVal
  widget_control, self.thresholdButton, set_button=1
  
END

PRO FMElaborationSelectionGUI::fillDiagramList

  widget_control, self.diagramList, SET_VALUE=self->getAllDiagramNames()
  
END

;PRO FMElaborationSelectionGUI::fillParameterList
;
;  parameterNames=self.info->getParametersBySelectedElab()
;  widget_control, self.parameterList, set_value=parameterNames
;  self->userParameterSelections, [0]
;;widget_control, self.parameterList, SET_VALUE=self->getAllParametersNames()
;;widget_control, self.parameterList, SET_LIST_SELECT=names
;;parameterNames=self.info->getParameterNamesBySelectedElab()
;;names=self.info->getParameterNames()
;  
;END

PRO FMElaborationSelectionGUI::fillElaborationList

  elabNames=self.info->getElabNamesBySelectedDiagram()
  widget_control, self.elaborationList, set_value=elabNames
  
  self->userElabSelection, 0
  
END

PRO FMElaborationSelectionGUI::cancelThresholdsData

  widget_control, self.thresholdText, set_value=''
  widget_control, self.thresholdButton, set_button=0
  
END

;PRO FMElaborationSelectionGUI::fillAxisList
;
;  axisNames=self.info->getAxisNamesBySelectedDiagram()
;  widget_control, self.axisList, set_value=axisNames
;  
;  self->userAxisSelection, 0
;  
;END

; gui building
PRO FMElaborationSelectionGUI::build

  title=self->getTitle()
  
  base = widget_base(/COLUMN, $
    uvalue=self, TLB_FRAME_ATTR=1, $
    title=title, /TLB_KILL_REQUEST_EVENTS, event_pro=self.eventprefix+'destroyWindow')
    
  mainBase = widget_base(base, /COLUMN)
  
  subBase11 = widget_base(mainbase,xpad=self.xPad, ypad=self.yPad,space=0,/ROW)
  subBase12 = widget_base(mainbase,xpad=self.xPad, ypad=self.yPad,space=0,/ROW, /ALIGN_CENTER)
  subBase111 = widget_base(subBase11,xpad=1, ypad=1,space=1,/COLUMN, FRAME=1)
  subBase112 = widget_base(subBase11,xpad=1, ypad=1,space=1,/COLUMN, FRAME=1)
  
  ;self->buildElaborationAndParameterSection, subBase111
  self->buildElaborationSection, subBase111
  self->buildGroupAndDateSection, subBase112
  self->buildOKButton, subBase12
  
  self->SetTopBase, base
  xmanager, 'fairmode', base, /JUST_REG, /CATCH
  
END

FUNCTION FMElaborationSelectionGUI::getElaborationSectionXSize

  return, self->getVisibleXSize()-self->getGroupSectionXSize()
  
END

FUNCTION FMElaborationSelectionGUI::getElaborationSectionYSize

  ; Available Y space subtract titles spaces (3) and exclusive button space (1)
  return, self->getVisibleYSize()-(4*self->getLabelYSize())
  
END

;FUNCTION FMElaborationSelectionGUI::getParameterAndGroupSectionXSize
;
;  return, self->getVisibleXSize()*.40
;  
;END

FUNCTION FMElaborationSelectionGUI::getGroupSectionXSize

  return, self->getVisibleXSize()*.40
  
END

FUNCTION FMElaborationSelectionGUI::getLabelYSize

  return, 20
  
END

;FUNCTION FMElaborationSelectionGUI::getParameterAndGroupSectionYSize
;
;  return, self->getVisibleYSize()*.70
;  
;END

FUNCTION FMElaborationSelectionGUI::getGroupSectionYSize

  return, self->getVisibleYSize()*.70
  
END

FUNCTION FMElaborationSelectionGUI::getElaborationListBoxXSize

  return, self->getElaborationSectionXSize()*.50
  
END

;FUNCTION FMElaborationSelectionGUI::getParameterListBoxXSize
;
;  return, self->getElaborationSectionXSize()*.50
;  
;END

FUNCTION FMElaborationSelectionGUI::getElaborationDescrXSize

  return, self->getElaborationSectionXSize()
  
END

FUNCTION FMElaborationSelectionGUI::getMultipleInfoTextXSize

  return, self->getElaborationSectionXSize()
  
END

FUNCTION FMElaborationSelectionGUI::getElaborationListBoxYSize

  return, self->getElaborationSectionYSize()*.25
  
END

;FUNCTION FMElaborationSelectionGUI::getParameterAndGroupListBoxXSize
;
;  return, self->getParameterAndGroupSectionXSize()*.40
;  
;END

FUNCTION FMElaborationSelectionGUI::getGroupListBoxXSize

  return, self->getGroupSectionXSize()*.40
  
END

;FUNCTION FMElaborationSelectionGUI::getParameterAndGroupListBoxYSize
;
;  return, self->getParameterAndGroupSectionYSize()*.40
;  
;END

FUNCTION FMElaborationSelectionGUI::getGroupListBoxYSize

  return, self->getGroupSectionYSize()*.40
  
END

;FUNCTION FMElaborationSelectionGUI::getParameterAndGroupDescrXSize
;
;  return, self->getParameterAndGroupSectionXSize()-self->getParameterAndGroupListBoxXSize()
;  
;END

;FUNCTION FMElaborationSelectionGUI::getParameterAndGroupDescrYSize
;
;  return, self->getParameterAndGroupSectionYSize()*.25
;  
;END

FUNCTION FMElaborationSelectionGUI::getElaborationDescrYSize

  return, self->getElaborationSectionYSize()*.15
  
END

FUNCTION FMElaborationSelectionGUI::getButtonXSize

  return, self->getVisibleXSize()*.10
  
END

FUNCTION FMElaborationSelectionGUI::getButtonYSize

  return, self->getVisibleYSize()*.08
  
END

FUNCTION FMElaborationSelectionGUI::getGroupByXSize, groupNo

  return, (self->getGroupSectionXSize())/groupNo
  
END

FUNCTION FMElaborationSelectionGUI::getGroupByYSize, valNo

  return, self->getGroupSectionYSize()*.35/valNo
  
END

FUNCTION FMElaborationSelectionGUI::getExtraFlagsLongLabelXSize

  return, self->getElaborationSectionXSize()*.40
  
END

FUNCTION FMElaborationSelectionGUI::getExtraFlagsShortLabelXSize

  return, self->getElaborationSectionXSize()*.20
  
END

;FUNCTION FMElaborationSelectionGUI::getParameterBoxYSize
;
;  return, self->getElaborationSectionYSize()*.30
;  
;END

FUNCTION FMElaborationSelectionGUI::getMultipleInfoTextYSize

  return, self->getElaborationSectionYSize()*.30
  
END

;PRO FMElaborationSelectionGUI::buildElaborationAndParameterSection, base
PRO FMElaborationSelectionGUI::buildElaborationSection, base

  sectionTitle=widget_label(base, value='Elaboration', SCR_YSIZE=self->getLabelYSize(), /ALIGN_LEFT, font=self.titleFont)
  
  tripleListBase=widget_base(base, xpad=0, ypad=0, space=0, /ROW)
  descrListBase=widget_base(base, xpad=0, ypad=0, space=0, /ROW)
  extraFlagBase=widget_base(base, xpad=0, ypad=0, space=0, /COLUMN)
  
;  parameterTitle=widget_label(base, value='Variable', $
;    scr_ysize=self->getLabelYSize(), /ALIGN_LEFT)
;    
;  parameterBase=widget_base(base, xpad=0, ypad=0, space=0, /ROW)
  
  multipleInfoTitle=widget_label(base, value='Multiple choice info', $
    scr_ysize=self->getLabelYSize(), font=self.titleFont, /ALIGN_LEFT)
    
  multipleInfoBase=widget_base(base, xpad=0, ypad=0, space=0, /ROW)

  diagramBase=widget_base(tripleListBase, xpad=0, ypad=0, space=0, /COLUMN)
  elaborationBase=widget_base(tripleListBase, xpad=0, ypad=0, space=0, /COLUMN)
;  axisBase=widget_base(tripleListBase, xpad=0, ypad=0, space=0, /COLUMN)
  
  diagramTitle=widget_label(diagramBase, value='Diagram', scr_ysize=self->getLabelYSize(), font=self.titleFont, /ALIGN_LEFT)
  self.diagramList=widget_list(diagramBase, value=self->getAllDiagramNames(), $
    SCR_XSIZE=self->getElaborationListBoxXSize(), SCR_YSIZE=self->getElaborationListBoxYSize(), $
    event_pro=self.eventPrefix+'elabDiagramListSelection')
    
  elaborationTitle=widget_label(elaborationBase, value='Statistics', scr_ysize=self->getLabelYSize(), font=self.titleFont, /ALIGN_LEFT)
  self.elaborationList=widget_list(elaborationBase, value=self->getAllNames(), $ ;select by type!!!
    SCR_XSIZE=self->getElaborationListBoxXSize(), SCR_YSIZE=self->getElaborationListBoxYSize(), $
    event_pro=self.eventPrefix+'elabNameListSelection')
    
;  axisTitle=widget_label(axisBase, value='Axis', scr_ysize=self->getLabelYSize(), /ALIGN_LEFT)
;  self.axisList=widget_list(axisBase, value=self->getAllAxisNames(), $ ;select by type!!!
;    SCR_XSIZE=self->getElaborationListBoxXSize(), SCR_YSIZE=self->getElaborationListBoxYSize(), $
;    event_pro=self.eventPrefix+'elabAxisListSelection')
  ; getExtraFlagsLongLabelXSize+ getExtraFlagsLongLabelXSize+getExtraFlagsShortLabelXSize
  
  thresholdsBase=widget_base(extraFlagBase, xpad=0, ypad=0, space=0, /ROW)
  criteriaBase=widget_base(extraFlagBase, xpad=0, ypad=0, space=0, /ROW)
  
  thrsBase = widget_base(thresholdsBase, $
    XOFFSET=0 ,YOFFSET=0, /NONEXCLUSIVE, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /COLUMN)
  self.thresholdButton = widget_button(thrsBase, $
    XOFFSET=0 ,YOFFSET=0, VALUE='Need thresholds (v1#v2...)', event_pro=self.eventPrefix+'useThresholdsButton', $
    SCR_YSIZE=self->getLabelYSize(), SCR_XSIZE=self->getExtraFlagsLongLabelXSize())
  self.thresholdText = widget_text(thresholdsBase, UNAME='THRESHOLDSTXT', XOFFSET=0, $
    SCR_XSIZE=self->getExtraFlagsLongLabelXSize(), SCR_YSIZE=self->getLabelYSize() ,SENSITIVE=1 ,/ALL_EV, $
    font=self.textFont, event_pro=self.eventPrefix+'useThresholdsButton')
  self.measureUnitLabel=widget_label(thresholdsBase, value='mu', UNAME='MULABEL', font=self.titleFont, $
    SCR_XSIZE=self->getExtraFlagsShortLabelXSize(), scr_ysize=self->getLabelYSize(), /ALIGN_LEFT)
    
  critBase = widget_base(criteriaBase, $
    XOFFSET=0 ,YOFFSET=0, /NONEXCLUSIVE, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /COLUMN)
  self.criteriaButton = widget_button(critBase, $
    XOFFSET=0 ,YOFFSET=0, VALUE='Display criteria...', event_pro=self.eventPrefix+'useCriteriaButton', $
    SCR_YSIZE=self->getLabelYSize(), SCR_XSIZE=self->getExtraFlagsLongLabelXSize(), sensitive=0)
  ; criteriaText = widget_Text(thresholdsBase, UNAME='CRITERIATXT', XOFFSET=0, $
  ;  SCR_XSIZE=self->getExtraFlagsLongLabelXSize(), SCR_YSIZE=self->getLabelYSize() ,SENSITIVE=1 ,/ALL_EV, $
  ;  SCR_XSIZE=self->getExtraFlagsLongLabelXSize(), font=self.textFont)
  ; criteriaLabel=widget_label(thresholdsBase, value='mu', UNAME='MULABEL', $
  ;  SCR_XSIZE=self->getExtraFlagsShortLabelXSize(), scr_ysize=self->getLabelYSize(), /ALIGN_LEFT)
    
  self.elaborationDescriptionText = widget_text(descrListBase, UNAME='ELABDESCRTXT', XOFFSET=0, $
    SCR_XSIZE=self->getMultipleInfoTextXSize() ,SCR_YSIZE=self->getElaborationDescrYSize() ,SENSITIVE=1, $
    font=self.textFont, /WRAP, /SCROLL)
    
;  self.parameterList=widget_list(parameterBase, value=[''], $
;    SCR_XSIZE=self->getParameterListBoxXSize(), SCR_YSIZE=self->getParameterBoxYSize(), $
;    event_pro=self.eventPrefix+'parameterListSelection', /MULTIPLE)
;    
;  self.parameterDescriptionText= widget_text(parameterBase, UNAME='parameterDESCTXT', XOFFSET=0, SENSITIVE=1 ,/ALL_EV, $
;    SCR_XSIZE=self->getParameterListBoxXSize(), SCR_YSIZE=self->getParameterBoxYSize(), $
;    font=self.textFont, /ALIGN_LEFT)
  self.multipleInfoText= widget_text(multipleInfoBase, UNAME='MULTIPLEDESCTXT', XOFFSET=0, $
    SCR_XSIZE=self->getMultipleInfoTextXSize(), SCR_YSIZE=self->getMultipleInfoTextYSize(), $
    font=self.textFont, /ALIGN_LEFT, /WRAP, /SCROLL, /SENSITIVE, EDIT=0)

    
END

PRO FMElaborationSelectionGUI::buildGroupAndDateSection, base

  subSection2Title=widget_label(base, value='Data', $
    scr_ysize=self->getLabelYSize(), /ALIGN_LEFT, font=self.titleFont)
    
  groupBase=widget_base(base, xpad=0, ypad=0, space=0, /ROW)
  dateBase=widget_base(base, xpad=0, ypad=0, space=0, /ROW)
  periodBase=widget_base(base, xpad=0, ypad=0, space=0, /ROW)
  
  self->buildGroupSection, groupBase
  self->buildDateSection, dateBase
  self->buildPeriodSection, periodBase;, self.info->
  
END

PRO FMElaborationSelectionGUI::buildDateSection, base

  senslist=[1b,1,1,1]
  presencelist=[1b,1,1,0]
  elements=total(presencelist)
  ;print, 'xSize; ', totSize
  id=''
  comboXSize=((self->getGroupSectionXSize())/(elements*2))>33
  labelYSize=self->getLabelYSize()
  
  compoundBase=widget_base(base, ypad=0, space=0, /ROW)
  
  text=widget_label(compoundBase, value='Date', scr_xsize=40, /ALIGN_LEFT)
  
  hours=strtrim(indgen(24), 1)
  days=strtrim(indgen(31)+1, 1)
  months=strtrim(indgen(12)+1, 1)
  years=strtrim(indgen(20)+2000, 1)
  startPrefix=self->getStartPrefix()
  endPrefix=self->getEndPrefix()
  postfix='Selection'
  
  selectDateBase=widget_base(compoundBase, /ROW)
  
  if presenceList[0] then begin
    id='Hour'
    ;print, id+startPrefix
    elementDateBase=widget_base(selectDateBase, /COLUMN, sensitive=sensList[0])
    hourL=widget_label(elementDateBase, value='Hour', SCR_XSIZE=comboXSize, SCR_YSIZE=labelYSize)
    startHourCB=widget_combobox(elementDateBase, value=hours, UNAME=startPrefix+id, UVALUE=startPrefix+id, $
      SCR_XSIZE=comboXSize, SCR_YSIZE=self->getLabelYSize(), $
      event_pro=self.eventPrefix+startPrefix+id+postfix)
    endHourCB=widget_combobox(elementDateBase, value=hours, UNAME=endPrefix+id, UVALUE=endPrefix+id, $
      SCR_XSIZE=comboXSize, SCR_YSIZE=self->getLabelYSize(), $
      event_pro=self.eventPrefix+endPrefix+id+postfix)
  endif
  
  if presenceList[1] then begin
    id='Day'
    elementDateBase=widget_base(selectDateBase, /COLUMN, sensitive=sensList[1])
    dayL=widget_label(elementDateBase, value='Day', SCR_XSIZE=comboXSize, SCR_YSIZE=labelYSize)
    dayCB=widget_combobox(elementDateBase, value=days, UVALUE='STARTDAY'+id, UNAME=startPrefix+id, $
      SCR_XSIZE=comboXSize, SCR_YSIZE=labelYSize, $
      event_pro=self.eventPrefix+startPrefix+id+postfix)
    dayCB=widget_combobox(elementDateBase, value=days, UVALUE='ENDDAY'+id, UNAME=endPrefix+id, $
      SCR_XSIZE=comboXSize, SCR_YSIZE=labelYSize, $
      event_pro=self.eventPrefix+endPrefix+id+postfix)
  endif
  
  if presenceList[2] then begin
    id='Month'
    elementDateBase=widget_base(selectDateBase, /COLUMN, sensitive=sensList[2])
    monthL=widget_label(elementDateBase, value='Month', SCR_XSIZE=comboXSize, SCR_YSIZE=labelYSize)
    monthCB=widget_combobox(elementDateBase, value=months, UVALUE='STARTMONTH'+id, UNAME=startPrefix+id, $
      SCR_XSIZE=comboXSize, SCR_YSIZE=labelYSize, $
      event_pro=self.eventPrefix+startPrefix+id+postfix)
    monthCB=widget_combobox(elementDateBase, value=months, UVALUE='ENDMONTH'+id, UNAME=endPrefix+id, $
      SCR_XSIZE=comboXSize, SCR_YSIZE=labelYSize, $
      event_pro=self.eventPrefix+endPrefix+id+postfix)
  endif
  
  if presenceList[3] then begin
    id='Year'
    elementDateBase=widget_base(selectDateBase, /COLUMN, sensitive=sensList[3])
    yearL=widget_label(elementDateBase, value='Year', SCR_XSIZE=comboXSize, SCR_YSIZE=labelYSize)
    yearCB=widget_combobox(elementDateBase, value=years, UVALUE='STARTYEAR'+id, UNAME=startPrefix+id, $
      SCR_XSIZE=comboXSize+15, SCR_YSIZE=labelYSize, $
      event_pro=self.eventPrefix+startPrefix+id+postfix)
    yearCB=widget_combobox(elementDateBase, value=years, UVALUE='ENDYEAR'+id, UNAME=endPrefix+id, $
      SCR_XSIZE=comboXSize+15, SCR_YSIZE=labelYSize, $
      event_pro=self.eventPrefix+endPrefix+id+postfix)
  endif
  
END

FUNCTION FMElaborationSelectionGUI::getStartPrefix

  return, 'start'
  
END

FUNCTION FMElaborationSelectionGUI::getEndPrefix

  return, 'end'
  
END

PRO FMElaborationSelectionGUI::buildPeriodSection, base

  periodTitles=self.info->getPeriodTitles()
  periodNo=n_elements(periodTitles)
  sNames=[self.info->getSeasonNames(), 'N/A']
  sCodes=[self.info->getSeasonCodes(), '-1']
  pNames=[self.info->getDayPeriodNames(), 'N/A']
  pCodes=[self.info->getDayPeriodCodes(), '-1']
  names=[ptr_new(sNames, /NO_COPY), ptr_new(pNames, /NO_COPY)]
  codes=[ptr_new(sCodes, /NO_COPY), ptr_new(pCodes, /NO_COPY)]
  xSize=self->getGroupByXSize(periodNo)
  eventRoutines=['periodSeasonSelection', 'periodDaySelection']
  ;check
  for i=1, periodNo do begin
    periodNames=*names[i-1]
    periodCodes=*codes[i-1]
    elems=n_elements(periodNames)
    aGroupBase = widget_base(base, UNAME='WID_BASE_4', $
      XOFFSET=0 ,YOFFSET=0, FRAME=1, $
      TITLE='IDL' ,SPACE=1 ,XPAD=0 ,YPAD=0, /COLUMN)
    aGroupTitle=widget_label(aGroupBase, value=periodTitles[i-1], font=self.titleFont, $
      /ALIGN_LEFT)
    ;build exclusive buttons
    ySize=self->getGroupByYSize(elems)
    groupValuesExclusiveBase = Widget_Base(aGroupBase, UNAME='WID_BASE_4', $
      XOFFSET=0 ,YOFFSET=0, /EXCLUSIVE, $
      TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /COLUMN)
    ;  for j=0, elems-1 do aCatValuesButton= widget_button(groupValuesExclusiveBase, $
    ;                     XOFFSET=0 ,YOFFSET=0, VALUE=periodNames[j], $
    ;                     UNAME=self->buildPeriodUName(periodTitles[i-1], j), $
    ;                     SCR_XSIZE=xSize, SCR_YSIZE=ySize, $
    ;                     UVALUE=j, $
    ;                     event_pro=self.eventPrefix+eventRoutines[i-1])
    for j=0, elems-1 do aCatValuesButton= widget_button(groupValuesExclusiveBase, $
      XOFFSET=0 ,YOFFSET=0, VALUE=periodNames[j], $
      UNAME=self->buildPeriodUName(i-1, j), $
      SCR_XSIZE=xSize, SCR_YSIZE=ySize, $
      UVALUE=j, $
      event_pro=self.eventPrefix+eventRoutines[i-1])
    widget_control, aCatValuesButton, sensitive=0
  endfor
  
END

PRO FMElaborationSelectionGUI::buildGroupSection, base

  groupTitles=self.info->getGroupByTitles()
  groupNo=self.info->getGroupByNumbers()
  eventRoutines=['groupByTimeSelection', 'groupByStatUserSelection']
  xSize=self->getGroupByXSize(groupNo)
  ;print, xSize, ySize
  for i=1, groupNo do begin
    groupNames=self.info->getGroupNamesByIndex(i, CODES=CODES)
    groupNames=[groupNames, 'N/A']
    CODES=[CODES, '-1']
    elems=n_elements(groupNames)
    aGroupBase = widget_base(base, UNAME='WID_BASE_4', $
      XOFFSET=0 ,YOFFSET=0, FRAME=1, $
      TITLE='IDL' ,SPACE=1 ,XPAD=0 ,YPAD=0, /COLUMN)
    aGroupTitle=widget_label(aGroupBase, value=groupTitles[i-1], $
      font=self.titleFont, /ALIGN_LEFT)
    ;build exclusive buttons
    ySize=self->getGroupByYSize(elems)
    groupValuesExclusiveBase = Widget_Base(aGroupBase, UNAME='WID_BASE_4', $
      XOFFSET=0 ,YOFFSET=0, /EXCLUSIVE, $
      TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /COLUMN)
    for j=0, elems-2 do aCatValuesButton= widget_button(groupValuesExclusiveBase, $
      XOFFSET=0 ,YOFFSET=0, VALUE=groupNames[j], $
      UNAME=self->buildGroupUName(i-1, j), $
      UVALUE=j, $
      event_pro=self.eventPrefix+eventRoutines[i-1], $
      SCR_XSIZE=xSize, SCR_YSIZE=ySize)
    lastCatValuesButton= widget_button(groupValuesExclusiveBase, $
      XOFFSET=0 ,YOFFSET=0, VALUE=groupNames[elems-1], $
      UVALUE=j, $
      SCR_XSIZE=xSize, SCR_YSIZE=ySize, sensitive=0, $
      UNAME=self->buildGroupUName(i-1, j))
  ;event_pro=self.eventPrefix+eventRoutines[i-1], $
  endfor
  
END

FUNCTION FMElaborationSelectionGUI::buildGroupUName, typeIndex, valueIndex, STAT=STAT, TIME=TIME

  if keyword_set(STAT) then typeIndex=1
  if keyword_set(TIME) then typeIndex=0
  groupTitles=self.info->getGroupByTitles()
  groupNames=self.info->getGroupNamesByIndex(typeIndex+1, CODES=CODES)
  groupNames=[groupNames, 'N/A]']
  uName='ELABGROUP**'+groupTitles[typeIndex]+'*'+groupNames[valueIndex]
  ;print, uname
  return, uName
  
END

FUNCTION FMElaborationSelectionGUI::buildPeriodUName, periodIndex, nameIndex, DAY=DAY, SEASON=SEASON, buttonIndex=buttonIndex

  periodTitles=self.info->getPeriodTitles()
  if keyword_set (SEASON) then periodIndex=0
  if keyword_set (DAY) then periodIndex=1
  uName='PERIOD**'+periodTitles[periodIndex]+'*'+strcompress(nameIndex, /REMOVE)
  ;print, '-->', periodTitle
  ;print, '-->', periodName
  return, uName
  
END

; set/get
FUNCTION FMElaborationSelectionGUI::getTitle

  return, 'Analysis'
  
END

; info interaction
; ** Diagram **
FUNCTION FMElaborationSelectionGUI::getDiagramDescriptionAtIndex, index

  descrs=self.info->getDiagramDescriptions()
  return, descrs[index]
  
END

FUNCTION FMElaborationSelectionGUI::getCurrentDiagramDescription

  return, self->getDiagramDescriptionAtIndex(self.info->getDiagramSelection())
  
END

;FUNCTION FMElaborationSelectionGUI::getAllAxisNames
;
;  descrs=self.info->getAxisNames()
;  return, descrs
;  
;END

FUNCTION FMElaborationSelectionGUI::getAllDiagramDescriptions

  descrs=self.info->getDiagramDescriptions()
  return, descrs
  
END

FUNCTION FMElaborationSelectionGUI::getDiagramNameAtIndex, index

  names=self.info->getDiagramNames()
  return, names[index]
  
END

FUNCTION FMElaborationSelectionGUI::getCurrentDiagramName

  return, self->getDiagramNameAtIndex(self.info->getDiagramSelection())
  
END

FUNCTION FMElaborationSelectionGUI::getAllDiagramNames

  names=self.info->getDiagramNames()
  return, names
  
END

;; Diagram section - code
FUNCTION FMElaborationSelectionGUI::getDiagramCodeAtIndex, index

  codes=self.info->getDiagramCodes()
  return, codes[index]
  
END

FUNCTION FMElaborationSelectionGUI::getCurrentDiagramCode

  return, self->getDiagramCodeAtIndex(self.info->getDiagramSelection())
  
END

FUNCTION FMElaborationSelectionGUI::getAllDiagramCodes

  codes=self.info->getDiagramCodes()
  return, codes
  
END
;Elaboration section - description
FUNCTION FMElaborationSelectionGUI::getDescriptionAtIndex, index

  descrs=self.info->getElabDescriptions()
  return, descrs[index]
  
END

FUNCTION FMElaborationSelectionGUI::getCurrentDescription

  return, self.info->getCurrentElabDescription()
  
END

FUNCTION FMElaborationSelectionGUI::getAllDescriptions

  descrs=self.info->getElabDescriptions()
  return, descrs
  
END
;Elaboration section - name
FUNCTION FMElaborationSelectionGUI::getNameAtIndex, index

  names=self.info->getElabNames()
  return, names[index]
  
END

FUNCTION FMElaborationSelectionGUI::getCurrentName

  return, self->getNameAtIndex(self.info->getElabSelection())
  
END

FUNCTION FMElaborationSelectionGUI::getAllNames

  names=self.info->getElabNames()
  return, names
  
END
;Elaboration section - code
FUNCTION FMElaborationSelectionGUI::getCodeAtIndex, index

  codes=self.info->getCodes()
  return, codes[index]
  
END

FUNCTION FMElaborationSelectionGUI::getCurrentCode

  return, self->getCodeAtIndex(self.info->getElabSelection())
  
END

FUNCTION FMElaborationSelectionGUI::getAllCodes

  codes=self.info->getCodes()
  return, codes
  
END
;;parameter section
;FUNCTION FMElaborationSelectionGUI::getAllParameterNames
;
;  names=self.info->getParameterNames()
;  return, names
;  
;END
;
;FUNCTION FMElaborationSelectionGUI::getAllParameterCodes
;
;  codes=self.info->getParameterCodes()
;  return, codes
;  
;END
;
;FUNCTION FMElaborationSelectionGUI::getParameterMeasureUnitAtIndex, index
;
;  names=self.info->getParameterMeasureUnits()
;  return, names[index]
;  
;END
;
;FUNCTION FMElaborationSelectionGUI::getCurrentParameterSelections
;
;  return, self->getParameterMeasureUnitAtIndex(self.info->getParametersSelection())
;  
;END

FUNCTION FMElaborationSelectionGUI::getThresholdString

  thresholds=self.info->getThresholdValues()
  strThr=strcompress(thresholds[0], /REMOVE)
  for i=1, n_elements(thresholds)-1 do begin
    strThr=strThr+'*'+strcompress(thresholds[i])
  endfor
  return, strThr
  
END
; constructor/destructor
FUNCTION FMElaborationSelectionGUI::init, info, mgr, fonts=fonts

  if not self -> FMInfoSelectionGUI :: init(info, mgr, fonts=fonts) then return , 0
  return , 1
  
  
END

PRO FMElaborationSelectionGUI::cleanUp

  self -> FMInfoSelectionGUI::cleanUp
  
END

;****************************************************************************************

PRO FMElaborationSelectionGUI__Define

  Struct = { FMElaborationSelectionGUI , $
    elaborationDescriptionText: 0l, $
    criteriaButton: 0l, $
    measureUnitLabel: 0l, $
    thresholdText: 0l, $
    thresholdButton: 0l, $
    diagramList: 0l, $
    multipleInfoText: 0l, $
    elaborationList: 0l, $
    radioSelections: intarr(4), $
    Inherits FMInfoSelectionGUI $
    }
    
END

;****************************************************************************************

