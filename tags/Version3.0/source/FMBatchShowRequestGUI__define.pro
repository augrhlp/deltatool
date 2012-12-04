FUNCTION FMBatchShowRequestGUI::getInfoLineXSize

  return, (float(self.dimensions[0])-(float(self.dimensions[0])*.02))/self.columns
  
END

FUNCTION FMBatchShowRequestGUI::getInfoTitleXSize

  return, self->getInfoLineXSize()/2
  
END

FUNCTION FMBatchShowRequestGUI::getInfoTextXSize

  return, self->getInfoLineXSize()-self->getInfoTitleXSize()
  
END

PRO FMBatchShowRequestGUI::updateToCaller

  self.mgr->showRequestOK, self
  
END

FUNCTION FMBatchShowRequestGUI::getLabelYSize

  return, 20
  
END

PRO FMBatchShowRequestGUI::build

  title=self->getTitle()
  
  base = widget_base(/COLUMN, $
    uvalue=self, TLB_FRAME_ATTR=1, $
    title=title, /TLB_KILL_REQUEST_EVENTS, event_pro=self.eventprefix+'destroyWindow')
    
  mainBase = widget_base(base, /COLUMN)
  
  subBase11 = widget_base(mainbase,xpad=self.xPad, ypad=self.yPad,space=0,/ROW)
  subBase12 = widget_base(mainbase,xpad=self.xPad, ypad=self.yPad,space=0,/ROW, /ALIGN_CENTER)
  subBase111 = widget_base(subBase11,xpad=1, ypad=1,space=1,/COLUMN, FRAME=1)
  subBase112 = widget_base(subBase11,xpad=1, ypad=1,space=1,/COLUMN, FRAME=1)
  
  self->buildInfoSection, subBase111
  self->buildMessageSection, subBase111
  self->buildOKButton, subBase12
  
  self->SetTopBase, base
  xmanager, 'fairmode', base, /JUST_REG, /CATCH
  
END

PRO FMBatchShowRequestGUI::buildMessageSection, base

  smallTextY=20
  messageTitle=widget_label(base, value='Scroll the text area to see ALL contents of each request element.', SCR_YSIZE=self->getLabelYSize(), /ALIGN_LEFT, font=self.titleFont)

END

PRO FMBatchShowRequestGUI::buildInfoSection, base

  smallTextY=20
  sectionTitle=widget_label(base, value='Request', SCR_YSIZE=self->getLabelYSize(), /ALIGN_LEFT, font=self.titleFont)
  
  tripleListBase=widget_base(base, xpad=0, ypad=0, space=0, /ROW)
  
  infoNames=*self.requestElementNames
  infoColumns=lonarr(self.columns)
  fieldsNo=n_elements(infoNames)
  
  for i=0, self.columns-1 do infoColumns[i]=widget_base(tripleListBase, /COLUMN, FRAME=1)
  infoLabels=lonarr(fieldsNo) & infoTexts=lonarr(fieldsNo)
  infoPerColumn=(fieldsNo/self.columns)+1
  for i=0, fieldsNo-1 do begin
    columnIdx=i/infoPerColumn
    infoLineBase=widget_base(infoColumns[columnIdx], /ROW)
    infoLabels[i] = widget_label(infoLineBase, UNAME=infoNames[i]+'LABEL', $
      SCR_XSIZE=self->getInfoTitleXSize() ,SCR_YSIZE=smallTextY ,/ALIGN_RIGHT, $
      VALUE='Label', font=self.labelLittleFont)
      
    ;infoTexts[i] = widget_text(infoLineBase, UNAME=infoNames[i], XOFFSET=29, $
    infoTexts[i] = widget_text(infoLineBase, UNAME=infoNames[i]+'TEXT', $
      SCR_XSIZE=self->getInfoTextXSize() ,SCR_YSIZE=self->getValueYDim() ,SENSITIVE=1 ,/ALL_EV $
      , VALUE='Info', font=self.textFont, /SCROLL, /WRAP, event_pro=self.eventprefix+'doNothing')
  endfor
  self.infoLabels=ptr_new(infoLabels, /NO_COPY)
  self.infoTexts=ptr_new(infoTexts, /NO_COPY)
  
END

PRO FMBatchShowRequestGUI::exitRequest

  ;print, 'Destroy Info gui'
  ;self.mgr->setBlockWindowControl, /OFF
  self.info=obj_new()
  obj_destroy, self
  
END

PRO FMBatchShowRequestGUI::closeShowRequest

  self.mgr->closeShowRequest
  obj_destroy, self
  
END

PRO FMBatchShowRequestGUI::OKRequest

  if self->checkIntegrity() then begin
    ;self.mgr->setBlockWindowControl, /OFF
    self->updateToCaller
  endif else begin
    print, 'Bad ', obj_class(self)
  ;exitMessage=['Something wrong in your selection', 'Check data']
  ;dlgResult=dialog_message(exitMessage, dialog_parent=self->getTopBase(), Title='Warning')
  endelse
  
END

; external
FUNCTION FMBatchShowRequestGUI::checkIntegrity

  ;return, self.info->checkIntegrity(self)
  ; Only show, no check...
  return, 1
  
END

; gui check/conf/fill/update...
PRO FMBatchShowRequestGUI::setLineInfo, infoLabel, infoText

  label=widget_info(self->getTopBase(), FIND_BY_UNAME=infoLabel+'LABEL')
  info=widget_info(self->getTopBase(), FIND_BY_UNAME=infoLabel+'TEXT')
  
  widget_control, label, set_value=infoLabel
  widget_control, info, set_value=infoText
  
END

FUNCTION FMBatchShowRequestGUI::mapTimeStampAsDisplayText, info

  noElems=n_elements(info)
  infoTexts=strarr(noElems)
  for i=0, noElems-1 do begin
    infoTexts[i]=strcompress(info[i].value, /REMOVE)+'('+strcompress(info[i].template, /REMOVE)+')'
  endfor
  return, infoTexts
  
END

FUNCTION FMBatchShowRequestGUI::mapDateTimeAsDisplayText, info

  noElems=n_elements(info)
  infoTexts=strarr(noElems)
  for i=0, noElems-1 do begin
    infoTexts[i]='day:'+strcompress(info[i].day, /REMOVE)+' month:'+strcompress(info[i].month, /REMOVE)
  endfor
  return, infoTexts
  
END

FUNCTION FMBatchShowRequestGUI::mapGCValuesAsDisplayText, info

  infoNo=n_elements(info)
  infoTexts=strarr(infoNo)
  for i=0, infoNo-1 do infoTexts[i]='#'+strcompress(i+1, /REMOVE)+': '+ strcompress(info[i], /REMOVE)
  return, infoTexts

END

PRO FMBatchShowRequestGUI::configure

  infoElements=*self.requestElementNames
  
  req=self->getInfo()
  
  ;requestInfoFields[0]='diagramName'
  infoLabel=infoElements[0]
  infoText=req->getDiagramName()
  self->setLineInfo, infoLabel, infoText
  
  ;requestInfoFields[1]='diagramCode'
  infoLabel=infoElements[1]
  infoText=strcompress(req->getDiagramCode(), /REMOVE)
  self->setLineInfo, infoLabel, infoText
  
  ;  ;plotRoutine: '', $
  ;  requestInfoFields[2]='modelCodes'
  infoLabel=infoElements[2]
  infoText=strcompress(req->getModelCodes(), /REMOVE)
  self->setLineInfo, infoLabel, infoText
  
  ;  requestInfoFields[3]='modelNames'
  infoLabel=infoElements[3]
  infoText=strcompress(req->getModelNames(), /REMOVE)
  self->setLineInfo, infoLabel, infoText
  
  ;  requestInfoFields[4]='scenarioCodes'
  infoLabel=infoElements[4]
  infoText=strcompress(req->getScenarioCodes(), /REMOVE)
  self->setLineInfo, infoLabel, infoText
  
  ;  requestInfoFields[5]='scenarioNames'
  infoLabel=infoElements[5]
  infoText=strcompress(req->getScenarioNames(), /REMOVE)
  self->setLineInfo, infoLabel, infoText
  
  ;  requestInfoFields[6]='runNames'
  infoLabel=infoElements[6]
  infoText=strcompress(req->getRunNames(), /REMOVE)
  self->setLineInfo, infoLabel, infoText
  
  ;  requestInfoFields[7]='runCodes'
  infoLabel=infoElements[7]
  infoText=strcompress(req->getRunCodes(), /REMOVE)
  self->setLineInfo, infoLabel, infoText
  
  ;  requestInfoFields[8]='runFileNames'
  infoLabel=infoElements[8]
  infoText=strcompress(req->getRunFileNames(), /REMOVE)
  self->setLineInfo, infoLabel, infoText
  
  ;  requestInfoFields[9]='runResultType'
  infoLabel=infoElements[9]
  infoText=req->getRunResultType()
  self->setLineInfo, infoLabel, infoText
  
  ;  requestInfoFields[10]='useObservedModel'
  infoLabel=infoElements[10]
  infoText=req->getUseObservedModel()
  if infoText eq 0 then infoText='False' else infoText='True'
  self->setLineInfo, infoLabel, infoText
  
  ;  requestInfoFields[11]='singleObsCodes'
  infoLabel=infoElements[11]
  infoText=strcompress(req->getSingleObsCodes(), /REMOVE)
  self->setLineInfo, infoLabel, infoText
  
  ;  requestInfoFields[12]='singleObsNames'
  infoLabel=infoElements[12]
  infoText=strcompress(req->getSingleObsNames(), /REMOVE)
  self->setLineInfo, infoLabel, infoText
  
  ;  ;    singleObsLongitudes : ptr_new(), $
  ;  ;    singleObsLatitudes : ptr_new(), $
  ;  ;    singleObsAltitudes : ptr_new(), $
  ;  ;    singleObsGMTs : ptr_new(), $
  ;  ;    singleObsCountries : ptr_new(), $
  
  ;  requestInfoFields[13]='singleShortObsNames'
  infoLabel=infoElements[13]
  infoText=strcompress(req->getSingleShortObsNames(), /REMOVE)
  self->setLineInfo, infoLabel, infoText
  ;  ;    singleObsCatInfos : ptr_new(), $
  ;  requestInfoFields[14]='groupTitles'
  infoLabel=infoElements[14]
  infoText=strcompress(req->getGroupTitles(), /REMOVE)
  self->setLineInfo, infoLabel, infoText
  
  ;  requestInfoFields[15]='groupCodes'
  infoLabel=infoElements[15]
  grCodes=req->getGroupCodes()
  ;infoText=strcompress(req->getGroupCodes(), /REMOVE)
  infoText=''
  if size(grCodes, /TYPE) eq 2 then infoText=["", strcompress(grCodes, /REMOVE)] else for i=0, n_elements(grCodes)-1 do infoText=[infoText, *(grCodes[i]), "*"]  
  self->setLineInfo, infoLabel, infoText[1:*]
  
  ;  requestInfoFields[16]='groupNames'
  infoLabel=infoElements[16]
  grNames=req->getGroupNames()
  infoText=''
  if size(grNames, /TYPE) eq 2 then infoText=["", strcompress(grNames, /REMOVE)] else for i=0, n_elements(grNames)-1 do infoText=[infoText, *(grNames[i]), "*"]  
  self->setLineInfo, infoLabel, infoText[1:*]
  
  ;  requestInfoFields[17]='groupStatToApplyName'
  infoLabel=infoElements[17]
  infoText=strcompress(req->getGroupStatToApplyName(), /REMOVE)
  self->setLineInfo, infoLabel, infoText
  
  ;  requestInfoFields[18]='groupStatToApplyCode'
  infoLabel=infoElements[18]
  infoText=strcompress(req->getGroupStatToApplyCode(), /REMOVE)
  self->setLineInfo, infoLabel, infoText
  
  ;  requestInfoFields[19]='elaborationCode'
  infoLabel=infoElements[19]
  infoText=strcompress(req->getElaborationCode(), /REMOVE)
  self->setLineInfo, infoLabel, infoText
  
  ;  requestInfoFields[20]='elaborationName'
  infoLabel=infoElements[20]
  infoText=req->getElaborationName()
  self->setLineInfo, infoLabel, infoText
  
  ;  ;    elaborationRoutine: '', $
  ;  requestInfoFields[21]='elaborationOCUse'
  infoLabel=infoElements[21]
  infoText=req->getElaborationOCUse()
  if infoText eq 1 then infoText='True' else infoText='False'
  self->setLineInfo, infoLabel, infoText
  
  ;  requestInfoFields[22]='elaborationOCStat'
  infoLabel=infoElements[22]
  infoText=req->getElaborationOCStat()
  self->setLineInfo, infoLabel, infoText
  
  ;  requestInfoFields[23]='elaborationOCTimeAvgName'
  infoLabel=infoElements[23]
  infoText=req->getElaborationOCTimeAvgName()
  self->setLineInfo, infoLabel, infoText
  
  ;  requestInfoFields[24]='parameterCodes'
  infoLabel=infoElements[24]
  infoText=strcompress(req->getParameterCodes(), /REMOVE)
  self->setLineInfo, infoLabel, infoText
  
  ;  requestInfoFields[25]='parameterNames'
  infoLabel=infoElements[25]
  infoText=req->getParameterNames()
  self->setLineInfo, infoLabel, infoText
  
  ;  requestInfoFields[26]='parameterMeasureUnits'
  infoLabel=infoElements[26]
  infoText=req->getParameterMeasureUnits()
  self->setLineInfo, infoLabel, infoText
  
  ;  requestInfoFields[27]='extraValues'
  infoLabel=infoElements[27]
  infoText=strcompress(req->getExtraValues(), /REMOVE)
  self->setLineInfo, infoLabel, infoText
  
  ;  requestInfoFields[28]='observedGroupStatInfo'
  infoLabel=infoElements[28]
  infoText=strcompress(req->getObservedGroupStatInfo(), /REMOVE)
  self->setLineInfo, infoLabel, infoText
  
  ;  requestInfoFields[29]='seasonInfo'
  infoLabel=infoElements[29]
  infoText=self->mapTimeStampAsDisplayText(req->getSeasonInfo())
  self->setLineInfo, infoLabel, infoText
  
  ;  requestInfoFields[30]='hourInfo'
  infoLabel=infoElements[30]
  infoText=self->mapTimeStampAsDisplayText(req->getHourInfo())
  self->setLineInfo, infoLabel, infoText
  
  ;  requestInfoFields[31]='groupByTimeInfo'
  infoLabel=infoElements[31]
  infoText=self->mapTimeStampAsDisplayText(req->getGroupByTimeInfo())
  self->setLineInfo, infoLabel, infoText
  
  ;  requestInfoFields[32]='groupByStatInfo'
  infoLabel=infoElements[32]
  infoText=strcompress(req->getGroupByStatInfo(), /REMOVE)
  self->setLineInfo, infoLabel, infoText
  
  ;  requestInfoFields[33]='seasonType'
  infoLabel=infoElements[33]
  infoText=strcompress(req->getSeasonType(), /REMOVE)
  self->setLineInfo, infoLabel, infoText
  
  ;  requestInfoFields[34]='hourType'
  infoLabel=infoElements[34]
  infoText=strcompress(req->getHourType(), /REMOVE)
  self->setLineInfo, infoLabel, infoText
  
  ;  requestInfoFields[35]='startDate'
  infoLabel=infoElements[35]
  infoText=self->mapDateTimeAsDisplayText(req->getStartDate())
  self->setLineInfo, infoLabel, infoText
  
  ;  requestInfoFields[36]='endDate'
  infoLabel=infoElements[36]
  infoText=self->mapDateTimeAsDisplayText(req->getEndDate())
  self->setLineInfo, infoLabel, infoText
  
  ;  requestInfoFields[37]='startIndex'
  infoLabel=infoElements[37]
  infoText=strcompress(req->getStartIndex(), /REMOVE)
  self->setLineInfo, infoLabel, infoText
  
  ;  requestInfoFields[38]='endIndex'
  infoLabel=infoElements[38]
  infoText=strcompress(req->getEndIndex(), /REMOVE)
  self->setLineInfo, infoLabel, infoText
  
  ;  ;    startPlotIndex: 0, $
  ;  ;    endPlotIndex: 0, $
  ;  ;    googleEarthLocation:'',$
  ;  requestInfoFields[39]='modelInfo'
  infoLabel=infoElements[39]
  infoText=strarr(3)
  info=req->getModelInfo()
  infoText[0]='Scale: '+info.scale
  infoText[1]='frequency: '+info.frequency
  infoText[2]='Year: '+strcompress(info.year, /REMOVE)
  self->setLineInfo, infoLabel, infoText
  
  infoLabel=infoElements[40]
  infoText=req->getElaborationOCUse()
  if infoText eq 0 then infoText='False' else infoText='True'
  self->setLineInfo, infoLabel, infoText
  
  infoLabel=infoElements[41]
  info=req->getGoalsCriteriaValues(/CONTENTS, NOVALUES=NOVALUES)
  infoText=self->mapGCValuesAsDisplayText(info)
  self->setLineInfo, infoLabel, infoText

END

FUNCTION FMBatchShowRequestGUI::getTitle

  return, 'Request Batch Data selection'
  
END

FUNCTION FMBatchShowRequestGUI::getRequestInfoFieldNames

  ;fileName : '', $
  ;    entityFileName : '', $
  ;    elaborationFileName : '', $
  ;    plotDeviceName: '', $
  ;    pageBreak: '', $
  ;    printOrient: '', $
  ;    location: fltarr(4), $
  ;    multipleChoiceUserSelectionFlags : bytarr(4), $
  requestInfoFields=strarr(42)
  requestInfoFields[0]='diagramName'
  requestInfoFields[1]='diagramCode'
  ;plotRoutine: '', $
  requestInfoFields[2]='modelCodes'
  requestInfoFields[3]='modelNames'
  requestInfoFields[4]='scenarioCodes'
  requestInfoFields[5]='scenarioNames'
  requestInfoFields[6]='runNames'
  requestInfoFields[7]='runCodes'
  requestInfoFields[8]='runFileNames'
  requestInfoFields[9]='runResultType'
  requestInfoFields[10]='useObservedModel'
  requestInfoFields[11]='singleObsCodes'
  requestInfoFields[12]='singleObsNames'
  ;    singleObsLongitudes : ptr_new(), $
  ;    singleObsLatitudes : ptr_new(), $
  ;    singleObsAltitudes : ptr_new(), $
  ;    singleObsGMTs : ptr_new(), $
  ;    singleObsCountries : ptr_new(), $
  requestInfoFields[13]='singleShortObsNames'
  ;    singleObsCatInfos : ptr_new(), $
  requestInfoFields[14]='groupTitles'
  requestInfoFields[15]='groupCodes'
  requestInfoFields[16]='groupNames'
  requestInfoFields[17]='groupStatToApplyName'
  requestInfoFields[18]='groupStatToApplyCode'
  requestInfoFields[19]='elaborationCode'
  requestInfoFields[20]='elaborationName'
  ;    elaborationRoutine: '', $
  requestInfoFields[21]='elaborationOCUse'
  requestInfoFields[22]='elaborationOCStat'
  requestInfoFields[23]='elaborationOCTimeAvgName'
  requestInfoFields[24]='parameterCodes'
  requestInfoFields[25]='parameterNames'
  requestInfoFields[26]='parameterMeasureUnits'
  requestInfoFields[27]='extraValues'
  requestInfoFields[28]='observedGroupStatInfo'
  requestInfoFields[29]='seasonInfo'
  requestInfoFields[30]='hourInfo'
  requestInfoFields[31]='groupByTimeInfo'
  requestInfoFields[32]='groupByStatInfo'
  requestInfoFields[33]='seasonType'
  requestInfoFields[34]='hourType'
  requestInfoFields[35]='startDate'
  requestInfoFields[36]='endDate'
  requestInfoFields[37]='startIndex'
  requestInfoFields[38]='endIndex'
  ;    startPlotIndex: 0, $
  ;    endPlotIndex: 0, $
  ;    googleEarthLocation:'',$
  requestInfoFields[39]='modelInfo'
  requestInfoFields[40]='goalsAndCriteriaNeeded'
  requestInfoFields[41]='goalsAndCriteriaValues'
  
  return, requestInfoFields
  
END

; constructor/destructor
FUNCTION FMBatchShowRequestGUI::init, info, mgr, fonts=fonts

  if not self -> FMInfoSelectionGUI :: init(info, mgr, fonts=fonts) then return , 0
  ptr_free, self.infoLabels
  ptr_free, self.infoTexts
  self.columns=3
  self.labelLittleFont="TIMES ROMAN*12*BOLD"
  requestElementNames=self->getRequestInfoFieldNames()
  self.requestElementNames=ptr_new(requestElementNames, /NO_COPY)
  return , 1
  
END

PRO FMBatchShowRequestGUI::cleanUp

  self ->FMInfoSelectionGUI::cleanUp
  ptr_free, self.infoLabels
  ptr_free, self.infoTexts
  obj_destroy, self.info
  
END

;****************************************************************************************

PRO FMBatchShowRequestGUI__Define

  Struct = { FMBatchShowRequestGUI , $
    ;    allObservationsFlag: 0b, $
    ;    allModelFlag: 0b, $
    ;    allParameterFlag: 0b, $
    ;    allScenarioFlag: 0b, $
    labelLittleFont: '', $
    requestElementNames: ptr_new(), $
    infoLabels: ptr_new(), $
    infoTexts: ptr_new(), $
    columns: 0, $
    Inherits FMInfoSelectionGUI $
    }
    
END

;****************************************************************************************

