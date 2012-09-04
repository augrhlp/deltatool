;********************
@structure_definition
;********************
FUNCTION ElaborationDisplayInfo::buildTextMultiple, flags

  ;return "human" text from 4 element array
  ; parameters, models, scenarios, observations
  text=''
  if flags[0] eq 1 then text=text+'parameters, '
  if flags[1] eq 1 then text=text+'models, '
  if flags[2] eq 1 then text=text+'scenarios, '
  if flags[3] eq 1 then text=text+'observations, '
  return, strmid(text, 0, strlen(text)-2)+'.'

END

FUNCTION ElaborationDisplayInfo::getCurrentElabMultipleChoiceFlagsNumber

  flags=self->getCurrentElabMultipleChoiceFlags()
  dims=size(*flags, /DIM)
  return, dims[0]

END

PRO ElaborationDisplayInfo::setEndYear

  self->setEndHourSelection, 23
  self->setEndDaySelection, 30
  self->setEndMonthSelection, 11

END

PRO ElaborationDisplayInfo::setExclusives, exclValues

  self.groupByTimeSelection=exclValues[0]
  self.groupByStatSelection=exclValues[1]
  self.seasonSelection=exclValues[2]
  self.dayPeriodSelection=exclValues[3]

END

FUNCTION ElaborationDisplayInfo::isDayPeriodSelected

  return, self->getDayPeriodSelection() ne -1

END

FUNCTION ElaborationDisplayInfo::isSeasonSelected

  return, self->getSeasonSelection() ne -1

END

FUNCTION ElaborationDisplayInfo::isGroupByStatSelected

  return, self->getGroupByStatSelection() ne -1

END

FUNCTION ElaborationDisplayInfo::isGroupByTimeSelected

  return, self->getGroupByTimeSelection() ne -1

END

PRO ElaborationDisplayInfo::userSetReferenceValues, values, CANCEL=CANCEL

  if keyword_set(CANCEL) then begin
    self->resetThresholds
  endif else begin
    self->setThresholdValues, float(values)
    self->setThresholdFlag, 1
  endelse

END

FUNCTION ElaborationDisplayInfo::checkIntegrity, view, NODISPLAYCHECK=NODISPLAYCHECK

  groupExclInfo=self->getExclusiveInfo()
  periodSSel=self->getSeasonSelection()
  periodDSel=self->getDayPeriodSelection()
  groupBTSel=self->getGroupByTimeSelection()
  groupBSSel=self->getGroupByStatSelection()
  if groupExclInfo.gbTime eq 'FREE' and groupBTSel eq -1 then begin
    aa=view->dialogMessage('Select a specific -group by time-',title='Check your data')
    return, 0
  endif
  if groupExclInfo.gbStat eq 'FREE' and groupBSSel eq -1 then begin
    aa=view->dialogMessage('Select a specific -group by stat-',title='Check your data')
    return, 0
  endif
  if groupExclInfo.dayPeriod eq 'FREE' and periodDSel eq -1 then begin
    aa=view->dialogMessage('Select a specific -day period-',title='Check your data')
    return, 0
  endif
  if groupExclInfo.season eq 'FREE' and periodSSel eq -1 then begin
    aa=view->dialogMessage('Select a specific -season-',title='Check your data')
    return, 0
  endif
  if ~keyword_set(NODISPLAYCHECK) then begin
    refValues=view->getReferenceValueContents()
    refValueRequested=self->getNumberReferenceValues()
    if refValueRequested ne 0 then begin
      ;print, '+ di 0'
      refValues=strsplit(refValues, '#', /EXTRACT)
      ;print, n_elements(testRV), refValueRequested
      if n_elements(refValues) ne refValueRequested then begin
        aa=view->dialogMessage('Check thresholds values number (need '+strcompress(refValueRequested, /REMOVE)+'). "#" is separator.',title='Check your data')
        return, 0
      endif
      for i=0, n_elements(refValues)-1 do begin
        if self.utility->IsNumber(refValues[i]) then continue
        msg=view->dialogMessage('Check thresholds values contents',title='Check your data')
        return, 0
      endfor
      self->userSetReferenceValues, refValues
    endif else begin
      self->userSetReferenceValues, /CANCEL
    endelse
  endif
  ;KeesC leapyear
  year=2009
  dateExists=self.dtu->checkDate(year, month=self->getStartMonthSelection()+1, day=self->getStartDaySelection()+1, hour=self->getStartHourSelection(), julnumber=sDate)
  if dateExists ne 1 then begin
    msg=view->dialogMessage('Check start date existence',title='Check your data')
    return, 0
  endif

  dateExists=self.dtu->checkDate(year, month=self->getEndMonthSelection()+1, day=self->getEndDaySelection()+1, hour=self->getEndHourSelection(), julnumber=eDate)
  if dateExists ne 1 then begin
    msg=view->dialogMessage('Check end date existence',title='Check your data')
    return, 0
  endif
  if eDate-sDate le 0 then begin
    msg=view->dialogMessage('End date must be greater than start date',title='Check your data')
    return, 0
  endif
  return, 1

END

PRO ElaborationDisplayInfo::resetThresholds

  self->setThresholdFlag, 0
  ptr_free, self.thresholdValues

END

FUNCTION ElaborationDisplayInfo::getCurrentDiagramMaxMultipleChoiceNumber

  diagramIndex=self->getDiagramSelection()
  mmc=self->getDiagramMaxMultipleChoice()

  return, mmc[diagramIndex]

END

FUNCTION ElaborationDisplayInfo::getCurrentElabMultipleChoiceFlags

  mcFlags=self->getElabMultipleChoiceFlags()
  elabCode=self->getSelectedElabCode()
  allCodes=self->getElabCodes()

  idx=(where(elabCode eq allCodes))[0]

  return, mcFlags[idx]

END

FUNCTION ElaborationDisplayInfo::getCurrentGoalsCriteriaFlag

  gcocFlags=self->getElabGCOC()
  elabCode=self->getSelectedElabCode()
  allCodes=self->getElabCodes()

  idx=(where(elabCode eq allCodes))[0]

  return, gcocFlags[idx]

END

FUNCTION ElaborationDisplayInfo::getNumberReferenceValues

  refValues=self->getElabNumberRefValues()
  elabCode=self->getSelectedElabCode()
  allCodes=self->getElabCodes()

  idx=(where(elabCode eq allCodes))[0]

  return, refValues[idx]

END

FUNCTION ElaborationDisplayInfo::getExclusiveInfo

  gbTC=self->getElabGroupByTimeCodes()
  gbSC=self->getElabGroupByStatCodes()
  dpCodes=self->getElabDayPeriodCodes()
  sCodes=self->getElabSeasonCodes()

  elabCode=self->getSelectedElabCode()
  allCodes=self->getElabCodes()

  idx=(where(elabCode eq allCodes))[0]

  return, {gbStat: gbSC[idx], gbTime:gbTC[idx], dayPeriod:dpCodes[idx], season:sCodes[idx]}

END

;diagramMaxMultipleChoice
FUNCTION ElaborationDisplayInfo::getDiagramMaxMultipleChoice

  if ptr_valid(self.diagramMaxMultipleChoice) then return, *self.diagramMaxMultipleChoice else return, ['-1']

END

PRO ElaborationDisplayInfo::setDiagramMaxMultipleChoice, list

  ptr_free, self.diagramMaxMultipleChoice
  self.diagramMaxMultipleChoice=ptr_new(list, /NO_COPY)

END
;elabMultipleChoiceFlags
FUNCTION ElaborationDisplayInfo::getElabMultipleChoiceFlags

  if ptr_valid(self.elabMultipleChoiceFlags) then return, *self.elabMultipleChoiceFlags else return, ['-1']

END

PRO ElaborationDisplayInfo::setElabMultipleChoiceFlags, list

  ptr_free, self.elabMultipleChoiceFlags
  self.elabMultipleChoiceFlags=ptr_new(list, /NO_COPY)

END

FUNCTION ElaborationDisplayInfo::getElabGroupByTimeCodes

  if ptr_valid(self.elabGroupByTimeCodes) then return, *self.elabGroupByTimeCodes else return, ['-1']

END

PRO ElaborationDisplayInfo::setElabGroupByTimeCodes, list

  ptr_free, self.elabGroupByTimeCodes
  self.elabGroupByTimeCodes=ptr_new(list, /NO_COPY)

END

FUNCTION ElaborationDisplayInfo::getElabGroupByStatCodes

  if ptr_valid(self.elabGroupByStatCodes) then return, *self.elabGroupByStatCodes else return, ['-1']

END

PRO ElaborationDisplayInfo::setElabGroupByStatCodes, list

  ptr_free, self.elabGroupByStatCodes
  self.elabGroupByStatCodes=ptr_new(list, /NO_COPY)

END

FUNCTION ElaborationDisplayInfo::getElabDayPeriodCodes

  if ptr_valid(self.elabDayPeriodCodes) then return, *self.elabDayPeriodCodes else return, ['-1']

END

PRO ElaborationDisplayInfo::setElabDayPeriodCodes, list

  ptr_free, self.elabDayPeriodCodes
  self.elabDayPeriodCodes=ptr_new(list, /NO_COPY)

END

FUNCTION ElaborationDisplayInfo::getElabSeasonCodes

  if ptr_valid(self.elabSeasonCodes) then return, *self.elabSeasonCodes else return, ['-1']

END

PRO ElaborationDisplayInfo::setElabSeasonCodes, list

  ptr_free, self.elabSeasonCodes
  self.elabSeasonCodes=ptr_new(list, /NO_COPY)

END

FUNCTION ElaborationDisplayInfo::getSelectedDayPeriodCode

  codes=*self.dayPeriodCodes
  selIdx=self->getDayPeriodSelection()

  return, codes[selIdx]

END

FUNCTION ElaborationDisplayInfo::getSelectedSeasonCode

  codes=*self.seasonCodes
  selIdx=self->getSeasonSelection()

  return, codes[selIdx]

END

FUNCTION ElaborationDisplayInfo::getSelectedDayPeriodName

  selIdx=self->getDayPeriodSelection()
  if selIdx eq -1 then return, 'N/A'
  allNames=self->getDayPeriodNames()
  name=allNames[selIdx]

  return, name

END

FUNCTION ElaborationDisplayInfo::getSelectedSeasonName

  selIdx=self->getSeasonSelection()
  if selIdx eq -1 then return, 'N/A'
  allNames=self->getSeasonNames()
  name=allNames[selIdx]

  return, name

END

FUNCTION ElaborationDisplayInfo::getSelectedGroupByStatName

  selIdx=self->getGroupByStatSelection()
  if selIdx eq -1 then return, 'N/A'
  allNames=self->getGroupByStatNames()
  name=allNames[selIdx]

  return, name

END

FUNCTION ElaborationDisplayInfo::getSelectedGroupByTimeName

  selIdx=self->getGroupByTimeSelection()
  if selIdx eq -1 then return, 'N/A'
  allNames=self->getGroupByTimeNames()
  name=allNames[selIdx]

  return, name

END

;FUNCTION ElaborationDisplayInfo::getAxisCodesByDiagram, diagramCode
;
; allAxis=self->getDiagramAxisCodes()
; allDiagramCodes=self->getDiagramCodes()
; idx=(where(diagramCode eq allDiagramCodes))[0]
; return, *allAxis[idx]
;
;END

;PRO ElaborationDisplayInfo::setDiagramAxisCodes, list
;
;  ptr_free, self.diagramAxisCodes
;  self.diagramAxisCodes=ptr_new(list, /NO_COPY)
;
;END

;FUNCTION ElaborationDisplayInfo::getDiagramAxisCodes
;
;  if ptr_valid(self.diagramAxisCodes) then return, *self.diagramAxisCodes
;  return, -1
;
;END

;FUNCTION ElaborationDisplayInfo::getAxisNamesByDiagram, diagramCode
;
;  allDiagramCodes=self->getDiagramCodes()
;  allAxisNames=self->getAxisNames()
;  idx=(where(diagramCode eq allDiagramCodes))[0]
;  pIndexes=self->getDiagramAxisCodesByIndex(idx)
;  if pIndexes[0] ne -1 then return, allAxisNames[pIndexes]
;
;END

;FUNCTION ElaborationDisplayInfo::getDiagramAxisCodesByIndex, idx
;
;  if ptr_valid(self.diagramAxisCodes) then return, *(*self.diagramAxisCodes)[idx] else return, -1
;
;END

FUNCTION ElaborationDisplayInfo::getSelectedElabName

  diagramIndex=self->getDiagramSelection()
  elabIndex=self->getElabSelection()

  allCodes=self->getElabCodes()
  allNames=self->getElabNames()
  codes=self->getElaborationsCodesByDiagram(diagramIndex)
  thisCode=codes[elabIndex]
  name=allNames[(where(thisCode eq allCodes))[0]]

  return, name

END

FUNCTION ElaborationDisplayInfo::getSelectedElabCode

  elabIndex=self->getElabSelection()
  diagramIndex=self->getDiagramSelection()

  allCodes=self->getElabCodes()
  ;allNames=self->getElabNames()
  codes=self->getElaborationsCodesByDiagram(diagramIndex)
  thisCode=codes[elabIndex]
  return, thisCode

END

FUNCTION ElaborationDisplayInfo::getSelectedDiagramName

  diagramIndex=self->getDiagramSelection()

  allNames=self->getDiagramNames()
  name=allNames[diagramIndex]

  return, name

END

FUNCTION ElaborationDisplayInfo::getSelectedDiagramCode

  diagramIndex=self->getDiagramSelection()

  allNames=self->getDiagramCodes()
  name=allNames[diagramIndex]

  return, name

END

;FUNCTION ElaborationDisplayInfo::getSelectedAxisCode
;
;  axisIndex=self->getAxisSelection()
;  diagramCode=self->getSelectedDiagramCode()
;
;  allCodes=self->getAxisCodes()
;  codes=self->getAxisCodesByDiagram(diagramCode)
;  thisCode=codes[axisIndex]
;
;  return, thisCode
;
;END

;FUNCTION ElaborationDisplayInfo::getSelectedAxisName
;
;  axisIndex=self->getAxisSelection()
;  diagramCode=self->getSelectedDiagramCode()
;
;  allCodes=self->getAxisCodes()
;  names=self->getAxisNamesByDiagram(diagramCode)
;  thisName=names[axisIndex]
;
;  return, thisName
;
;END

PRO ElaborationDisplayInfo::setStartDateInfo, dateTime

  self.startDateInfo=dateTime

END

PRO ElaborationDisplayInfo::setEndDateInfo, dateTime

  self.endDateInfo=dateTime

END

FUNCTION ElaborationDisplayInfo::getStartDateInfo, REAL=REAL

  sDate=self.startDateInfo
  if keyword_set(real) then begin
    sDate.day=sDate.day+1
    sDate.month=sDate.month+1
  endif
  return, sDate

END

FUNCTION ElaborationDisplayInfo::getStartHourSelection

  return, self.startDateInfo.hour

END

FUNCTION ElaborationDisplayInfo::getStartDaySelection

  return, self.startDateInfo.day

END

FUNCTION ElaborationDisplayInfo::getStartMonthSelection

  return, self.startDateInfo.month

END

FUNCTION ElaborationDisplayInfo::getEndDateInfo, REAL=REAL

  eDate=self.endDateInfo
  if keyword_set(REAL) then begin
    eDate.day=eDate.day+1
    eDate.month=eDate.month+1
  endif
  return, eDate

END

FUNCTION ElaborationDisplayInfo::getEndHourSelection

  return, self.endDateInfo.hour

END

FUNCTION ElaborationDisplayInfo::getEndDaySelection

  return, self.endDateInfo.day

END

FUNCTION ElaborationDisplayInfo::getEndMonthSelection

  return, self.endDateInfo.month

END

PRO ElaborationDisplayInfo::setStartHourSelection, index

  self.startDateInfo.hour=index

END

PRO ElaborationDisplayInfo::setStartDaySelection, index

  self.startDateInfo.day=index

END

PRO ElaborationDisplayInfo::setStartMonthSelection, index

  self.startDateInfo.month=index

END

PRO ElaborationDisplayInfo::setEndHourSelection, index

  self.endDateInfo.hour=index

END

PRO ElaborationDisplayInfo::setEndDaySelection, index

  self.endDateInfo.day=index

END

PRO ElaborationDisplayInfo::setEndMonthSelection, index

  self.endDateInfo.month=index

END

FUNCTION ElaborationDisplayInfo::getCurrentElabDescription

  elabIndex=self->getElabSelection()
  diagramIndex=self->getDiagramSelection()

  allCodes=self->getElabCodes()
  codes=self->getElaborationsCodesByDiagram(diagramIndex)
  thisCode=codes[elabIndex]
  descrs=self->getElabDescriptions()

  desc=descrs[(where(thisCode eq allCodes))[0]]
  return, desc

END

FUNCTION  ElaborationDisplayInfo::getElabNamesBySelectedDiagram

  diagramIndex=self->getDiagramSelection()
  codes=self->getElaborationsCodesByDiagram(diagramIndex)
  names=self->getElaborationsNamesByCodes(codes)
  return, names

END

;FUNCTION  ElaborationDisplayInfo::getAxisNamesBySelectedDiagram
;
;  diagramCode=self->getSelectedDiagramCode()
;  names=self->getAxisNamesByDiagram(diagramCode)
;
;  return, names
;
;END

FUNCTION ElaborationDisplayInfo::getElaborationsNamesByCodes, codesList;, ALL=ALL

  names=['']
  allElabNames=self->getElabNames()
  allElabCodes=self->getElabCodes()
  if codesList[0] ne -1 then begin
    for i=0, n_elements(codesList)-1 do begin
      idx=where(codesList[i] eq allElabCodes)
      thisName=allElabNames[(where(codesList[i] eq allElabCodes))[0]]
      names=[names, thisName]
    endfor
    names=names[1:*]
  endif
  return, names

END

FUNCTION ElaborationDisplayInfo::getElaborationsCodesByDiagram, diagramIndex, CODE=CODES;, ALL=ALL

  elabDiagramList=self->getElabDiagramCodes()
  allDiagramCodes=self->getDiagramCodes()
  allElabCodes=self->getElabCodes()
  thisDiagram=allDiagramCodes[diagramIndex]
  elabIdxs=where(thisDiagram eq elabDiagramList, count)
  if count ne 0 then return, allElabCodes[elabIdxs] else return, [-1]

END

FUNCTION ElaborationDisplayInfo::getElabDiagramCodes

  if ptr_valid(self.elabDiagramCodes) then return, *self.elabDiagramCodes
  return, -1

END

PRO ElaborationDisplayInfo::setDayPeriodNames, list

  ptr_free, self.dayPeriodNames
  self.dayPeriodNames=ptr_new(list, /NO_COPY)

END

PRO ElaborationDisplayInfo::setDayPeriodCodes, list

  ptr_free, self.dayPeriodCodes
  self.dayPeriodCodes=ptr_new(list, /NO_COPY)

END

FUNCTION ElaborationDisplayInfo::getDayPeriodNames

  if ptr_valid(self.dayPeriodNames) then begin
    return, *self.dayPeriodNames
  endif
  return, -1

END

FUNCTION ElaborationDisplayInfo::getDayPeriodCodes

  if ptr_valid(self.dayPeriodCodes) then return, *self.dayPeriodCodes else return, -1

END

PRO ElaborationDisplayInfo::setSeasonCodes, list

  ptr_free, self.seasonCodes
  self.seasonCodes=ptr_new(list, /NO_COPY)

END

PRO ElaborationDisplayInfo::setSeasonNames, list

  ptr_free, self.seasonNames
  self.seasonNames=ptr_new(list, /NO_COPY)

END

FUNCTION ElaborationDisplayInfo::getSeasonNames

  if ptr_valid(self.seasonNames) then return, *self.seasonNames else return, -1

END

FUNCTION ElaborationDisplayInfo::getSeasonCodes

  if ptr_valid(self.seasonCodes) then return, *self.seasonCodes else return, -1

END

FUNCTION ElaborationDisplayInfo::getPeriodTitles

  return, self.periodTitles

END

PRO ElaborationDisplayInfo::setPeriodTitles, list

  self.periodTitles=list

END

FUNCTION ElaborationDisplayInfo::getDayPeriodSelection

  return, self.dayPeriodSelection

END

PRO ElaborationDisplayInfo::setDayPeriodSelection, index, NONE=NONE

  if keyword_set(NONE) then self.dayPeriodSelection=-1 else self.dayPeriodSelection=index

END

FUNCTION ElaborationDisplayInfo::getSeasonSelection

  return, self.seasonSelection

END

PRO ElaborationDisplayInfo::setSeasonSelection, index, NONE=NONE

  if keyword_set(NONE) then self.seasonSelection=-1 else self.seasonSelection=index

END

FUNCTION ElaborationDisplayInfo::getGroupNamesByIndex, groupIndex, CODES=CODES

  case groupIndex of
    1:return, self->getGroupByTimeNames(CODES=CODES)
    2:return, self->getGroupByStatNames(CODES=CODES)
    else:message, 'Group not mapped on DB!!!'
  endcase

  return, -1

END
;****************************************************************************************
; index/code of elaboration plot user selection
;PRO ElaborationDisplayInfo::setAxisSelection, index
;
;  self.axisSelection=index
;
;END
;
;FUNCTION ElaborationDisplayInfo::getAxisSelection
;
;  return, self.axisSelection
;
;END
; index/code of elaboration type user selection
PRO ElaborationDisplayInfo::setDiagramSelection, index

  self.diagramSelection=index

END

FUNCTION ElaborationDisplayInfo::getDiagramSelection

  return, self.diagramSelection

END

; index/code of elaboration user selection

PRO ElaborationDisplayInfo::setElabSelection, index

  self.elabSelection=index

END

FUNCTION ElaborationDisplayInfo::getElabSelection

  return, self.elabSelection

END

PRO ElaborationDisplayInfo::setElabNumberRefValues, list


  ptr_free, self.elabNumberRefValues
  self.elabNumberRefValues=ptr_new(list, /NO_COPY)
;if value gt 0 then self->setThresholdFlag, 1 else self->setThresholdFlag, 0

END

FUNCTION ElaborationDisplayInfo::getElabNumberRefValues

  if ptr_valid(self.elabNumberRefValues) then return, *self.elabNumberRefValues else return, -1

END

; boolean true if threshold needed
PRO ElaborationDisplayInfo::setThresholdFlag, value

  self.thresholdFlag=value

END

FUNCTION ElaborationDisplayInfo::getThresholdFlag

  return, self.thresholdFlag

END

; boolean true if threshold needed
; boolean true if threshold needed
PRO ElaborationDisplayInfo::setGoalsCriteriaOCFlag, value

  self.goalsCriteriaOCFlag=value

END

FUNCTION ElaborationDisplayInfo::getGoalsCriteriaOCFlag

  return, self.goalsCriteriaOCFlag

END
; strarr(2) for group by titles time/stat selection (exclusive buttons)

PRO ElaborationDisplayInfo::setGroupByTitles, list

  self.groupByTitles=list

END

FUNCTION ElaborationDisplayInfo::getGroupByTitles

  return, self.groupByTitles

END

FUNCTION ElaborationDisplayInfo::getGroupByNumbers

  return, n_elements(self.groupByTitles)

END

; Index/code of group by time selection (exclusive buttons)

PRO ElaborationDisplayInfo::setGroupByTimeSelection, index, NONE=NONE

  if keyword_set(NONE) then self.groupByTimeSelection=-1 else self.groupByTimeSelection=index

END

FUNCTION ElaborationDisplayInfo::getGroupByTimeSelection

  return, self.groupByTimeSelection

END

; Index/code of group by stat selection (exclusive buttons)

PRO ElaborationDisplayInfo::setGroupByStatSelection, index, NONE=NONE

  if keyword_set(NONE) then self.groupByStatSelection=-1 else self.groupByStatSelection=index

END

FUNCTION ElaborationDisplayInfo::getGroupByStatSelection

  return, self.groupByStatSelection

END

; name to be linked to each exclusive button (group by section)
PRO ElaborationDisplayInfo::setGroupByStatNames, list

  ptr_free, self.groupByStatNames
  self.groupByStatNames=ptr_new(list, /NO_COPY)

END

PRO ElaborationDisplayInfo::setGroupByStatCodes, list

  ptr_free, self.groupByStatCodes
  self.groupByStatCodes=ptr_new(list, /NO_COPY)

END

FUNCTION ElaborationDisplayInfo::getGroupByStatNames, CODES=CODES

  if ptr_valid(self.groupByStatNames) then begin
    CODES=*self.groupByStatCodes
    return, *self.groupByStatNames
  endif
  return, -1

END

; name to be linked to each exclusive button (group by time)

PRO ElaborationDisplayInfo::setGroupByTimeNames, list

  ptr_free, self.groupByTimeNames
  self.groupByTimeNames=ptr_new(list, /NO_COPY)

END

PRO ElaborationDisplayInfo::setGroupByTimeCodes, list

  ptr_free, self.groupByTimeCodes
  self.groupByTimeCodes=ptr_new(list, /NO_COPY)

END

FUNCTION ElaborationDisplayInfo::getGroupByTimeNames, CODES=CODES

  if ptr_valid(self.groupByTimeNames) then begin
    CODES=*self.groupByTimeCodes
    return, *self.groupByTimeNames
  endif
  return, -1

END

; list of names for elaboration type list

PRO ElaborationDisplayInfo::setDiagramNames, list

  ptr_free, self.diagramNames
  self.diagramNames=ptr_new(list, /NO_COPY)

END

FUNCTION ElaborationDisplayInfo::getDiagramNames

  if ptr_valid(self.diagramNames) then return, *self.diagramNames
  return, -1

END

; list of linking codes for elaboration type list

PRO ElaborationDisplayInfo::setDiagramCodes, list

  ptr_free, self.diagramCodes
  self.diagramCodes=ptr_new(list, /NO_COPY)

END

FUNCTION ElaborationDisplayInfo::getDiagramCodes

  if ptr_valid(self.diagramCodes) then return, *self.diagramCodes
  return, -1

END

; list of descriptions for elaboration type list

PRO ElaborationDisplayInfo::setDiagramDescriptions, list

  ptr_free, self.diagramDescriptions
  self.diagramDescriptions=ptr_new(list, /NO_COPY)

END

FUNCTION ElaborationDisplayInfo::getDiagramDescriptions

  if ptr_valid(self.diagramDescriptions) then return, *self.diagramDescriptions
  return, -1

END

;ptr_free, self.elabSelection
; list of need ref values for elaboration

PRO ElaborationDisplayInfo::setElabGCOC, list

  ptr_free, self.elabGCOC
  self.elabGCOC=ptr_new(list, /NO_COPY)

END

FUNCTION ElaborationDisplayInfo::getElabGCOC

  if ptr_valid(self.elabGCOC) then return, *self.elabGCOC
  return, -1

END
; list of need ref values for elaboration

PRO ElaborationDisplayInfo::setElabRefValues, list

  ptr_free, self.elabRefValues
  self.elabRefValues=ptr_new(list, /NO_COPY)

END

FUNCTION ElaborationDisplayInfo::getElabRefValues

  if ptr_valid(self.elabRefValues) then return, *self.elabRefValues
  return, -1

END
; list of user max parameters number for elaboration

; list of user selection parameters for elaboration
PRO ElaborationDisplayInfo::setElabUserParameterSelection, list

  ptr_free, self.elabUserParameterSelection
  self.elabUserParameterSelection=ptr_new(list, /NO_COPY)

END

FUNCTION ElaborationDisplayInfo::getElabUserParameterSelection

  if ptr_valid(self.elabUserParameterSelection) then return, *self.elabUserParameterSelection
  return, -1

END
; list of parameters for elaboration

PRO ElaborationDisplayInfo::setElabParameters, list

  ptr_free, self.elabParameters
  self.elabParameters=ptr_new(list, /NO_COPY)

END

FUNCTION ElaborationDisplayInfo::getElabParameters

  if ptr_valid(self.elabParameters) then return, *self.elabParameters
  return, -1

END
; list of type for elaboration

PRO ElaborationDisplayInfo::setElabDiagramCodes, list

  ptr_free, self.elabDiagramCodes
  self.elabDiagramCodes=ptr_new(list, /NO_COPY)

END

; list of names for elaboration

PRO ElaborationDisplayInfo::setElabNames, list

  ptr_free, self.elabNames
  self.elabNames=ptr_new(list, /NO_COPY)

END

FUNCTION ElaborationDisplayInfo::getElabNames

  if ptr_valid(self.elabNames) then return, *self.elabNames
  return, -1

END

; list of codes for elaboration

PRO ElaborationDisplayInfo::setElabCodes, list

  ptr_free, self.elabCodes
  self.elabCodes=ptr_new(list, /NO_COPY)

END

FUNCTION ElaborationDisplayInfo::getElabCodes

  if ptr_valid(self.elabCodes) then return, *self.elabCodes
  return, -1

END

; list of descriptions for elaboration list

PRO ElaborationDisplayInfo::setElabDescriptions, list

  ptr_free, self.elabDescriptions
  self.elabDescriptions=ptr_new(list, /NO_COPY)

END

FUNCTION ElaborationDisplayInfo::getElabDescriptions

  if ptr_valid(self.elabDescriptions) then return, *self.elabDescriptions
  return, -1

END

;;
; list of names for plot

;PRO ElaborationDisplayInfo::setAxisNames, list
;
;  ptr_free, self.axisNames
;  self.axisNames=ptr_new(list, /NO_COPY)
;
;END
;
;FUNCTION ElaborationDisplayInfo::getAxisNames
;
;  if ptr_valid(self.axisNames) then return, *self.axisNames
;  return, '-1'
;
;END

; list of codes for plot
;PRO ElaborationDisplayInfo::setAxisCodes, list
;
;  ptr_free, self.axisCodes
;  self.axisCodes=ptr_new(list, /NO_COPY)
;
;END
;
;FUNCTION ElaborationDisplayInfo::getAxisCodes
;
;  if ptr_valid(self.axisCodes) then return, *self.axisCodes
;  return, -1
;
;END

; list of descriptions for plot list

;PRO ElaborationDisplayInfo::setAxisDescriptions, list
;
;  ptr_free, self.axisDescriptions
;  self.axisDescriptions=ptr_new(list, /NO_COPY)
;
;END
;
;FUNCTION ElaborationDisplayInfo::getAxisDescriptions
;
;  if ptr_valid(self.axisDescriptions) then return, *self.axisDescriptions
;  return, -1
;
;END

;;


; user data for thresholds

PRO ElaborationDisplayInfo::setThresholdValues, list

  ptr_free, self.thresholdValues
  self.thresholdValues=ptr_new(list, /NO_COPY)

END

FUNCTION ElaborationDisplayInfo::getThresholdValues

  if ptr_valid(self.thresholdValues) then return, *self.thresholdValues
  return, -1

END

FUNCTION ElaborationDisplayInfo::getFileFormatVersion

  return, '2.0'

END

PRO ElaborationDisplayInfo::saveData, filename

  ; open file for write
  openw, unit, filename, /GET_LUN
  header1='#ElaborationDisplayInfo - Save File - Never change order of contents'
  printf, unit, header1

  record=self.utility->buildFileDataStream('Version', self->getFileFormatVersion())
  printf, unit, record
  ; Elaboration section
  ;  data=self->getElabNames()
  ;  record=self.utility->buildFileDataStream('ElabNames', data)
  ;printf, unit, record

  ;  data=self->getElabCodes()
  ;  record=self.utility->buildFileDataStream('ElabCodes', data)
  ;printf, unit, record

  ;  data=self->getElabDescriptions()
  ;  record=self.utility->buildFileDataStream('ElabDescriptions', data)
  ;printf, unit, record

  data=strcompress(self->getElabSelection(), /REMOVE)
  record=self.utility->buildFileDataStream('ElabSelection', data)
  printf, unit, record

  ;  data=self->getElabDiagramCodes()
  ;  record=self.utility->buildFileDataStream('ElabDiagramCodes', data)
  ;printf, unit, record

  ;  data=self->getElabGroupByTimeCodes()
  ;  record=self.utility->buildFileDataStream('ElabGroupByTimeCodes', data)
  ;printf, unit, record

  ;  data=self->getElabGroupByStatCodes()
  ;  record=self.utility->buildFileDataStream('ElabGroupByStatCodes', data)
  ;printf, unit, record

  ;  data=self->getElabDayPeriodCodes()
  ;  record=self.utility->buildFileDataStream('ElabDayPeriodCodes', data)
  ;printf, unit, record

  ;  data=self->getElabSeasonCodes()
  ;record=self.utility->buildFileDataStream('getElabSeasonCodes', data)
  ;printf, unit, record

  ;  data=strcompress(self->getElabNumberRefValues(), /REMOVE)
  ;  record=self.utility->buildFileDataStream('ElabNumberRefValues', data)
  ;  printf, unit, record
  ; Run section

  data=strcompress(fix(self->getThresholdFlag()), /REMOVE)
  record=self.utility->buildFileDataStream('ThresholdFlag', data)
  printf, unit, record

  ;  data=self->getElabMultipleChoiceFlags()
  ;  record=self.utility->buildFileDataStream('ElabMultipleChoiceFlags', data)
  ;  printf, unit, record

  ;  data=self->getElabGCOC()
  ;  record=self.utility->buildFileDataStream('ElabGCOC', data)
  ;  printf, unit, record

  ;  data=strcompress(fix(self->getGoalsCriteriaOCFlag()), /REMOVE)
  ;  record=self.utility->buildFileDataStream('GoalsCriteriaOCFlag', data)
  ;  printf, unit, record

  data=strcompress(self->getDiagramSelection(), /REMOVE)
  record=self.utility->buildFileDataStream('DiagramSelection', data)
  printf, unit, record

  ;  data=self->getDiagramNames()
  ;  record=self.utility->buildFileDataStream('DiagramNames', data)
  ;printf, unit, record

  ;  data=self->getDiagramCodes()
  ;  record=self.utility->buildFileDataStream('DiagramCodes', data)
  ;printf, unit, record
  ; Scenario section

  data=strcompress(self->getThresholdValues(), /REMOVE)
  record=self.utility->buildFileDataStream('ThresholdValues', data)
  printf, unit, record

  ;  data=self->getdiagramDescriptions()
  ;  record=self.utility->buildFileDataStream('diagramDescriptions', data)
  ;  printf, unit, record

  ;  data=self->getperiodTitles()
  ;  record=self.utility->buildFileDataStream('periodTitles', data)
  ;printf, unit, record

  ;  data=strcompress(self->getdayPeriodNames(), /REMOVE)
  ;  record=self.utility->buildFileDataStream('dayPeriodNames', data)
  ;  printf, unit, record
  ; Model section

  ;  data=self->getDiagramMaxMultipleChoice()
  ;  record=self.utility->buildFileDataStream('DiagramMaxMultipleChoice', data)
  ;printf, unit, record

  ;  data=self->getSeasonNames()
  ;  record=self.utility->buildFileDataStream('SeasonNames', data)
  ;printf, unit, record

  ;  data=self->getDayPeriodCodes()
  ;  record=self.utility->buildFileDataStream('DayPeriodCodes', data)
  ;printf, unit, record

  ;  data=strcompress(self->getSeasonCodes(), /REMOVE)
  ;  record=self.utility->buildFileDataStream('SeasonCodes', data)
  ;  printf, unit, record

  ; Category section
  data=strcompress(self->getDayPeriodSelection(), /REMOVE)
  record=self.utility->buildFileDataStream('DayPeriodSelection', data)
  printf, unit, record

  data=strcompress(self->getSeasonSelection(), /REMOVE)
  record=self.utility->buildFileDataStream('SeasonSelection', data)
  printf, unit, record

  ;  data=self->getGroupByTitles()
  ;  record=self.utility->buildFileDataStream('GroupByTitles', data)
  ;printf, unit, record

  ;data=self->getGroupByTimeNames()
  ;record=self.utility->buildFileDataStream('GroupByTimeNames', data)
  ;printf, unit, record

  ;data=self->getGroupByTimeCodes()
  ;record=self.utility->buildFileDataStream('GroupByTimeCodes', data)
  ;printf, unit, record

  data=strcompress(self->getGroupByTimeSelection(), /REMOVE)
  record=self.utility->buildFileDataStream('GroupByTimeSelection', data)
  printf, unit, record

  ; Obs section
  ;  data=self->getGroupByStatNames()
  ;  record=self.utility->buildFileDataStream('GroupByStatNames', data)
  ;printf, unit, record

  ;  data=self->getGroupByStatCodes()
  ;  record=self.utility->buildFileDataStream('GroupByStatCodes', data)
  ;printf, unit, record

  data=strcompress(self->getGroupByStatSelection(), /REMOVE)
  record=self.utility->buildFileDataStream('GroupByStatSelection', data)
  printf, unit, record

  data=self->getStartDateInfo()
  dateTime=strcompress([string(data.second),string(data.minute),string(data.hour),string(data.day),string(data.month),string(data.year)], /REMOVE)
  record=self.utility->buildFileDataStream('StartDateInfo(SS*MI*HH*DD*MM*YYYY)', dateTime)
  printf, unit, record

  data=self->getEndDateInfo()
  dateTime=strcompress([string(data.second),string(data.minute),string(data.hour),string(data.day),string(data.month),string(data.year)], /REMOVE)
  record=self.utility->buildFileDataStream('EndDateInfo(SS*MI*HH*DD*MM*YYYY)', dateTime)
  printf, unit, record

  close, unit
  free_lun, unit

END

FUNCTION ElaborationDisplayInfo::restoreData, filename

  ; open file for write
  ERROR=0
  catch, error_status

  if error_status NE 0 THEN BEGIN
    ERROR=1
    catch, /CANCEL & close, /ALL
    errMsg=dialog_message('problem with file: <'+fileName+'> check format version, existence or read permission.', /ERROR)
    return, 0
  endif

  openr, unit, fileName, /GET_LUN

  bufferString=''

  readf, unit, bufferString

  readf, unit, bufferString
  self.utility->convertStreamDataFile, bufferString, dataName, datalist
  if dataName eq 'Version' then version=datalist[0] else message, 'File version not compatible'
  if version ne self->getFileFormatVersion() then message, 'File version not compatible'

  ; Parameter section
  ;readf, unit, bufferString
  ;  data=self->getParameterTypeNames()
  ;  record=self.utility->buildFileDataStream('ParameterTypeNames', data)

  ;readf, unit, bufferString
  ;  data=self->getParameterTypeCodes()
  ;  record=self.utility->buildFileDataStream('getParameterTypeCodes', data)

  ;readf, unit, bufferString
  ;  data=self->getParameterTypeDescriptions()
  ;  record=self.utility->buildFileDataStream('getParameterTypeDescriptions', data)

  readf, unit, bufferString
  ;record=self.utility->buildFileDataStream('getParameterTypeSelections', data)
  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  self->setElabSelection, fix(dataList)

  ;readf, unit, bufferString
  ;  data=self->getParameterNames()
  ;  record=self.utility->buildFileDataStream('getParameterNames', data)

  ;  readf, unit, bufferString
  ;  data=self->getParameterCodes()
  ;  record=self.utility->buildFileDataStream('getParameterCodes', data)

  ;readf, unit, bufferString
  ;  data=self->getParameterTypes()
  ;  record=self.utility->buildFileDataStream('getParameterTypes', data)

  ;readf, unit, bufferString
  ;  data=self->getParameterMeasureUnits()
  ;  record=self.utility->buildFileDataStream('getParameterMeasureUnits', data)

  ;readf, unit, bufferString
  ;  data=self->getParameterDescriptions()
  ;  record=self.utility->buildFileDataStream('getParameterDescriptions', data)

  ;readf, unit, bufferString
  ;data=self->getParameterObservedCodes()
  ;record=self.utility->buildFileDataStream('getParameterObservedCodes', data)

  readf, unit, bufferString
  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  tFlag=fix(dataList)
  ;self->setThresholdFlag, fix(dataList)

  ; Run section

  ;readf, unit, bufferString
  ;  data=self->getrunNames()
  ;  record=self.utility->buildFileDataStream('getrunNames', data)

  ;readf, unit, bufferString
  ;  data=self->getrunCodes()
  ;  record=self.utility->buildFileDataStream('getrunCodes', data)

  ;readf, unit, bufferString
  ;  data=self->getrunDescriptions()
  ;  record=self.utility->buildFileDataStream('getrunDescriptions', data)

  readf, unit, bufferString
  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  self->setDiagramSelection, fix(dataList)

  readf, unit, bufferString
  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  ptr_free, self.thresholdValues
  ;if dataList[0] ne '-1' then self->setThresholdValues, float(dataList)
  if tFlag eq 1 then self->userSetReferenceValues, float(dataList) else self->userSetReferenceValues, /CANCEL
  ;readf, unit, bufferString
  ;  data=self->getrunQueryCodes()
  ;  record=self.utility->buildFileDataStream('getrunQueryCodes', data)

  ;readf, unit, bufferString
  ;  data=self->getrunScenarioCodes()
  ;  record=self.utility->buildFileDataStream('getrunScenarioCodes', data)

  ;readf, unit, bufferString
  ;  data=self->getrunModelCodes()
  ;  record=self.utility->buildFileDataStream('getrunModelCodes', data)
  ; Scenario section

  ;readf, unit, bufferString
  ;  data=self->getscenarioNames()
  ;  record=self.utility->buildFileDataStream('getscenarioNames', data)

  ;readf, unit, bufferString
  ;  data=self->getscenarioCodes()
  ;  record=self.utility->buildFileDataStream('getscenarioCodes', data)

  ;readf, unit, bufferString
  ;  data=self->getscenarioDescriptions()
  ;  record=self.utility->buildFileDataStream('getscenarioDescriptions', data)

  readf, unit, bufferString
  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  self->setDayPeriodSelection, fix(dataList)
  ; Model section

  ;readf, unit, bufferString
  ;  data=self->getmodelNames()
  ;  record=self.utility->buildFileDataStream('getmodelNames', data)

  ;readf, unit, bufferString
  ;  data=self->getmodelCodes()
  ;  record=self.utility->buildFileDataStream('getmodelCodes', data)

  ;readf, unit, bufferString
  ;  data=self->getmodelDescriptions()
  ;  record=self.utility->buildFileDataStream('getmodelDescriptions', data)

  readf, unit, bufferString
  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  self->setSeasonSelection, fix(dataList)
  ;record=self.utility->buildFileDataStream('getmodelSelections', data)

  ; Category section
  ;readf, unit, bufferString
  ;data=self->getcategoryPresenceFlags()
  ;record=self.utility->buildFileDataStream('getcategoryPresenceFlags', data)

  ;readf, unit, bufferString
  ;  data=self->getcategoryCodes()
  ;  record=self.utility->buildFileDataStream('getcategoryCodes', data)

  ;readf, unit, bufferString
  ;  data=self->getcategoryTitles()
  ;  record=self.utility->buildFileDataStream('getcategoryTitles', data)

  ;readf, unit, bufferString
  ;data=self->getcategoryValues()
  ;record=self.utility->buildFileDataStream('getcategoryValues', data)

  ;readf, unit, bufferString
  ;data=self->getcategoryWidgetTypes()
  ;record=self.utility->buildFileDataStream('getcategoryWidgetTypes', data)

  readf, unit, bufferString
  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  self->setGroupByTimeSelection, fix(dataList)
  ;record=self.utility->buildFileDataStream('getcategorySelections', data)

  ; Obs section
  ;readf, unit, bufferString
  ;  data=self->getobsCatCategoryCodes()
  ;  record=self.utility->buildFileDataStream('getobsCatCategoryCodes', data)

  ;readf, unit, bufferString
  ;  data=self->getobsCatObservedCodes()
  ;  record=self.utility->buildFileDataStream('getobsCatObservedCodes', data)

  ;readf, unit, bufferString
  ;  data=self->getobsCatValues()
  ;  record=self.utility->buildFileDataStream('getobsCatValues', data)

  ;readf, unit, bufferString
  ;  data=self->getobservedNames()
  ;  record=self.utility->buildFileDataStream('getobservedNames', data)

  ;readf, unit, bufferString
  ;  data=self->getobservedShortNames()
  ;  record=self.utility->buildFileDataStream('getobservedShortNames', data)

  ;readf, unit, bufferString
  ;  data=self->getobservedCodes()
  ;  record=self.utility->buildFileDataStream('getobservedCodes', data)

  ;  readf, unit, bufferString
  ;  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  ;  ptr_free, self.observedQueryCodesSelections
  ;  if dataList[0] ne '-1' then self->setObservedQueryCodesSelections, dataList
  ;  ;record=self.utility->buildFileDataStream('getobservedQueryCodesSelections', data)
  ;
  ;  readf, unit, bufferString
  ;  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  ;  ptr_free, self.observedCodesSelections
  ;  if dataList[0] ne '-1' then self->setObservedCodesSelections, dataList
  ;  ;record=self.utility->buildFileDataStream('getobservedCodesSelections', data)
  ;
  ;  readf, unit, bufferString
  ;  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  ;  ptr_free, self.observedGroupNames
  ;  if dataList[0] ne '-1' then self->setObservedGroupNames, dataList
  ;record=self.utility->buildFileDataStream('getobservedGroupNames', data)

  ;  readf, unit, bufferString
  ;  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  ;  ptr_free, self.observedCodesGroupSelections
  ;  if dataList[0] ne '-1' then self->setObservedCodesGroupSelections, dataList
  ;  ;record=self.utility->buildFileDataStream('getobservedCodesGroupSelections', data)

  readf, unit, bufferString
  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  self->setGroupByStatSelection, fix(dataList)
  ;record=self.utility->buildFileDataStream('getuseObservedModelFlag', data)

  readf, unit, bufferString
  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  dateTime=getDateTimeStruct()
  dateTime.second=fix(dataList[0]) & dateTime.minute=fix(dataList[1]) & dateTime.hour=fix(dataList[2])
  dateTime.day=fix(dataList[3]) & dateTime.month=fix(dataList[4]) & dateTime.year=fix(dataList[5])
  self->setStartDateInfo, dateTime

  readf, unit, bufferString
  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  dateTime=getDateTimeStruct()
  dateTime.second=fix(dataList[0]) & dateTime.minute=fix(dataList[1]) & dateTime.hour=fix(dataList[2])
  dateTime.day=fix(dataList[3]) & dateTime.month=fix(dataList[4]) & dateTime.year=fix(dataList[5])
  self->setEndDateInfo, dateTime

  close, unit
  free_lun, unit
  return, 1

END

FUNCTION ElaborationDisplayInfo::clone, DEEP=DEEP

  clone=obj_new('ElaborationDisplayInfo')
  if keyword_set(DEEP) then begin
    if ptr_valid(self.elabDescriptions) then begin
      list=*self.elabDescriptions
      clone.elabDescriptions=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.elabCodes) then begin
      list=*self.elabCodes
      clone.elabCodes=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.elabNames) then begin
      list=*self.elabNames
      clone.elabNames=ptr_new(list, /NO_COPY)
    endif

    if ptr_valid(self.elabDiagramCodes) then begin
      list=*self.elabDiagramCodes
      clone.elabDiagramCodes=ptr_new(list, /NO_COPY)
    endif
    ;    if ptr_valid(self.elabParameters) then begin
    ;      list=*self.elabParameters
    ;      clone.elabParameters=ptr_new(list, /NO_COPY)
    ;    endif
    if ptr_valid(self.elabGCOC) then begin
      list=*self.elabGCOC
      clone.elabGCOC=ptr_new(list, /NO_COPY)
    endif
    ;    if ptr_valid(self.axisNames) then begin
    ;      list=*self.axisNames
    ;      clone.axisNames=ptr_new(list, /NO_COPY)
    ;    endif
    ;    if ptr_valid(self.axisCodes) then begin
    ;      list=*self.axisCodes
    ;      clone.axisCodes=ptr_new(list, /NO_COPY)
    ;    endif
    ;    if ptr_valid(self.axisDescriptions) then begin
    ;      list=*self.axisDescriptions
    ;      clone.axisDescriptions=ptr_new(list, /NO_COPY)
    ;    endif
    ;    if ptr_valid(self.diagramAxisCodes) then begin
    ;      list=*self.diagramAxisCodes
    ;      howMany=n_elements(list)
    ;      listOfList=ptrarr(howMany)
    ;      for i=0, howMany-1 do begin
    ;        llist=*list[i]
    ;        listOfList[i]=ptr_new(llist, /NO_COPY)
    ;      endfor
    ;      clone.diagramAxisCodes=ptr_new(listOfList, /NO_COPY)
    ;    endif
    if ptr_valid(self.elabGroupByTimeCodes) then begin
      list=*self.elabGroupByTimeCodes
      clone.elabGroupByTimeCodes=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.elabGroupByStatCodes) then begin
      list=*self.elabGroupByStatCodes
      clone.elabGroupByStatCodes=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.elabDayPeriodCodes) then begin
      list=*self.elabDayPeriodCodes
      clone.elabDayPeriodCodes=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.elabSeasonCodes) then begin
      list=*self.elabSeasonCodes
      clone.elabSeasonCodes=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.elabDiagramCodes) then begin
      list=*self.elabDiagramCodes
      clone.elabDiagramCodes=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.elabMultipleChoiceFlags) then begin
      list=*self.elabMultipleChoiceFlags
      clone.elabMultipleChoiceFlags=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.diagramCodes) then begin
      list=*self.diagramCodes
      clone.diagramCodes=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.diagramNames) then begin
      list=*self.diagramNames
      clone.diagramNames=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.diagramDescriptions) then begin
      list=*self.diagramDescriptions
      clone.diagramDescriptions=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.diagramMaxMultipleChoice) then begin
      list=*self.diagramMaxMultipleChoice
      clone.diagramMaxMultipleChoice=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.thresholdValues) then begin
      list=*self.thresholdValues
      clone.thresholdValues=ptr_new(list, /NO_COPY)
    endif
    ;    if ptr_valid(self.parameterNames) then begin
    ;      list=*self.parameterNames
    ;      clone.parameterNames=ptr_new(list, /NO_COPY)
    ;    endif
    ;    if ptr_valid(self.parametersSelection) then begin
    ;      list=*self.parametersSelection
    ;      clone.parametersSelection=ptr_new(list, /NO_COPY)
    ;    endif
    ;    if ptr_valid(self.parameterDescriptions) then begin
    ;      list=*self.parameterDescriptions
    ;      clone.parameterDescriptions=ptr_new(list, /NO_COPY)
    ;    endif
    ;    if ptr_valid(self.parameterMeasureUnits) then begin
    ;      list=*self.parameterMeasureUnits
    ;      clone.parameterMeasureUnits=ptr_new(list, /NO_COPY)
    ;    endif
    ;    if ptr_valid(self.parameterCodes) then begin
    ;      list=*self.parameterCodes
    ;      clone.parameterCodes=ptr_new(list, /NO_COPY)
    ;    endif
    if ptr_valid(self.groupByStatNames) then begin
      list=*self.groupByStatNames
      clone.groupByStatNames=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.groupByTimeNames) then begin
      list=*self.groupByTimeNames
      clone.groupByTimeNames=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.groupByStatCodes) then begin
      list=*self.groupByStatCodes
      clone.groupByStatCodes=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.groupByTimeCodes) then begin
      list=*self.groupByTimeCodes
      clone.groupByTimeCodes=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.dayPeriodNames) then begin
      list=*self.dayPeriodNames
      clone.dayPeriodNames=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.seasonNames) then begin
      list=*self.seasonNames
      clone.seasonNames=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.dayPeriodCodes) then begin
      list=*self.dayPeriodCodes
      clone.dayPeriodCodes=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.elabNumberRefValues) then begin
      list=*self.elabNumberRefValues
      clone.elabNumberRefValues=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.seasonCodes) then begin
      list=*self.seasonCodes
      clone.seasonCodes=ptr_new(list, /NO_COPY)
    endif
  endif else begin
    clone.elabNames=self.elabNames
    clone.elabCodes=self.elabCodes
    clone.elabDescriptions=self.elabDescriptions
    clone.elabDiagramCodes=self.elabDiagramCodes
    ;clone.elabParameters=self.elabParameters
    clone.elabNumberRefValues=self.elabNumberRefValues
    clone.elabGCOC=self.elabGCOC
    clone.elabDescriptions=self.elabDescriptions
    clone.elabGroupByTimeCodes=self.elabGroupByTimeCodes
    clone.elabGroupByStatCodes=self.elabGroupByStatCodes
    clone.elabDayPeriodCodes=self.elabDayPeriodCodes
    clone.elabSeasonCodes=self.elabSeasonCodes
    clone.elabMultipleChoiceFlags=self.elabMultipleChoiceFlags
    clone.diagramNames=self.diagramNames
    clone.diagramCodes=self.diagramCodes
    clone.diagramDescriptions=self.diagramDescriptions
    clone.diagramMaxMultipleChoice=self.diagramMaxMultipleChoice
    ;    clone.diagramAxisCodes=self.diagramAxisCodes
    ;    clone.axisNames=self.axisNames
    ;    clone.axisCodes=self.axisCodes
    ;    clone.axisDescriptions=self.axisDescriptions
    ;    clone.parameterNames=self.parameterNames
    ;    clone.parameterCodes=self.parameterCodes
    ;    clone.parameterMeasureUnits=self.parameterMeasureUnits
    ;    clone.parameterDescriptions=self.parameterDescriptions
    ;    clone.parametersSelection=self.parametersSelection
    clone.groupByTimeNames=self.groupByTimeNames
    clone.groupByStatNames=self.groupByStatNames
    clone.groupByTimeCodes=self.groupByTimeCodes
    clone.groupByStatCodes=self.groupByStatCodes
    clone.thresholdValues=self.thresholdValues
    clone.dayPeriodNames=self.dayPeriodNames
    clone.seasonNames=self.seasonNames
    clone.dayPeriodCodes=self.dayPeriodCodes
    clone.seasonCodes=self.seasonCodes
  endelse
  clone.elabSelection =self.elabSelection
  ;  clone.axisSelection =self.axisSelection
  clone.diagramSelection=self.diagramSelection
  clone.periodTitles=self.periodTitles
  clone.dayPeriodSelection=self.dayPeriodSelection
  clone.seasonSelection=self.seasonSelection
  clone.groupByTitles=self.groupByTitles
  clone.groupByTimeSelection=self.groupByTimeSelection
  clone.groupByStatSelection=self.groupByStatSelection
  clone.thresholdFlag=self.thresholdFlag
  clone.goalsCriteriaOCFlag =self.goalsCriteriaOCFlag
  clone.startDateInfo=self.startDateInfo
  clone.endDateInfo=self.endDateInfo
  return, clone

END

FUNCTION ElaborationDisplayInfo::init

  if not self -> Object :: init() then return , 0
  self.utility=obj_new("FMUtility")
  self.dtu=obj_new('DateTimeUtility')
  self->setEndYear
  return , 1

END

PRO ElaborationDisplayInfo::cleanUp

  obj_destroy, self.utility
  obj_destroy, self.dtu

  ptr_free, self.elabNames
  ptr_free, self.elabCodes
  ptr_free, self.elabDescriptions

  ptr_free, self.elabDiagramCodes
  ptr_free, self.elabNumberRefValues
  ptr_free, self.elabGCOC
  ptr_free, self.elabGroupByTimeCodes
  ptr_free, self.elabGroupByStatCodes
  ptr_free, self.elabDayPeriodCodes
  ptr_free, self.elabSeasonCodes
  ptr_free, self.elabMultipleChoiceFlags

  ptr_free, self.diagramNames
  ptr_free, self.diagramCodes
  ptr_free, self.diagramDescriptions
  ptr_free, self.diagramMaxMultipleChoice

  ptr_free, self.groupByTimeNames
  ptr_free, self.groupByStatNames

  ptr_free, self.dayPeriodNames
  ptr_free, self.seasonNames
  ptr_free, self.dayPeriodCodes
  ptr_free, self.seasonCodes
  self -> Object::cleanUp

END

;****************************************************************************************

PRO ElaborationDisplayInfo__Define

  Struct = { ElaborationDisplayInfo , $
    elabNames: ptr_new(), $
    elabCodes: ptr_new(), $
    elabSelection: 0, $
    elabDescriptions: ptr_new(), $
    elabDiagramCodes: ptr_new(), $
    elabGroupByTimeCodes: ptr_new(), $
    elabGroupByStatCodes: ptr_new(), $
    elabDayPeriodCodes: ptr_new(), $
    elabSeasonCodes: ptr_new(), $
    elabNumberRefValues: ptr_new(), $
    elabMultipleChoiceFlags: ptr_new(), $
    elabGCOC: ptr_new(), $
    goalsCriteriaOCFlag: 0b, $
    diagramSelection: 0, $
    diagramNames: ptr_new(), $
    diagramCodes: ptr_new(), $
    diagramDescriptions: ptr_new(), $
    diagramMaxMultipleChoice: ptr_new(), $
    thresholdFlag: 0b, $
    thresholdValues: ptr_new(), $
    periodTitles: strarr(2), $
    dayPeriodNames: ptr_new(), $
    seasonNames: ptr_new(), $
    dayPeriodCodes: ptr_new(), $
    seasonCodes: ptr_new(), $
    dayPeriodSelection: 0, $
    seasonSelection: 0, $
    groupByTitles: strarr(2), $
    groupByTimeNames: ptr_new(), $
    groupByTimeCodes: ptr_new(), $
    groupByTimeSelection: 0, $
    groupByStatNames: ptr_new(), $
    groupByStatCodes: ptr_new(), $
    groupByStatSelection: 0, $
    startDateInfo: getDateTimeStruct(), $
    endDateInfo: getDateTimeStruct(), $
    utility: obj_new(), $
    dtu: obj_new('DateTimeUtility'), $
    Inherits Object $t
  }

END

;****************************************************************************************
