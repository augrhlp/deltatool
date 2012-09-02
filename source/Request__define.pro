;********************
@structure_definition
;********************
PRO Request::setPrintOrient, value

 self.printOrient=value

END

FUNCTION Request::getPrintOrient

 return, self.printOrient

END

PRO Request::setPageBreak, value

 self.pageBreak=value

END

FUNCTION Request::getPageBreak

 return, self.pageBreak

END

PRO Request::setSingleObsCatInfos, matrix

  ptr_free, self.singleObsCatInfos
  self.singleObsCatInfos=ptr_new(matrix, /NO_COPY)

END

FUNCTION Request::getSingleObsCatInfos

  if ptr_valid(self.singleObsCatInfos) then return, *self.singleObsCatInfos

END

FUNCTION Request::getGroupNames

  if ptr_valid(self.singleObsCatInfos) then return, *self.singleObsCatInfos
  return, [-1]

END

PRO  Request::setGoogleEarthLocation, location

 self.googleEarthLocation=location

END

FUNCTION  Request::getGoogleEarthLocation

 return, self.googleEarthLocation

END

PRO  Request::setPlotDeviceName, value

 self.plotDeviceName=value

END

FUNCTION  Request::getPlotDeviceName

 return, self.plotDeviceName

END

PRO Request::setLocation, location

  self.location=location

END

FUNCTION Request::getLocation

  return, self.location

END

PRO Request::testJRCMethods

  ; check for documentations
  ;  testVal=self->getAxisCode()
  ;  print, 'getAxisCode'
  ;  help, testval, /struct & print, testval
  ;  testVal=self->getAxisName()
  ;  print, 'getAxisName'
  ;  help, testval, /struct & print, testval
  ;testVal=self->getDateInterval()
  testVal=self->getDateInterval(self->getFirstStandardDate(), self->getLastStandardDate(), HOURS=HOURS)
  print, 'getDateInterval'
  help, testval, /struct & print, testval

  testVal=self->getDiagramCode()
  print, 'getDiagramCode'
  help, testval, /struct & print, testval
  testVal=self->getDiagramName()
  print, 'getDiagramName'
  help, testval, /struct & print, testval
  testVal=self->getElaborationCode()
  print, 'getElaborationCode'
  help, testval, /struct & print, testval
  testVal=self->getElaborationName()
  print, 'getElaborationName'
  help, testval, /struct & print, testval
  testVal=self->getElaborationRoutine()
  print, 'getElaborationRoutine'
  help, testval, /struct & print, testval
  testVal=self->getEndDate()
  print, 'getEndDate'
  help, testval, /struct & print, testval
  testVal=self->getEndIndex()
  print, 'getEndIndex'
  help, testval, /struct & print, testval
  testVal=self->getEndPlotIndex()
  print, 'getEndPlotIndex'
  help, testval, /struct & print, testval
  testVal=self->getExtraValues()
  print, 'getExtraValues'
  help, testval, /struct & print, testval
  testVal=self->getExtraValuesNumber()
  print, 'getExtraValuesNumber'
  help, testval, /struct & print, testval
  testVal=self->getFileName()
  print, 'getFileName'
  help, testval, /struct & print, testval
  testVal=self->getFirstStandardDate()
  print, 'getFirstStandardDate'
  help, testval, /struct & print, testval
  mParameter='PM10' & mScalename='LOCAL' & mStatName='IOA' & timeAvgName='N/A'
  testval=self->getGoalsCriteriaValues(parameter=mParameter, scalename=mScalename, statname=mStatName, timeAvgName=timeAvgName, NOVALUES=NOVALUES)
  print, 'getGoalsCriteriaValues, NOVALUES check'
  print, keyword_set(NOVALUES)
  print, 'getGoalsCriteriaValues'
  help, testval, /struct & print, testval

  testVal=self->getGroupByStatInfo()
  print, 'getGroupByStat'
  help, testval, /struct & print, testval
  ;  testVal=self->getGroupByStatInfo()
  ;  print, 'getGroupByStatInfo'
  ;  help, testval, /struct & print, testval
  testVal=self->getGroupByTimeInfo()
  print, 'getGroupByTimeInfo'
  help, testval, /struct & print, testval
  testVal=self->getGroupObsNumber()
  print, 'getGroupObsNumber'
  help, testval, /struct & print, testval
  if testVal gt 0 then testVal=self->getGroupObs()
  print, 'getGroupObs'
  if testVal gt 0 then testVal=self->getGroupShortObs()
  print, 'getGroupShortObs'
  help, testval, /struct & print, testval
  testVal=self->getHourInfo()
  print, 'getHourInfo'
  help, testval & print, testval

  testVal=self->getHourType()
  print, 'getHourType'
  help, testval & print, testval
  testVal=self->getLastStandardDate()
  print, 'getLastStandardDate'
  help, testval & print, testval
  testVal=self->getModelCodes()
  print, 'getModelCodes'
  help, testval & print, testval
  testVal=self->getModelNames()
  print, 'getModelNames'
  help, testval & print, testval
  testVal=self->getModelNumber()
  print, 'getModelNumber'
  help, testval & print, testval
  testVal=self->getParameterCodes()
  print, 'getParameterCodes'
  help, testval & print, testval
  testVal=self->getParameterMeasureUnits()
  print, 'getParameterMeasureUnits'
  help, testval & print, testval
  testVal=self->getParameterNames()
  print, 'getParameterNames'
  help, testval & print, testval
  testVal=self->getParameterNumber()
  print, 'getParameterNumber'
  help, testval & print, testval
  testVal=self->getPlotRoutine()
  print, 'getPlotRoutine'
  help, testval & print, testval
  testVal=self->getRunCodes()
  print, 'getRunCodes'
  help, testval & print, testval
  testVal=self->getRunFileNames()
  print, 'getRunFileNames'
  help, testval & print, testval
  testVal=self->getRunNames()
  print, 'getRunNames'
  help, testval & print, testval
  testVal=self->getRunNumber()
  print, 'getRunNumber'
  help, testval & print, testval
  testVal=self->getRunResultType()
  print, 'getRunResultType'
  help, testval & print, testval
  testVal=self->getScenarioCodes()
  print, 'getScenarioCodes'
  help, testval & print, testval
  testVal=self->getScenarioNames()
  print, 'getScenarioNames'
  help, testval & print, testval
  testVal=self->getScenarioNumber()
  print, 'getScenarioNumber'
  help, testval & print, testval
  testVal=self->getSeasonInfo()
  print, 'getSeasonInfo'
  help, testval & print, testval
  testVal=self->getSeasonType()
  print, 'getSeasonType'
  help, testval & print, testval
  testVal=self->getSingleObsNumber()
  print, 'getSingleObsNumber'
  help, testval & print, testval
  if testVal gt 0 then testVal=self->getSingleObsNames()
  print, 'getSingleObsNames'
  if testVal gt 0 then testVal=self->getSingleShortObsNames()
  print, 'getSingleShortObsNames'
  help, testval & print, testval
  testVal=self->getStandardInterval()
  print, 'getStandardInterval'
  help, testval & print, testval
  testVal=self->getStartDate()
  print, 'getStartDate'
  help, testval & print, testval
  testVal=self->getStartIndex()
  print, 'getStartIndex'
  help, testval & print, testval
  testVal=self->getStartPlotIndex()
  print, 'getStartPlotIndex'
  help, testval & print, testval
  testVal=self->getUseObservedModel()
  print, 'getUseObservedModel'
  help, testval & print, testval
  testVal=self->getRegionofObs()
  print, 'getRegionofObs'
  help, testval & print, testval


END

PRO Request::setGroupStatToApplyCode, code

  self.groupStatToApplyCode=code

END

FUNCTION Request::getGroupStatToApplyCode

  return, self.groupStatToApplyCode

END

PRO Request::setGroupStatToApplyName, name

  self.groupStatToApplyName=name

END

FUNCTION Request::getGroupStatToApplyName

  return, self.groupStatToApplyName

END

PRO Request::freeGroupCodes

  if ptr_valid(self.groupCodes) then begin
    grCodes=*self.groupCodes
    nElem=n_elements(grCodes)
    for i=0, n_elements(grCodes)-1 do ptr_free, grCodes[i]
  endif
  ptr_free, self.groupCodes

END

PRO Request::freeGroupNames

  if ptr_valid(self.groupNames) then begin
    grNames=*self.groupNames
    nElem=n_elements(grNames)
    for i=0, n_elements(grNames)-1 do ptr_free, grNames[i]
  endif
  ptr_free, self.groupNames

END

PRO Request::setGroupCodes, list

  self->freeGroupCodes
  self.groupCodes=ptr_new(list, /NO_COPY)

END

PRO Request::setGroupNames, list

  self->freeGroupNames
  self.groupNames=ptr_new(list, /NO_COPY)

END

FUNCTION Request::getGroupNames

  if ptr_valid(self.groupNames) then return, *self.groupNames
  return, [-1]

END

FUNCTION Request::getGroupCodes

  if ptr_valid(self.groupCodes) then return, *self.groupCodes
  return, [-1]

END

FUNCTION Request::buildAllGroupNames

  names=self->getGroupNames()
  all=['']
  for i=0, n_elements(names)-1 do begin
    list=*names[i]
    all=[all, list]
  endfor
  return, all[1:*]

END

PRO Request::setScaleInfo, value

  self.scaleInfo=value

END

FUNCTION Request::getScaleInfo

  return, self.scaleInfo

END

; End february 1st MM
PRO Request::openDataDumpFile, fileName, prefix=prefix, addSysTime=addSysTime

 util=obj_new('FMUtility')
 dumpDir=self.fileSysMgr->getDumpDir(/WITH)

 if n_elements(fileName) eq 1 then fullFileName=dumpDir+fileName else fullFileName=dumpDir+strcompress(self->getElaborationName(),/REMOVE)
 ;if keyword_set(prefix) then fullFileName=dumpDir+prefix else fullFileName=dumpDir+strcompress(self->getElaborationName(),/REMOVE)
 if keyword_set(addSysTime) then fullFileName=fullFileName+'_'+strcompress(util->getSysTime(/FILECOMPATIBILITY),/REMOVE)

 ;fullFileName=fullFileName+'.txt'
 close, /all
 openw, unit, fullFileName, /GET_LUN
 self.dataDumpFileUnit=unit

END
PRO Request::closeDataDumpFile

 close, self.dataDumpFileUnit
 free_lun, self.dataDumpFileUnit

END

PRO Request::writeDataDumpFileRecord, record

 printf, self.dataDumpFileUnit, record

END
;PRO Request::openDataDumpFile, prefix=prefix, addSysTime=addSysTime
;
;  dumpDir=self.fileSysMgr->getDumpDir(/WITH)
;
;  if keyword_set(prefix) then fullFileName=dumpDir+prefix else fullFileName=dumpDir+strcompress(self->getElaborationName(),/REMOVE)
;  if keyword_set(addSysTime) then fullFileName=fullFileName+'_'+strcompress(self.utility->getSysTime(/FILECOMPATIBILITY),/REMOVE)
;
;  fullFileName=fullFileName+'.txt'
;  openw, unit, fullFileName, /GET_LUN
;  self.dataDumpFileUnit=unit
;
;END

;PRO Request::closeDataDumpFile
;
;  close, self.dataDumpFileUnit
;  free_lun, self.dataDumpFileUnit
;
;END
;
;PRO Request::writeDataDumpFileRecord, record
;
;  printf, self.dataDumpFileUnit, record
;
;END

FUNCTION Request::getRegionofObs, obsCode

  return, self.obsCatMgr->getRegionOfObs(obsCode)

END

FUNCTION Request::getEntityFileName

  return, self.entityFileName

END

PRO Request::setEntityFileName, fileName

  self.entityFileName=fileName

END

FUNCTION Request::getElaborationFileName

  return, self.elaborationFileName

END

PRO Request::setElaborationFileName, fileName

  self.elaborationFileName=fileName

END

FUNCTION Request::getMultipleChoiceUserSelectionFlags

  return, self.multipleChoiceUserSelectionFlags

END

PRO Request::setMultipleChoiceUserSelectionFlags, fourFlags

  self.multipleChoiceUserSelectionFlags=fourFlags

END

FUNCTION Request::getGroupTitlesNumber

  if not(ptr_valid(self.groupTitles)) then return, 0
  sList=self->getGroupTitles()
  if sList[0] ne strtrim(-1) then return, n_elements(sList) else return, 0

END

FUNCTION Request::isSingleObsPresent

  if ptr_valid(self.singleObsCodes) then return, 1 else return, 0

END

FUNCTION Request::isGroupObsPresent

  if self->getGroupTitlesNumber() ne 0 then return, 1 else return, 0

END

FUNCTION Request::getGroupTitlesNumber

  if ptr_valid(self.groupTitles) then return, n_elements(self.groupTitles) else return, 0

END

FUNCTION Request::getSingleObsNumber

  if not(ptr_valid(self.singleObsNames)) then return, 0
  sList=self->getSingleObsNames()
  if sList[0] ne '-1' then return, n_elements(sList) else return, 0

END

FUNCTION Request::getScenarioNumber

  if not(ptr_valid(self.scenarioCodes)) then return, 0
  sList=self->getScenarioCodes()
  if sList[0] ne '-1' then return, n_elements(sList) else return, 0

END

FUNCTION Request::getExtraValuesNumber

  if not(ptr_valid(self.extraValues)) then return, 0
  sList=self->getExtraValues()
  if sList[0] ne -1 then return, n_elements(sList) else return, 0

END

FUNCTION Request::getModelNumber

  if not(ptr_valid(self.modelCodes)) then return, 0
  sList=self->getModelCodes()
  if sList[0] ne '-1' then return, n_elements(sList) else return, 0

END

FUNCTION Request::getRunNumber

  if not(ptr_valid(self.runCodes)) then return, 0
  sList=self->getRunCodes()
  if sList[0] ne -1 then return, n_elements(sList) else return, 0

END

FUNCTION Request::getGoalsCriteriaValues, parameter=parameter, scalename=scalename, statname=statname, timeAvgName=timeAvgName, NOVALUES=NOVALUES

  return, self.gcMgr->getValuesByKey(parameterCode=parameter, scaleName=scalename, statNickName=statname, timeAvgName=timeAvgName, NOVALUES=NOVALUES)

END

FUNCTION Request::getParameterNumber

  if not(ptr_valid(self.parameterCodes)) then return, 0
  sList=self->getParameterCodes()
  if sList[0] ne '-1' then return, n_elements(sList) else return, 0

END

FUNCTION Request::getFirstStandardDate

  firstDate=getDateTimeStruct()
  firstDate.second=0
  firstDate.minute=0
  firstDate.hour=0
  firstDate.day=1
  firstDate.month=1
  firstDate.year=2001

  return, firstDate

END

FUNCTION Request::getLastStandardDate

  lastDate=getDateTimeStruct()
  lastDate.second=59
  lastDate.minute=59
  lastDate.hour=23
  lastDate.day=31
  lastDate.month=12
  lastDate.year=2001

  return, lastDate

END

FUNCTION Request::getDateInterval, firstDate, lastDate, HOURS=HOURS

  sDate=firstDate
  eDate=lastDate
  sDays=self.dtu->calcDayOfYear(sDate, outHour=sOutHour)
  eDays=self.dtu->calcDayOfYear(eDate, outHour=eOutHour)
  eHours=(eDays+1)*24+eOutHour
  sHours=sDays*24+sOutHour
  return, eHours-sHours

END

FUNCTION Request::getStandardInterval, HOURS=HOURS

  return, self->getDateInterval(self->getFirstStandardDate(), self->getLastStandardDate(), HOURS=HOURS)

END

FUNCTION Request::getRunNames

  if ptr_valid(self.runNames) then return, *self.runNames else return, ['']

END

PRO Request::setRunNames, names

  ptr_free, self.runNames
  self.runNames=ptr_new(names, /NO_COPY)

END

FUNCTION Request::getStartPlotIndex

  return, self.startPlotIndex

END

PRO Request::setStartPlotIndex, value

  self.startPlotIndex=value

END

FUNCTION Request::getEndPlotIndex

  return, self.endPlotIndex

END

PRO Request::setEndPlotIndex, value

  self.endPlotIndex=value

END

FUNCTION Request::getStartIndex

  return, self.startIndex

END

PRO Request::setStartIndex, value

  self.startIndex=value

END

FUNCTION Request::getEndIndex

  return, self.endIndex

END

PRO Request::setEndIndex, value

  self.endIndex=value

END
; dateInterval in hours
FUNCTION Request::dateInterval, HOURS=HOURS

  sDate=self->getStartDate()
  eDate=self->getEndDate()
  sDays=self.dtu->calcDayOfYear(sDate, outHour=sOutHour)
  eDays=self.dtu->calcDayOfYear(eDate, outHour=eOutHour)
  eHours=(eDays+1)*24+eOutHour
  sHours=sDays*24+sOutHour
  obj_destroy, dtu
  return, eHours-sHours

END

FUNCTION Request::getSplitDateTimeInfo, dateInfo

  resDates=dateInfo[0]
  for i=0, n_elements(self.seasonInfo)-1 do begin
    info=strsplit(dateInfo[i].value, '*', /EXTRACT)
    infoNumber=n_elements(info)
    appendDates=dateInfo[i]
    if infoNumber gt 1 then begin
      appendDates=replicate(getTimeStampStruct(), infoNumber)
      for j=0, infoNumber-1 do begin
        appendDates[j].value=info[j]
        appendDates[j].template=dateInfo[0].template
      endfor
    endif
    resDates=[resDates, appendDates]
  endfor
  return, resDates[1:*]

END

FUNCTION Request::getSplitSeasonInfo

  return, self->getSplitDateTimeInfo(self.seasonInfo)

END

FUNCTION Request::getSplitHourInfo

  return, self->getSplitDateTimeInfo(self.hourInfo)

END
;*****************************
; get/set
;*****************************
;PRO Request::setAxisCode, value
;
;  self.axisCode=value
;
;END
;
;FUNCTION Request::getAxisCode
;
;  return, self.axisCode
;
;END
;
;PRO Request::setAxisName, value
;
;  self.axisName=value
;
;END
;
;FUNCTION Request::getAxisName
;
;  return, self.axisName
;
;END

PRO Request::setPlotRoutine, value

  self.plotRoutine=value

END

FUNCTION Request::getPlotRoutine

  return, self.plotRoutine

END

PRO Request::setGroupByTimeInfo, value

  self.groupByTimeInfo=value

END

FUNCTION Request::getGroupByTimeInfo

  return, self.groupByTimeInfo

END

PRO Request::setGroupByStatInfo, list

  ptr_free, self.groupByStatInfo
  self.groupByStatInfo=ptr_new(list, /NO_COPY)

END

FUNCTION Request::getGroupByStatInfo

  if ptr_valid(self.groupByStatInfo) then return, *self.groupByStatInfo else return, [-1]

END

PRO Request::setSeasonInfo, value

  self.seasonInfo=value

END

FUNCTION Request::getSeasonInfo

  return, self.seasonInfo

END

PRO Request::setHourInfo, value

  self.hourInfo=value

END

FUNCTION Request::getHourInfo

  return, self.hourInfo

END

PRO Request::setRunResultType, value

  self.runResultType=value

END

FUNCTION Request::getRunResultType

  return, self.runResultType

END

PRO Request::setFileName, value

  self.fileName=value

END

FUNCTION Request::getFileName

  return, self.fileName

END

PRO Request::setModelCodes, list

  ptr_free, self.modelCodes
  self.modelCodes=ptr_new(list, /NO_COPY)

END

FUNCTION Request::getModelCodes

  if ptr_valid(self.modelCodes) then return, *self.modelCodes else return, [-1]

END

PRO Request::setModelNames, list

  ptr_free, self.modelNames
  self.modelNames=ptr_new(list, /NO_COPY)

END

FUNCTION Request::getModelNames

  if ptr_valid(self.modelNames) then return, *self.modelNames else return, [-1]

END

PRO Request::setExtraValues, list

  ptr_free, self.extraValues
  self.extraValues=ptr_new(list, /NO_COPY)

END

FUNCTION Request::getExtraValues

  if ptr_valid(self.extraValues) then return, *self.extraValues else return, [-1]

END

PRO Request::setScenarioCodes, list

  ptr_free, self.scenarioCodes
  self.scenarioCodes=ptr_new(list, /NO_COPY)

END

FUNCTION Request::getScenarioCodes

  if ptr_valid(self.scenarioCodes) then return, *self.scenarioCodes else return, [-1]

END

PRO Request::setScenarioNames, list

  ptr_free, self.scenarioNames
  self.scenarioNames=ptr_new(list, /NO_COPY)

END

FUNCTION Request::getScenarioNames

  if ptr_valid(self.scenarioNames) then return, *self.scenarioNames else return, [-1]

END

PRO Request::setRunCodes, list

  ptr_free, self.runCodes
  self.runCodes=ptr_new(list, /NO_COPY)

END

FUNCTION Request::getRunCodes

  if ptr_valid(self.runCodes) then return, *self.runCodes else return, [-1]

END

PRO Request::setRunFileNames, list

  ptr_free, self.runFileNames
  self.runFileNames=ptr_new(list, /NO_COPY)

END

FUNCTION Request::getRunFileNames

  if ptr_valid(self.runFileNames) then return, *self.runFileNames else return, [-1]

END

PRO Request::setUseObservedModel, value

  self.useObservedModel=value

END

FUNCTION Request::getUseObservedModel

  return, self.useObservedModel

END

PRO Request::setSingleObsNames, list

  ptr_free, self.singleObsNames
  self.singleObsNames=ptr_new(list, /NO_COPY)

END

FUNCTION Request::getSingleObsNames

  if ptr_valid(self.singleObsNames) then return, *self.singleObsNames else return, [-1]

END

PRO Request::setSingleObsCodes, list

  ptr_free, self.singleObsCodes
  self.singleObsCodes=ptr_new(list, /NO_COPY)

END

FUNCTION Request::getSingleObsLongitudes

  if ptr_valid(self.singleObsLongitudes) then return, *self.singleObsLongitudes else return, [-1]

END

PRO Request::setSingleObsLongitudes, list

  ptr_free, self.singleObsLongitudes
  self.singleObsLongitudes=ptr_new(list, /NO_COPY)

END

FUNCTION Request::getSingleObsGMTs

  if ptr_valid(self.singleObsGMTs) then return, *self.singleObsGMTs else return, [-1]

END

PRO Request::setSingleObsGMTs, list

  ptr_free, self.singleObsGMTs
  self.singleObsGMTs=ptr_new(list, /NO_COPY)

END

FUNCTION Request::getSingleObsAltitudes

  if ptr_valid(self.singleObsAltitudes) then return, *self.singleObsAltitudes else return, [-1]

END

PRO Request::setSingleObsAltitudes, list

  ptr_free, self.singleObsAltitudes
  self.singleObsAltitudes=ptr_new(list, /NO_COPY)

END

FUNCTION Request::getSingleObsLatitudes

  if ptr_valid(self.singleObsLatitudes) then return, *self.singleObsLatitudes else return, [-1]

END

PRO Request::setSingleObsLatitudes, list

  ptr_free, self.singleObsLatitudes
  self.singleObsLatitudes=ptr_new(list, /NO_COPY)

END

FUNCTION Request::getSingleObsCountries

  if ptr_valid(self.singleObsCountries) then return, *self.singleObsCountries else return, [-1]

END

PRO Request::setSingleObsCountries, list

  ptr_free, self.singleObsCountries
  self.singleObsCountries=ptr_new(list, /NO_COPY)

END

FUNCTION Request::getSingleObsCodes

  if ptr_valid(self.singleObsCodes) then return, *self.singleObsCodes else return, [-1]

END

PRO Request::setSingleShortObsNames, list

  ptr_free, self.singleShortObsNames
  self.singleShortObsNames=ptr_new(list, /NO_COPY)

END

FUNCTION Request::getSingleShortObsNames

  if ptr_valid(self.singleShortObsNames) then return, *self.singleShortObsNames else return, [-1]

END

FUNCTION Request::getGroupTitles

  if ptr_valid(self.groupTitles) then return, *self.groupTitles else return, [-1]

END

PRO Request::setGroupTitles, list

  ptr_free, self.groupTitles
  self.groupTitles=ptr_new(list, /NO_COPY)

END

PRO Request::setDiagramCode, value

  self.diagramCode=value

END

FUNCTION Request::getDiagramCode

  return, self.diagramCode

END
PRO Request::setDiagramName, value

  self.diagramName=value

END

FUNCTION Request::getDiagramName

  return, self.diagramName

END

PRO Request::setElaborationCode, value

  self.elaborationCode=value

END

FUNCTION Request::getElaborationCode

  return, self.elaborationCode

END

FUNCTION Request::getElaborationName

  return, self.elaborationName

END

PRO Request::setElaborationName, value

  self.elaborationName=value

END

PRO Request::setElaborationRoutine, value

  self.elaborationRoutine=value

END

FUNCTION Request::getElaborationRoutine

  return, self.elaborationRoutine

END

PRO Request::setParameterCodes, list

  ptr_free, self.parameterCodes
  self.parameterCodes=ptr_new(list, /NO_COPY)

END

FUNCTION Request::getParameterCodes

  if ptr_valid(self.parameterCodes) then return, *self.parameterCodes else return, [-1]

END

PRO Request::setParameterNames, list

  ptr_free, self.parameterNames
  self.parameterNames=ptr_new(list, /NO_COPY)

END

FUNCTION Request::getParameterNames

  if ptr_valid(self.parameterNames) then return, *self.parameterNames else return, [-1]

END

PRO Request::setparameterMeasureUnits, list

  ptr_free, self.parameterMeasureUnits
  self.parameterMeasureUnits=ptr_new(list, /NO_COPY)

END

FUNCTION Request::getParameterMeasureUnits

  if ptr_valid(self.parameterMeasureUnits) then return, *self.parameterMeasureUnits else return, [-1]

END

;PRO Request::setGroupByStatInfo, list
;
;  ptr_free, self.groupByStatInfo
;  self.groupByStatInfo=ptr_new(list, /NO_COPY)
;
;END

PRO Request::setObservedGroupStatInfo, list

  ptr_free, self.observedGroupStatInfo
  self.observedGroupStatInfo=ptr_new(list, /NO_COPY)

END

FUNCTION Request::getObservedGroupStatInfo

  if ptr_valid(self.observedGroupStatInfo) then return, *self.observedGroupStatInfo else return, -1

END

;FUNCTION Request::getGroupByStatInfo
;
;  if ptr_valid(self.groupByStatInfo) then return, *self.groupByStatInfo else return, [-1]
;
;END

PRO Request::setSeasonType, value

  self.seasonType=value

END

FUNCTION Request::getSeasonType

  return, self.seasonType

END

PRO Request::setHourType, value

  self.hourType=value

END

FUNCTION Request::getHourType

  return, self.hourType

END

PRO Request::setStartDate, value

  self.startDate=value

END

FUNCTION Request::getStartDate

  return, self.startDate

END

PRO Request::setEndDate, value

  self.endDate=value
  self.endDate.minute=59
  self.endDate.second=59

END

FUNCTION Request::getEndDate

  return, self.endDate

END

;*****************************
; import/export utility
;*****************************

FUNCTION Request::getFileFormatVersion

  return, '3.2'

END

FUNCTION Request::restoreDataVerTwoDotZero, filename, path

  ;open file for read
  if n_elements(path) eq 0 then path=""
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

  ;1 row of comment
  readf, unit, bufferString

  readf, unit, bufferString
  self.utility->convertStreamDataFile, bufferString, dataName, datalist
  if dataName eq 'Version' then version=datalist[0] else message, 'File version not compatible'
  if version ne self->getFileFormatVersion() then message, 'File version not compatible'

  readf, unit, bufferString
  self.utility->convertStreamDataFile, bufferString, dataName, entityFileName
  if dataName eq 'EntityFileName' then self->setEntityFileName, path+entityFileName else message, 'EntityFileName in a wrong position'

  readf, unit, bufferString
  self.utility->convertStreamDataFile, bufferString, dataName, elaborationFileName
  if dataName eq 'ElaborationFileName' then self->setElaborationFileName, path+elaborationFileName else message, 'ElaborationFileName in a wrong position'


  readf, unit, bufferString
  self.utility->convertStreamDataFile, bufferString, dataName, location
  if dataName eq 'RequestLocation' then self->setLocation, float(location) else message, 'RequestLocation in a wrong position'

  close, unit
  free_lun, unit
  return, 1

END

FUNCTION Request::restoreData, filename, path

  ;open file for read
  if n_elements(path) eq 0 then path=""
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

  ;1 row of comment
  readf, unit, bufferString

  readf, unit, bufferString
  self.utility->convertStreamDataFile, bufferString, dataName, datalist
  if dataName eq 'Version' then version=datalist[0] else message, 'File version not compatible'
  if version ne self->getFileFormatVersion() then message, 'File version not compatible'

  readf, unit, bufferString
  self.utility->convertStreamDataFile, bufferString, dataName, requestFileName
  if dataName eq 'RequestFileName' then self->setFileName, requestFileName else message, 'RequestFileName in a wrong position'

  readf, unit, bufferString
  self.utility->convertStreamDataFile, bufferString, dataName, entityFileName
  if dataName eq 'EntityFileName' then self->setEntityFileName, path+entityFileName else message, 'EntityFileName in a wrong position'

  readf, unit, bufferString
  self.utility->convertStreamDataFile, bufferString, dataName, elaborationFileName
  if dataName eq 'ElaborationFileName' then self->setElaborationFileName, path+elaborationFileName else message, 'ElaborationFileName in a wrong position'

  readf, unit, bufferString
  self.utility->convertStreamDataFile, bufferString, dataName, pageBreak
  if dataName eq 'PageBreak' then self->setPageBreak, pageBreak else message, 'PageBreak in a wrong position'

  readf, unit, bufferString
  self.utility->convertStreamDataFile, bufferString, dataName, printOrient
  if dataName eq 'PrintOrient' then self->setPrintOrient, printOrient else message, 'PrintOrient in a wrong position'

  readf, unit, bufferString
  self.utility->convertStreamDataFile, bufferString, dataName, location
  if dataName eq 'RequestLocation' then self->setLocation, float(location) else message, 'RequestLocation in a wrong position'

  readf, unit, bufferString
  self.utility->convertStreamDataFile, bufferString, dataName, plotDeviceName
  if dataName eq 'PlotDeviceName' then self->setPlotDeviceName, plotDeviceName else message, 'RequestLocation in a wrong position'

  close, unit
  free_lun, unit
  return, 1

END

PRO Request::saveDataVerTwoDotZero, fileName

  openw, unit, fileName, /GET_LUN
  header1='#Request - Save File - Never change order of contents'
  printf, unit, header1

  record=self.utility->buildFileDataStream('Version', self->getFileFormatVersion())
  printf, unit, record

  record=self.utility->buildFileDataStream('EntityFileName', self.entityFileName)
  printf, unit, record
  record=self.utility->buildFileDataStream('ElaborationFileName', self.elaborationFileName)
  printf, unit, record
  record=self.utility->buildFileDataStream('RequestLocation', strcompress(self.location, /REMOVE))
  printf, unit, record

  close, unit
  free_lun, unit

END

PRO Request::saveData, fileName

  openw, unit, fileName, /GET_LUN
  header1='#Request - Save File - Never change order of contents'
  printf, unit, header1

  record=self.utility->buildFileDataStream('Version', self->getFileFormatVersion())
  printf, unit, record

  record=self.utility->buildFileDataStream('RequestFileName', self.fileName)
  printf, unit, record
  record=self.utility->buildFileDataStream('EntityFileName', self.entityFileName)
  printf, unit, record
  record=self.utility->buildFileDataStream('ElaborationFileName', self.elaborationFileName)
  printf, unit, record
  record=self.utility->buildFileDataStream('PageBreak', strcompress(self.pageBreak, /REMOVE))
  printf, unit, record
  record=self.utility->buildFileDataStream('PrintOrient', strcompress(self.printOrient, /REMOVE))
  printf, unit, record
  record=self.utility->buildFileDataStream('RequestLocation', strcompress(self.location, /REMOVE))
  printf, unit, record
  record=self.utility->buildFileDataStream('PlotDeviceName', self.plotDeviceName)
  printf, unit, record

  close, unit
  free_lun, unit

END

PRO Request::streamPrint

  print, '***********************'
  print, '**Start of<',OBJ_CLASS(self),'>**'
  print, '**** fileName:', self->getFileName()
  print, '**** modelCodes:', self->getModelCodes()
  print, '**** scenarioCodes:', self->getScenarioCodes()
  print, '**** runCodes:', self->getRunCodes()
  print, '**** useObservedModel:', self->getUseObservedModel()
  print, '**** singleObs:', self->getSingleObs()
  print, '**** singleShortObs:', self->getSingleShortObs()
  print, '**** groupObs:', self->getGroupObs()
  print, '**** groupShortObs:', self->getGroupShortObs()
  print, '**** elaborationCode:', self->getElaborationCode()
  print, '**** parameterCodes:', self->getParameterCodes()
  print, '**** extraValues:', self->getExtraValues()
  print, '**** groupByStatInfo:', self->getGroupByStatInfo()
  print, '**** hroupByTime:', self->getGroupByTime()
  print, '**** seasonType:', self->getSeasonType()
  print, '**** hourType:', self->getHourType()
  print, '**** startDate:', self->getStartDate()
  print, '**** endDate:', self->getEndDate()

END

;*****************************
; constructor/destructor
;*****************************

PRO Request::cleanUp

  ptr_free, self.observedGroupStatInfo
  ptr_free, self.modelCodes
  ptr_free, self.modelNames
  ptr_free, self.scenarioCodes
  ptr_free, self.scenarioNames
  ptr_free, self.runNames
  ptr_free, self.runCodes
  ptr_free, self.runFileNames
  ptr_free, self.singleObsCodes
  ptr_free, self.singleObsNames
  ptr_free, self.singleObsLongitudes
  ptr_free, self.singleObsLatitudes
  ptr_free, self.singleObsAltitudes
  ptr_free, self.singleObsGMTs
  ptr_free, self.singleObsCountries
  ptr_free, self.singleShortObsNames
  ptr_free, self.singleObsCatInfos
  ptr_free, self.parameterCodes
  ptr_free, self.parameterNames
  ptr_free, self.parameterMeasureUnits
  ptr_free, self.extraValues
  ptr_free, self.groupTitles
  self->freeGroupCodes
  self->freeGroupNames
  obj_destroy, self.fileSysMgr
  obj_destroy, self.fileSysMgr
  self.gcMgr=obj_new()
  self.obsCatMgr=obj_new()
  ;obj_destroy, self.gcMgr
  ;obj_destroy, self.obsCatMgr
  obj_destroy, self.utility
  obj_destroy, self.dtu
  self->Object::cleanup

END

FUNCTION Request::init, gcMgr, obsCatMgr

  if not (self -> Object :: init()) then return, 0
  if n_elements( gcMgr) eq 1 then self.gcMgr=gcMgr
  if n_elements( obsCatMgr) eq 1 then self.obsCatMgr=obsCatMgr
  ;if n_elements( fileName ) eq 1 then self.filename = filename
  self.plotDeviceName=!d.NAME
  self.fileSysMgr=obj_new('FMFileSystemManager')
  self.utility=obj_new("FMUtility")
  self.dtu=obj_new('DateTimeUtility')
  return, 1

END

PRO Request__Define

  Struct = { Request , $
    fileName : '', $
    entityFileName : '', $
    elaborationFileName : '', $
    plotDeviceName: '', $
    pageBreak: '', $
    printOrient: '', $
    location: fltarr(4), $
    multipleChoiceUserSelectionFlags : bytarr(4), $
    diagramName: '', $
    diagramCode: '', $
    plotRoutine: '', $
    modelCodes: ptr_new(), $
    modelNames: ptr_new(), $
    scenarioCodes: ptr_new(), $
    scenarioNames: ptr_new(), $
    runNames: ptr_new(), $
    runCodes: ptr_new(), $
    runFileNames: ptr_new(), $
    runResultType: '', $
    useObservedModel: 0b, $
    singleObsCodes : ptr_new(), $
    singleObsNames : ptr_new(), $
    singleObsLongitudes : ptr_new(), $
    singleObsLatitudes : ptr_new(), $
    singleObsAltitudes : ptr_new(), $
    singleObsGMTs : ptr_new(), $
    singleObsCountries : ptr_new(), $
    singleShortObsNames : ptr_new(), $
    singleObsCatInfos : ptr_new(), $
    groupTitles: ptr_new(), $
    groupCodes: ptr_new(), $
    groupNames: ptr_new(), $
    groupStatToApplyName: '', $
    groupStatToApplyCode: '', $
    elaborationCode: '', $
    elaborationName: '',$
    elaborationRoutine: '', $
    parameterCodes: ptr_new(), $
    parameterNames: ptr_new(), $
    parameterMeasureUnits: ptr_new(), $
    extraValues: ptr_new(), $
    observedGroupStatInfo: ptr_new(), $
    seasonInfo: replicate(getTimeStampStruct(), 2), $
    hourInfo: replicate(getTimeStampStruct(), 2), $
    groupByTimeInfo: getTimeStampStruct(), $
    groupByStatInfo: ptr_new(), $
    seasonType: 0, $
    hourType: 0, $
    startDate: getDateTimeStruct(), $
    endDate: getDateTimeStruct(), $
    startIndex: 0, $
    endIndex: 0, $
    startPlotIndex: 0, $
    endPlotIndex: 0, $
    googleEarthLocation:'',$
    fileSysMgr: obj_new(), $
    gcMgr: obj_new(), $
    obsCatMgr: obj_new(), $
    scaleInfo: '', $
    utility: obj_new(), $
    dtu: obj_new(), $
    datadumpfileunit: 0L, $
    Inherits Object $
    }

END