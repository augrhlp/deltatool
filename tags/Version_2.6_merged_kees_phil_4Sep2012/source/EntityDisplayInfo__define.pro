FUNCTION EntityDisplayInfo::buildGroupObsNames

  groups=self->getObservedCodesGroupSelections()
  nElem=n_elements(groups)
  selectedNames=ptrarr(nElem)
  for i=0, nElem-1 do begin
    codes=*groups[i]
    nameList=self->getObservedNamesByCodes(codes)
    selectedNames[i]=ptr_new(nameList, /NO_COPY)
  endfor
  
  return, selectedNames
  
END

FUNCTION EntityDisplayInfo::buildModelNames

  return, self->getSelectedModelCodes()
  
END

FUNCTION EntityDisplayInfo::buildScenarioNames

  return, self->getSelectedScenarioCodes()
  
END
FUNCTION EntityDisplayInfo::buildGroupsObsNames

  codes=self->getObservedGroupCodesSelections()
  nameList=self->getObservedNamesByCodes(codes)
  return, nameList
  
END

FUNCTION EntityDisplayInfo::buildSinglesObsNames

  codes=self->getObservedCodesSelections()
  nameList=self->getObservedNamesByCodes(codes)
  return, nameList
  
END

FUNCTION EntityDisplayInfo::buildSinglesObsCodes

  codes=self->getObservedCodesSelections()
  return, codes
  
END

FUNCTION EntityDisplayInfo::buildSinglesObsGMTs

  codes=self->getObservedCodesSelections()
  nameList=self->getObservedGMTsByCodes(codes)
  return, nameList
  
END

FUNCTION EntityDisplayInfo::buildSinglesObsCountries

  codes=self->getObservedCodesSelections()
  nameList=self->getObservedCountriesByCodes(codes)
  return, nameList
  
END

FUNCTION EntityDisplayInfo::buildSinglesObsLongitudes

  codes=self->getObservedCodesSelections()
  nameList=self->getObservedLongitudesByCodes(codes)
  return, nameList
  
END

FUNCTION EntityDisplayInfo::buildSinglesObsLatitudes

  codes=self->getObservedCodesSelections()
  nameList=self->getObservedLatitudesByCodes(codes)
  return, nameList
  
END

FUNCTION EntityDisplayInfo::buildSinglesObsAltitudes

  codes=self->getObservedCodesSelections()
  nameList=self->getObservedAltitudesByCodes(codes)
  return, nameList
  
END

FUNCTION EntityDisplayInfo::buildSinglesObsShortNames

  codes=self->getObservedCodesSelections()
  nameList=self->getObservedShortNamesByCodes(codes)
  return, nameList
  
END

FUNCTION EntityDisplayInfo::buildSinglesObsCategory

  aa=self->getObsCatCategoryCodes()
  catCodes=self->getCategoryCodes()
  catNo=self->getCategoryNumber()
  for i=1, catNo do begin
    catValues=self->getCategoryValues(catCodes[i-1])
    for j=0, n_elements(catValues)-1 do print, catValues[j]
  endfor
  ;nameList=self->getCategoryValues(codes[0])
  return, nameList
  
END

PRO EntityDisplayInfo::setAllScenariosFlag, flag, FILEMODE=FILEMODE

  if keyword_set(FILEMODE) then begin
    self.utility->convertStreamDataFile, flag, dataName, dataList
    if dataName ne "AllScenariosFlag" then message, "File corrupted"
    self->setAllScenariosFlag, fix(dataList)
    return
  endif
  self.allScenariosFlag=flag
  
END

FUNCTION EntityDisplayInfo::getAllScenariosFlag, FILEMODE=FILEMODE

  if keyword_set(FILEMODE) then begin
    data=strcompress(fix(self->getAllScenariosFlag()), /REMOVE)
    record=self.utility->buildFileDataStream('AllScenariosFlag', data)
    return, record
  endif
  return, self.allScenariosFlag
  
END

PRO EntityDisplayInfo::setAllModelsFlag, flag, FILEMODE=FILEMODE

  if keyword_set(FILEMODE) then begin
    self.utility->convertStreamDataFile, flag, dataName, dataList
    if dataName ne "AllModelsFlag" then message, "File corrupted"
    self->setAllModelsFlag, fix(dataList)
    return
  endif
  self.allModelsFlag=flag
  
END

FUNCTION EntityDisplayInfo::getAllModelsFlag, FILEMODE=FILEMODE

  if keyword_set(FILEMODE) then begin
    data=strcompress(fix(self->getAllModelsFlag()), /REMOVE)
    record=self.utility->buildFileDataStream('AllModelsFlag', data)
    return, record
  endif
  return, self.allModelsFlag
  
END

PRO EntityDisplayInfo::setAllObservationsFlag, flag, FILEMODE=FILEMODE

  if keyword_set(FILEMODE) then begin
    self.utility->convertStreamDataFile, flag, dataName, dataList
    if dataName ne "AllObservationsFlag" then message, "File corrupted"
    self->setAllObservationsFlag, fix(dataList)
    return
  endif
  self.allObservationsFlag=flag
  
END

FUNCTION EntityDisplayInfo::getAllObservationsFlag, FILEMODE=FILEMODE

  if keyword_set(FILEMODE) then begin
    data=strcompress(fix(self->getAllObservationsFlag()), /REMOVE)
    record=self.utility->buildFileDataStream('AllObservationsFlag', data)
    return, record
  endif
  return, self.allObservationsFlag
  
END

PRO EntityDisplayInfo::setObservedGroupStatSelectionIndex, index

  all=self->getObservedGroupStatCodes()
  self->setObservedGroupStatCodeSelection, all[index]
  
END

FUNCTION EntityDisplayInfo::getObservedGroupStatSelectionIndex

  all=self->getObservedGroupStatCodes()
  return, (where(self.observedGroupStatCodeSelection eq all))[0]
  
END

FUNCTION EntityDisplayInfo::filterStationsCodesByParameters, originalCodes, NOMATCH=NOMATCH

  allStatCodes=self->getObservationCategoryList(/NOCATFILTER)
  selParStat=self->getObservationParameterList(allStatCodes, parGroupName=parGroupName, zeroPList=zeroPList)
  
  if zeroPList ne -1 then begin
  
    ;singleCodes=self->getObservedCodesSelections()
    ;singleNumber=n_elements(singleCodes)
    codeNumber=n_elements(originalCodes)
    valids=intarr(codeNumber)
        hlp=strcompress(originalCodes,/remove_all)
    if hlp[0] ne '-1' then $
      for i=0, codeNumber-1 do valids[i]=(where(originalCodes[i] eq selParStat))[0]     
    subsIndexes=where(valids ne -1, count)
    if count ne 0 then begin
      validCodes=originalCodes[where(valids ne -1)]
    endif else begin
      NOMATCH=1
      return, [-1]
    endelse
    if n_elements(validCodes) ne n_elements(originalCodes) then NOMATCH=1
  endif
  return, validCodes
  
END

FUNCTION EntityDisplayInfo::getObservedGroupStatNames, CODES=CODES

  if ptr_valid(self.observedGroupStatNames) then begin
    CODES=*self.observedGroupStatCodes
    return, *self.observedGroupStatNames
  endif
  return, -1
  
END

PRO EntityDisplayInfo::setObservedGroupStatNames, list

  ptr_free, self.observedGroupStatNames
  self.observedGroupStatNames=ptr_new(list, /NO_COPY)
  
END

PRO EntityDisplayInfo::setObservedGroupStatCodes, list

  ptr_free, self.observedGroupStatCodes
  self.observedGroupStatCodes=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getObservedGroupStatCodes

  if ptr_valid(self.observedGroupStatCodes) then return, *self.observedGroupStatCodes
  return, -1
  
END

PRO EntityDisplayInfo::setObservedGroupStatCodeSelection, code, FILEMODE=FILEMODE

  if keyword_set(FILEMODE) then begin
    self.utility->convertStreamDataFile, code, dataName, dataList
    if dataName ne "ObservedGroupStatCodeSelection" then message, "File corrupted"
    self->setObservedGroupStatCodeSelection, dataList
    return
  endif
  if keyword_set(NONE) then self.observedGroupStatCodeSelection=-1 else self.observedGroupStatCodeSelection=code
  
END

FUNCTION EntityDisplayInfo::getObservedGroupStatCodeSelection, FILEMODE=FILEMODE

  if keyword_set(FILEMODE) then begin
    data=strcompress(self->getObservedGroupStatCodeSelection(), /REMOVE)
    record=self.utility->buildFileDataStream('ObservedGroupStatCodeSelection', data)
    return, record
  endif
  return, self.observedGroupStatCodeSelection
  
END

FUNCTION EntityDisplayInfo::getSelectedGroupByStatName

  selIdx=self->getObservedGroupStatCodeSelection()
  if selIdx eq -1 then return, 'N/A'
  allNames=self->getObservedGroupStatNames()
  name=allNames[selIdx]
  
  return, name
  
END

PRO EntityDisplayInfo::executeObservationQuery, groupName=groupName, NORESULT=NORESULT

  ;self->buildObservationTypeList, typeGroupName=typeGroupName, zeroTList=zeroTList
  self->buildObservationCategoryList, categoryGroupName=categoryGroupName, zeroTList=zeroTList
  self->buildObservationParameterList, parGroupName=parGroupName, zeroPList=zeroPList
  if keyword_set(zeroPList) or keyword_set(zeroTList) then begin
    NORESULT=1
    self->setObservedQueryCodesSelections, ['-1']
  endif else begin
    NORESULT=0
  endelse
  ;groupName=parGroupName+'-'+typeGroupName
  groupName=parGroupName+'-'+categoryGroupName
  
END

FUNCTION EntityDisplayInfo::getObservationParameterList, codes, parGroupName=parGroupName, zeroPList=zeroPList

  zeroPList=0
  parGroupName=''
  ;if n_elements(refCodes) ne 0 then codes=refCodes else codes=self->getObservedQueryCodesSelections()
  ;print, self->getParameterSelections()
  ;print, self->getParameterCodesBySelectedTypes()
  ;parCodes=(self->getParameterCodesBySelectedTypes())[self->getParameterSelections()]
  parCodes=self->getParameterCodeSelections()
  ;print, parCodes
  parNo=n_elements(parCodes)
  checkCodes=[-1]
  parObs=self->getParameterObservedCodes()
  allPar=self->getParameterCodes()
  obsBuildList=['-1']
  for i=0, parNo-1 do begin
    idx=(where(parCodes[i] eq allPar))[0]
    obsList=*(parObs[idx])
    if obsList[0] ne '' then obsBuildList=[obsBuildList, obsList]
  ;checkObsCode=where(parCodes[j] eq obsList)
  ;newObsCodes=[newObsCodes, checkObsCode]
  endfor
  if n_elements(obsBuildList) le 1 then begin
    zeroPList=1
    return,[-1]
  endif
  obsBuildList=obsBuildList[1:*]
  obsList = obsBuildList[uniq(obsBuildList, sort(obsBuildList))]
  finalObs=['']
  for i=0, n_elements(obsList)-1 do begin
    check1=where(obsList[i] eq codes, typeCount)
    check2=where(obsList[i] eq obsBuildList, parCount)
    if (parCount eq parNo) and (typeCount gt 0) then begin
      finalObs=[finalObs, obsList[i]]
    endif
  endfor
  if n_elements(finalObs) le 1 then begin
    zeroPList=1
    return,[-1]
  endif
  finalObsCodes=finalObs[1:*]
  ; order the stations
  ; get unique station list
  ; count how many times a station appear in the list: if this count is equal parNo the station is right
  ;do pars check
  parNames=self->getParameterNamesByCodes(parCodes)
  pars=''
  for i=0, n_elements(parNames)-1 do pars=pars+'_'+parNames[i]
  parGroupName=strmid(pars, 1, strlen(pars))
  return, finalObsCodes
  
END

FUNCTION EntityDisplayInfo::getAllObservationCodesByParameters

  allCodes=self->getObservedCodes()
  codes=self->getObservationParameterList(allCodes, parGroupName=parGroupName, zeroPList=zeroPList)
  return, codes
  
END

PRO EntityDisplayInfo::buildObservationParameterList, parGroupName=parGroupName, zeroPList=zeroPList

  codes=self->getObservationParameterList(self->getObservedQueryCodesSelections(), parGroupName=parGroupName, zeroPList=zeroPList)
  if zeroPList eq 1 then return
  self->setObservedQueryCodesSelections, codes
  
END

;PRO  EntityDisplayInfo::buildObservationParameterList, parGroupName=parGroupName, zeroPList=zeroPList
;
;  zeroPList=0
;  parGroupName=''
;  codes=self->getObservedQueryCodesSelections()
;  ;print, self->getParameterSelections()
;  ;print, self->getParameterCodesBySelectedTypes()
;  parCodes=(self->getParameterCodesBySelectedTypes())[self->getParameterSelections()]
;  ;print, parCodes
;  parNo=n_elements(parCodes)
;  checkCodes=[-1]
;  parObs=self->getParameterObservedCodes()
;  allPar=self->getParameterCodes()
;  obsBuildList=['-1']
;  for i=0, parNo-1 do begin
;    idx=(where(parCodes[i] eq allPar))[0]
;    obsList=*(parObs[idx])
;    if obsList[0] ne '' then obsBuildList=[obsBuildList, obsList]
;  ;checkObsCode=where(parCodes[j] eq obsList)
;  ;newObsCodes=[newObsCodes, checkObsCode]
;  endfor
;  if n_elements(obsBuildList) le 1 then begin
;    zeroPList=1
;    return
;  endif
;  obsBuildList=obsBuildList[1:*]
;  obsList = obsBuildList[uniq(obsBuildList, sort(obsBuildList))]
;  finalObs=['']
;  for i=0, n_elements(obsList)-1 do begin
;    check1=where(obsList[i] eq codes, typeCount)
;    check2=where(obsList[i] eq obsBuildList, parCount)
;    if (parCount eq parNo) and (typeCount gt 0) then begin
;      finalObs=[finalObs, obsList[i]]
;    endif
;  endfor
;  if n_elements(finalObs) le 1 then begin
;    zeroPList=1
;    return
;  endif
;  finalObsCodes=finalObs[1:*]
;  ; order the stations
;  ; get unique station list
;  ; count how many times a station appear in the list: if this count is equal parNo the station is right
;  ;do pars check
;  parNames=self->getParameterNamesByCodes(parCodes)
;  pars=''
;  for i=0, n_elements(parNames)-1 do pars=pars+'*'+parNames[i]
;  parGroupName=pars
;  self->setObservedQueryCodesSelections, codes
;
;END

;PRO EntityDisplayInfo::buildObservationTypeList, typeGroupName=typeGroupName, zeroTList=zeroTList, noCatFilter=noCatFilter
PRO EntityDisplayInfo::buildObservationCategoryList, categoryGroupName=categoryGroupName, zeroTList=zeroTList, noCatFilter=noCatFilter

  ;obsCatCodes=self->getObservationTypeList(typeGroupName=typeGroupName, zeroTList=zeroTList, noCatFilter=noCatFilter)
  obsCatCodes=self->getObservationCategoryList(categoryGroupName=categoryGroupName, zeroTList=zeroTList, noCatFilter=noCatFilter)
  self->setObservedQueryCodesSelections, obsCatCodes
  
END

FUNCTION EntityDisplayInfo::getObservationCategoryList, categoryGroupName=categoryGroupName, zeroTList=zeroTList, noCatFilter=noCatFilter

  ; manage border line (no observation, all observation...)
  catNo=self->getCategoryNumber()
  if keyword_set(noCatFilter) then begin
    categorySelections=intarr(catNo)
    categorySelections[*]=-1
  endif else begin
    categorySelections=self->getCategorySelections()
  endelse
  ;print, categorySelections
  catCodes=self->getCategoryCodes()
  selecteds=ptrarr(catNo)
  jumps=bytarr(catNo)
  elements=intarr(catNo)
  allCodesFromCat=self->getObsCatObservedCodes()
  groupName='All'
  
  for i=0, catNo-1 do begin
    selCode=catCodes[i]
    catValues=self->getCategoryValues(selCode)
    if categorySelections[i] ne -1 then begin
      selValue=catValues[categorySelections[i]]
      if selValue ne 'All' then groupName=groupName+'_'+selValue
      indxs=self->getObsCodesByValue(selCode, selValue, count=count)
    endif else begin
      indxs=self->getObsCodesByValue(selCode, /ALL, count=count)
    endelse
    if count gt 0 then selecteds[i]=ptr_new(allCodesFromCat[indxs], /NO_COPY) else jumps[i]=1b
    elements[i]=count
  endfor
  ;print, '--->', groupname
  ;typeGroupName=groupname
  categoryGroupName=groupname
  ;self->setLastGroupName, groupname
  
  okList=['-1']
  idx=where(jumps ne 1b, count)
  if count ne 0 then begin
    selecteds=selecteds[idx]
    elements=elements[idx]
  endif
  catNo=n_elements(selecteds)
  newArray=*selecteds[0]
  for i=1, catNo-1 do begin
    nextCodes=*selecteds[i]
    for j=0, n_elements(newArray)-1 do begin
      ok=where(newArray[j] eq nextCodes, count)
      if count ne 0 then begin
        ;print, 'append this...', nextCodes[ok]
        okList=[okList, nextCodes[ok]]
      endif
    endfor
    newArray=okList
    okList=['-1']
  endfor
  
  elements=n_elements(newArray)
  if elements gt 1 then newArray=newArray[1:elements-1]
  ;print, "********"
  ;print, newarray
  ;print, "********"
  return, newArray
  
END

PRO EntityDisplayInfo::setParameterObservedCodes, list

  ptr_free, self.parameterObservedCodes
  self.parameterObservedCodes=ptr_new(list, /NO_COPY)
  
END

PRO EntityDisplayInfo::setParameterObservedCodes, list

  ptr_free, self.parameterObservedCodes
  self.parameterObservedCodes=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getParameterObservedCodes

  if ptr_valid(self.parameterObservedCodes) then return, *self.parameterObservedCodes
  return, -1
  
END

FUNCTION EntityDisplayInfo::getParameterNamesBySelectedTypes

  parameterTypeIndexes=self->getParameterTypeSelections()
  codes=self->getParametersCodesByType(parameterTypeIndexes)
  names=self->getParameterNamesByCodes(codes)
  return, names
  
END

FUNCTION EntityDisplayInfo::getParameterCodesBySelectedTypes

  parameterTypeIndexes=self->getParameterTypeSelections()
  codes=self->getParametersCodesByType(parameterTypeIndexes)
  return, codes
  
END

FUNCTION EntityDisplayInfo::getParameterNamesByCodes, codes

  allNames=self->getParameterNames()
  allCodes=self->getParameterCodes()
  howMany=n_elements(codes)
  names=strarr(howMany)
  ;idxs=intarr(howMany)
  
  for i=0, howMany-1 do names[i]=allNames[(where(codes[i] eq allCodes))]
  
  return, names
  
END

FUNCTION EntityDisplayInfo::getParameterIndexesByCodes, codes

  allCodes=self->getParameterCodesBySelectedTypes()
  ;allCodes=self->getParameterCodes()
  howMany=n_elements(codes)
  if howMany eq 0 then return, allCodes[0]
  indexes=intarr(howMany)
  ;idxs=intarr(howMany)
  
  for i=0, howMany-1 do indexes[i]=(where(codes[i] eq allCodes))
  
  return, indexes
  
END

FUNCTION EntityDisplayInfo::getParametersCodesByType, typeIndexes

  allParCodes=self->getParameterCodes()
  allNo=n_elements(allParCodes)
  allParTypes=self->getParameterTypes()
  avTypes=self->getParameterTypeCodes()
  selTypes=avTypes[typeIndexes]
  tNo=n_elements(selTypes)
  idxs=['-1']
  
  for i=0, tNo-1 do begin
    checkIdxs=where(allParTypes eq selTypes[i], count)
    if count gt 0 then idxs=[idxs, checkIdxs]
  endfor
  
  if n_elements(idxs) gt 1 then idxs=idxs[1:*]
  return, allParCodes[idxs]
  
END

PRO EntityDisplayInfo::setParameterTypeNames, list

  ptr_free, self.parameterTypeNames
  self.parameterTypeNames=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getParameterTypeNames

  if ptr_valid(self.parameterTypeNames) then return, *self.parameterTypeNames
  return, -1
  
END

PRO EntityDisplayInfo::setParameterTypeCodes, list

  ptr_free, self.parameterTypeCodes
  self.parameterTypeCodes=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getParameterTypeCodes

  if ptr_valid(self.parameterTypeCodes) then return, *self.parameterTypeCodes
  return, -1
  
END

FUNCTION EntityDisplayInfo::getParameterTypeNumber

  return, n_elements(self->getParameterTypeCodes())
  
END

PRO EntityDisplayInfo::setParameterTypeDescriptions, list

  ptr_free, self.parameterTypeDescriptions
  self.parameterTypeDescriptions=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getParameterTypeDescriptions

  if ptr_valid(self.parameterTypeDescriptions) then return, *self.parameterTypeDescriptions
  return, -1
  
END

PRO EntityDisplayInfo::setParameterTypeSelections, list, FILEMODE=FILEMODE

  if keyword_set(FILEMODE) then begin
    self.utility->convertStreamDataFile, list, dataName, dataList
    if dataName ne "ParameterTypeSelections" then message, "File corrupted"
    self->setParameterTypeSelections, long(dataList)
    return
  endif
  ptr_free, self.parameterTypeSelections
  self.parameterTypeSelections=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getParameterTypeSelections, FILEMODE=FILEMODE

  if keyword_set(FILEMODE) then begin
    data=strcompress(self->getParameterTypeSelections(), /REMOVE)
    record=self.utility->buildFileDataStream('ParameterTypeSelections', data)
    return, record
  endif
  if ptr_valid(self.parameterTypeSelections) then return, *self.parameterTypeSelections
  return, -1
  
END
; Parameter section
PRO EntityDisplayInfo::setParameterTypes, list, FILEMODE=FILEMODE

  ptr_free, self.parameterTypes
  self.parameterTypes=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getParameterTypes, FILEMODE=FILEMODE

  if ptr_valid(self.parameterTypes) then return, *self.parameterTypes
  return, -1
  
END

PRO EntityDisplayInfo::setParameterNames, list

  ptr_free, self.parameterNames
  self.parameterNames=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getParameterNames

  if ptr_valid(self.parameterNames) then return, *self.parameterNames
  return, -1
  
END

; list of codes for parameters list

PRO EntityDisplayInfo::setParameterCodes, list

  ptr_free, self.parameterCodes
  self.parameterCodes=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getParameterCodes

  if ptr_valid(self.parameterCodes) then return, *self.parameterCodes
  return, -1
  
END

; list of measure unit for parameter list

PRO EntityDisplayInfo::setParameterMeasureUnits, list

  ptr_free, self.parameterMeasureUnits
  self.parameterMeasureUnits=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getParameterMeasureUnits

  if ptr_valid(self.parameterMeasureUnits) then return, *self.parameterMeasureUnits
  return, -1
  
END

; list of description for parameter list

PRO EntityDisplayInfo::setParameterDescriptions, list

  ptr_free, self.parameterDescriptions
  self.parameterDescriptions=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getParameterDescriptions

  if ptr_valid(self.parameterDescriptions) then return, *self.parameterDescriptions
  return, -1
  
END

; user selection/s for parameter/s

;PRO EntityDisplayInfo::setParameterSelections, list, FILEMODE=FILEMODE
;
;  if keyword_set(FILEMODE) then begin
;    self.utility->convertStreamDataFile, list, dataName, dataList
;    if dataName ne "ParameterSelections" then message, "File corrupted"
;    self->setParameterSelections, long(dataList)
;    return
;  endif
;  ptr_free, self.parameterSelections
;  self.parameterSelections=ptr_new(list, /NO_COPY)
;  
;END
;
;FUNCTION EntityDisplayInfo::getParameterSelections, FILEMODE=FILEMODE
;
;  if keyword_set(FILEMODE) then begin
;    data=strcompress(self->getParameterSelections(), /REMOVE)
;    record=self.utility->buildFileDataStream('ParameterSelections', data)
;    return, record
;  endif
;  if ptr_valid(self.parameterSelections) then return, *self.parameterSelections
;  return, -1
;  
;END

PRO EntityDisplayInfo::setParameterCodeSelections, list, FILEMODE=FILEMODE

  if keyword_set(FILEMODE) then begin
    self.utility->convertStreamDataFile, list, dataName, dataList
    if dataName ne "ParameterCodeSelections" then message, "File corrupted"
    self->setParameterCodeSelections, dataList
    return
  endif
  ptr_free, self.parameterCodeSelections
  self.parameterCodeSelections=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getParameterCodeSelections, FILEMODE=FILEMODE

  if keyword_set(FILEMODE) then begin
    data=strcompress(self->getParameterCodeSelections(), /REMOVE)
    record=self.utility->buildFileDataStream('ParameterCodeSelections', data)
    return, record
  endif
  if ptr_valid(self.parameterCodeSelections) then return, *self.parameterCodeSelections
  return, '-1'
  
END

FUNCTION EntityDisplayInfo::getParameterNamesByCode, codes, ALL=ALL

  allCodes=self->getParameterCodes()
  allnames=self->getParameterNames()
  
  if keyword_set(ALL) then return, allNames
  elems=n_elements(codes)
  names=strarr(elems)
  for i=0, elems-1 do names[i]=allnames[(where(codes[i] eq allCodes))[0]]
  return, names
  
END

FUNCTION EntityDisplayInfo::getParametersBySelectedType

  ;ToDo: Change this code
  elabIndex=self->getElabSelection()
  diagramIndex=self->getDiagramSelection()
  codes=self->getElaborationsCodesByDiagram(diagramIndex)
  thisCode=codes[elabIndex]
  allCodes=self->getElabCodes()
  allparameters=self->getElabParameters()
  parameters=*allparameters[(where(thisCode eq allCodes))[0]]
  if parameters[0] eq '-1' then ALL=1
  
  names=self->getParameterNamesByCode(parameters, ALL=ALL)
  
  return, names
  
END

FUNCTION EntityDisplayInfo::getParametersSelectedCodes

  ;parCodes=(self->getParameterCodesBySelectedTypes())[self->getParameterSelections()]
  parCodes=self->getParameterCodeSelections()
  return, parCodes
  
END

FUNCTION EntityDisplayInfo::getParametersSelectedNames

  ;parCodes=(self->getParameterCodesBySelectedTypes())[self->getParameterSelections()>0]
  parCodes=self->getParameterCodeSelections()
  parNames=self->getParameterNamesByCodes(parCodes)
  
  return, parNames
  
END
;****************************************************************************************
FUNCTION EntityDisplayInfo::checkIntegrity, view

  if ~self->getAllModelsFlag() then begin
    mods=self->getModelSelections()
    if mods[0] eq -1 then begin
      aa=view->dialogMessage(['No model selected...'], title=['Check your selections'])
      return, 0
    endif
  endif else begin
    mods=indgen(n_elements(self->getModelCodes()))
    self->setModelSelections, mods
    mods=self->getScenarioSelections()
  endelse
  
  if ~self->getAllScenariosFlag() then begin
    scens=self->getScenarioSelections()
    if scens[0] eq -1 then begin
      aa=view->dialogMessage(['No scenario selected...'], title=['Check your selections'])
      return, 0
    endif
  endif else begin
    scens=indgen(n_elements(self->getScenarioCodes()))
    self->setScenarioSelections, scens
    scens=self->getScenarioSelections()
  endelse
  
  runList=self->buildRunList(scens, mods, RUNINDEXES=RUNINDEXES)
  if runList[0] eq '-1' then begin
    runNumber=n_elements(RUNINDEXES)
    if runNumber gt 1 then self->setRunQueryCodes, RUNINDEXES[1:*]
    aa=view->dialogMessage(['No runs for models/scenario selected...'], title=['Check your selections'])
    return, 0
  endif
  numSingleObs=n_elements(self->getSelectedSingleObsNames())
  numGroupObs=n_elements(self->getObservedGroupTitles())
  ;  if numObs gt 9 then begin
  ;    aa=view->dialogMessage(['We suggest maximun 10 single observations... ', 'Your selections is: '+strcompress(numObs, /REMOVE)], title=['Check your selections'], /INFORMATION)
  ;    return, 1
  ;  endif
  ; ToDO: check at least one element selected
  answer="Boh"
  obsCheck=0
  if self->IsSingleObsSelected() eq 1 then begin
    codes=self->getObservedCodesSelections()
    ;aa=view->dialogMessage(['No single observations selected...'], title=['Check your selections'])
    validCodes=self->filterStationsCodesByParameters(codes, NOMATCH=NOMATCH)
    if n_elements(NOMATCH) eq 1 then begin
      if validCodes[0] eq -1 then begin
        answer=view->dialogMessage(['None of selected stations contain selected parameter.','Change selection'], title=['Check your selections'], /WARNING)
        return, 0
      endif
      answer=view->dialogMessage(['Not all selected stations contain selected parameter.','Press "Yes" to auto correction, "No" to abort selection'], title=['Check your selections'], /QUESTION)
      if answer eq "Yes" then self->setObservedCodesSelections, validCodes else return, 0
    endif
    obsCheck=1
  endif
  if self->IsGroupObsSelected() eq 1 then begin
    gTitles=self->getObservedGroupTitles()
    ptrToCodes=self->getObservedCodesGroupSelections()
    newPtrToCodes=ptrarr(n_elements(ptrToCodes))
    for i=0, n_elements(ptrToCodes)-1 do begin
      thisCodes=*ptrToCodes[i]
      validCodes=self->filterStationsCodesByParameters(thisCodes, NOMATCH=NOMATCH)
      if n_elements(NOMATCH) eq 1 then begin
        if validCodes[0] eq -1 then begin
          answer=view->dialogMessage(['None of selected stations in group <'+gTitles[i]+ '> contains selected parameter.','Change selection'], title=['Check your selections'], /WARNING)
          return, 0
        endif
        if answer eq "Boh" then answer=view->dialogMessage(['Not all selected stations contain selected parameter.','Press "Yes" to auto correction, "No" to abort selection'], title=['Check your selections'], /QUESTION)
        if answer ne "Yes" then return, 0
        a=ptr_new(NOMATCH, /NO_COPY)
      endif
      newPtrToCodes[i]=ptr_new(validCodes, /NO_COPY)
    endfor
    self->setObservedCodesGroupSelections, newPtrToCodes
    obsCheck=1
  endif
  if obsCheck ne 1 then begin
    aa=view->dialogMessage(['No monitoring observations selected'], title=['Check your selections'])
    return, 0
  endif
  
  return, 1
  
; mods=widget_info(self.modelList, /LIST_SELECT)
; if mods[0] eq -1 then begin
;  aa=self->dialogMessage(['No model selected...'], title=['Check your selections'])
;  return, 0
; endif
; scens=widget_info(self.scenarioList, /LIST_SELECT)
; if scens[0] eq -1 then begin
;  aa=self->dialogMessage(['No scenario selected...'], title=['Check your selections'])
;  return, 0
; endif
; obsSinglesCodes=self.info->getObservedCodesSelections()
; if obsSinglesCodes[0] eq -1 then begin
;  singlesNumber=0
;  aa=self->dialogMessage(['No single observations selected...'], title=['Check your selections'])
;  return, 1
; endif
; singlesNumber=n_elements(obsSinglesCodes)
; obsGroupNames=self.info->getObservedGroupNames()
; obsGroupCodes=self.info->getObservedCodesGroupSelections()
; if obsGroupNames[0] eq '-1' then begin
;  groupNumber=0
;  aa=self->dialogMessage(['No group observations selected...'], title=['Check your selections'])
;  return, 1
; endif
; groupNumber=n_elements(obsGroupNames)
; return, 1
  
END

FUNCTION  EntityDisplayInfo::IsSingleObsSelected

  if ptr_valid(self.observedCodesSelections) then return, 1b else return, 0b
  
END

FUNCTION  EntityDisplayInfo::IsGroupObsSelected

  if ptr_valid(self.observedCodesGroupSelections) then return, 1b else return, 0b
  
END

FUNCTION  EntityDisplayInfo::isRunPresent

  if ptr_valid(self.runQueryCodes) then begin
    if (*self.runQueryCodes)[0] ne -1 then return, 1b
  endif
  return, 0b
  
END

FUNCTION EntityDisplayInfo::getSelectedGroupObsNames

  displayCodes=self->getObservedCodesGroupSelections()
  if self->isValidObservedGroupTitles() then begin
    titleList=self->getObservedGroupTitles()
    return, titleList
  endif
  return, 'No group'
  
END

FUNCTION EntityDisplayInfo::getSelectedSingleObsNames

  displayCodes=self->getObservedCodesSelections()
  if displayCodes[0] ne '-1' then begin
    nameList=self->getObservedNamesSelections()
    return, nameList
  endif
  return, 'No Single'
  
END

FUNCTION EntityDisplayInfo::getSelectedRunNames

  scenarioIndexes=self->getScenarioSelections()
  modelIndexes=self->getModelSelections()
  names=self->buildRunList(scenarioIndexes, modelIndexes)
  return, names
  
END

FUNCTION EntityDisplayInfo::getSelectedScenarioNames

  sels=self->getScenarioSelections()
  names=self->getScenarioNames()
  if sels[0] ne -1 then return, names[sels] else return, ''
  
END

FUNCTION EntityDisplayInfo::getSelectedScenarioCodes

  sels=self->getScenarioSelections()
  allCodes=self->getScenarioCodes()
  if sels[0] ne -1 then return, allCodes[sels] else return, ''
  
END

FUNCTION EntityDisplayInfo::getSelectedModelCodes

  sels=self->getModelSelections()
  allCodes=self->getModelCodes()
  if sels[0] ne -1 then return, allCodes[sels] else return, ''
  
END

FUNCTION EntityDisplayInfo::getSelectedModelNames

  sels=self->getModelSelections()
  names=self->getModelNames()
  if sels[0] ne -1 then return, names[sels] else return, ''
  
END

PRO EntityDisplayInfo::cancelObservedData

  ptr_free, self.observedCodesSelections
  ptr_free, self.observedGroupTitles
  ptr_free, self.observedCodesGroupSelections
  
END

FUNCTION EntityDisplayInfo::buildObsGroupDescription, index

  groupCodeLists=self->getObservedCodesGroupSelections()
  groupTitleList=self->getObservedGroupTitles()
  groupCodes=groupCodeLists[index]
  k=0
  
  for i=0, n_elements(groupTitleList)-1 do begin
    codesList=*groupCodeLists[index]
    obs=self->getObservedNamesByCodes(codesList)
    txt=''
    for j=0, n_elements(obs)-1 do txt=txt+"*"+strcompress(j+1, /REMOVE)+"*"+obs[j]
  endfor
  ;help, groupNameList[index]
  ;help, string(*groupCodeLists[index])
  return, groupTitleList[index]+txt
  
END

FUNCTION EntityDisplayInfo::isValidObservedGroupTitles

  return, ptr_valid(self.observedGroupTitles)
  
END

FUNCTION EntityDisplayInfo::getObservedCodesGroupSelections, FILEMODE=FILEMODE

  if keyword_set(FILEMODE) then begin
    if ptr_valid(self.observedCodesGroupSelections) then begin
      data=self->getObservedCodesGroupSelections()
    endif else begin
      data=ptr_new("NONE", /NO_COPY)
    endelse
    record=self.utility->buildFileDataStream('ObservedCodesGroupSelections', data, /POINTER)
    return, record
  endif
  ;do a copy
  if ptr_valid(self.observedCodesGroupSelections) then begin
    oCGS=*self.observedCodesGroupSelections
    nElem=n_elements(oCGS)
    copy=ptrarr(nElem)
    for i=0, nElem-1 do begin
      thisList=*oCGS[i]
      copy[i]=ptr_new(thisList, /NO_COPY)
    endfor
    return, copy
  endif
  return, -1
  
END

PRO EntityDisplayInfo::freeObservedCodesGroupSelections

  if ptr_valid(self.observedCodesGroupSelections) then begin
    oCGS=self.observedCodesGroupSelections
    nElem=n_elements(oCGS)
    for i=0, nElem-1 do ptr_free,oCGS[i]
  endif
  ptr_free, self.observedCodesGroupSelections
  
END

PRO EntityDisplayInfo::setRunQueryCodes, list

  ptr_free, self.runQueryCodes
  self.runQueryCodes=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getRunQueryCodes

  if ptr_valid(self.runQueryCodes) then return, *self.runQueryCodes
  return, [-1]
  
END

PRO EntityDisplayInfo::setObservedCodesGroupSelections, list, FILEMODE=FILEMODE

  if keyword_set(FILEMODE) then begin
    self.utility->convertStreamDataFile, list, dataName, dataList, /POINTER
    if dataName ne "ObservedCodesGroupSelections" then message, "File corrupted"
    check=*dataList[0]
    self->freeObservedCodesGroupSelections
    if check[0] ne "NONE" then begin
      if n_elements(dataList) ne 0 then begin
        if ptr_valid(dataList[0]) then self->setObservedCodesGroupSelections, dataList
      endif
    endif
    return
  endif
  self->freeObservedCodesGroupSelections
  self.observedCodesGroupSelections=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getObservedNamesSelections

  codesList=self->getObservedCodesSelections()
  return, self->getObservedNamesByCodes(codesList)
  
END

PRO EntityDisplayInfo::removeObservations, codesList, groupName, SINGLE=SINGLE

  if keyword_set(SINGLE) then begin
    codes=self->getObservedCodesSelections()
    elems=n_elements(codesList)
    
    for i=0, elems-1 do begin
      saveIdxs=where(codes ne codesList[i], count)
      if count ne 0 then begin
        codes=codes[saveIdxs]
      endif else begin
        ptr_free, self.observedCodesSelections
        return
      endelse
    endfor
    self->setObservedCodesSelections, codes
  endif else begin
    groupTitleList=self->getObservedGroupTitles()
    groupCodeLists=self->getObservedCodesGroupSelections()
    elems=n_elements(codesList)
    alls=indgen(n_elements(groupTitleList))
    for i=0, elems-1 do begin
      saveIdxs=where(alls ne codesList[i], count)
      if count ne 0 then begin
        groupCodeLists=groupCodeLists[saveIdxs]
        groupTitleList=groupTitleList[saveIdxs]
        alls=alls[saveIdxs]
      endif else begin
        ptr_free, self.observedCodesGroupSelections
        ptr_free, self.observedGroupTitles
        return
      endelse
    endfor
    self->setObservedCodesGroupSelections, groupCodeLists
    self->setObservedGroupTitles, groupTitleList
  endelse
  
END

PRO EntityDisplayInfo::addObservations, codesList, groupName, SINGLE=SINGLE

  if keyword_set(SINGLE) then begin
    if ptr_valid(self.observedCodesSelections) then previousList=self->getObservedCodesSelections() else previousList=codesList[0]
    updateList=[previousList, codesList]
    ;one obs/group can appear only one time....
    updateList = updateList[UNIQ(updateList, SORT(updateList))]
    self->setObservedCodesSelections, updateList
  endif else begin
    if ptr_valid(self.observedGroupTitles) then begin
      previousNameList=self->getObservedGroupTitles()
      previousCodeLists=self->getObservedCodesGroupSelections()
      elems=n_elements(previousNameList)
      codess=ptrarr(elems+1)
      names=strarr(elems+1)
      codess[0:elems-1]=previousCodeLists
      codess[elems]=ptr_new(codesList, /NO_COPY)
      names[0:elems-1]=previousNameList
      names[elems]=groupName
    endif else begin
      codess=ptrarr(1)
      codess[0]=ptr_new(codesList, /NO_COPY)
      names=groupName
    endelse
    self->setObservedCodesGroupSelections, codess
    self->setObservedGroupTitles, names
  endelse
  
END

FUNCTION EntityDisplayInfo::getObservedQueryNames

  codesList=self->getObservedQueryCodesSelections()
  return, self->getObservedNamesByCodes(codesList)
  
END

FUNCTION EntityDisplayInfo::getObservedNamesByCodes, codes

  allCodes=self->getObservedCodes()
  allNames=self->getObservedNames()
  elems=n_elements(codes)
  nameList=strarr(elems)
  for i=0, elems-1 do nameList[i]=allNames[(where(codes[i] eq allCodes))[0]]
  return, nameList
  
END

FUNCTION EntityDisplayInfo::getObservedShortNamesByCodes, codes

  allCodes=self->getObservedCodes()
  allNames=self->getObservedShortNames()
  elems=n_elements(codes)
  nameList=strarr(elems)
  for i=0, elems-1 do nameList[i]=allNames[(where(codes[i] eq allCodes))[0]]
  return, nameList
  
END

; march 15 2011 MM
FUNCTION EntityDisplayInfo::getObservedGMTsByCodes, codes

  allCodes=self->getObservedCodes()
  allNames=self->getObservedGMTs()
  elems=n_elements(codes)
  nameList=strarr(elems)
  for i=0, elems-1 do nameList[i]=allNames[(where(codes[i] eq allCodes))[0]]
  return, nameList
  
END

FUNCTION EntityDisplayInfo::getObservedCountriesByCodes, codes

  allCodes=self->getObservedCodes()
  allNames=self->getObservedCountries()
  elems=n_elements(codes)
  nameList=strarr(elems)
  for i=0, elems-1 do nameList[i]=allNames[(where(codes[i] eq allCodes))[0]]
  return, nameList
  
END

FUNCTION EntityDisplayInfo::getObservedLongitudesByCodes, codes

  allCodes=self->getObservedCodes()
  allNames=self->getObservedLongitudes()
  elems=n_elements(codes)
  nameList=strarr(elems)
  for i=0, elems-1 do nameList[i]=allNames[(where(codes[i] eq allCodes))[0]]
  return, nameList
  
END

FUNCTION EntityDisplayInfo::getObservedAltitudesByCodes, codes

  allCodes=self->getObservedCodes()
  allNames=self->getObservedAltitudes()
  elems=n_elements(codes)
  nameList=strarr(elems)
  for i=0, elems-1 do nameList[i]=allNames[(where(codes[i] eq allCodes))[0]]
  return, nameList
  
END

FUNCTION EntityDisplayInfo::getObservedLatitudesByCodes, codes

  allCodes=self->getObservedCodes()
  allNames=self->getObservedLatitudes()
  elems=n_elements(codes)
  nameList=strarr(elems)
  for i=0, elems-1 do nameList[i]=allNames[(where(codes[i] eq allCodes))[0]]
  return, nameList
  
END
; end MM
FUNCTION EntityDisplayInfo::getObsCodesByValue, categoryCode, value, count=count, ALL=ALL

  catList=*self.obsCatCategoryCodes
  obsList=*self.obsCatObservedCodes
  valList=*self.obsCatValues
  
  if keyword_set(ALL) then begin
    idxs=where(catList eq categoryCode, count)
  endif else begin
    idxs=where(catList eq categoryCode and valList eq value, count)
  endelse
  
  if count gt 0 then begin
    ;print, obsList[idxs], catList[idxs], valList[idxs]
    return, idxs
  endif
  return, ['-1']
  
END

FUNCTION EntityDisplayInfo::buildQueryObsList

  runIdxList=[-1]
  thisList=*self.list
  indexes=indgen(n_elements(thisList.value))
  for i=0, n_elements(categoriesIndexes)-1 do begin
    idxes=where(thisList[i].categoryCode eq categoriesIndexes[i] and $
      thisList[i].value eq values[i], count)
    if count ne 0 then begin
      subindexes=where(thisList[i].value[idxes] eq values[i], count2)
      if count2 ne 0 then begin
        candidates=thisList.code[idxes[subindexes]]
      endif
    endif
  endfor
  return, thisList.code[idxes]
  
END

PRO EntityDisplayInfo::setObsCatValues, list

  ptr_free, self.obsCatValues
  self.obsCatValues=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getObsCatValues

  if ptr_valid(self.obsCatValues) then return, *self.obsCatValues
  return, -1
  
END

PRO EntityDisplayInfo::setObsCatObservedCodes, list

  ptr_free, self.obsCatObservedCodes
  self.obsCatObservedCodes=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getObsCatObservedCodes

  if ptr_valid(self.obsCatObservedCodes) then return, *self.obsCatObservedCodes
  return, -1
  
END

PRO EntityDisplayInfo::setObsCatCategoryCodes, list

  ptr_free, self.obsCatCategoryCodes
  self.obsCatCategoryCodes=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getObsCatCategoryCodes

  if ptr_valid(self.obsCatCategoryCodes) then return, *self.obsCatCategoryCodes
  return, -1
  
END

PRO EntityDisplayInfo::setCategorySelections, list, FILEMODE=FILEMODE

  if keyword_set(FILEMODE) then begin
    self.utility->convertStreamDataFile, list, dataName, dataList
    if dataName ne "CategorySelections" then message, "File corrupted"
    self->setCategorySelections, long(dataList)
    return
  endif
  self.categorySelections=list
  
END

FUNCTION EntityDisplayInfo::getCategorySelections, FILEMODE=FILEMODE

  if keyword_set(FILEMODE) then begin
    data=strcompress(self->getCategorySelections(), /REMOVE)
    record=self.utility->buildFileDataStream('CategorySelections', data)
    return, record
  endif
  return, self.categorySelections
  
END

PRO EntityDisplayInfo::setRunNames, list

  ptr_free, self.runNames
  self.runNames=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getRunNames

  if ptr_valid(self.runNames) then return, *self.runNames
  return, -1
  
END
;****************************************************************************************
PRO EntityDisplayInfo::setRunCodes, list

  ptr_free, self.runCodes
  self.runCodes=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getRunCodes

  if ptr_valid(self.runCodes) then return, *self.runCodes
  return, -1
  
END
;****************************************************************************************
PRO EntityDisplayInfo::setRunDescriptions, list

  ptr_free, self.runDescriptions
  self.runDescriptions=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getRunDescriptions

  if ptr_valid(self.runDescriptions) then return, *self.runDescriptions
  return, -1
  
END
;****************************************************************************************
PRO EntityDisplayInfo::setRunSelections, list, FILEMODE=FILEMODE

  if keyword_set(FILEMODE) then begin
    self.utility->convertStreamDataFile, list, dataName, dataList
    if dataName ne "RunSelections" then message, "File corrupted"
    if dataList[0] ne -1 then self->setRunSelections, long(dataList)
    return
  endif
  ptr_free, self.runSelections
  self.runSelections=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getRunSelections, FILEMODE=FILEMODE

  if keyword_set(FILEMODE) then begin
    data=strcompress(self->getRunSelections(), /REMOVE)
    record=self.utility->buildFileDataStream('RunSelections', data)
    return, record
  endif
  if ptr_valid(self.runSelections) then return, *self.runSelections
  return, -1
  
END
;****************************************************************************************
PRO EntityDisplayInfo::setRunScenarioCodes, list

  ptr_free, self.runScenarioCodes
  self.runScenarioCodes=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getRunScenarioCodes

  if ptr_valid(self.runScenarioCodes) then return, *self.runScenarioCodes
  return, -1
  
END
;****************************************************************************************
PRO EntityDisplayInfo::setRunModelCodes, list

  ptr_free, self.runModelCodes
  self.runModelCodes=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getRunModelCodes

  if ptr_valid(self.runModelCodes) then return, *self.runModelCodes
  return, -1
  
END

FUNCTION EntityDisplayInfo::buildRunList, scenarioIndexes, modelIndexes, RUNINDEXES=RUNINDEXES

  scenCodes=(*self.scenarioCodes)[scenarioIndexes]
  modelCodes=(*self.modelCodes)[modelIndexes]
  runScenarioCodes=(*self.runScenarioCodes)
  runModelCodes=(*self.runModelCodes)
  runIdxList=[-1]
  for i= 0, n_elements(scenCodes)-1 do begin
    for j= 0, n_elements(modelCodes)-1 do begin
      thisRunIdx=where(scenCodes[i] eq runScenarioCodes[*] and modelCodes[j] eq runModelCodes[*], count)
      if count eq 1 then runIdxList=[runIdxList, thisRunIdx]
    endfor
  endfor
  if n_elements(runIdxList) gt 1 then begin
    RUNINDEXES=runIdxList
    self->setRunQueryCodes, runIdxList[1:*]
    return, (*self.runNames)[runIdxList[1:*]]
  endif
  return, ['-1']
  
END

FUNCTION EntityDisplayInfo::getSelectedRunCodes

  scenarioIndexes=self->getScenarioSelections()
  modelIndexes=self->getModelSelections()
  scenCodes=(*self.scenarioCodes)[scenarioIndexes]
  modelCodes=(*self.modelCodes)[modelIndexes]
  runScenarioCodes=(*self.runScenarioCodes)
  runModelCodes=(*self.runModelCodes)
  runCodes=(*self.runCodes)
  runIdxList=[-1]
  for i= 0, n_elements(scenCodes)-1 do begin
    for j= 0, n_elements(modelCodes)-1 do begin
      thisRunIdx=where(scenCodes[i] eq runScenarioCodes[*] and modelCodes[j] eq runModelCodes[*], count)
      if count eq 1 then runIdxList=[runIdxList, thisRunIdx]
    endfor
  endfor
  if n_elements(runIdxList) eq 1 then return, [-1]
  return, runCodes[runIdxList[1:*]]
  
END

;****************************************************************************************
PRO EntityDisplayInfo::setScenarioNames, list

  ptr_free, self.scenarioNames
  self.scenarioNames=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getScenarioNames

  if ptr_valid(self.scenarioNames) then return, *self.scenarioNames
  return, -1
  
END

PRO EntityDisplayInfo::setScenarioCodes, list

  ptr_free, self.scenarioCodes
  self.scenarioCodes=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getScenarioCodes

  if ptr_valid(self.scenarioCodes) then return, *self.scenarioCodes
  return, -1
  
END

PRO EntityDisplayInfo::setScenarioDescriptions, list

  ptr_free, self.scenarioDescriptions
  self.scenarioDescriptions=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getScenarioDescriptions

  if ptr_valid(self.scenarioDescriptions) then return, *self.scenarioDescriptions
  return, -1
  
END

PRO EntityDisplayInfo::setScenarioSelections, list, FILEMODE=FILEMODE

  if keyword_set(FILEMODE) then begin
    self.utility->convertStreamDataFile, list, dataName, dataList
    if dataName ne "ScenarioSelections" then message, "File corrupted"
    if dataList[0] eq "ALL" then self->setScenarioSelections, indgen(n_elements(self->getScenarioCodes())) else self->setScenarioSelections, long(dataList)
    return
  endif
  ptr_free, self.scenarioSelections
  self.scenarioSelections=ptr_new(list)
  
END

FUNCTION EntityDisplayInfo::getScenarioSelections, FILEMODE=FILEMODE

  if keyword_set(FILEMODE) then begin
    if (self.allScenariosFlag) then data="ALL" else data=strcompress(self->getScenarioSelections(), /REMOVE)
    record=self.utility->buildFileDataStream('ScenarioSelections', data)
    return, record
  endif
  if ptr_valid(self.scenarioSelections) then return, *self.scenarioSelections
  return, -1
  
END

PRO EntityDisplayInfo::setModelNames, list

  ptr_free, self.modelNames
  self.modelNames=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getModelNames

  if ptr_valid(self.modelNames) then return, *self.modelNames
  return, -1
  
END

PRO EntityDisplayInfo::setModelCodes, list

  ptr_free, self.modelCodes
  self.modelCodes=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getModelCodes

  if ptr_valid(self.modelCodes) then return, *self.modelCodes
  return, -1
  
END

PRO EntityDisplayInfo::setModelDescriptions, list

  ptr_free, self.modelDescriptions
  self.modelDescriptions=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getModelDescriptions

  if ptr_valid(self.modelDescriptions) then return, *self.modelDescriptions
  return, -1
  
END

PRO EntityDisplayInfo::setModelSelections, list, FILEMODE=FILEMODE

  if keyword_set(FILEMODE) then begin
    self.utility->convertStreamDataFile, list, dataName, dataList
    if dataName ne "ModelSelections" then message, "File corrupted"
    if dataList[0] eq "ALL" then self->setModelSelections, indgen(n_elements(self->getModelCodes())) else self->setModelSelections, long(dataList)
    return
  endif
  ptr_free, self.modelSelections
  self.modelSelections=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getModelSelections, FILEMODE=FILEMODE

  if keyword_set(FILEMODE) then begin
    if (self.allModelsFlag) then data="ALL" else data=strcompress(self->getModelSelections(), /REMOVE)
    record=self.utility->buildFileDataStream('ModelSelections', data)
    return, record
  endif
  if ptr_valid(self.modelSelections) then return, *self.modelSelections
  return, -1
  
END

FUNCTION EntityDisplayInfo::getCategoryNumber

  presences=where(self.categoryPresenceFlags eq 1b, count)
  return, count
  
END

PRO EntityDisplayInfo::setCategoryPresenceFlags, flags

  self.categoryPresenceFlags=flags
  
END

FUNCTION EntityDisplayInfo::getCategoryPresenceFlags

  return, self.categoryPresenceFlags
  
END

PRO EntityDisplayInfo::setCategoryTitles, list

  self.categoryTitles=list
  
END

FUNCTION EntityDisplayInfo::getCategoryTitles

  return, self.categoryTitles
  
END

PRO EntityDisplayInfo::setCategoryCodes, list

  self.categoryCodes[*]=''
  self.categoryCodes[0:n_elements(list)-1]=list
  
END

FUNCTION EntityDisplayInfo::getCategoryCodes

  return, self.categoryCodes
  
END

PRO EntityDisplayInfo::setCategoryWidgetTypes, list

  self.categoryWidgetTypes=list
  
END

FUNCTION EntityDisplayInfo::getCategoryWidgetTypes

  return, self.categoryWidgetTypes
  
END

PRO EntityDisplayInfo::setCategoryValues, list, index=index

  if (n_elements(index) ne 0) then begin
    ptr_free, self.categoryValues[index]
    howMany=n_elements(list)
    if howMany gt 4 then self.categoryWidgetTypes[index]=1b else self.categoryWidgetTypes[index]=0b
    self.categoryValues[index]=ptr_new(list, /NO_COPY)
    return
  endif
  for i=0, n_elements(list)-1 do begin
    ptr_free, self.categoryValues[i]
    howMany=n_elements(*list[i])
    if howMany gt 4 then self.categoryWidgetTypes[i]=1b else self.categoryWidgetTypes[i]=0b
    self.categoryValues[i]=ptr_new(*list[i], /NO_COPY)
  endfor
  
END

FUNCTION EntityDisplayInfo::getCategoryValues, code

  index=(where(code eq self.categoryCodes))[0]
  if ptr_valid(self.categoryValues[index]) then return, *self.categoryValues[index]
  return, -1
  
END

PRO EntityDisplayInfo::setParameterObservedCodes, list

  ptr_free, self.parameterObservedCodes
  ; ptr_free for each elements... 'cause is a pointer of pointers
  self.parameterObservedCodes=ptr_new(list, /NO_COPY)
  
END

PRO EntityDisplayInfo::setObservedNames, list

  ptr_free, self.observedNames
  self.observedNames=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getObservedNames

  if ptr_valid(self.observedNames) then return, *self.observedNames
  return, -1
  
END

PRO EntityDisplayInfo::setObservedLatitudes, list

  ptr_free, self.observedLatitudes
  self.observedLatitudes=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getObservedLatitudes

  if ptr_valid(self.observedLatitudes) then return, *self.observedLatitudes
  return, -1
  
END

PRO EntityDisplayInfo::setObservedLongitudes, list

  ptr_free, self.observedLongitudes
  self.observedLongitudes=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getObservedLongitudes

  if ptr_valid(self.observedLongitudes) then return, *self.observedLongitudes
  return, -1
  
END

PRO EntityDisplayInfo::setObservedAltitudes, list

  ptr_free, self.observedAltitudes
  self.observedAltitudes=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getObservedAltitudes

  if ptr_valid(self.observedAltitudes) then return, *self.observedAltitudes
  return, -1
  
END

PRO EntityDisplayInfo::setObservedCountries, list

  ptr_free, self.observedCountries
  self.observedCountries=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getObservedCountries

  if ptr_valid(self.observedCountries) then return, *self.observedCountries
  return, -1
  
END

PRO EntityDisplayInfo::setObservedGMTs, list

  ptr_free, self.observedGMTs
  self.observedGMTs=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getObservedGMTs

  if ptr_valid(self.observedGMTs) then return, *self.observedGMTs
  return, -1
  
END

PRO EntityDisplayInfo::setObservedShortNames, list

  ptr_free, self.observedShortNames
  self.observedShortNames=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getObservedShortNames

  if ptr_valid(self.observedShortNames) then return, *self.observedShortNames
  return, -1
  
END

FUNCTION EntityDisplayInfo::getObservedCatInfoByCodes, codes

  catList=*self.obsCatCategoryCodes
  obsList=*self.obsCatObservedCodes
  valList=*self.obsCatValues
  
  titles=self->getCategoryTitles()
  
  catInfo=strarr(n_elements(titles),n_elements(codes))
  for i=0, n_elements(codes)-1 do begin
    idxs=where(obsList eq codes[i])
    catInfo[*, i]=valList[idxs]
  endfor
  
  return, catInfo
  
END

FUNCTION EntityDisplayInfo::buildSingleObservedCatInfos

  codes=self->getObservedCodesSelections()
  return, self->getObservedCatInfoByCodes(codes)
  
END

FUNCTION EntityDisplayInfo::buildGroupObservedCatInfo

  catList=*self.obsCatCategoryCodes
  obsList=*self.obsCatObservedCodes
  valList=*self.obsCatValues
  
  codes=self->getObservedCodesSelections()
  titles=self->getCategoryTitles()
  
  catInfo=strarr(n_elements(titles),n_elements(codes))
  for i=0, n_elements(codes)-1 do begin
    idxs=where(obsList eq codes[i])
    catInfo[*, i]=valList[idxs]
    print, valList[idxs]
  endfor
  
  return, catInfo
  
END

PRO EntityDisplayInfo::setObservedCodes, list

  ptr_free, self.observedCodes
  self.observedCodes=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getObservedCodes

  if ptr_valid(self.observedCodes) then return, *self.observedCodes
  return, -1
  
END

PRO EntityDisplayInfo::setObservedQueryCodesSelections, list, FILEMODE=FILEMODE

  if keyword_set(FILEMODE) then begin
    self.utility->convertStreamDataFile, list, dataName, dataList
    if dataName ne "ObservedQueryCodesSelections" then message, "File corrupted"
    if dataList[0] eq "NONE" then self->setObservedQueryCodesSelections, dataList else   ptr_free, self.observedQueryCodesSelections
    return
  endif
  ptr_free, self.observedQueryCodesSelections
  self.observedQueryCodesSelections=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getObservedQueryCodesSelections, FILEMODE=FILEMODE

  if keyword_set(FILEMODE) then begin
    data=strcompress(self->getObservedQueryCodesSelections(), /REMOVE)
    record=self.utility->buildFileDataStream('ObservedQueryCodesSelections', data)
    return, record
  endif
  if ptr_valid(self.observedQueryCodesSelections) then return, *self.observedQueryCodesSelections
  return, -1
  
END

PRO EntityDisplayInfo::setObservedCodesSelections, list, FILEMODE=FILEMODE

  if keyword_set(FILEMODE) then begin
    self.utility->convertStreamDataFile, list, dataName, dataList
    if dataName ne "ObservedCodesSelections" then message, "File corrupted"
    ptr_free, self.observedCodesSelections
    if dataList[0] eq "NONE" then return
    if dataList[0] eq "ALL" then begin
      ;self->setObservedCodesSelections, self->getAllObservationCodesByParameters()
      queryCodes=self->getObservationCategoryList()
      parCodes=self->getObservationParameterList(queryCodes)
      self->setObservedCodesSelections, parCodes
      return
    endif
    self->setObservedCodesSelections, dataList
    return
  endif
  ptr_free, self.observedCodesSelections
  self.observedCodesSelections=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getObservedCodesSelections, FILEMODE=FILEMODE

  if keyword_set(FILEMODE) then begin
    if ptr_valid(self.observedCodesSelections) then data=self->getObservedCodesSelections() else data="NONE"
    if self.allObservationsFlag then data="ALL"
    data=strcompress(data, /REMOVE)
    record=self.utility->buildFileDataStream('ObservedCodesSelections', data)
    return, record
  endif
  if ptr_valid(self.observedCodesSelections) then return, *self.observedCodesSelections
  return, -1
  
END

PRO EntityDisplayInfo::setObservedGroupTitles, list, FILEMODE=FILEMODE

  if keyword_set(FILEMODE) then begin
    self.utility->convertStreamDataFile, list, dataName, dataList
    if dataName ne "ObservedGroupTitles" then message, "File corrupted"
    if dataList[0] ne "NONE" then self->setObservedGroupTitles, dataList else ptr_free, self.observedGroupTitles
    return
  endif
  ptr_free, self.observedGroupTitles
  self.observedGroupTitles=ptr_new(list, /NO_COPY)
  
END

FUNCTION EntityDisplayInfo::getObservedGroupTitles, FILEMODE=FILEMODE

  if keyword_set(FILEMODE) then begin
    if ptr_valid(self.observedGroupTitles) then data=strcompress(self->getObservedGroupTitles(), /REMOVE) else data="NONE"
    record=self.utility->buildFileDataStream('ObservedGroupTitles', data)
    return, record
  endif
  if ptr_valid(self.observedGroupTitles) then return, *self.observedGroupTitles
  return, -1
  
END

PRO EntityDisplayInfo::setUseObservedModelFlag, flag, FILEMODE=FILEMODE

  if keyword_set(FILEMODE) then begin
    self.utility->convertStreamDataFile, flag, dataName, dataList
    if dataName ne "UseObservedModelFlag" then message, "File corrupted"
    self->setUseObservedModelFlag, fix(dataList)
    return
  endif
  self.useObservedModelFlag=flag
  
END

FUNCTION EntityDisplayInfo::getUseObservedModelFlag, FILEMODE=FILEMODE

  if keyword_set(FILEMODE) then begin
    data=strcompress(fix(self->getUseObservedModelFlag()), /REMOVE)
    record=self.utility->buildFileDataStream('UseObservedModelFlag', data)
    return, record
  endif
  return, self.useObservedModelFlag
  
END

FUNCTION EntityDisplayInfo::getFileFormatVersion

  return, '3.1'
  
END

PRO EntityDisplayInfo::saveData, filename

  ; open file for write
  openw, unit, filename, /GET_LUN
  header1='#EntityDisplayInfo - Save File - Never change order of contents'
  printf, unit, header1
  
  record=self.utility->buildFileDataStream('Version', self->getFileFormatVersion())
  printf, unit, record
  
  ; Added Oct/November 2011 MM
  printf, unit, self->getAllModelsFlag(/FILEMODE)
  ; MM end
  
  ; Addeed Oct/November 2011 MM
  printf, unit, self->getAllObservationsFlag(/FILEMODE)
  ; MM end
  
  ; Replaced Oct/November 2011 MM
  printf, unit, self->getAllScenariosFlag(/FILEMODE)
  ; MM end
  
  ; Parameter section
  ;  data=self->getParameterTypeNames()
  ;  record=self.utility->buildFileDataStream('ParameterTypeNames', data)
  ;printf, unit, record
  
  ;  data=self->getParameterTypeCodes()
  ;  record=self.utility->buildFileDataStream('getParameterTypeCodes', data)
  ;printf, unit, record
  
  ;  data=self->getParameterTypeDescriptions()
  ;  record=self.utility->buildFileDataStream('getParameterTypeDescriptions', data)
  ;printf, unit, record
  
  ;  data=strcompress(self->getParameterTypeSelections(), /REMOVE)
  ;  record=self.utility->buildFileDataStream('ParameterTypeSelections', data)
  ;  printf, unit, record
  ; Replaced 2 Oct 2011 MM
  printf, unit, self->getParameterTypeSelections(/FILEMODE)
  ; MM end
  ;  data=self->getParameterNames()
  ;  record=self.utility->buildFileDataStream('getParameterNames', data)
  ;printf, unit, record
  
  ;  data=self->getParameterCodes()
  ;  record=self.utility->buildFileDataStream('getParameterCodes', data)
  ;printf, unit, record
  
  ;  data=self->getParameterTypes()
  ;  record=self.utility->buildFileDataStream('getParameterTypes', data)
  ;printf, unit, record
  
  ;  data=self->getParameterMeasureUnits()
  ;  record=self.utility->buildFileDataStream('getParameterMeasureUnits', data)
  ;printf, unit, record
  
  ;  data=self->getParameterDescriptions()
  ;  record=self.utility->buildFileDataStream('getParameterDescriptions', data)
  ;printf, unit, record
  
  ;  data=self->getParameterObservedCodes()
  ;record=self.utility->buildFileDataStream('getParameterObservedCodes', data)
  ;printf, unit, record
  
  ;  data=strcompress(self->getParameterSelections(), /REMOVE)
  ;  record=self.utility->buildFileDataStream('ParameterSelections', data)
  ;  printf, unit, record
  ; Replaced 2 Oct 2011 MM
  ; Replaced 10 Mar 2012 MM (Now Version is 3.1)
  ;printf, unit, self->getParameterSelections(/FILEMODE)
  printf, unit, self->getParameterCodeSelections(/FILEMODE)
  ; MM end
  ; MM end
  ; Run section
  
  ;  data=self->getrunNames()
  ;  record=self.utility->buildFileDataStream('getrunNames', data)
  ;printf, unit, record
  
  ;  data=self->getrunCodes()
  ;  record=self.utility->buildFileDataStream('getrunCodes', data)
  ;printf, unit, record
  
  ;  data=self->getrunDescriptions()
  ;  record=self.utility->buildFileDataStream('getrunDescriptions', data)
  ;printf, unit, record
  
  ;  data=strcompress(self->getRunSelections(), /REMOVE)
  ;  record=self.utility->buildFileDataStream('RunSelections', data)
  ;  printf, unit, record
  ; Replaced 2 Oct 2011 MM
  printf, unit, self->getRunSelections(/FILEMODE)
  ; MM end
  
  ;  data=self->getrunQueryCodes()
  ;  record=self.utility->buildFileDataStream('getrunQueryCodes', data)
  ;printf, unit, record
  
  ;  data=self->getrunScenarioCodes()
  ;  record=self.utility->buildFileDataStream('getrunScenarioCodes', data)
  ;printf, unit, record
  
  ;  data=self->getrunModelCodes()
  ;  record=self.utility->buildFileDataStream('getrunModelCodes', data)
  ;printf, unit, record
  ; Scenario section
  
  ;  data=self->getscenarioNames()
  ;  record=self.utility->buildFileDataStream('getscenarioNames', data)
  ;printf, unit, record
  
  ;  data=self->getscenarioCodes()
  ;  record=self.utility->buildFileDataStream('getscenarioCodes', data)
  ;printf, unit, record
  
  ;  data=self->getscenarioDescriptions()
  ;  record=self.utility->buildFileDataStream('getscenarioDescriptions', data)
  ;printf, unit, record
  
  ;  data=strcompress(self->getScenarioSelections(), /REMOVE)
  ;  record=self.utility->buildFileDataStream('ScenarioSelections', data)
  ;  printf, unit, record
  ; Replaced 2 Oct 2011 MM
  printf, unit, self->getScenarioSelections(/FILEMODE)
  ; MM end
  ; Model section
  
  ;  data=self->getmodelNames()
  ;  record=self.utility->buildFileDataStream('getmodelNames', data)
  ;printf, unit, record
  
  ;  data=self->getmodelCodes()
  ;  record=self.utility->buildFileDataStream('getmodelCodes', data)
  ;printf, unit, record
  
  ;  data=self->getmodelDescriptions()
  ;  record=self.utility->buildFileDataStream('getmodelDescriptions', data)
  ;printf, unit, record
  
  ;  data=strcompress(self->getModelSelections(), /REMOVE)
  ;  record=self.utility->buildFileDataStream('ModelSelections', data)
  ;  printf, unit, record
  ; Replaced 2 Oct 2011 MM
  printf, unit, self->getModelSelections(/FILEMODE)
  ; MM end
  
  ; Category section
  ;data=self->getcategoryPresenceFlags()
  ;record=self.utility->buildFileDataStream('getcategoryPresenceFlags', data)
  ;printf, unit, record
  
  ;  data=self->getcategoryCodes()
  ;  record=self.utility->buildFileDataStream('getcategoryCodes', data)
  ;printf, unit, record
  
  ;  data=self->getcategoryTitles()
  ;  record=self.utility->buildFileDataStream('getcategoryTitles', data)
  ;printf, unit, record
  
  ;data=self->getcategoryValues()
  ;record=self.utility->buildFileDataStream('getcategoryValues', data)
  ;printf, unit, record
  
  ;data=self->getcategoryWidgetTypes()
  ;record=self.utility->buildFileDataStream('getcategoryWidgetTypes', data)
  ;printf, unit, record
  
  ;  data=strcompress(self->getCategorySelections(), /REMOVE)
  ;  record=self.utility->buildFileDataStream('CategorySelections', data)
  ;  printf, unit, record
  ; Replaced 2 Oct 2011 MM
  printf, unit, self->getCategorySelections(/FILEMODE)
  ; MM end
  
  ; Obs section
  ;  data=self->getobsCatCategoryCodes()
  ;  record=self.utility->buildFileDataStream('getobsCatCategoryCodes', data)
  ;printf, unit, record
  
  ;  data=self->getobsCatObservedCodes()
  ;  record=self.utility->buildFileDataStream('getobsCatObservedCodes', data)
  ;printf, unit, record
  
  ;  data=self->getobsCatValues()
  ;  record=self.utility->buildFileDataStream('getobsCatValues', data)
  ;printf, unit, record
  
  ;  data=self->getobservedNames()
  ;  record=self.utility->buildFileDataStream('getobservedNames', data)
  ;printf, unit, record
  
  ;  data=self->getobservedShortNames()
  ;  record=self.utility->buildFileDataStream('getobservedShortNames', data)
  ;printf, unit, record
  
  ;  data=self->getobservedCodes()
  ;  record=self.utility->buildFileDataStream('getobservedCodes', data)
  ;printf, unit, record
  
  ;  data=self->getObservedQueryCodesSelections()
  ;  record=self.utility->buildFileDataStream('ObservedQueryCodesSelections', data)
  ;  printf, unit, record
  ; Replaced 2 Oct 2011 MM
  printf, unit, self->getObservedQueryCodesSelections(/FILEMODE)
  ; MM end
  
  ;  data=self->getObservedCodesSelections()
  ;  record=self.utility->buildFileDataStream('ObservedCodesSelections', data)
  ;  printf, unit, record
  ; Replaced 2 Oct 2011 MM
  printf, unit, self->getObservedCodesSelections(/FILEMODE)
  ; MM end
  
  ;  data=strcompress(self->getObservedGroupNames(), /REMOVE)
  ;  record=self.utility->buildFileDataStream('ObservedGroupNames', data)
  ;  printf, unit, record
  ; Replaced 2 Oct 2011 MM
  printf, unit, self->getObservedGroupTitles(/FILEMODE)
  ; MM end
  
  ;  data=strcompress(self->getObservedCodesGroupSelections(), /REMOVE)
  ;  record=self.utility->buildFileDataStream('ObservedCodesGroupSelections', data)
  ;  printf, unit, record
  ; Replaced 2 Oct 2011 MM
  printf, unit, self->getObservedCodesGroupSelections(/FILEMODE)
  ; MM end
  
  ;  data=strcompress(fix(self->getUseObservedModelFlag()), /REMOVE)
  ;  record=self.utility->buildFileDataStream('UseObservedModelFlag', data)
  ;  printf, unit, record
  ; Replaced 2 Oct 2011 MM
  printf, unit, self->getUseObservedModelFlag(/FILEMODE)
  ; MM end
  
  ;  data=strcompress(self->getMonitoringGroupStatSelection(), /REMOVE)
  ;  record=self.utility->buildFileDataStream('MonitoringGroupStatSelection', data)
  ;  printf, unit, record
  ; Replaced 2 Oct 2011 MM
  printf, unit, self->getObservedGroupStatCodeSelection(/FILEMODE)
  ; MM end
  
  close, unit
  free_lun, unit
  
END

PRO EntityDisplayInfo::saveDataVerTwoDotZero, filename

  ; open file for write
  openw, unit, filename, /GET_LUN
  header1='#EntityDisplayInfo - Save File - Never change order of contents'
  printf, unit, header1
  
  record=self.utility->buildFileDataStream('Version', self->getFileFormatVersion())
  printf, unit, record
  
  ; Parameter section
  ;  data=self->getParameterTypeNames()
  ;  record=self.utility->buildFileDataStream('ParameterTypeNames', data)
  ;printf, unit, record
  
  ;  data=self->getParameterTypeCodes()
  ;  record=self.utility->buildFileDataStream('getParameterTypeCodes', data)
  ;printf, unit, record
  
  ;  data=self->getParameterTypeDescriptions()
  ;  record=self.utility->buildFileDataStream('getParameterTypeDescriptions', data)
  ;printf, unit, record
  
  ;  data=strcompress(self->getParameterTypeSelections(), /REMOVE)
  ;  record=self.utility->buildFileDataStream('ParameterTypeSelections', data)
  ;  printf, unit, record
  ; Replaced 2 Oct 2011 MM
  printf, unit, self->getParameterTypeSelections(/FILEMODE)
  ; MM end
  ;  data=self->getParameterNames()
  ;  record=self.utility->buildFileDataStream('getParameterNames', data)
  ;printf, unit, record
  
  ;  data=self->getParameterCodes()
  ;  record=self.utility->buildFileDataStream('getParameterCodes', data)
  ;printf, unit, record
  
  ;  data=self->getParameterTypes()
  ;  record=self.utility->buildFileDataStream('getParameterTypes', data)
  ;printf, unit, record
  
  ;  data=self->getParameterMeasureUnits()
  ;  record=self.utility->buildFileDataStream('getParameterMeasureUnits', data)
  ;printf, unit, record
  
  ;  data=self->getParameterDescriptions()
  ;  record=self.utility->buildFileDataStream('getParameterDescriptions', data)
  ;printf, unit, record
  
  ;  data=self->getParameterObservedCodes()
  ;record=self.utility->buildFileDataStream('getParameterObservedCodes', data)
  ;printf, unit, record
  
  ;  data=strcompress(self->getParameterSelections(), /REMOVE)
  ;  record=self.utility->buildFileDataStream('ParameterSelections', data)
  ;  printf, unit, record
  ; Replaced 2 Oct 2011 MM
  printf, unit, self->getParameterSelections(/FILEMODE)
  ; MM end
  ; Run section
  
  ;  data=self->getrunNames()
  ;  record=self.utility->buildFileDataStream('getrunNames', data)
  ;printf, unit, record
  
  ;  data=self->getrunCodes()
  ;  record=self.utility->buildFileDataStream('getrunCodes', data)
  ;printf, unit, record
  
  ;  data=self->getrunDescriptions()
  ;  record=self.utility->buildFileDataStream('getrunDescriptions', data)
  ;printf, unit, record
  
  ;  data=strcompress(self->getRunSelections(), /REMOVE)
  ;  record=self.utility->buildFileDataStream('RunSelections', data)
  ;  printf, unit, record
  ; Replaced 2 Oct 2011 MM
  printf, unit, self->getRunSelections(/FILEMODE)
  ; MM end
  
  ;  data=self->getrunQueryCodes()
  ;  record=self.utility->buildFileDataStream('getrunQueryCodes', data)
  ;printf, unit, record
  
  ;  data=self->getrunScenarioCodes()
  ;  record=self.utility->buildFileDataStream('getrunScenarioCodes', data)
  ;printf, unit, record
  
  ;  data=self->getrunModelCodes()
  ;  record=self.utility->buildFileDataStream('getrunModelCodes', data)
  ;printf, unit, record
  ; Scenario section
  
  ;  data=self->getscenarioNames()
  ;  record=self.utility->buildFileDataStream('getscenarioNames', data)
  ;printf, unit, record
  
  ;  data=self->getscenarioCodes()
  ;  record=self.utility->buildFileDataStream('getscenarioCodes', data)
  ;printf, unit, record
  
  ;  data=self->getscenarioDescriptions()
  ;  record=self.utility->buildFileDataStream('getscenarioDescriptions', data)
  ;printf, unit, record
  
  ;  data=strcompress(self->getScenarioSelections(), /REMOVE)
  ;  record=self.utility->buildFileDataStream('ScenarioSelections', data)
  ;  printf, unit, record
  ; Replaced 2 Oct 2011 MM
  printf, unit, self->getScenarioSelections(/FILEMODE)
  ; MM end
  ; Model section
  
  ;  data=self->getmodelNames()
  ;  record=self.utility->buildFileDataStream('getmodelNames', data)
  ;printf, unit, record
  
  ;  data=self->getmodelCodes()
  ;  record=self.utility->buildFileDataStream('getmodelCodes', data)
  ;printf, unit, record
  
  ;  data=self->getmodelDescriptions()
  ;  record=self.utility->buildFileDataStream('getmodelDescriptions', data)
  ;printf, unit, record
  
  ;  data=strcompress(self->getModelSelections(), /REMOVE)
  ;  record=self.utility->buildFileDataStream('ModelSelections', data)
  ;  printf, unit, record
  ; Replaced 2 Oct 2011 MM
  printf, unit, self->getModelSelections(/FILEMODE)
  ; MM end
  
  ; Category section
  ;data=self->getcategoryPresenceFlags()
  ;record=self.utility->buildFileDataStream('getcategoryPresenceFlags', data)
  ;printf, unit, record
  
  ;  data=self->getcategoryCodes()
  ;  record=self.utility->buildFileDataStream('getcategoryCodes', data)
  ;printf, unit, record
  
  ;  data=self->getcategoryTitles()
  ;  record=self.utility->buildFileDataStream('getcategoryTitles', data)
  ;printf, unit, record
  
  ;data=self->getcategoryValues()
  ;record=self.utility->buildFileDataStream('getcategoryValues', data)
  ;printf, unit, record
  
  ;data=self->getcategoryWidgetTypes()
  ;record=self.utility->buildFileDataStream('getcategoryWidgetTypes', data)
  ;printf, unit, record
  
  ;  data=strcompress(self->getCategorySelections(), /REMOVE)
  ;  record=self.utility->buildFileDataStream('CategorySelections', data)
  ;  printf, unit, record
  ; Replaced 2 Oct 2011 MM
  printf, unit, self->getCategorySelections(/FILEMODE)
  ; MM end
  
  ; Obs section
  ;  data=self->getobsCatCategoryCodes()
  ;  record=self.utility->buildFileDataStream('getobsCatCategoryCodes', data)
  ;printf, unit, record
  
  ;  data=self->getobsCatObservedCodes()
  ;  record=self.utility->buildFileDataStream('getobsCatObservedCodes', data)
  ;printf, unit, record
  
  ;  data=self->getobsCatValues()
  ;  record=self.utility->buildFileDataStream('getobsCatValues', data)
  ;printf, unit, record
  
  ;  data=self->getobservedNames()
  ;  record=self.utility->buildFileDataStream('getobservedNames', data)
  ;printf, unit, record
  
  ;  data=self->getobservedShortNames()
  ;  record=self.utility->buildFileDataStream('getobservedShortNames', data)
  ;printf, unit, record
  
  ;  data=self->getobservedCodes()
  ;  record=self.utility->buildFileDataStream('getobservedCodes', data)
  ;printf, unit, record
  
  ;  data=self->getObservedQueryCodesSelections()
  ;  record=self.utility->buildFileDataStream('ObservedQueryCodesSelections', data)
  ;  printf, unit, record
  ; Replaced 2 Oct 2011 MM
  printf, unit, self->getObservedQueryCodesSelections(/FILEMODE)
  ; MM end
  
  ;  data=self->getObservedCodesSelections()
  ;  record=self.utility->buildFileDataStream('ObservedCodesSelections', data)
  ;  printf, unit, record
  ; Replaced 2 Oct 2011 MM
  printf, unit, self->getObservedCodesSelections(/FILEMODE)
  ; MM end
  
  ;  data=strcompress(self->getObservedGroupNames(), /REMOVE)
  ;  record=self.utility->buildFileDataStream('ObservedGroupNames', data)
  ;  printf, unit, record
  ; Replaced 2 Oct 2011 MM
  printf, unit, self->getObservedGroupTitles(/FILEMODE)
  ; MM end
  
  ;  data=strcompress(self->getObservedCodesGroupSelections(), /REMOVE)
  ;  record=self.utility->buildFileDataStream('ObservedCodesGroupSelections', data)
  ;  printf, unit, record
  ; Replaced 2 Oct 2011 MM
  printf, unit, self->getObservedCodesGroupSelections(/FILEMODE)
  ; MM end
  
  ;  data=strcompress(fix(self->getUseObservedModelFlag()), /REMOVE)
  ;  record=self.utility->buildFileDataStream('UseObservedModelFlag', data)
  ;  printf, unit, record
  ; Replaced 2 Oct 2011 MM
  printf, unit, self->getUseObservedModelFlag(/FILEMODE)
  ; MM end
  
  ;  data=strcompress(self->getMonitoringGroupStatSelection(), /REMOVE)
  ;  record=self.utility->buildFileDataStream('MonitoringGroupStatSelection', data)
  ;  printf, unit, record
  ; Replaced 2 Oct 2011 MM
  printf, unit, self->getObservedGroupStatCodeSelection(/FILEMODE)
  ; MM end
  
  close, unit
  free_lun, unit
  
END

FUNCTION EntityDisplayInfo::restoreData, filename

  ; open file for read
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
  
  ;  readf, unit, bufferString
  ;  ;record=self.utility->buildFileDataStream('getParameterTypeSelections', data)
  ;  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  ;  self->setParameterTypeSelections, long(dataList)
  
  ; Added Oct/November 2011 MM
  readf, unit, bufferString
  self->setAllModelsFlag, bufferString, /FILEMODE
  ; MM end
  
  ; Added Oct/November 2011 MM
  readf, unit, bufferString
  self->setAllObservationsFlag, bufferString, /FILEMODE
  ; MM end
  
  ; Added Oct/November 2011 MM
  readf, unit, bufferString
  self->setAllScenariosFlag, bufferString, /FILEMODE
  ; MM end
  
  ; Replaced 2 Oct 2011 MM
  readf, unit, bufferString
  self->setParameterTypeSelections, bufferString, /FILEMODE
  ; MM end
  
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
  
  ;  readf, unit, bufferString
  ;  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  ;  self->setParameterSelections, long(dataList)
  ;
  ; Replaced 2 Oct 2011 MM
  ; Replaced 10 Mar 2012 MM (Now Version is 3.1)
  readf, unit, bufferString
  self->setParameterCodeSelections, bufferString, /FILEMODE
  ; MM end
  ; MM end
  
  ;readf, unit, bufferString
  ;  data=self->getrunNames()
  ;  record=self.utility->buildFileDataStream('getrunNames', data)
  
  ;readf, unit, bufferString
  ;  data=self->getrunCodes()
  ;  record=self.utility->buildFileDataStream('getrunCodes', data)
  
  ;readf, unit, bufferString
  ;  data=self->getrunDescriptions()
  ;  record=self.utility->buildFileDataStream('getrunDescriptions', data)
  
  ;  readf, unit, bufferString
  ;  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  ;  ptr_free, self.runSelections
  ;  if dataList[0] ne '-1' then self->setRunSelections, long(dataList)
  ;  ;record=self.utility->buildFileDataStream('getrunSelections', data)
  ; Replaced 2 Oct 2011 MM
  readf, unit, bufferString
  self->setRunSelections, bufferString, /FILEMODE
  ; MM end
  
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
  
  ;  readf, unit, bufferString
  ;  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  ;  self->setScenarioSelections, long(dataList)
  ;  ;record=self.utility->buildFileDataStream('getscenarioSelections', data)
  ; Model section
  ; Replaced 2 Oct 2011 MM
  readf, unit, bufferString
  self->setScenarioSelections, bufferString, /FILEMODE
  ; MM end
  
  ;readf, unit, bufferString
  ;  data=self->getmodelNames()
  ;  record=self.utility->buildFileDataStream('getmodelNames', data)
  
  ;readf, unit, bufferString
  ;  data=self->getmodelCodes()
  ;  record=self.utility->buildFileDataStream('getmodelCodes', data)
  
  ;readf, unit, bufferString
  ;  data=self->getmodelDescriptions()
  ;  record=self.utility->buildFileDataStream('getmodelDescriptions', data)
  
  ;  readf, unit, bufferString
  ;  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  ;  self->setModelSelections, long(dataList)
  ;  ;record=self.utility->buildFileDataStream('getmodelSelections', data)
  ; Replaced 2 Oct 2011 MM
  readf, unit, bufferString
  self->setModelSelections, bufferString, /FILEMODE
  ; MM end
  
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
  
  ;  readf, unit, bufferString
  ;  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  ;  self->setCategorySelections, long(dataList)
  ;  ;record=self.utility->buildFileDataStream('getcategorySelections', data)
  ; Replaced 2 Oct 2011 MM
  readf, unit, bufferString
  self->setCategorySelections, bufferString, /FILEMODE
  ; MM end
  
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
  ; Replaced 2 Oct 2011 MM
  readf, unit, bufferString
  self->setObservedQueryCodesSelections, bufferString, /FILEMODE
  ; MM end
  
  ;  readf, unit, bufferString
  ;  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  ;  ptr_free, self.observedCodesSelections
  ;  if dataList[0] ne '-1' then self->setObservedCodesSelections, dataList
  ;  ;record=self.utility->buildFileDataStream('getobservedCodesSelections', data)
  ; Replaced 2 Oct 2011 MM
  readf, unit, bufferString
  self->setObservedCodesSelections, bufferString, /FILEMODE
  ; MM end
  
  ;  readf, unit, bufferString
  ;  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  ;  if dataList[0] ne '-1' then self->setObservedGroupNames, dataList
  ;  ;record=self.utility->buildFileDataStream('getobservedGroupNames', data)
  ; Replaced 2 Oct 2011 MM
  readf, unit, bufferString
  self->setObservedGroupTitles, bufferString, /FILEMODE
  ; MM end
  
  ;  readf, unit, bufferString
  ;  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  ;  if dataList[0] ne '-1' then self->setObservedCodesGroupSelections, dataList
  ;  ;record=self.utility->buildFileDataStream('getobservedCodesGroupSelections', data)
  ; Replaced 2 Oct 2011 MM
  readf, unit, bufferString
  ;ptr_free, self.observedCodesGroupSelections
  self->setObservedCodesGroupSelections, bufferString, /FILEMODE
  ; MM end
  
  ;  readf, unit, bufferString
  ;  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  ;  self->setUseObservedModelFlag, fix(dataList)
  ;  ;record=self.utility->buildFileDataStream('getuseObservedModelFlag', data)
  ; Replaced 2 Oct 2011 MM
  readf, unit, bufferString
  self->setUseObservedModelFlag, bufferString, /FILEMODE
  ; MM end
  
  ;  readf, unit, bufferString
  ;  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  ;  self->setMonitoringGroupStatSelection, fix(dataList)
  ; Replaced 2 Oct 2011 MM
  readf, unit, bufferString
  self->setObservedGroupStatCodeSelection, bufferString, /FILEMODE
  ; MM end
  
  close, unit
  free_lun, unit
  return, 1
  
END

FUNCTION EntityDisplayInfo::restoreDataVerTwoDotZero, filename

  ; open file for read
  ERROR=0
  catch, error_status
  
  if error_status NE 0 THEN BEGIN
    ERROR=1
    catch, /CANCEL & close, /ALL
    errMsg=dialog_message('problem with file: <'+fileName+'> check existence, read or file version permission.', /ERROR)
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
  
  ;  readf, unit, bufferString
  ;  ;record=self.utility->buildFileDataStream('getParameterTypeSelections', data)
  ;  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  ;  self->setParameterTypeSelections, long(dataList)
  ; Replaced 2 Oct 2011 MM
  readf, unit, bufferString
  self->setParameterTypeSelections, bufferString, /FILEMODE
  ; MM end
  
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
  
  ;  readf, unit, bufferString
  ;  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  ;  self->setParameterSelections, long(dataList)
  ;
  ; Replaced 2 Oct 2011 MM
  readf, unit, bufferString
  self->setParameterSelections, bufferString, /FILEMODE
  ; MM end
  
  ;readf, unit, bufferString
  ;  data=self->getrunNames()
  ;  record=self.utility->buildFileDataStream('getrunNames', data)
  
  ;readf, unit, bufferString
  ;  data=self->getrunCodes()
  ;  record=self.utility->buildFileDataStream('getrunCodes', data)
  
  ;readf, unit, bufferString
  ;  data=self->getrunDescriptions()
  ;  record=self.utility->buildFileDataStream('getrunDescriptions', data)
  
  ;  readf, unit, bufferString
  ;  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  ;  ptr_free, self.runSelections
  ;  if dataList[0] ne '-1' then self->setRunSelections, long(dataList)
  ;  ;record=self.utility->buildFileDataStream('getrunSelections', data)
  ; Replaced 2 Oct 2011 MM
  readf, unit, bufferString
  self->setRunSelections, bufferString, /FILEMODE
  ; MM end
  
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
  
  ;  readf, unit, bufferString
  ;  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  ;  self->setScenarioSelections, long(dataList)
  ;  ;record=self.utility->buildFileDataStream('getscenarioSelections', data)
  ; Model section
  ; Replaced 2 Oct 2011 MM
  readf, unit, bufferString
  self->setScenarioSelections, bufferString, /FILEMODE
  ; MM end
  
  ;readf, unit, bufferString
  ;  data=self->getmodelNames()
  ;  record=self.utility->buildFileDataStream('getmodelNames', data)
  
  ;readf, unit, bufferString
  ;  data=self->getmodelCodes()
  ;  record=self.utility->buildFileDataStream('getmodelCodes', data)
  
  ;readf, unit, bufferString
  ;  data=self->getmodelDescriptions()
  ;  record=self.utility->buildFileDataStream('getmodelDescriptions', data)
  
  ;  readf, unit, bufferString
  ;  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  ;  self->setModelSelections, long(dataList)
  ;  ;record=self.utility->buildFileDataStream('getmodelSelections', data)
  ; Replaced 2 Oct 2011 MM
  readf, unit, bufferString
  self->setModelSelections, bufferString, /FILEMODE
  ; MM end
  
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
  
  ;  readf, unit, bufferString
  ;  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  ;  self->setCategorySelections, long(dataList)
  ;  ;record=self.utility->buildFileDataStream('getcategorySelections', data)
  ; Replaced 2 Oct 2011 MM
  readf, unit, bufferString
  self->setCategorySelections, bufferString, /FILEMODE
  ; MM end
  
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
  ; Replaced 2 Oct 2011 MM
  readf, unit, bufferString
  self->setObservedQueryCodesSelections, bufferString, /FILEMODE
  ; MM end
  
  ;  readf, unit, bufferString
  ;  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  ;  ptr_free, self.observedCodesSelections
  ;  if dataList[0] ne '-1' then self->setObservedCodesSelections, dataList
  ;  ;record=self.utility->buildFileDataStream('getobservedCodesSelections', data)
  ; Replaced 2 Oct 2011 MM
  readf, unit, bufferString
  self->setObservedCodesSelections, bufferString, /FILEMODE
  ; MM end
  
  ;  readf, unit, bufferString
  ;  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  ;  if dataList[0] ne '-1' then self->setObservedGroupNames, dataList
  ;  ;record=self.utility->buildFileDataStream('getobservedGroupNames', data)
  ; Replaced 2 Oct 2011 MM
  readf, unit, bufferString
  self->setObservedGroupTitles, bufferString, /FILEMODE
  ; MM end
  
  ;  readf, unit, bufferString
  ;  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  ;  if dataList[0] ne '-1' then self->setObservedCodesGroupSelections, dataList
  ;  ;record=self.utility->buildFileDataStream('getobservedCodesGroupSelections', data)
  ; Replaced 2 Oct 2011 MM
  readf, unit, bufferString
  ;ptr_free, self.observedCodesGroupSelections
  self->setObservedCodesGroupSelections, bufferString, /FILEMODE
  ; MM end
  
  ;  readf, unit, bufferString
  ;  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  ;  self->setUseObservedModelFlag, fix(dataList)
  ;  ;record=self.utility->buildFileDataStream('getuseObservedModelFlag', data)
  ; Replaced 2 Oct 2011 MM
  readf, unit, bufferString
  self->setUseObservedModelFlag, bufferString, /FILEMODE
  ; MM end
  
  ;  readf, unit, bufferString
  ;  self.utility->convertStreamDataFile, bufferString, dataName, dataList
  ;  self->setMonitoringGroupStatSelection, fix(dataList)
  ; Replaced 2 Oct 2011 MM
  readf, unit, bufferString
  self->setObservedGroupStatCodeSelection, bufferString, /FILEMODE
  ; MM end
  
  close, unit
  free_lun, unit
  return, 1
  
END

FUNCTION EntityDisplayInfo::clone, super, DEEP=DEEP

  if (n_elements(super) eq 1) then clone=super else clone=obj_new('EntityDisplayInfo')
  if keyword_set(DEEP) then begin
    ; new
    if ptr_valid(self.parameterTypeNames) then begin
      list=*self.parameterTypeNames
      clone.parameterTypeNames=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.parameterTypeCodes) then begin
      list=*self.parameterTypeCodes
      clone.parameterTypeCodes=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.parameterTypeDescriptions) then begin
      list=*self.parameterTypeDescriptions
      clone.parameterTypeDescriptions=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.parameterTypeSelections) then begin
      list=*self.parameterTypeSelections
      clone.parameterTypeSelections=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.parameterNames) then begin
      list=*self.parameterNames
      clone.parameterNames=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.observedGroupStatCodes) then begin
      list=*self.observedGroupStatCodes
      clone.observedGroupStatCodes=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.observedGroupStatNames) then begin
      list=*self.observedGroupStatNames
      clone.observedGroupStatNames=ptr_new(list, /NO_COPY)
    endif
;    if ptr_valid(self.parameterSelections) then begin
;      list=*self.parameterSelections
;      clone.parameterSelections=ptr_new(list, /NO_COPY)
;    endif
    if ptr_valid(self.parameterCodeSelections) then begin
      list=*self.parameterCodeSelections
      clone.parameterCodeSelections=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.parameterDescriptions) then begin
      list=*self.parameterDescriptions
      clone.parameterDescriptions=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.parameterMeasureUnits) then begin
      list=*self.parameterMeasureUnits
      clone.parameterMeasureUnits=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.parameterCodes) then begin
      list=*self.parameterCodes
      clone.parameterCodes=ptr_new(list, /NO_COPY)
    endif
    ; end
    if ptr_valid(self.parameterTypes) then begin
      list=*self.parameterTypes
      clone.parameterTypes=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid (self.scenarioNames) then begin
      list=*self.scenarioNames
      clone.scenarioNames=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid (self.scenarioCodes) then begin
      list=*self.scenarioCodes
      clone.scenarioCodes=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid (self.scenarioDescriptions) then begin
      list=*self.scenarioDescriptions
      clone.scenarioDescriptions=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid (self.scenarioSelections) then begin
      list=*self.scenarioSelections
      clone.scenarioSelections=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid (self.modelNames) then begin
      list=*self.modelNames
      clone.modelNames=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid (self.modelCodes) then begin
      list=*self.modelCodes
      clone.modelCodes=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid (self.modelDescriptions) then begin
      list=*self.modelDescriptions
      clone.modelDescriptions=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid (self.modelSelections) then begin
      list=*self.modelSelections
      clone.modelSelections=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid (self.observedNames) then begin
      list=*self.observedNames
      clone.observedNames=ptr_new(list, /NO_COPY)
    endif
    ; march 15 MM
    if ptr_valid (self.observedLatitudes) then begin
      list=*self.observedLatitudes
      clone.observedLatitudes=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid (self.observedLongitudes) then begin
      list=*self.observedLongitudes
      clone.observedLongitudes=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid (self.observedAltitudes) then begin
      list=*self.observedAltitudes
      clone.observedAltitudes=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid (self.observedCountries) then begin
      list=*self.observedCountries
      clone.observedCountries=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid (self.observedGMTs) then begin
      list=*self.observedGMTs
      clone.observedGMTs=ptr_new(list, /NO_COPY)
    endif
    ;
    if ptr_valid (self.observedShortNames) then begin
      list=*self.observedShortNames
      clone.observedShortNames=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid (self.observedCodes) then begin
      list=*self.observedCodes
      clone.observedCodes=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid (self.observedQueryCodesSelections) then begin
      list=*self.observedQueryCodesSelections
      clone.observedQueryCodesSelections=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid (self.observedCodesSelections) then begin
      list=*self.observedCodesSelections
      clone.observedCodesSelections=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid (self.observedGroupTitles) then begin
      list=*self.observedGroupTitles
      clone.observedGroupTitles=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid (self.observedCodesGroupSelections) then begin
      list=*self.observedCodesGroupSelections
      ptrs=ptrarr(n_elements(list))
      for i=0, n_elements(list)-1 do begin
        thisList=*list[i]
        ptrs[i]=ptr_new(thisList, /NO_COPY)
      endfor
      clone.observedCodesGroupSelections=ptr_new(ptrs, /NO_COPY)
    endif
    if ptr_valid (self.parameterObservedCodes) then begin
      list=*self.parameterObservedCodes
      ptrs=ptrarr(n_elements(list))
      for i=0, n_elements(list)-1 do begin
        thisList=*list[i]
        ptrs[i]=ptr_new(thisList, /NO_COPY)
      endfor
      clone.parameterObservedCodes=ptr_new(ptrs, /NO_COPY)
    endif
    for i=0, n_elements(self.categoryValues)-1 do begin
      if ptr_valid(self.categoryValues[i]) then begin
        list=*self.categoryValues[i]
        clone.categoryValues[i]=ptr_new(list, /NO_COPY)
      endif
    endfor
    if ptr_valid (self.runQueryCodes) then begin
      list=*self.runQueryCodes
      clone.runQueryCodes=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid (self.runNames) then begin
      list=*self.runNames
      clone.runNames=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid (self.runCodes) then begin
      list=*self.runCodes
      clone.runCodes=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid (self.runDescriptions) then begin
      list=*self.runDescriptions
      clone.runDescriptions=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid (self.runSelections) then begin
      list=*self.runSelections
      clone.runSelections=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid (self.runScenarioCodes) then begin
      list=*self.runScenarioCodes
      clone.runScenarioCodes=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid (self.runModelCodes) then begin
      list=*self.runModelCodes
      clone.runModelCodes=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid (self.obsCatCategoryCodes) then begin
      list=*self.obsCatCategoryCodes
      clone.obsCatCategoryCodes=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid (self.obsCatObservedCodes) then begin
      list=*self.obsCatObservedCodes
      clone.obsCatObservedCodes=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid (self.obsCatValues) then begin
      list=*self.obsCatValues
      clone.obsCatValues=ptr_new(list, /NO_COPY)
    endif
  endif else begin
    clone.parameterTypeNames=self.parameterTypeNames
    clone.parameterTypeCodes=self.parameterTypeCodes
    clone.parameterTypeDescriptions=self.parameterTypeDescriptions
    clone.parameterTypeSelections=self.parameterTypeSelections
    clone.parameterNames=self.parameterNames
    clone.parameterCodes=self.parameterCodes
    clone.parameterMeasureUnits=self.parameterMeasureUnits
    clone.parameterDescriptions=self.parameterDescriptions
    ;clone.parameterSelections=self.parametersSelections
    clone.parameterCodeSelections=self.parametersCodeSelections
    clone.parameterTypes=self.parameterTypes
    clone.parameterObservedCodes=self.parameterObservedCodes
    clone.scenarioNames=self.scenarioNames
    clone.scenarioCodes=self.scenarioCodes
    clone.scenarioDescriptions=self.scenarioDescrs
    clone.scenarioSelections=self.scenarioSelections
    clone.modelNames=self.modelNames
    clone.modelCodes=self.modelCodes
    clone.modelDescriptions=self.modelDescriptions
    clone.modelSelections=self.modelSelections
    clone.observedNames=self.observedNames
    clone.observedShortNames=self.observedShortNames
    clone.observedLatitudes=self.observedLatitudes
    clone.observedLongitudes=self.observedLongitudes
    clone.observedAltitudes=self.observedAltitudes
    clone.observedCountries=self.observedCountries
    clone.observedGMTs=self.observedGMTs
    clone.observedCodes=self.observedCodes
    clone.observedQueryCodesSelections=self.observedQueryCodesSelections
    clone.observedCodesSelections=self.observedCodesSelections
    clone.observedGroupTitles=self.observedGroupTitles
    clone.observedCodesGroupSelections=clone.observedCodesGroupSelections
    clone.obsCatCategoryCode=self.obsCatCategoryCodes
    clone.obsCatObservedCodes=self.obsCatObservedCodes
    clone.obsCatValues=self.obsCatValues
    clone.categoryValues=self.categoryValues
    clone.runQueryCodes=self.runQueryCodes
    clone.runNames=self.runNames
    clone.runCodes=self.runCodes
    clone.runDescriptions=self.runDescriptions
    clone.runSelections=self.runSelections
    clone.runScenarioCodes=self.runScenarioCodes
    clone.runModelCodes=self.runModelCodes
    clone.observedGroupStatNames=self.observedGroupStatNames
    clone.observedGroupStatCodes=self.observedGroupStatCodes
  endelse
  clone.allObservationsFlag=self.allObservationsFlag
  clone.allScenariosFlag=self.allScenariosFlag
  clone.allModelsFlag=self.allModelsFlag
  print, clone.allObservationsFlag, clone.allScenariosFlag, clone.allModelsFlag
  clone.categoryCodes=self.categoryCodes
  clone.categoryTitles=self.categoryTitles
  clone.categoryWidgetTypes=self.categoryWidgetTypes
  clone.useObservedModelFlag=self.useObservedModelFlag
  clone.observedGroupStatCodeSelection=self.observedGroupStatCodeSelection
  clone.categoryPresenceFlags=self.categoryPresenceFlags
  clone.categorySelections=self.categorySelections
  return, clone
  
END

FUNCTION EntityDisplayInfo::init

  if not self -> Object :: init() then return , 0
  self.utility=obj_new("FMUtility")
  self.dtu=obj_new("DateTimeUtility")
  return , 1
  
END

PRO EntityDisplayInfo::cleanUp

  ptr_free, self.parameterTypeNames
  ptr_free, self.parameterTypeCodes
  ptr_free, self.parameterTypeDescriptions
  ptr_free, self.parameterTypeSelections
  ptr_free, self.parameterCodes
  ptr_free, self.parameterNames
  ptr_free, self.parameterMeasureUnits
  ptr_free, self.parameterDescriptions
  ptr_free, self.parameterTypes
  ;ptr_free, self.parameterSelections
  ptr_free, self.parameterCodeSelections
  ptr_free, self.parameterObservedCodes
  ptr_free, self.scenarioNames
  ptr_free, self.scenarioCodes
  ptr_free, self.scenarioDescriptions
  ptr_free, self.scenarioSelections
  ptr_free, self.modelNames
  ptr_free, self.modelCodes
  ptr_free, self.modelDescriptions
  ptr_free, self.modelSelections
  ptr_free, self.observedNames
  ptr_free, self.observedShortNames
  ptr_free, self.observedCodes
  ptr_free, self.observedQueryCodesSelections
  ptr_free, self.observedCodesSelections
  ptr_free, self.observedGroupTitles
  self->freeObservedCodesGroupSelections
  ptr_free, self.categoryValues
  ptr_free, self.runQueryCodes
  ptr_free, self.runNames
  ptr_free, self.runCodes
  ptr_free, self.runDescriptions
  ptr_free, self.runSelections
  ptr_free, self.runScenarioCodes
  ptr_free, self.runModelCodes
  ptr_free, self.obsCatCategoryCodes
  ptr_free, self.obsCatObservedCodes
  ptr_free, self.obsCatValues
  ptr_free, self.observedGroupStatCodes
  ptr_free, self.observedGroupStatNames
  obj_destroy, self.utility
  obj_destroy, self.dtu
  self -> Object::cleanUp
  
END

;****************************************************************************************

PRO EntityDisplayInfo__Define

  Struct = { EntityDisplayInfo , $
    parameterTypeNames: ptr_new(), $
    parameterTypeCodes: ptr_new(), $
    parameterTypeDescriptions: ptr_new(), $
    parameterTypeSelections: ptr_new(), $
    parameterNames: ptr_new(), $
    parameterCodes: ptr_new(), $
    parameterTypes: ptr_new(), $
    parameterMeasureUnits: ptr_new(), $
    parameterDescriptions: ptr_new(), $
    parameterObservedCodes: ptr_new(), $ ;All pointer of pointers
    ;parameterSelections: ptr_new(), $
    parameterCodeSelections: ptr_new(), $
    ; end
    runNames: ptr_new(), $
    runCodes: ptr_new(), $
    runDescriptions: ptr_new(), $
    runSelections: ptr_new(), $
    runQueryCodes: ptr_new(), $
    runScenarioCodes: ptr_new(), $
    runModelCodes: ptr_new(), $
    scenarioNames: ptr_new(), $
    scenarioCodes: ptr_new(), $
    scenarioDescriptions: ptr_new(), $
    scenarioSelections: ptr_new(), $
    modelNames: ptr_new(), $
    modelCodes: ptr_new(), $
    modelDescriptions: ptr_new(), $
    modelSelections: ptr_new(), $
    categoryPresenceFlags: bytarr(4), $
    categoryCodes: strarr(4), $
    categoryTitles: strarr(4), $
    categoryValues: ptrarr(4), $
    categoryWidgetTypes: bytarr(4), $ ; 1:Exclusive buttons, 2:Combobox
    categorySelections: strarr(4), $ ; a selected code for each category
    obsCatCategoryCodes: ptr_new(), $
    obsCatObservedCodes: ptr_new(), $
    obsCatValues: ptr_new(), $
    observedNames: ptr_new(), $ ;All
    observedShortNames: ptr_new(), $ ;All
    observedLatitudes: ptr_new(), $ ;All
    observedLongitudes: ptr_new(), $ ;All
    observedAltitudes: ptr_new(), $ ;All
    observedCountries: ptr_new(), $ ;All
    observedGMTs: ptr_new(), $ ;All
    observedCodes: ptr_new(), $ ;All
    observedQueryCodesSelections: ptr_new(), $ ;Fill left side
    observedCodesSelections: ptr_new(), $ ;pointers (to manage single)
    observedGroupTitles: ptr_new(), $ ;store title of "group"
    observedCodesGroupSelections: ptr_new(), $ ;pointer of pointers
    observedGroupStatNames: ptr_new(), $ ;store names of mon group stat selection
    observedGroupStatCodes: ptr_new(), $ ;store codes of mon group stat selection
    observedGroupStatCodeSelection: "", $
    allObservationsFlag: 0b, $
    allScenariosFlag: 0b, $
    allModelsFlag: 0b, $
    utility: obj_new(), $
    dtu: obj_new(), $
    useObservedModelFlag: 0b, $
    Inherits Object $
    }
    
END

;****************************************************************************************
