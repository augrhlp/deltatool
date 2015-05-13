PRO Result::setMultipleDrawMainTitle, value

 self.multipleDrawMainTitle=value

END

FUNCTION Result::getMultipleDrawMainTitle, value

 return, self.multipleDrawMainTitle

END

PRO Result::setGenericPlotInfo, XYs, symbols, colors, legendNames, legendColors, legendSymbols

  obj_destroy, self.genericPlotInfo
  self.genericPlotInfo=obj_new('GenericPlotInfo')
  self.genericPlotInfo->setXYs, XYs
  self.genericPlotInfo->setSymbols, symbols
  self.genericPlotInfo->setColors, colors
  if n_elements(legendNames) gt 0 then begin
    self.genericPlotInfo->setLegendNames, legendNames
    self.genericPlotInfo->setLegendColors, legendColors
    self.genericPlotInfo->setLegendSymbols, legendSymbols
  endif
  
END

FUNCTION Result::getGenericPlotInfo

  return, self.genericPlotInfo
  
END

FUNCTION Result::HaveRun

  return, ptr_valid(self.runStatValues)
  
END

FUNCTION Result::HaveObs

  return, ptr_valid(self.obsStatValues)
  
END

PRO Result::buildBarPlotInfo, request, legoNames, symbolNames

  bpInfo=obj_new('BarPlotInfo')
  
  symbolRefNames=strarr(1, n_elements(symbolNames))
  symbolRefNames[0, *]=symbolNames
  haveRuns=self->HaveRun()
  haveObs=self->HaveObs()
  
  if haveRuns then begin
    runStatValues=self->getRunStatValues()
    runDim=size(runStatValues, /DIM)
    if n_elements(runDim) eq 1 then begin
      symbolRefValues=fltarr(1, n_elements(runStatValues))
      symbolRefValues[0, *]=runStatValues
    endif else begin
      symbolRefValues=runStatValues
    endelse
    nullObs=reform(fltarr(1, n_elements(legoNames)), 1, n_elements(legoNames))
    bpInfo->setSymbolValues, symbolRefValues
    bpInfo->setSymbolSequenceNames, symbolRefNames
    nullObs[*]=!VALUES.F_NAN
  endif
  if haveObs then begin
    obsStatValues=self->getObsStatValues()
    obsDim=size(obsStatValues, /DIM)
    if n_elements(obsDim) eq 1 then begin
      legoRefValues=fltarr(1, n_elements(obsStatValues))
      legoRefValues[0, *]=obsStatValues
    endif else begin
      legoRefValues=obsStatValues
    endelse
    bpInfo->setLegoValues, legoRefValues
  endif else begin
    bpInfo->setLegoValues, nullObs
  endelse
  bpInfo->setLegoNames, legoNames
  
  ;bpInfo->setLegoSequenceNames, scenNames[0]
  util=obj_new('FMUtility')
  ;set main title
  title=request->getDiagramName()+' - '+request->getElaborationName()
  ;set y title
  mus=request->getParameterMeasureUnits()
  ytitle=mus[0]+'#'
  elabName=request->getElaborationName()
  if elabName eq 'CorrCoeff' or elabName eq 'IOA' or elabName eq 'RMSEs/RMSEu' or elabName eq 'Target' then ytitle='#'
  if elabName eq 'ExcDays'  then ytitle='nb of days '
  if elabName eq 'RDE'  or elabName eq 'RPE' or elabName eq 'FAC2' $
    or elabName eq 'MFB' or elabName eq 'MFE' or elabName eq 'MBias' then ytitle='% '
  ;  yRange=[-100.,0.]
  ;for i=1, n_elements(mus)-1 do yTitle=yTitle+mus[i]+'#'
  ytitle=strmid(ytitle, 0, strlen(ytitle)-1)
  self->mapPlotInfo, title=title, $
    xTitle=xTitle, yTitle=yTitle, xRange=xRange, yRange=yRange, $
    xTicks=xTicks, yTicks=yTicks, xTickNames=xTickNames, yStyle=yStyle, xStyle=xStyle, $
    xCharSize=xCharSize, yCharSize=yCharSize, charSize=charSize, xLabels=xLabels
  bpInfo->standardSetting
  self->setBarPlotInfo, bpInfo
  obj_destroy, util
  
END

PRO Result::testJRCMethods

END

PRO Result::setRunStatInfos, infos

  ptr_free, self.runStatInfos
  self.runStatInfos=ptr_new(infos, /NO_COPY)
  
END

FUNCTION Result::getRunStatInfos

  if ptr_valid(self.runStatInfos) then return, *self.runStatInfos else return, [-1]
  
END

PRO Result::setObsStatInfos, infos

  ptr_free, self.obsStatInfos
  self.obsStatInfos=ptr_new(infos, /NO_COPY)
  
END

FUNCTION Result::getObsStatInfos

  if ptr_valid(self.obsStatInfos) then return, *self.obsStatInfos else return, [-1]
  
END

PRO Result::setObsStatValues, values

  ptr_free, self.obsStatValues
  self.obsStatValues=ptr_new(values, /NO_COPY)
  
END

FUNCTION Result::getObsStatValues

  return, *self.obsStatValues
  
END

PRO Result::setRunStatValues, values

  ptr_free, self.runStatValues
  self.runStatValues=ptr_new(values, /NO_COPY)
  
END

FUNCTION Result::getRunStatValues

  return, *self.runStatValues
  
END

FUNCTION Result::getTempDataFile

  return, self.tempDataFile
  
END

PRO Result::setBarPlotInfo, info

  self.barPlotInfo=info
  
END

FUNCTION Result::getBarPlotInfo

  return, self.barPlotInfo
  
END

PRO Result::setRawMonMinVal, sValue, gValue

  if n_elements(sValue) ne 0 then self.singleRawMonMinVal=sValue
  if n_elements(gValue) ne 0 then self.groupRawMonMinVal=gValue
  
END

FUNCTION Result::getRawMonMinVal

  return, [self.singleRawMonMinVal, self.groupRawMonMinVal]
  
END

PRO Result::setRawMonMaxVal, sValue, gValue

  if n_elements(sValue) ne 0 then self.singleRawMonMaxVal=sValue
  if n_elements(gValue) ne 0 then self.groupRawMonMaxVal=gValue
  
END

FUNCTION Result::getRawMonMaxVal

  return, [self.singleRawMonMaxVal, self.groupRawMonMaxVal]
  
END

PRO Result::setRawRunMinVal, sValue, gValue

  if n_elements(sValue) ne 0 then self.singleRawRunMinVal=sValue
  if n_elements(gValue) ne 0 then self.groupRawRunMinVal=gValue
  
END

FUNCTION Result::getRawRunMinVal

  return, [self.singleRawRunMinVal, self.groupRawRunMinVal]
  
END

PRO Result::setRawRunMaxVal, sValue, gValue

  if n_elements(sValue) ne 0 then self.singleRawRunMaxVal=sValue
  if n_elements(gValue) ne 0 then self.groupRawRunMaxVal=gValue
  
END

FUNCTION Result::getRawRunMaxVal

  return, [self.singleRawRunMaxVal, self.groupRawRunMaxVal]
  
END

FUNCTION Result::getSingleRawDataCheckMatrix, monitIndexes, runIndexes

  if ptr_valid(self.singleRawData) then begin
    raws=*self.singleRawData
    rawNo=n_elements(raws)
    checkMatrix=intarr(rawNo, 2)
    monitIndexes=[-1] & runIndexes=[-1]
    for i=0, rawNo-1 do begin
      if ptr_valid(raws[i].observedData) then begin
        checkMatrix[i, 0]=1
        monitIndexes=[monitIndexes, i]
      endif
      if ptr_valid(raws[i].runData) then begin
        checkMatrix[i, 1]=1
        runIndexes=[runIndexes, i]
      endif
    endfor
  endif
  if n_elements(runIndexes) gt 1 then runIndexes=runIndexes[1:*]
  if n_elements(monitIndexes) gt 1 then monitIndexes=monitIndexes[1:*]
  return, checkMatrix
  
  
END

FUNCTION Result::getSingleRawDataByIndex, index

  return, (*self.singleRawData)[index]
  
END

FUNCTION Result::getGroupRawDataCheckMatrix, monitIndexes, runIndexes

  if ptr_valid(self.groupRawData) then begin
    raws=*self.groupRawData
    rawNo=n_elements(raws)
    checkMatrix=intarr(rawNo, 2)
    monitIndexes=[-1] & runIndexes=[-1]
    for i=0, rawNo-1 do begin
      if ptr_valid(raws[i].observedData) then begin
        checkMatrix[i, 0]=1
        monitIndexes=[monitIndexes, i]
      endif
      if ptr_valid(raws[i].runData) then begin
        checkMatrix[i, 1]=1
        runIndexes=[runIndexes, i]
      endif
    endfor
  endif
  if n_elements(runIndexes) gt 1 then runIndexes=runIndexes[1:*]
  if n_elements(monitIndexes) gt 1 then monitIndexes=monitIndexes[1:*]
  return, checkMatrix
  
  
END

FUNCTION Result::getGroupRawDataByIndex, index

  return, (*self.groupRawData)[index]
  
END

FUNCTION Result::getPlotInfo

  return, self.plotInfo
  
END

PRO Result::mapPlotInfo, title=title, xTitle=xTitle, yTitle=yTitle, xRange=xRange, yRange=yRange, $
    xTicks=xTicks, yTicks=yTicks, xTickNames=xTickNames, yStyle=yStyle, xStyle=xStyle, $
    xCharSize=xCharSize, yCharSize=yCharSize, charSize=charSize, xLabels=xLabels
    
  if n_elements(title)  ne 0 then self.plotInfo->setTitle, title
  if n_elements(xTitle)  ne 0 then self.plotInfo->setXTitle, xTitle
  if n_elements(yTitle)  ne 0 then self.plotInfo->setYTitle, yTitle
  if n_elements(xRange)  ne 0 then self.plotInfo->setXRange, xRange
  if n_elements(yRange)  ne 0 then self.plotInfo->setYRange, yRange
  if n_elements(xTicks)  ne 0 then self.plotInfo->setXTicks, xTicks
  if n_elements(yTicks)  ne 0 then self.plotInfo->setYTicks, yTicks
  if n_elements(yStyle)  ne 0 then self.plotInfo->setYStyle, yStyle
  if n_elements(xStyle)  ne 0 then self.plotInfo->setXStyle, xStyle
  if n_elements(xCharSize)  ne 0 then self.plotInfo->setXCharSize, xCharSize
  if n_elements(yCharSize)  ne 0 then self.plotInfo->setYCharSize, yCharSize
  if n_elements(charSize)  ne 0 then self.plotInfo->setCharSize, charSize
  if n_elements(xLabels)  ne 0 then self.plotInfo->setXLabels, xLabels
  if n_elements(xTickNames)  ne 0 then self.plotInfo->buildXTickNames, xTickNames
  if n_elements(yTickNames)  ne 0 then self.plotInfo->setYTickNames, yTickNames
  
END

FUNCTION Result::getRawElementNumber

  return, self->getSingleRawElementNumber()+self->getGroupRawElementNumber()
  
  
END

FUNCTION Result::getSingleRawElementNumber

  if ptr_valid(self.singleRawData) then return, n_elements(*self.singleRawData)
  
END

FUNCTION Result::getGroupRawElementNumber

  if ptr_valid(self.groupRawData) then return, n_elements(*self.groupRawData)
  
END

PRO Result::applySeasonPeriodSelection, startDate, endDate
;ToDo: from raw data set to #Nan for no-selected season
;for i=0, sPeriods-1 do result->allYearHours[startS[i]:endS[i]]=1

END

PRO Result::applyHourPeriodSelection, startDate, endDate
;ToDo: from raw data set to #Nan for no-selected season
;  for i=0, hPeriods-1 do begin
;    validStartHours=indgen(365)*24+startH[i]
;    validEndHours=indgen(365)*24+endH[i]
;    allYearHours[startS[i]:endS[i]]=1
;  endfor

END

;*****************************
; get/set
;*****************************
PRO Result::setGraphTitle, value

  self.graphTitle=value
  
END

FUNCTION Result::getGraphTitle

  return, self.graphTitle
  
END

PRO Result::setLegendInfo, value

  obj_destroy, self.legendInfo & self.legendInfo=value
  
END

FUNCTION Result::getLegendInfo

  return, self.legendInfo
  
END

PRO Result::setBarPlot, value

  self.barPlot=value
  
END

FUNCTION Result::getBarPlot

  return, self.barPlot
  
END

PRO Result::setLinePlot, value

  self.linePlot=value
  
END

FUNCTION Result::getLinePlot

  return, self.linePlot
  
END

PRO Result::setXAxisLength, value

  self.xAxisLength=value
  
END

FUNCTION Result::getXAxisLength

  return, self.xAxisLength
  
END

PRO Result::setXValues, list

  ptr_free, self.xValues
  self.xValues=ptr_new(list, /NO_COPY)
  
END

FUNCTION Result::getXValues

  if ptr_valid(self.xValues) then return, *self.xValues else return, [-1]
  
END

PRO Result::setRawData, singleList, groupList, monMin=monMin, monMax=monMax, runMin=runMin, runMax=runMax

  if n_elements(singleList) ne 0 then self.singleRawData=ptr_new(singleList, /NO_COPY)
  if n_elements(groupList) ne 0 then self.groupRawData=ptr_new(groupList, /NO_COPY)
  
END

FUNCTION Result::getSingleRawData

  if ptr_valid(self.singleRawData) then return, *self.singleRawData else return, [-1]
  
END

FUNCTION Result::getGroupRawData

  if ptr_valid(self.groupRawData) then return, *self.groupRawData else return, [-1]
  
END

PRO Result::setxTickMarks, list

  ptr_free, self.xTickMarks
  self.xTickMarks=ptr_new(list, /NO_COPY)
  
END

FUNCTION Result::getXTickMarks

  if ptr_valid(self.xTickMarks) then return, *self.xTickMarks else return, [-1]
  
END

PRO Result::setXTickMarksAreDate, value

  self.xTickMarksAreDate=value
  
END

FUNCTION Result::getXTickMarksAreDate

  return, self.xTickMarksAreDate
  
END

PRO Result::setXTickMarksTimeStamp, value

  self.xTickMarksTimeStamp=value
  
END

FUNCTION Result::getXTickMarksTimeStamp

  return, self.xTickMarksTimeStamp
  
END

;*****************************
; import/export utility
;*****************************

PRO Result::streamPrint

  print, '***********************'
  print, '**Start of<',OBJ_CLASS(self),'>**'
  print, '**** graphTitle:', self->getGraphTitle()
  if obj_valid(self.legendInfo) then self.legendInfo->streamPrint else print, '**** legendInfo:'
  print, '**** barPlot:', self->getBarPlot()
  print, '**** linePlot:', self->getLinePlot()
  print, '**** taylorPlot:', self->getTaylorPlot()
  print, '**** scatterPlot:', self->getScatterPlot()
  print, '**** soccerPlot:', self->getSoccerPlot()
  print, '**** xAxisLength:', self->getXAxisLength()
  if ptr_valid(self.xValues) then print, '**** xValues:', self->getXValues() else print, '**** xValues:'
  if ptr_valid(self.xTickMarks) then print, '**** xTickMarks:', self->getXTickMarks() else print, '**** xTickMarks:'
  print, '**** xTickMarksAreDate:', self->getXTickMarksAreDate()
  print, '**** xTickMarksTimeStamp:', self->getXTickMarksTimeStamp()
  if obj_valid(self.plotInfo) then self.plotInfo->streamPrint else print, '**** plotInfo:'
  ;if ptr_valid(self.rawData) then print, '**** rawData:', self->getRawData() else print, '**** rawData:'
  if ptr_valid(self.singleRawData) then print, '**** singleRawData:', self->getSingleRawData() else print, '**** singleRawData:'
  if ptr_valid(self.groupRawData) then print, '**** groupRawData:', self->getGroupRawData() else print, '**** groupRawData:'
  if ptr_valid(self.runStatValues) then print, '**** runStatValues:', self->getRunStatValues() else print, '**** runStatValues:'
  if ptr_valid(self.obsStatValues) then print, '**** obsStatValues:', self->getObsStatValues() else print, '**** obsStatValues:'
  sgMonMin=self->getRawMonMinVal()
  sgMonMax=self->getRawMonMaxVal()
  sgRunMin=self->getRawRunMinVal()
  sgRunMax=self->getRawRunMaxVal()
  print, '**** singleRawMonMinVal:', sgMonMin[0]
  print, '**** singleRawMonMaxVal:', sgMonMax[0]
  print, '**** singleRawRunMinVal:', sgRunMin[0]
  print, '**** singleRawRunMaxVal:', sgRunMax[0]
  print, '**** groupRawMonMinVal:', sgMonMin[1]
  print, '**** groupRawMonMaxVal:', sgMonMax[1]
  print, '**** groupRawRunMinVal:', sgRunMin[1]
  print, '**** groupRawRunMaxVal:', sgRunMax[1]
  if obj_valid(self.barPlotInfo) then self.barPlotInfo->streamPrint else print, '**** barPlotInfo:'
  if obj_valid(self.genericPlotInfo) then self.genericPlotInfo->streamPrint else print, '**** genericPlotInfo:'
  
END

PRO Result::freeRawData

  if ptr_valid(self.singleRawData) then begin
    rData=*self.singleRawData
    rrData=rData.runData
    for i=0, n_elements(rrData)-1 do ptr_free, rrData[i]
    orData=rData.observedData
    for i=0, n_elements(orData)-1 do ptr_free, orData[i]
  endif
  ptr_free, self.singleRawData
  
  if ptr_valid(self.groupRawData) then begin
    rData=*self.groupRawData
    rrData=rData.runData
    for i=0, n_elements(rrData)-1 do ptr_free, rrData[i]
    orData=rData.observedData
    for i=0, n_elements(orData)-1 do ptr_free, orData[i]
  endif
  ptr_free, self.groupRawData
  
END
;
;*****************************
; constructor/destructor
;*****************************

PRO Result::cleanUp

  ;obj_destroy, self.fileSysMgr
  obj_destroy, self.legendInfo
  ptr_free, self.xValues
  ptr_free, self.xTickMarks
  self->freeRawData
  ptr_free,  self.runStatValues
  ptr_free,  self.obsStatValues
  ptr_free,  self.obsStatInfos
  ptr_free,  self.runStatInfos
  obj_destroy, self.barPlotInfo
  obj_destroy, self.plotInfo
  obj_destroy, self.genericPlotInfo
  
  self->Object::cleanup
  
END

FUNCTION Result::init, filename, tempDataFile

  if not (self -> Object :: init()) then return, 0
  
  if n_elements( fileName ) eq 1 then self.filename = filename
  if n_elements(tempDataFile) eq 1 then self.tempDataFile = tempDataFile
  self.plotInfo=obj_new('PlotInfo')
  self.genericPlotInfo=obj_new('GenericPlotInfo')
  self.multipleDrawMainTitle='fill the title with Result->multipleDrawMainTitle, yourText method'
  return, 1
  
END

PRO Result__Define

  Struct = { Result , $
    fileName: '', $
    tempDataFile: '', $
    graphTitle: '', $
    legendInfo: obj_new(),$
    barPlot: 0, $ ; true if output plot requested is "Bar"
    linePlot: 0, $ ; true if output plot requested is "Line"
    taylorPlot: 0, $ ; true if output plot requested is "Taylor"
    targetPlot: 0, $ ; true if output plot requested is "Target"
    scatterPlot: 0, $ ; true if output plot requested is "Scatter"
    soccerPlot: 0, $ ; true if output plot requested is "Soccer"
    multipleDrawMainTitle: '', $
    xAxisLength: 0, $
    xValues: ptr_new(), $ ; pointer of multiple data array
    xTickMarks : ptr_new(), $ ; pointer of tickmarks
    xTickMarksAreDate : 0, $ ; true/false
    xTickMarksTimeStamp : 0, $ ; original time stamp of xTickMarks
    plotInfo: obj_new(), $
    singleRawMonMinVal: 0., $
    singleRawMonMaxVal: 0., $
    singleRawRunMinVal: 0., $
    singleRawRunMaxVal: 0., $
    groupRawMonMinVal: 0., $
    groupRawMonMaxVal: 0., $
    groupRawRunMinVal: 0., $
    groupRawRunMaxVal: 0., $
    singleRawData : ptr_new(), $ ; original data get from file(s)/db
    groupRawData : ptr_new(), $ ; original data get from file(s)/db
    runStatValues : ptr_new(), $
    obsStatValues : ptr_new(), $
    runStatInfos: ptr_new(), $
    obsStatInfos: ptr_new(), $
    barPlotInfo : obj_new(), $
    genericPlotInfo : obj_new(), $
    Inherits Object $
    }
    
END