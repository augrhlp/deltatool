PRO FM_Generic, request, result
  startIndex=request->getStartIndex()
  endIndex=request->getEndIndex()
  npar=request->getParameterNumber()
  nmod=request->getModelNumber()
  nsce=request->getScenarioNumber()
  parCodes=request->getParameterCodes()
  modelCodes=request->getModelCodes()
  scenarioCodes=request->getScenarioCodes()
  diagramCode=request->getDiagramCode()
  elabcode=request->getElaborationCode()
  statType=request->getGroupByStatInfo()
  isSingleSelection=request->isSingleObsPresent()
  isGroupSelection=request->isGroupObsPresent()
  extraValNumber=request->getExtraValuesNumber()
  if extraValNumber gt 0 then begin
    extraValues=request->getExtraValues()
  endif else begin
    print,'ExtraValues not available'
  endelse
  
  nobsS=0
  nreg=0
  if isSingleSelection then begin
    obsNames=request->getSingleObsNames()
    nobsS=request->getSingleObsNumber()
    singleRawData=result->getSingleRawData()
    singleRDMatrix=result->getSingleRawDataCheckMatrix(sMonitIndexes, sRunIndexes)
    mChoice1runS=singleRawData[sRunIndexes].parameterCode
    mChoice2runS=singleRawData[sRunIndexes].modelCode
    mChoice3runS=singleRawData[sRunIndexes].scenarioCode
    mChoice4runS=singleRawData[sRunIndexes].observedCode
  endif
  ngroup=0
  nobsG=0
  if isGroupSelection then begin
    allGroupStations=request->buildAllGroupNames()
    nobsG=n_elements(allGroupStations)
    groupTitles=request->getGroupTitles()
    ngroup=n_elements(groupTitles)
    groupStatToApplyCode=request->getGroupStatToApplyCode()
    groupCodes=request->getGroupCodes()
    groupNames=request->getGroupNames()
    groupRawData=result->getGroupRawData()
;    print,'FM_Generic',groupRawData
    groupRDMatrix=result->getGroupRawDataCheckMatrix(gMonitIndexes, gRunIndexes)
    mChoice1runG=groupRawData[gRunIndexes].parameterCode
    mChoice2runG=groupRawData[gRunIndexes].modelCode
    mChoice3runG=groupRawData[gRunIndexes].scenarioCode
    mChoice4runG=groupRawData[gRunIndexes].observedCode
  endif
  nobs=nobsS+nobsG  ; totla number of stations (ie single stations and all stations in the groups - with double counting)

  if isSingleSelection eq 1 and isGroupSelection eq 0 then begin  ; only single stations
    mChoice1run=mChoice1runS
    mChoice2run=mChoice2runS
    mChoice3run=mChoice3runS
    mChoice4run=mChoice4runS
    obsNames=obsNames
    MonitIndexes=sMonitIndexes
    RunIndexes=sRunIndexes
    RawData=singleRawData
  endif
  if isSingleSelection eq 0 and isGroupSelection eq 1 then begin  ;only groups
    mChoice1run=mChoice1runG
    mChoice2run=mChoice2runG
    mChoice3run=mChoice3runG
    mChoice4run=mChoice4runG
    obsNames=allGroupStations
    MonitIndexes=gMonitIndexes
    RunIndexes=gRunIndexes
    RawData=groupRawData
  endif
  if isSingleSelection eq 1 and isGroupSelection eq 1 then begin  ; single stations and groups
    mChoice1run=[mChoice1runS,mChoice1runG]
    mChoice2run=[mChoice2runS,mChoice2runG]
    mChoice3run=[mChoice3runS,mChoice3runG]
    mChoice4run=[mChoice4runS,mChoice4runG]
    obsNames=[obsnames,allGroupStations]
    MonitIndexes=[sMonitIndexes,max(sMonitIndexes)+1+gMonitIndexes]
    RunIndexes=[sRunIndexes,max(sRunIndexes)+1+gRunIndexes]
    RawData=[singleRawData,groupRawData]
  endif
  SG_Computing, $      ; this is the combination of the old single- and groupcomputing routines
    request, result, $
    npar, nmod, nsce, nobs, nobsS, nobsG, $
    mChoice1run, mChoice2run, mChoice3run, mChoice4run,$
    parCodes, modelCodes, ScenarioCodes, obsNames, $
    startIndex, endIndex,$
    MonitIndexes, RunIndexes, RawData, $
    elabcode, statType, extraValues,$
    statXYResult

  nobs=nobsS+ngroup   ; redefined = number of single stations + number of groups
  
; KeesC suppress stations with NaN: station should have values for all pars, all mods, and all sceno  
  statValidO=intarr(nobs) & statValidO(*)=-1 
  statValidR=intarr(nobs) & statValidR(*)=-1
  for iobs=0,nobs-1 do begin
    if total(finite(statXYResult(*,*,*,iobs,0))) eq npar*nmod*nsce then statValidO[iobs]=iobs  
    if total(finite(statXYResult(*,*,*,iobs,1))) eq npar*nmod*nsce then statValidR[iobs]=iobs
  endfor
  statValid=intarr(nobs)
  for iobs=0,nobs-1 do begin
    if diagramCode eq 0 then statValid(iobs)=max([statValidO(iobs),statValidR(iobs)])
    if diagramCode ne 0 then statValid(iobs)=min([statValidO(iobs),statValidR(iobs)])
  endfor
  statValid2=where(statValid ne -1,numStatValid)  
  if numStatValid ge 1 then begin
    statXYResult=statXYResult(*,*,*,statValid2,*)
  endif else begin
    statXYResult(*,*,*,*,*)=!values.f_nan
  endelse 
  if numStatValid eq 0 then begin
    legendNames=strarr(10,10)
    statColors=intarr(10)
    statSymbols=intarr(10)
    legendColors=intarr(10)
    legendSymbols=intarr(10)
    statXYResult='AllNaN'
    goto, nocalcul
  endif  

  PrepareLegends, request, result, ifree, npar, nmod, nsce, nobsS, nobs, obsNames, numStatValid, $
    statValid2, legendNames, legendColors, legendSymbols, statSymbols, statColors

; KeesC : Put into linear structure for input to PH plotroutines (i.e. diagramCode ne 0)
  nobs=numStatValid
  if diagramCode ne 0 then begin   
    nmulti=npar*nmod*nsce*nobs
    legHlp=strarr(nmulti)
    statC=intarr(nmulti)
    statS=intarr(nmulti)
    statXY=fltarr(nmulti,2)     
    k=0
    for ipar=0,npar-1 do begin
    for imod=0,nmod-1 do begin
    for isce=0,nsce-1 do begin
    for iobs=0,nobs-1 do begin
      if ifree eq '1000' then legHLP(k)=legendNames(ipar,0)
      if ifree eq '0100' then legHLP(k)=legendNames(imod,1)
      if ifree eq '0010' then legHLP(k)=legendNames(isce,2)
      if ifree eq '0001' then legHLP(k)=legendNames(iobs,3)
      if ifree eq '1100' then legHlp(k)=legendNames(imod,1)+'&'+legendNames(ipar,0)
      if ifree eq '1010' then legHlp(k)=legendNames(isce,2)+'&'+legendNames(ipar,0)
      if ifree eq '1001' then legHlp(k)=legendNames(iobs,3)+'&'+legendNames(ipar,0)
      if ifree eq '0110' then legHlp(k)=legendNames(isce,2)+'&'+legendNames(imod,1)
      if ifree eq '0101' then legHlp(k)=legendNames(iobs,3)+'&'+legendNames(imod,1)
      if ifree eq '0011' then legHlp(k)=legendNames(iobs,3)+'&'+legendNames(isce,2)
      statXY(k,0)=statXYResult(ipar,imod,isce,iobs,0)
      statXY(k,1)=statXYResult(ipar,imod,isce,iobs,1)
      statC(k)=statColors(ipar,imod,isce,iobs)
      statS(k)=statSymbols(ipar,imod,isce,iobs)
      k=k+1
    endfor
    endfor
    endfor
    endfor
    statXYResult=statXY
    legendNames=legHlp
    statColors=statC
    statSymbols=statS
    legendColors=statColors
    legendSymbols=statSymbols
    statXY=0
  endif

  ;KeesC 19MAY2012
  nocalcul:
  
  result->setGenericPlotInfo, statXYResult, statSymbols, statColors, legendNames, legendColors,legendSymbols

END
;************************************************************************
PRO SG_Computing, $
    request, result, $
    Index1, Index2, Index3, Index4, nobsS, nobsG, $
    mChoice1run, mChoice2run, mChoice3run, mChoice4run,$
    test1, test2, test3, test4, $
    startIndex, endIndex,$
    MonitIndexes, RunIndexes, RawData, $
    elabcode, statType, extraValues,$
    statXYResult
    
  isSingleSelection=request->isSingleObsPresent()
  isGroupSelection=request->isGroupObsPresent()  
  hourStat=request->getGroupByTimeInfo() ;HourType
  flag_average=hourStat[0].value

  dimAll=(Index1)*(Index2)*(Index3)*(Index4)
  statXYResult=fltarr(Index1,Index2,Index3,Index4,2)   ;0=obs, 1=run;
  statXYGroup=fltarr(index1,index2,index3,index4) & statXYGroup(*,*,*,*)=!values.f_nan
  
  for i1=0, Index1-1 do begin   ;par
    for i2=0, Index2-1 do begin  ; mod
      for i3=0, Index3-1 do begin  ;scen
        for i4=0, Index4-1 do begin    ;obs
          choiceIdx1=(where(mChoice1run eq test1(i1) and mChoice4run eq test4(i4)))[0]
          if choiceIdx1 eq -1 then begin
            rsult=dialog_message(['INCONSISTENT DATA:',$
              'Check availability of OBS-'+test1(i1)+' at Station '+test4(i4)],/error)
            stop
          endif
          obsTemp=*RawData[MonitIndexes[choiceIdx1]].observedData
          choiceIdx1=(where(mChoice1run eq test1[i1] and mChoice2run eq test2[i2] and $
            mChoice3run eq test3[i3] and mChoice4run eq test4[i4]))[0]
          if choiceIdx1 eq -1 then begin
            rsult=dialog_message(['INCONSISTENT DATA:',$
              'Check availability of Parameter '+test1(i1)+' at Station '+test4(i4),$
              'in model '+test2[i2]+' for scenario '+test3(i3)],/error)
            stop
          endif
          runTemp=*RawData[RunIndexes[choiceIdx1]].runData
          
          if elabCode ne 71 and elabCode ne 72 and elabCode ne 73 then begin
            time_operations, request, result, obsTemp, runTemp
            obs_run_nan,request,result,obsTemp, runTemp
          endif
          longShort=0
          if elabCode eq 10 then longShort=1
          CheckCriteria, request, result, 'OU', criteriaOU, obsTemp, longShort,alpha,criteriaOrig,LV,nobsAv
          
          if elabcode eq 0 then begin
            statXYResult[i1,i2,i3,i4,0]=mean(obsTemp)
            statXYResult[i1,i2,i3,i4,1]=mean(runTemp)
            statXYGroup[i1,i2,i3,i4]=abs(nmb(obsTemp,runTemp))
          endif
          if elabcode eq 1 then begin         
            statXYResult[i1,i2,i3,i4,0]=stddevOM(obsTemp)
            statXYResult[i1,i2,i3,i4,1]=stddevOM(runTemp)
            statXYGroup[i1,i2,i3,i4]=abs(nmsd(obsTemp,runTemp))
          endif
          if elabcode eq 2 then begin
            statXYResult[i1,i2,i3,i4,0]=correlate(obsTemp,runtemp)
            statXYResult[i1,i2,i3,i4,1]=correlate(obsTemp,runtemp)
            statXYGroup[i1,i2,i3,i4]=abs(correlate(obsTemp,runtemp))
          endif
;          if elabcode eq 22 then begin
;            statxyresult[i1,i2,i3,i4,0]=mean(obstemp)
;            statxyresult[i1,i2,i3,i4,1]=mean(runtemp)
;            statxygroup[i1,i2,i3,i4]=abs(correlate(obstemp,runtemp))  ;not used
;          endif
          if elabcode eq 3 then begin
            statXYResult[i1,i2,i3,i4,0]=mean(runTemp)-mean(obsTemp)
            statXYResult[i1,i2,i3,i4,1]=mean(runTemp)-mean(obsTemp)
            statXYGroup[i1,i2,i3,i4]=abs(nmb(obsTemp,runTemp))
          endif
          if elabcode eq 4 then begin
            statXYResult[i1,i2,i3,i4,0]=rmse(obsTemp,runTemp)
            statXYResult[i1,i2,i3,i4,1]=rmse(obsTemp,runTemp)
            statXYGroup[i1,i2,i3,i4]=abs(statXYResult[i1,i2,i3,i4,0])
          endif
          if elabcode eq 5 then begin
            statXYResult[i1,i2,i3,i4,0]=rmses(obsTemp,runTemp)/rmseu(obsTemp,runTemp)
            statXYResult[i1,i2,i3,i4,1]=rmses(obsTemp,runTemp)/rmseu(obsTemp,runTemp)
            statXYGroup[i1,i2,i3,i4]=abs(nmb(obsTemp,runTemp))
          endif
          if elabcode eq 6 or elabCode eq 21 then begin
            statXYResult[i1,i2,i3,i4,0]=mean(obsTemp)
            statXYResult[i1,i2,i3,i4,1]=mean(runTemp)
            statXYGroup[i1,i2,i3,i4]=abs(nmb(obsTemp,runTemp))
          endif
          if elabcode eq 7 then begin
            statXYResult[i1,i2,i3,i4,0]=ioa(obsTemp, runTemp)
            statXYResult[i1,i2,i3,i4,1]=ioa(obsTemp, runTemp)
            statXYGroup[i1,i2,i3,i4]=abs(ioa(obsTemp, runTemp))
          endif
          if elabcode eq 8 then begin
            statXYResult[i1,i2,i3,i4,0]=rde(obstemp,runTemp,extraValues(0))
            statXYResult[i1,i2,i3,i4,1]=rde(obstemp,runTemp,extraValues(0))
            statXYGroup[i1,i2,i3,i4]=abs(rde(obstemp,runTemp,extraValues(0)))
          endif
          if elabcode eq 9 then begin
            cfin1=where(finite(obsTemp) eq 1,countFin1)
            cfin0=where(finite(obsTemp) eq 0,countFin0)
            if countFin0 ge 1 then obsTemp(cfin0)=-999
            cobs=where(obsTemp gt extraValues(0),countobs)
            statXYResult[i1,i2,i3,i4,0]=countobs
            if statType ge 1 then statXYResult[i1,i2,i3,i4,0]=countobs/24
            if countFin1 eq 0 then statXYResult[i1,i2,i3,i4,0]=!values.f_nan
            cfin1=where(finite(runTemp) eq 1,countFin1)
            cfin0=where(finite(runTemp) eq 0,countFin0)
            if countFin0 ge 1 then runTemp(cfin0)=-999
            crun=where(runTemp gt extraValues(0),countrun)
            statXYResult[i1,i2,i3,i4,1]=countrun
            if statType ge 1 then statXYResult[i1,i2,i3,i4,1]=countrun/24
            if countFin1 eq 0 then statXYResult[i1,i2,i3,i4,1]=!values.f_nan
            statXYGroup[i1,i2,i3,i4]=statXYResult[i1,i2,i3,i4,0]
          endif
          if elabcode eq 10 then begin ;category NMB
            statXYResult[i1,i2,i3,i4,0]=(mean(runTemp)-mean(obsTemp))/(2.*criteriaOU)
            statXYResult[i1,i2,i3,i4,1]=i4  ;not used
            statXYGroup[i1,i2,i3,i4]=abs(statXYResult[i1,i2,i3,i4,0])
          endif
          if elabcode eq 11 then begin ;category R
            if finite(mean(obsTemp)) eq 1 then begin
              statXYResult[i1,i2,i3,i4,0]=(1.-correlate(obsTemp,runTemp))/(2.*(criteriaOU/stddevOM(obsTemp))^2)
            endif else begin
              statXYResult[i1,i2,i3,i4,0]=!values.f_nan
            endelse
            statXYResult[i1,i2,i3,i4,1]=i4  ;not used
            statXYGroup[i1,i2,i3,i4]=abs(statXYResult[i1,i2,i3,i4,0])
          endif
          if elabcode eq 14 then begin  ;Taylor
            statXYResult[i1,i2,i3,i4,0]=stddevOM(runTemp)/(2.*criteriaOU)
            statXYResult[i1,i2,i3,i4,1]=correlate(obsTemp,runTemp)
            statXYGroup[i1,i2,i3,i4]=crmse(obsTemp,runTemp)
          endif
          if elabcode eq 15 then begin ; R buggle
            statXYResult[i1,i2,i3,i4,0]=criteriaOU/stddevOM(obsTemp)
            statXYResult[i1,i2,i3,i4,1]=correlate(obsTemp, runTemp)
            statXYGroup[i1,i2,i3,i4]=abs(correlate(obsTemp, runTemp))
          endif
          if elabcode eq 16 then begin ; Bias buggle
            statXYResult[i1,i2,i3,i4,0]=2*criteriaOU
            statXYResult[i1,i2,i3,i4,1]=mean(runTemp)-mean(obsTemp)
            statXYGroup[i1,i2,i3,i4]=abs(mean(runTemp)-mean(obsTemp))
          endif
          if elabcode eq 17 then begin ;Soccer
            statXYResult[i1,i2,i3,i4,0]=bias(obsTemp, runTemp)
            statXYResult[i1,i2,i3,i4,1]=rmse(obsTemp, runTemp)
            statXYGroup[i1,i2,i3,i4]=(rmse(obsTemp, runTemp)^2+bias(obsTemp, runTemp)^2)
          endif
          if elabcode eq 18 then begin ;category NMSD
            if criteriaOU*mean(obsTemp) ne 0 then statXYResult[i1,i2,i3,i4,0]=(stddevOM(runTemp) - stddevOM(obsTemp))/(2.*criteriaOU)
            if criteriaOU*mean(obsTemp) eq 0 then statXYResult[i1,i2,i3,i4,0]=!values.f_nan
            statXYResult[i1,i2,i3,i4,1]=i4  ;not used
            statXYGroup[i1,i2,i3,i4]=statXYResult[i1,i2,i3,i4,0]
          endif
          if elabcode eq 19 then begin  ;Taylor
            statXYResult[i1,i2,i3,i4,0]=stddevOM(runTemp)/stddevOM(obsTemp)
            statXYResult[i1,i2,i3,i4,1]=correlate(obsTemp,runTemp)
            statXYGroup[i1,i2,i3,i4]=crmse(obsTemp,runTemp)
          endif
          if elabcode eq 20 then begin  ;Soccer AQ
            ahlp=abs(stddevOM(runTemp)-stddevOM(obsTemp))/(2.*criteriaOU)
            sign=-1.
            if ahlp le 1 then sign=1.
            statXYResult[i1,i2,i3,i4,0]=sign*(1.-correlate(obsTemp,runTemp))/(2.*(criteriaOU/stddevOM(obsTemp))^2)
            statXYResult[i1,i2,i3,i4,1]=(mean(runTemp)-mean(obsTemp))/(2.*criteriaOU)
            statXYGroup[i1,i2,i3,i4]=sign*rmse(obsTemp, runTemp)
          endif
          if elabcode eq 23 then begin
            statXYResult[i1,i2,i3,i4,0]=mnb(obsTemp, runTemp)
            statXYResult[i1,i2,i3,i4,1]=mnb(obsTemp, runTemp)
            statXYGroup[i1,i2,i3,i4]=mnb(obsTemp, runTemp)
          endif
          if elabcode eq 24 then begin
            statXYResult[i1,i2,i3,i4,0]=mfe(obsTemp, runTemp)
            statXYResult[i1,i2,i3,i4,1]=mfe(obsTemp, runTemp)
            statXYGroup[i1,i2,i3,i4]=mfe(obsTemp, runTemp)
          endif
          if elabcode eq 25 then begin  ;NMSD Bugle
            if stddevOM(obsTemp) ne 0 then statXYResult[i1,i2,i3,i4,0]=criteriaOU/stddevOM(obsTemp)
            if stddevOM(obsTemp) eq 0 then statXYResult[i1,i2,i3,i4,0]=!values.f_nan
            statXYResult[i1,i2,i3,i4,1]=nmb(stddevOM(obsTemp), stddevOM(runTemp))
            statXYGroup[i1,i2,i3,i4]=nmb(stddevOM(obsTemp), stddevOM(runTemp))
          endif
          if elabcode eq 26 then begin  ; AOTx
            ExtraValues=request->getExtraValues()
            refValue=ExtraValues[0]
            obsAOT=0.
            runAOT=0.
            cfin0=where(finite(obsTemp) eq 0,countFin0)
            cfin1=where(finite(obsTemp) eq 1,countFin1)
            if countFin0 ge 1 then obsTemp(cfin0)=-999
            obsAOT=total(  (obsTemp ge refValue)*(obsTemp-refValue) )
            if countFin1 eq 0 then obsAOT=!values.f_nan
            cfin0=where(finite(runTemp) eq 0,countFin0)
            cfin1=where(finite(runTemp) eq 1,countFin1)
            if countFin0 ge 1 then runTemp(cfin0)=-999
            runAOT=total(  (runTemp ge refValue)*(runTemp-refValue) )
            if countFin1 eq 0 then runAOT=!values.f_nan
            statXYResult[i1,i2,i3,i4,0]=obsAOT/1000.
            statXYResult[i1,i2,i3,i4,1]=runAOT/1000.
            statXYGroup[i1,i2,i3,i4]=abs(nmb(obsTemp,runTemp))
          endif
          if elabcode eq 27 then begin  ; SOMOx       
            ExtraValues=request->getExtraValues()
            refValue=ExtraValues[0]
            obsSOMO=0
            runSOMO=0
            cfin0=where(finite(obsTemp) eq 0,countFin0)
            cfin1=where(finite(obsTemp) eq 1,countFin1)
            if countFin0 ge 1 then obsTemp(cfin0)=-999
            obsSOMO=total(  (obsTemp ge refValue)*(obsTemp-refValue) )
            if statType ge 1 then obsSOMO=obsSOMO/24
            if countFin1 eq 0 then obsSOMO=!values.f_nan
            cfin0=where(finite(runTemp) eq 0,countFin0)
            cfin1=where(finite(runTemp) eq 1,countFin1)
            if countFin0 ge 1 then runTemp(cfin0)=-999
            runSOMO=total(  (runTemp ge refValue)*(runTemp-refValue) )
            if statType ge 1 then runSOMO=runSOMO/24
            if countFin1 eq 0 then runSOMO=!values.f_nan
            statXYResult[i1,i2,i3,i4,0]=obsSOMO/1000.
            statXYResult[i1,i2,i3,i4,1]=runSOMO/1000.
            statXYGroup[i1,i2,i3,i4]=abs(nmb(obsTemp,runTemp))
          endif
          if elabcode eq 28 then begin
            statXYResult[i1,i2,i3,i4,0]=rmse(obsTemp, runTemp)/stddevOM(obstemp)
            statXYResult[i1,i2,i3,i4,1]=rmse(obsTemp, runTemp)/stddevOM(obstemp)
            statXYGroup[i1,i2,i3,i4]=rmse(obsTemp, runTemp)/stddevOM(obstemp)
          endif
          if elabcode eq 30 then begin
            statXYResult[i1,i2,i3,i4,0]=rpe(obstemp,runTemp,extraValues(0))
            statXYResult[i1,i2,i3,i4,1]=rpe(obstemp,runTemp,extraValues(0))
            statXYGroup[i1,i2,i3,i4]=rpe(obstemp,runTemp,extraValues(0))
          endif
          if elabcode eq 33 then begin
            statXYResult[i1,i2,i3,i4,0]=fac2(obsTemp, runTemp)
            statXYResult[i1,i2,i3,i4,1]=fac2(obsTemp, runTemp)
            statXYGroup[i1,i2,i3,i4]=fac2(obsTemp, runTemp)
          endif
          if elabcode eq 52 then begin ;OU Target 
            signNum=2*(1.-correlate(obsTemp,runTemp))
            signDen=(stddevOM(obstemp)-stddevOM(runTemp))^2/(stddevOM(obstemp)*stddevOM(obsTemp))           
            sign=0.
            if finite(signNum) eq 1 and finite(signDen) eq 1 then begin
              if signNum gt signDen then sign=-1 ;Error dominated by R
              if signNum le signDen then sign=1 ;Error dominated by NMSD
            endif  
            if criteriaOU gt 0 then begin
              statXYResult[i1,i2,i3,i4,0]=sign*crmse(obsTemp, runTemp)/(CriteriaOU*2.)
              statXYResult[i1,i2,i3,i4,1]=bias(obsTemp, runTemp)/(CriteriaOU*2.)
              statXYGroup[i1,i2,i3,i4]=sign*rmse(obsTemp,runTemp)
            endif else begin
              statXYResult[i1,i2,i3,i4,0]=!values.f_nan
              statXYResult[i1,i2,i3,i4,1]=!values.f_nan
              statXYGroup[i1,i2,i3,i4]=!values.f_nan
            endelse
          endif
          if elabcode eq 54 then begin
            statXYResult[i1,i2,i3,i4,0]=100.*(stddevOM(runTemp)-stddevOM(obsTemp))/stddevOM(obsTemp)
            statXYResult[i1,i2,i3,i4,1]=100.*(stddevOM(runTemp)-stddevOM(obsTemp))/stddevOM(obsTemp)
            statXYGroup[i1,i2,i3,i4]=abs(statXYResult[i1,i2,i3,i4,0])
          endif
          if elabcode eq 55 then begin ; Not used anymore
            CheckCriteria, request, result, 'OU', criteriaOU, obsTemp, 0,alpha,criteriaOrig,LV,nobsAv
            sign=stddevOM(obsTemp)-stddevOM(runTemp)  ;only for target
            if finite(sign) eq 1 then sign=sign/abs(sign)   ;only for target
            if criteriaOU gt 0 then begin
              statXYResult[i1,i2,i3,i4,0]=sign*crmse(obsTemp, runTemp)/(CriteriaOU*2.)
              statXYResult[i1,i2,i3,i4,1]=bias(obsTemp, runTemp)/(CriteriaOU*2.)
              statXYGroup[i1,i2,i3,i4]=rmse(obsTemp,runTemp)/(CriteriaOU*2.)
            endif else begin
              statXYResult[i1,i2,i3,i4,0]=!values.f_nan
              statXYResult[i1,i2,i3,i4,1]=!values.f_nan
              statXYGroup[i1,i2,i3,i4]=!values.f_nan
            endelse
          endif
          if elabcode ge 71 and elabcode le 73 then begin
            ; Example 72: Summer part
            elabcode_sav=elabcode
            obsTemp1=obsTemp & runtemp1=runTemp
            time_operations, request, result, obsTemp1, runTemp1
            obs_run_nan,request,result,obsTemp1, runTemp1
            ; Example 72: Winter part
            elabcode=-elabcode_sav
            request->setElaborationCode, elabCode
            obsTemp2=obsTemp & runtemp2=runTemp
            time_operations, request, result, obsTemp2, runTemp2
            obs_run_nan,request,result,obsTemp2, runTemp2
            elabcode=elabcode_sav
            request->setElaborationCode, elabCode
            ; Formula
            statXYResult[i1,i2,i3,i4,0]=mean(obsTemp1)-mean(obsTemp2)
            ;        runGroupStatResult[k]=2.*(mean(obsTemp1)-mean(obsTemp2) - mean(runTemp1)+mean(runTemp2))/ $
            ;          (mean(obsTemp1)-mean(obsTemp2) + mean(runTemp1)-mean(runTemp2))
            statXYResult[i1,i2,i3,i4,1]=mean(runTemp1)-mean(runTemp2)
            statXYGroup[i1,i2,i3,i4]=nmb([obsTemp1,obsTemp2],[runTemp1,runTemp2])
          endif
          if elabcode eq 74 then begin ;OU Forecast
            sign=stddevOM(obsTemp)-stddevOM(runTemp)  ;only for target
            if finite(sign) eq 1 then sign=sign/abs(sign)   ;only for target
            statXYResult[i1,i2,i3,i4,0]=sign*crmse(obsTemp, runTemp)/resilience(obsTemp)
            statXYResult[i1,i2,i3,i4,1]=bias(obsTemp, runTemp)/resilience(obsTemp)
            statXYGroup[i1,i2,i3,i4]=rmse(obsTemp, runTemp)/resilience(obsTemp)
          endif
        endfor  ;i4
      endfor  ;i3  nsce
    endfor  ;i2  nmod
  endfor  ;i1  npar        
        
  if isGroupSelection then begin
    statXYResultHlp=statXYResult
    for i4=nobsS,Index4-1 do begin
      for i2=0,Index2-1 do begin
      for i3=0,Index3-1 do begin
      for i1=0,Index1-1 do begin
        if finite(statXYResult(i1,i2,i3,i4,0)) eq 0 or finite(statXYResult(i1,i2,i3,i4,1)) eq 0 then begin
          statXYResult(*,*,*,i4,0)=!values.f_nan
          statXYResult(*,*,*,i4,1)=!values.f_nan
          statXYGroup[*,*,*,i4]=!values.f_nan
        endif  
      endfor  
      endfor
      endfor
    endfor
    groupTitles=request->getGroupTitles()
    groupCodes=request->getGroupCodes()
    groupNames=request->getGroupNames()
    groupStatToApplyCode=request->getGroupStatToApplyCode()
    ngroup=n_elements(groupTitles)
    allGroupStations=request->buildAllGroupNames()
    for iG=0,ngroup-1 do begin   ;group
      currCodes=*groupCodes[iG]
      currNames=*groupNames[iG]
      ncurrNames=n_elements(currNames)
      currNumber=intarr(ncurrNames)
      for i=0,ncurrNames-1 do begin ; select stations in groupi4
        chlp=where(currNames[i] eq allGroupStations,ihlp)
        if ihlp ge 1 then currNumber[i]=chlp[0]
      endfor    
      for i1=0,Index1-1 do begin 
      for i2=0,Index2-1 do begin
      for i3=0,Index3-1 do begin 
        statXYGroupHlp=reform(statXYGroup(i1,i2,i3,nobsS+currNumber))
        statXY0=reform(statXYResult(i1,i2,i3,nobsS+currNumber,0))
        statXY1=reform(statXYResult(i1,i2,i3,nobsS+currNumber,1))
        ccFin=where(finite(statXY0) eq 1 and finite(statXY0) eq 1,countfinite)
        ;Mean 100% group         
        if groupStatToApplyCode eq 0 then begin      
          if countFinite gt 0 then begin
            obsGroupStatResult=reform(statXY0(ccFin))
            runGroupStatResult=reform(statXY1(ccFin))
            obsStatResult=mean(obsGroupStatResult)
            runStatResult=mean(runGroupStatResult)
            if elabCode eq 20 or elabcode eq 52 then begin            
              ccNeg=where(statXYGroupHlp lt 0.,countNeg)
              ccPos=where(statXYGroupHlp ge 0.,countPos)
              if countNeg gt countPos then obsStatResult=-obsStatResult
            endif 
          endif else begin
            obsStatResult=!values.f_nan
            runStatResult=!values.f_nan
          endelse
        endif  
        ;Worst 90%% group           
        if groupStatToApplyCode eq 1 then begin 
          if countFinite gt 0 then begin
            obsGroupStatResult=reform(statXY0(ccFin))
            runGroupStatResult=reform(statXY1(ccFin))
            GroupStatResult=reform(statXYGroupHlp(ccFin))
            resSort=sort(GroupStatResult)
            if elabcode eq 2 or elabcode eq 7 or elabcode eq 11 or elabCode eq 15 or elabcode eq 33 then resSort=reverse(resSort)
            medIdx=resSort[fix(0.9*n_elements(resSort))]
            obsStatResult=obsGroupStatResult(medIdx)
            runStatResult=runGroupStatResult(medIdx)
            if elabCode eq 20 or elabCode eq 52 then begin     
              ccNeg=where(statXYGroupHlp(0:medIdx) lt 0.,countNeg)
              ccPos=where(statXYGroupHlp(0:medIdx) ge 0.,countPos)
              if countNeg gt countPos then obsStatResult=-abs(obsStatResult)     
            endif
          endif else begin
            obsStatResult=!values.f_nan
            runStatResult=!values.f_nan
          endelse
        endif
        statXYResultHlp[i1,i2,i3,nobsS+iG,0]=obsStatResult
        statXYResultHlp[i1,i2,i3,nobsS+iG,1]=runStatResult
      endfor ;i3
      endfor ;i2
      endfor  ;i1    
    endfor   ;iG
    statXYResult[*,*,*,nobsS:nobsS+ngroup-1,*]=statXYResultHlp[*,*,*,nobsS:nobsS+ngroup-1,*]
  endif  
END
; ************************************************************************************
PRO PrepareLegends, request, result, ifree, npar, nmod, nsce, nobsS, nobs, obsNames, numStatValid, $
  statValid2, legendNames, legendColors, legendSymbols, statSymbols, statColors    

  groupTitles=request->getGroupTitles()
  parCodes=request->getParameterCodes()
  modelCodes=request->getModelCodes()
  scenarioCodes=request->getScenarioCodes()
  elabcode=request->getElaborationCode()
  obsShortNames=request->getSingleShortObsNames()

  legendNames=strarr(max([npar,nmod,nsce,nobs]),6) & legendNames(*)=''   ; npar, nmod, nsce, nobs
  legendNames(0:npar-1,0)=parCodes
  legendNames(0:nmod-1,1)=modelCodes
  legendNames(0:nsce-1,2)=scenarioCodes
  
  nreg=0
  if nobsS ge 1 then begin
    regNamesAll=strarr(nobsS)
    obsCodes=request->getSingleObsCodes()
    for i=0, nobsS-1 do regNamesAll[i]=request->getRegionofObs(obsCodes[i])
    regNames = regNamesAll[UNIQ(regNamesAll, SORT(regNamesAll))]
    nreg=n_elements(regNames)
    legendNames(0:nobsS-1,3)=obsNames[0:nobsS-1]
    if elabCode eq 10 or elabCode eq 11 or elabCode eq 18 then legendNames(0:nobsS-1,3)=$
      regnamesAll+'&'+obsNames[0:nobsS-1]
  endif  
    
  if nobs gt nobsS then legendNames(nobsS:nobs-1,3)=groupTitles
  if numStatValid ge 1 then hlp=legendNames(statValid2,3)  ; else hlp=' '
  legendNames(*,3)=''
  if numStatValid ge 1 then legendNames(0:numStatValid-1,3)=hlp
  
  if nobsS ge 1 then legendNames(0:nobsS-1,4)=obsshortNames
  if nobs gt nobsS then legendNames(nobsS:nobs-1,4)=groupTitles
  if numStatValid ge 1 then hlp=legendNames(statValid2,4)
  legendNames(*,4)=''
  if numStatValid ge 1 then legendNames(0:numStatValid-1,4)=hlp

  statSymbols=intarr(npar,nmod,nsce,nobs) & statSymbols(*,*,*,*)=-1
  statColors=intarr(npar,nmod,nsce,nobs) & statColors(*,*,*,*)=-1
  if npar ge 1 and nmod eq 1 and nsce eq 1 and nobs eq 1 then begin
    ifree='1000'
    for ipar=0,npar-1 do statColors(ipar,*,*,*)=ipar
    if nobsS eq 1 then statSymbols(*,*,*,*)=9 else statSymbols(*,*,*,*)=13
  endif
  if npar eq 1 and nmod ge 1 and nsce eq 1 and nobs eq 1 then begin
    ifree='0100'
    for imod=0,nmod-1 do statColors(*,imod,*,*)=imod
    if nobsS eq 1 then statSymbols(*,*,*,*)=9 else statSymbols(*,*,*,*)=13
  endif
  if npar eq 1 and nmod eq 1 and nsce ge 1 and nobs eq 1 then begin
    ifree='0010'
    for isce=0,nsce-1 do statColors(*,*,isce,*)=isce
    if nobsS eq 1 then statSymbols(*,*,*,*)=9 else statSymbols(*,*,*,*)=13
  endif
  if npar eq 1 and nmod eq 1 and nsce eq 1 and nobs ge 1 then begin
    ifree='0001'
    if nreg ge 1 then begin
      region_count=intarr(nreg)
      region_count(*)=0
    endif
    for iobs=0,nobs-1 do begin
      if iobs le nobsS-1 then begin
        creg=where(regNamesAll(iobs) eq regNames)
        symbolNumber=region_count(creg) mod 12
        statSymbols(*,*,*,iobs)=symbolNumber
        region_count(creg)=region_count(creg)+1
        statColors(*,*,*,iobs)=creg
      endif else begin
        statSymbols(*,*,*,iobs)=13
        statColors(*,*,*,iobs)=nreg+(iobs-nobsS)
      endelse
    endfor
  endif
  if npar gt 1 and nmod gt 1 and nsce eq 1 and nobs eq 1 then begin
    ifree='1100'
    if nobsS ge 1 then StatSymbols(*,*,*,0:nobsS-1)=9
    if nobs gt nobsS then StatSymbols(*,*,*,nobsS:nobs-1)=13
    for imod=0,nmod-1 do begin
      statColors(*,imod,*,*)=imod
    endfor
  endif
  if npar gt 1 and nmod eq 1 and nsce gt 1 and nobs eq 1 then begin
    ifree='1010'
    if nobsS ge 1 then StatSymbols(*,*,*,0:nobsS-1)=9
    if nobs gt nobsS then StatSymbols(*,*,*,nobsS:nobs-1)=13
    for ipar=0,npar-1 do begin
      statColors(ipar,*,*,*)=ipar
    endfor
  endif
  if npar gt 1 and nmod eq 1 and nsce eq 1 and nobs gt 1 then begin
    ifree='1001'
    if nobsS ge 1 then StatSymbols(*,*,*,0:nobsS-1)=9
    if nobs gt nobsS then StatSymbols(*,*,*,nobsS:nobs-1)=13
    for ipar=0,npar-1 do begin
      statColors(ipar,*,*,*)=ipar
    endfor
  endif
  if npar eq 1 and nmod gt 1 and nsce gt 1 and nobs eq 1 then begin
    ifree='0110'
    if nobsS ge 1 then StatSymbols(*,*,*,0:nobsS-1)=9
    if nobs gt nobsS then StatSymbols(*,*,*,nobsS:nobs-1)=13
    for isce=0,nsce-1 do begin
      statColors(*,*,isce,*)=isce
    endfor
  endif
  if npar eq 1 and nmod gt 1 and nsce eq 1 and nobs gt 1 then begin
    ifree='0101'
    if nobsS ge 1 then StatSymbols(*,*,*,0:nobsS-1)=9
    if nobs gt nobsS then StatSymbols(*,*,*,nobsS:nobs-1)=13
    for imod=0,nmod-1 do begin
      statColors(*,imod,*,*)=imod
    endfor
  endif
  if npar eq 1 and nmod eq 1 and nsce gt 1 and nobs gt 1 then begin
    ifree='0011'
    if nobsS ge 1 then StatSymbols(*,*,*,0:nobsS-1)=9
    if nobs gt nobsS then StatSymbols(*,*,*,nobsS:nobs-1)=13
    for isce=0,nsce-1 do begin
      statColors(*,*,isce,*)=isce
    endfor
  endif
  if npar eq 1 and nmod gt 1 and nsce gt 1 and nobs gt 1 then begin
    ifree='0111'
    if nobsS ge 1 then StatSymbols(*,*,*,0:nobsS-1)=9
    if nobs gt nobsS then StatSymbols(*,*,*,nobsS:nobs-1)=13
    for imod=0,nmod-1 do begin
      statColors(*,imod,*,*)=imod
    endfor      
  endif
  if npar gt 1 and nmod eq 1 and nsce gt 1 and nobs gt 1 then begin
    ifree='1011'
    if nobsS ge 1 then StatSymbols(*,*,*,0:nobsS-1)=9
    if nobs gt nobsS then StatSymbols(*,*,*,nobsS:nobs-1)=13
    for isce=0,nsce-1 do begin
      statColors(*,*,isce,*)=isce
    endfor      
  endif
  if npar gt 1 and nmod gt 1 and nsce eq 1 and nobs gt 1 then begin
    ifree='1101'
    if nobsS ge 1 then StatSymbols(*,*,*,0:nobsS-1)=9
    if nobs gt nobsS then StatSymbols(*,*,*,nobsS:nobs-1)=13
    for imod=0,nmod-1 do begin
      statColors(*,imod,*,*)=imod
    endfor  
  endif
  if npar gt 1 and nmod gt 1 and nsce gt 1 and nobs eq 1 then begin  ; not used 
    ifree='1110'
  endif  
  ; 1111   not used - too complicated for plotting

  if numStatValid ge 1 then begin
    statColors=statColors(*,*,*,statValid2)
    statSymbols=statSymbols(*,*,*,statValid2)
  endif

  legendColors=statColors
  legendSymbols=statSymbols
  legendNames(0,5)=ifree
  legendColors=reform(legendColors)
  legendSymbols=reform(legendSymbols)
  
END  

;****************  insert FM_MeanTS using FM_StatTarget as a base*************
pro FM_MeanTS, request, result

  ; start/end index -> first/last position of "time/data" user selection (datetime selection)
  startIndex=request->getStartIndex()
  endIndex=request->getEndIndex()
  ; standard functions available for "request" object
  npar=request->getParameterNumber()
  nmod=request->getModelNumber()
  modelCodes=request->getModelCodes()
  parCodes=request->getParameterCodes()
  scenarioCodes=request->getScenarioCodes()
  nsce=request->getScenarioNumber()
  isSingleSelection=request->isSingleObsPresent()
  isGroupSelection=request->isGroupObsPresent()
  nobs=0
  ; standard functions available for "result" object
  ; use sMonitIndexes & sRunIndexes to access rawData struct
  if isSingleSelection then begin
    obsNames=request->getSingleObsNames()
    obsCodes=request->getSingleObsCodes()
    obsShortNames=request->getSingleShortObsNames()
    nobs=nobs+request->getSingleObsNumber()
    singleRawData=result->getSingleRawData()
    singleRDMatrix=result->getSingleRawDataCheckMatrix(sMonitIndexes, sRunIndexes)
  endif
  if isGroupSelection then begin
    allGroupStations=request->buildAllGroupNames()
    groupTitlesNumber=request->getGroupTitlesNumber()
    groupTitles=request->getGroupTitles()
    groupStatToApplyCode=request->getGroupStatToApplyCode()
    groupStatToApplyName=request->getGroupStatToApplyName()
    groupCodes=request->getGroupCodes()
    groupNames=request->getGroupNames()
    for i=0, groupTitlesNumber-1 do begin
      print, "*************"
      print, groupTitles[i]
      print, *groupCodes[i]
      print, *groupNames[i]
      print, "*************"
    endfor
    nobs=nobs+n_elements(allGroupStations)
    groupRawData=result->getGroupRawData()
    groupRDMatrix=result->getGroupRawDataCheckMatrix(gMonitIndexes, gRunIndexes)
  endif

  extraValNumber=request->getExtraValuesNumber()
  if extraValNumber gt 0 then begin
    print, 'extra Values available #', extraValNumber
    print, 'ExtraValues: ', request->getExtraValues()
  endif else begin
    print, 'extra Values not available'
  endelse
  obsFact=1
  ; **example** **start**

  mcFlags=request->getMultipleChoiceUserSelectionFlags()
  ; which are multiples?
  whichAreMultiple=where(mcFlags eq 1b, multipleChoicesNo)

  if multipleChoicesNo eq 0 then begin
    multipleChoicesNo = 1
    whichAreMultiple[0] = 1
  endif

  ; ++++++++++++++1 multiple choice+++++++++++++++++
  ; only one multiple choice section **begin**
  ; useful memo: npar, nmod, nsce, nobs
  if multipleChoicesNo eq 1 then begin
    case whichAreMultiple[0] of
      0:begin ;parameters section
      print, '--> Only parameters are multiple'
    end
    ; Only models are multiple
    1:begin ;models section
    print, '--> Only models are multiple'
    if isSingleSelection then begin
      print, 'singles selected!'
      mChoice1runS=singleRawData[sRunIndexes].modelCode
      test1S=modelCodes
      targetPointsNo=n_elements(test1S)
      forSLastIndex=n_elements(test1S)-1
      legendNames=strarr(n_elements(test1S)+1)
      legendNames(0)='OBS'
      legendNames(1:targetPointsNo)=test1S
      targetColors=indgen(n_elements(test1S))
    endif
    if isGroupSelection then begin
      print, 'groups selected!'
      mChoice1runG=groupRawData[gRunIndexes].observedCode
      mChoice2runG=groupRawData[gRunIndexes].modelCode
      test1G=modelCodes
      targetPointsNo=n_elements(test1G)
      forGLastIndex=n_elements(test1G)-1
      legendNames=strarr(n_elements(test1G)+1)
      legendNames(0)='OBS'
      legendNames(1:targetPointsNo)=test1G
      targetColors=indgen(n_elements(test1G))
      forGLastIndex =n_elements(groupTitles)-1
      forGLastIndex2=n_elements(modelCodes)-1
    endif

  end
  ; Only scenarios are multiple **not allowed but leave code here for safe**
  2:begin ;scenarios section
  print, '--> Only scenarios are multiple'
  if isSingleSelection then begin
    print, 'singles selected!'
    mChoice1runS=singleRawData[sRunIndexes].scenarioCode
    test1S=scenarioCodes
    targetPointsNo=n_elements(test1S)
    forSLastIndex=n_elements(test1S)-1
    legendNames=strarr(n_elements(test1S)+1)
    legendNames(0)='OBS'
    legendNames(1:targetPointsNo)=test1S
    targetColors=indgen(n_elements(test1S))
  endif
  if isGroupSelection then begin
    print, 'groups selected!'
    mChoice1runG=groupRawData[gRunIndexes].observedCode
    mChoice2runG=groupRawData[gRunIndexes].scenarioCode
    test1G=scenarioCodes
    targetPointsNo=n_elements(test1G)
    forGLastIndex=n_elements(test1G)-1
    legendNames=strarr(n_elements(test1G)+1)
    legendNames(0)='OBS'
    legendNames(1:targetPointsNo)=test1G
    targetColors=indgen(n_elements(test1G))
    forGLastIndex =n_elements(groupTitles)-1
    forGLastIndex2=n_elements(scenarioCodes)-1
  endif
end
endcase
endif

; here calc a stat!!!!++++++++1 multiple choice sections: execute statistic-related operations+++++++
; 1 multiple choice sections: execute statistic-related operations
;**begin**
if multipleChoicesNo eq 1 then begin
  ; dimension: nb of models/scen+1 for observations; nb of points in time
  statXYResult=fltarr(targetPointsNo+1, endIndex-startIndex+1)
  statSymbols=strarr(targetPointsNo+1)
  statColors=intarr(targetPointsNo+1)

  if isSingleSelection then begin

    for i=0, forSLastIndex do begin
      ; only one choice
      choiceIdx1=(where(mChoice1runS eq test1S[i]))[0]

      obsTemp=*singleRawData[sMonitIndexes[choiceIdx1]].observedData
      runTemp=*singleRawData[sRunIndexes[choiceIdx1]].runData

      time_operations, request, result, obsTemp, runTemp

      obsTemp=obsTemp[startIndex:endIndex]
      runTemp=runTemp[startIndex:endIndex]

      idxs=where((obsTemp eq -999) or (obsTemp eq -8888), count)
      if count gt 0 then obsTemp(idxs)=!values.f_nan

      runIdxs=where(runTemp eq -999, count)
      if count gt 0 then runTemp(runIdxs)=!VALUES.F_NAN

      statXYResult[i+1,*]=runTemp
      statSymbols[i+1]=9
      statColors[i+1]=targetColors[i]
      statXYResult[0,*]=obsTemp
      statSymbols[0]=9
      statColors[0]=0 ;targetColors[i]

    endfor

  endif

  if isGroupSelection then begin

    for k=0,forGLastIndex2 do begin  ;loop models

      currentNames=*groupNames[0]
      validIdxs=n_elements(currentNames)
      obsGroupStatResult=fltarr(validIdxs,endIndex-startIndex+1)
      runGroupStatResult=fltarr(validIdxs,endIndex-startIndex+1)

      for j=0, validIdxs-1 do begin  ;groups loop

        choiceIdx1=(where(mChoice1runG eq currentNames(j) and mChoice2runG eq test1G(k)))[0]

;        print, 'gMonitIndexes[choiceIdx1]', gMonitIndexes[choiceIdx1]
        obsTemp=*groupRawData[gMonitIndexes[choiceIdx1]].observedData



        choiceIdx1=(where(mChoice1runG eq currentNames[j] and mChoice2runG eq test1G(k)))[0]

;        print, 'sRunIndexes[choiceIdx1]', gRunIndexes[choiceIdx1]
        runTemp=*groupRawData[gRunIndexes[choiceIdx1]].runData

        time_operations, request, result, obsTemp, runTemp

        idxs=where((obsTemp eq -999) or (obsTemp eq -8888), count)
        if count gt 0 then obsTemp(idxs)=!values.f_nan

        runIdxs=where(runTemp eq -999, count)
        if count gt 0 then runTemp(runIdxs)=!VALUES.F_NAN

        obsGroupStatResult(j,*)=obsTemp[startIndex:endIndex]
        runGroupStatResult(j,*)=runTemp[startIndex:endIndex]

      endfor

      for jj=0,endIndex-startIndex do begin
        if validIdxs gt 1 then begin
          obshlp=reform(obsGroupStatResult(*,jj))
          runhlp=reform(runGroupStatResult(*,jj))
          ccNan=where(finite(obshlp) eq 1,countFiniteObs)
          if countFiniteObs gt 0 then begin
            statXYResult(0,jj)=mean(obshlp(ccNan))
            statXYResult(k+1,jj)=mean(runhlp(ccNan))
          endif else begin
            statXYResult(0,jj)=!values.f_nan
            statXYResult(k+1,jj)=!values.f_nan
          endelse
        endif
        if validIdxs eq 1 then begin
          obshlp=obsGroupStatResult(jj)
          runhlp=runGroupStatResult(jj)
          if finite(obshlp) eq 1 then begin
            statXYResult(0,jj)=obshlp
            statXYResult(k+1,jj)=runhlp
          endif else begin
            statXYResult(0,jj)=!values.f_nan
            statXYResult(k+1,jj)=!values.f_nan
          endelse
        endif

      endfor

      statSymbols[k+1]=9
      statColors[k+1]=targetColors[k]
      statSymbols[0]=9
      statColors[0]=0

    endfor

  endif

endif
legendColors=statColors
legendSymbols=statsymbols
; 1 multiple choice section --end--

result->setGenericPlotInfo, statXYResult, statSymbols, statColors, legendNames, legendColors, legendSymbols

end



pro FM_StatTable2, request, result

  ; start/end index -> first/last position of "time/data" user selection (datetime selection)
  startIndex=request->getStartIndex()
  endIndex=request->getEndIndex()
  ; standard functions available for "request" object
  npar=request->getParameterNumber()
  nmod=request->getModelNumber()
  modelCodes=request->getModelCodes()
  parCodes=request->getParameterCodes()
  scenarioCodes=request->getScenarioCodes()
  nsce=request->getScenarioNumber()
  isSingleSelection=request->isSingleObsPresent()
  isGroupSelection=request->isGroupObsPresent()
  categoryInfo=request->getSingleObsCatInfos()
  elabcode=request->getElaborationCode()
  nobs=0
  ; standard functions available for "result" object
  ; use sMonitIndexes & sRunIndexes to access rawData struct
  if isSingleSelection then begin
    obsNames=request->getSingleObsNames()
    obsCodes=request->getSingleObsCodes()
    obsShortNames=request->getSingleShortObsNames()
    obsLatitudes=request->getSingleObsLatitudes()
    obsLongitudes=request->getSingleObsLongitudes()
    obsAltitudes=request->getSingleObsAltitudes()
    nobs=nobs+request->getSingleObsNumber()
    singleRawData=result->getSingleRawData()
    singleRDMatrix=result->getSingleRawDataCheckMatrix(sMonitIndexes, sRunIndexes)
  endif
  if isGroupSelection then begin
    allGroupStations=request->buildAllGroupNames()
    groupTitlesNumber=request->getGroupTitlesNumber()
    groupTitles=request->getGroupTitles()
    groupStatToApplyCode=request->getGroupStatToApplyCode()
    groupStatToApplyName=request->getGroupStatToApplyName()
    ;    groupStatToApply=request->getGroupStatToApply()
    groupCodes=request->getGroupCodes()
    groupNames=request->getGroupNames()
    for i=0, groupTitlesNumber-1 do begin
      print, "*************"
      print, groupTitles[i]
      print, *groupCodes[i]
      print, *groupNames[i]
      print, "*************"
    endfor
    nobs=nobs+n_elements(allGroupStations)
    groupRawData=result->getGroupRawData()
    groupRDMatrix=result->getGroupRawDataCheckMatrix(gMonitIndexes, gRunIndexes)
  ;nobs=nobs+nobs=nobs+
  endif

  extraValNumber=request->getExtraValuesNumber()
  if extraValNumber gt 0 then begin
    print, 'extra Values available #', extraValNumber
    print, 'ExtraValues: ', request->getExtraValues()
    extraVal=request->getExtraValues()
  endif else begin
    print, 'extra Values not available'
  endelse
  limitValue=extraVal(0)
  ;  PercValue=extraVal(1)

  scaleInfo=request->getScaleInfo()
  resScale=strsplit(scaleinfo,';',/extract)
  scaleName=resScale(0)
  scaleName=STRUPCASE(scaleName)


  ;set threshold values for RDE and RPE (only for O3 and PM10
  hourStat=request->getGroupByTimeInfo() ;HourType
;  flag_average=fix(hourStat[0].value)
  statType=request->getGroupByStatInfo() ;HourType



  mcFlags=request->getMultipleChoiceUserSelectionFlags()
  ; which are multiples?
  whichAreMultiple=where(mcFlags eq 1b, multipleChoicesNo)

  if multipleChoicesNo eq 0 then begin
    multipleChoicesNo = 1
    whichAreMultiple[0] = 3
  endif

  ; ++++++++++++++1 multiple choice+++++++++++++++++
  ; only one multiple choice section **begin**
  if multipleChoicesNo eq 1 then begin
    case whichAreMultiple[0] of

      ; Only observations are multiple
      3:begin ;observations section
      print, '--> Only observations are multiple'

      if isSingleSelection then begin
        print, 'singles selected!'
        mChoice1runS=singleRawData[sRunIndexes].observedCode
        test1=obsNames
        forSLastIndex=n_elements(test1)-1
        statsymbolsS=intarr(forSLastIndex+1)
        statSymbolsS(*)=9
        legendSymbolsS=obsNames
        statcolorsS=intarr(forSLastIndex+1)
        statcolorsS(*)=3
        nobsS=n_elements(test1)
      endif
      if isGroupSelection then begin
        print, 'groups selected!'
        mChoice1runG=groupRawData[gRunIndexes].observedCode
        forGLastIndex=n_elements(groupTitles)-1
        statsymbolsG=intarr(forGLastIndex+1)
        statSymbolsG(*)=13
        legendSymbolsG=groupTitles
        statcolorsG=indgen(forGLastIndex+1)+2
        ;        statcolorsG(*)=3
        nobsG=forGLastIndex+1
      endif
    end
  endcase
endif else begin
; multiple choice not allowed
endelse

nvar=8
legendNames=['Mean','Exceed,','Bias Norm','Corr Norm','StdDev Norm','Corr Norm','StdDeV Norm','RDE']

if isSingleSelection eq 0 then countFiniteS=0
if isSingleSelection eq 0 then nobsS=0
if isGroupSelection eq 0 then countFiniteG=0
if isGroupSelection eq 0 then nobsG=0

if isSingleSelection then begin

  regNamesAll=strarr(nobs)
  for i=0, nobsS-1 do regNamesAll[i]=request->getRegionofObs(obsCodes[i]);regionCode=request->getRegionofObs(obsCodes[i])
  ;  obsFact=1

  fileName=  modelCodes(0)+'_'+parcodes(0)+'.dat'  ;Printing only in case of single stations choice
  request->openDataDumpFile, fileName;/ADDSYSTIME; --> filename=StatisticName+systime+.txt
  request->writeDataDumpFileRecord, 'Name Obscode Region Type lon lat alt targ targY targX MO MM SO SM NMB R RDE NMSD ExcO ExcM TargOU OU'

  statXYResultS=fltarr(forSLastIndex+1,nvar)

  for i=0, forSLastIndex do begin

    choiceIdx1=(where(mChoice1runS eq test1(i)))[0]
    obsTemp=*singleRawData[sMonitIndexes[choiceIdx1]].observedData
    runTemp=*singleRawData[sRunIndexes[choiceIdx1]].runData
    time_operations, request, result, obsTemp, runTemp
    obs_run_nan,request,result,obsTemp, runTemp

    if elabCode eq 31 then CheckCriteria, request, result, 'OU', criteriaOU, obsTemp, 0,alpha,criteriaOrig,LV,nobsAv
    if elabCode eq 32 then CheckCriteria, request, result, 'OU', criteriaOU, obsTemp, 1,alpha,criteriaOrig,LV,nobsAv

    if elabcode eq 32 then obsTemp(*)=mean(obsTemp)
;    criteriaOU=criteria/100.

    statXYResultS(i,0)=mean(obsTemp)
    statXYResultS(i,6)=mean(runTemp)
    cExcMod=where(runTemp gt limitValue,countExcMod)
    cExcObs=where(obsTemp gt limitValue,countExcobs)
    if finite(statXYresultS(i,0)) eq 1 then statXYResultS(i,1)=countExcObs
    if finite(statXYresultS(i,0)) ne 1 then statXYResultS(i,1)=!values.f_nan
    statXYResultS(i,5)=countExcMod
    if statType gt 0 then statXYResultS(i,5)=statXYResultS(i,5)/24.
    if statType gt 0 then statXYResultS(i,1)=statXYResultS(i,1)/24.
    
    CheckCriteria, request, result, 'OU', criteriaOU, obsTemp, 1,alpha,criteriaOrig,LV,nobsAv
    statXYResultS(i,2)=(mean(runTemp)-mean(obsTemp))/(2*CriteriaOU)
    if elabCode eq 31 then CheckCriteria, request, result, 'OU', criteriaOU, obsTemp, 0,alpha,criteriaOrig,LV,nobsAv
    if elabCode eq 32 then CheckCriteria, request, result, 'OU', criteriaOU, obsTemp, 1,alpha,criteriaOrig,LV,nobsAv
    
    statXYResultS(i,3)=(1.-correlate(obsTemp, runTemp))/(2*(CriteriaOU/stddevOM(obsTemp))^2)
    statXYResultS(i,4)=(stddevOM(obsTemp)-stddevOM(runTemp))/(2.*CriteriaOU)
    statXYResultS(i,7)=rde(obsTemp,runTemp,limitValue)
    if elabcode eq 32 then statXYResultS(i,7)=rdeYearly(obsTemp,runTemp,limitValue)

    txt=string(obsnames(i),obsCodes(i), regNamesAll(i),categoryInfo(1,i),$
      obsLongitudes(i), obsLatitudes(i), obsAltitudes(i),$
      rmse(obsTemp, runTemp)/(2.*CriteriaOU(0)),$
      bias(obsTemp, runTemp)/(2.*CriteriaOU(0)),crmse(obsTemp, runTemp)/(2.*CriteriaOU(0)),$
      mean(obsTemp),mean(runTemp),stddevOM(obsTemp),stddevOM(runTemp),$
      nmb(obsTemp, runTemp)/100.,$
      correlate(obsTemp, runTemp),rde(obsTemp,runTemp,extraVal(0))/100.,$
      nmb(stddevOM(runTemp),stddevOM(obsTemp)),$
      statXYResultS(i,1),statXYResultS(i,5), $
      rmse(obsTemp, runTemp)/(2.*CriteriaOU(0)),CriteriaOU(0),$
      format='(a'+string(strlen(obsnames(i)))+',1x,a10,1x,a10,1x,a20,21(1x,f8.3))')
    request->writeDataDumpFileRecord, txt

  endfor
  request->closeDataDumpFile
  cc=where(finite(statXYResultS(*,0)) eq 1, countFiniteS)

  if countFiniteS gt 1 then begin
    adummy=statXYResultS(cc,0)
    CheckCriteria, request, result, 'OU', criteriaOU, adummy, 1,alpha,criteriaOrig,LV,nobsAv
    statXYResultS(*,5)=(1.-correlate(statXYResultS(cc,0), statXYResultS(cc,6)))/(2*(CriteriaOU/stddevOM(statXYResultS(cc,0)))^2)
    statXYResultS(*,6)=(stddevOM(statXYResultS(cc,0))-stddevOM(statXYResultS(cc,6)))/(2.*CriteriaOU)
  endif else begin
    statXYResultS(*,5)=!values.f_nan
    statXYResultS(*,6)=!values.f_nan
  endelse

  ahlp=statXYResultS(*,0)
  ccFin=where(finite(ahlp) eq 1,countFin)

  if countFin gt 0 then begin
    statXYResultS=reform(statXYResultS(ccFin,*))
    statSymbolsS=reform(statSymbolsS(ccFin))
    statcolorsS=reform(statcolorsS(ccFin))
    legendSymbolsS=reform(legendSymbolsS(ccFin))
  endif



endif

if isGroupSelection then begin

  statXYResultG=fltarr(forGLastIndex+1,nvar)

  for i=0, forGLastIndex do begin
    currentCodes=*groupCodes[i]
    currentNames=*groupNames[i]
    validIdxs=n_elements(currentNames)
    statXYResultInt=fltarr(validIdxs,nvar)

    for j=0, validIdxs-1 do begin
      choiceIdx1=(where(mChoice1runG eq currentNames[j]))[0]
;      print, 'gMonitIndexes[choiceIdx1]', gMonitIndexes[choiceIdx1]
      obsTemp=*groupRawData[gMonitIndexes[choiceIdx1]].observedData
      runTemp=*groupRawData[gRunIndexes[choiceIdx1]].runData
      time_operations, request, result, obsTemp, runTemp
      ;      obsTemp=obsTemp[startIndex:endIndex]
      ;      runTemp=runTemp[startIndex:endIndex]
      obs_run_nan,request,result,obsTemp, runTemp

      CheckCriteria, request, result, 'OU', criteriaOU, obsTemp, 0,alpha,criteriaOrig,LV,nobsAv

      statXYResultInt(j,0)=mean(obsTemp)
      statXYResultInt(j,6)=mean(runTemp)
      cExcMod=where(runTemp gt limitValue,countExcMod)
      cExcObs=where(obsTemp gt limitValue,countExcobs)
      if finite(statXYResultInt(j,0)) eq 1 then statXYResultInt(j,1)=countExcObs
      if finite(statXYResultInt(j,0)) ne 1 then statXYResultInt(j,1)=!values.f_nan
      statXYResultInt(j,5)=countExcMod
      if statType gt 0 then statXYResultInt(j,5)=statXYResultInt(j,5)/24.
      if statType gt 0 then statXYResultInt(j,1)=statXYResultInt(j,1)/24.
      statXYResultInt(j,2)=(mean(runTemp)-mean(obsTemp))/(2*CriteriaOU)
      statXYResultInt(j,3)=(1.-correlate(obsTemp, runTemp))/(2*(CriteriaOU/stddevOM(obsTemp))^2)
      statXYResultInt(j,4)=(stddevOM(obsTemp)-stddevOM(runTemp))/(2.*CriteriaOU)
      statXYResultInt(j,7)=rde(obsTemp,runTemp,limitValue)
    endfor

    ahlp=statXYResultInt(*,0)
    ccFin=where(finite(ahlp) eq 1,countfinite)
    if countfinite gt 0 then statXYResultInt=reform(statXYResultInt(ccFin,*))

    if groupStatToApplyCode eq 1 then begin ;worst among 90% percentile
      if countFinite gt 0 then begin
        for iv=0,nvar-1 do begin
          if countFinite eq 1 then ahlp=statXYResultInt(iv)
          if countFinite gt 1 then ahlp=statXYResultInt(*,iv)
          resSort=sort(abs(ahlp))
          if iv eq 3 then resSort=reverse(resSort)
          medIdx=resSort[fix(0.9*n_elements(resSort))]
          statXYResultG(i,iv)=ahlp(medIdx)
        endfor
      endif else begin
        statXYResultG(i,*)=!values.f_nan
      endelse
    endif
    if groupStatToApplyCode eq 0 then begin ;Mean
      if countFinite gt 0 then begin
        for iv=0,nvar-1 do begin
          if countFinite eq 1 then ahlp=statXYResultInt(iv)
          if countFinite gt 1 then ahlp=statXYResultInt(*,iv)
          statXYResultG(i,iv)=mean(ahlp,/nan)
        endfor
      endif else begin
        statXYResultG(i,*)=!values.f_nan
      endelse
    endif
    cc=where(finite(statXYResultG(*,0)) eq 1,countFiniteG)
    if countfinite gt 1 then begin
      adummy=statXYResultInt(ccFin,0)
      CheckCriteria, request, result, 'OU', criteria, adummy, 1,alpha,criteriaOrig,LV,nobsAv
      statXYResultG(*,5)=(1.-correlate(statXYResultInt(ccFin,0), statXYResultInt(ccFin,6)))/(2*(CriteriaOU/stddevOM(statXYResultInt(ccFin,0)))^2)
      statXYResultG(*,6)=(stddevOM(statXYResultInt(ccFin,0))-stddevOM(statXYResultInt(ccFin,6)))/(2.*CriteriaOU)
    endif else begin
      statXYResultG(i,5)=!values.f_nan
      statXYResultG(i,6)=!values.f_nan
    endelse
  endfor

  ahlp=statXYResultG(*,0)
  ccFin=where(finite(ahlp) eq 1,countFin)

  if countFin gt 0 then begin
    statXYResultG=reform(statXYResultG(ccFin,*))
    statSymbolsG=reform(statSymbolsG(ccFin))
    statcolorsG=reform(statcolorsG(ccFin))
    legendSymbolsG=reform(legendSymbolsG(ccFin))
  endif

endif

if countFiniteS+countFiniteG gt 0 then begin
  statXYResult=fltarr(countFiniteS+countFiniteG, nvar)
  statSymbols=intarr(countFiniteS+countFiniteG)
  statcolors=intarr(countFiniteS+countFiniteG)
  legendSymbols=strarr(countFiniteS+countFiniteG)
  ;
  if countFiniteG gt 0 then statXYResult(0:countFiniteG-1,*)=statXYResultG
  if countFiniteS gt 0 then statXYResult(countFiniteG:countFiniteG+countFiniteS-1,*)=statXYResultS
  if countFiniteG gt 0 then statSymbols(0:countFiniteG-1,*)=statSymbolsG
  if countFiniteS gt 0 then statSymbols(countFiniteG:countFiniteG+countFiniteS-1,*)=statSymbolsS
  if countFiniteG gt 0 then statcolors(0:countFiniteG-1,*)=statcolorsG
  if countFiniteS gt 0 then statcolors(countFiniteG:countFiniteG+countFiniteS-1,*)=statcolorsS
  if countFiniteG gt 0 then legendSymbols(0:countFiniteG-1,*)=legendSymbolsG
  if countFiniteS gt 0 then legendSymbols(countFiniteG:countFiniteG+countFiniteS-1,*)=legendSymbolsS

  legendColors=intarr(4)
  legendColors[1]=countFiniteS+countFiniteG
  legendColors[0]=nobsS+nobsG
  legendColors(2)=limitValue
;  legendColors(3)=PercValue

endif else begin

  statXYResult=fltarr(1, nvar)
  statXYResult(0,*)=!values.f_nan
  statSymbols=intarr(1)
  statSymbols=9
  statcolors=intarr(1)
  statcolors=2
  legendColors=intarr(4)
  legendColors[1]=countFiniteS+countFiniteG
  legendColors[0]=nobsS+nobsG
  legendColors(2)=limitValue
;  legendColors(3)=PercValue


endelse

result->setGenericPlotInfo, statXYResult, statSymbols, statColors, legendNames, legendColors, legendSymbols
end


;****************************************************************************************************

PRO FM_GoogleEarth, request, result

  ; start/end index -> first/last position of "time/data" user selection (datetime selection)
  startIndex=request->getStartIndex()
  endIndex=request->getEndIndex()
  ; standard functions available for "request" object
  npar=request->getParameterNumber()
  nmod=request->getModelNumber()
  modelCodes=request->getModelCodes()
  ;  obsNames=request->getSingleObsNames()
  ;  obsCodes=request->getSingleObsCodes()
  ;  obsShortNames=request->getSingleShortObsNames()
  obsLatitudes=request->getSingleObsLatitudes()
  obsLongitudes=request->getSingleObsLongitudes()
  obsAltitudes=request->getSingleObsAltitudes()
  parCodes=request->getParameterCodes()
  scenarioCodes=request->getScenarioCodes()
  nsce=request->getScenarioNumber()
  nobs=request->getSingleObsNumber()
  obsGMT=request->getSingleObsGMTs()
  isSingleSelection=request->isSingleObsPresent()
  isGroupSelection=request->isGroupObsPresent()
  statType=request->getGroupByStatInfo() ;HourType/Mean/Max/Min
  nobs=0
  ; standard functions available for "result" object
  ; use sMonitIndexes & sRunIndexes to access rawData struct
  if isSingleSelection then begin
    categoryInfo=request->getSingleObsCatInfos()
    obsNames=request->getSingleObsNames()
    obsCodes=request->getSingleObsCodes()
    obsShortNames=request->getSingleShortObsNames()
    nobs=nobs+request->getSingleObsNumber()
    singleRawData=result->getSingleRawData()
    singleRDMatrix=result->getSingleRawDataCheckMatrix(sMonitIndexes, sRunIndexes)
  endif
  Sing=1
  if isGroupSelection eq 1 then begin
    rsult=dialog_message(['GoogleEarth not available for current choice',$
      'GoogleEarth not available for groups',$
      'GoogleEarth only for individual stations',' ',$
      'CONTINUE for individual stations ?'],/question)
    if rsult eq 'No' then begin
      Sing=0
      goto,endGE
    endif
    if isSingleSelection eq 0 then begin
      rsult=dialog_message(['GoogleEarth not available for current choice',$
        'No individual stations selected'],/error)
      Sing=0
      goto,endGE
    endif
  endif

  regNamesAll=strarr(nobs)

  ;for i=0, nobs-1 do print, obsCodes[i],'**', obsNames[i],'**', request->getRegionofObs(obsCodes[i]);regionCode=request->getRegionofObs(obsCodes[i])
  for i=0, nobs-1 do regNamesAll[i]=request->getRegionofObs(obsCodes[i]);regionCode=request->getRegionofObs(obsCodes[i])
  regNames = regNamesAll[UNIQ(regNamesAll, SORT(regNamesAll))]

  ; access to goals & criteria table (see configuration dir)
  ; **example** **start**
  mParameter=parCodes[0] & mScalename='LOCAL' & mStatName='IOA' & timeAvgName='N/A'
  gcValues=request->getGoalsCriteriaValues(parameter=mParameter, scalename=mScalename, statname=mStatName, timeAvgName=timeAvgName, NOVALUES=NOVALUES)
  print, 'query GC for:', ' parameter=', mParameter, '**scalename=', mScalename, '**statname=', mStatName
  if keyword_set(NOVALUES) then print, 'No values' else print, gcValues
  print, '***End GC query***'
  ; **example** **end**

  ; access to extra values (thresholds, references...) user input example **start**
  ; **example** **start**
  extraValNumber=request->getExtraValuesNumber()
  if extraValNumber gt 0 then begin
    print, 'extra Values available #', extraValNumber
    print, 'ExtraValues: ', request->getExtraValues()
  endif else begin
    print, 'extra Values not available'
  endelse
  obsFact=1
  ; **example** **start**

  ;  rawData=result->getRawData()
  ;  rDMatrix=result->getRawDataCheckMatrix(monitIndexes, runIndexes)

  ; 2 multiple choices sections --begin--+++++++++++++++++++++++++++++++

  mixedMode='Mixed mode: observations+parameters+models'
  print,mixedMode
  mChoice1run=singlerawData[srunIndexes].observedCode
  mChoice2run=singlerawData[srunIndexes].parameterCode
  mChoice3run=singlerawData[srunIndexes].modelCode
  inan=0  ; 010
  legoTest1=singlerawData[smonitIndexes].observedCode
  legoTest2=singlerawData[smonitIndexes].parameterCode
  legoNames1=obsShortNames
  legoNames2=parCodes
  legoNames3=ModelCodes
  test1=obsNames
  test2=parCodes
  test3=modelCodes
  ntest1=n_elements(test1)
  ntest2=n_elements(test2)
  ntest3=n_elements(test3)
  legendColors=indgen(n_elements(test1)*n_elements(test2)*n_elements(test3))
  legendSymbols=n_elements(test1)*n_elements(test2)*n_elements(test3)

  LastIndex1=n_elements(test1)-1
  LastIndex2=n_elements(test2)-1
  LastIndex3=n_elements(test3)-1
  longstat=fltarr(ntest1+1,ntest2+1,ntest3+1)
  latstat=fltarr(ntest1+1,ntest2+1,ntest3+1)
  altstat=fltarr(ntest1+1,ntest2+1,ntest3+1)
  for j=0,ntest2-1 do begin
    for k=0,ntest3-1 do begin
      longstat(0:ntest1-1,j,k)=float(obslongitudes)
      latstat(0:ntest1-1,j,k)=float(obslatitudes)
      altstat(0:ntest1-1,j,k)=float(obsaltitudes)
    endfor
  endfor

  statXYResult=fltarr(ntest1+1,ntest2+1,ntest3+1,5) & statXYResult(*,*,*,*)=!values.f_nan
  statSymb=strarr(ntest1+1,ntest2+1,ntest3+1,5)
  for j=0,ntest2-1 do begin
    for k=0,ntest3-1 do begin
      statSymb(0:ntest1-1,j,k,0)=obsShortNames
      statSymb(0:ntest1-1,j,k,1)=obscodes
      statSymb(0:ntest1-1,j,k,2)=regNamesAll
      statSymb(0:ntest1-1,j,k,3)=obsGMT
      statSymb(0:ntest1-1,j,k,4)=test1
    endfor
  endfor
  statNb=0
  for i=0,ntest1-1 do begin
    for j=0,ntest2-1 do begin
      for k=0,ntest3-1 do begin
        legoIdx=(where(legoTest1 eq test1(i) and legoTest2 eq test2(j)))[0]
        obsTemp=*singlerawData[smonitIndexes[legoIdx]].observedData
        choiceIdx1=(where(mChoice1run eq test1[i] and mChoice2run eq test2[j] and $
          mChoice3run eq test3[k]))[0]
        if choiceIdx1 ne -1 then begin
          runTemp=*singlerawData[srunIndexes[choiceIdx1]].runData
          
          time_operations, request, result, obsTemp, runTemp
          obs_run_nan,request,result,obsTemp, runTemp
          elabcode=request->getElaborationCode() ;ElaborationCode 58 or 59, ...
          if finite(mean(obsTemp,/nan)) eq 1 and finite(mean(runTemp,/nan)) eq 1 then begin
            if elabCode eq 58 then begin  ;Mean values
              statXYResult[i,j,k,0]=mean(obsTemp)
              statXYResult[i,j,k,1]=mean(runTemp)
            endif
            if elabCode eq 59 then begin  ;ExcDays and ExcDays        
              ExtraValues=request->getExtraValues()
              refValue=ExtraValues[0]
              obsExc=0
              runExc=0
              cfin1=where(finite(obsTemp) eq 1,countFin1)
              cfin0=where(finite(obsTemp) eq 0,countFin0)
              if countFin0 ge 1 then obsTemp(cfin0)=-999
              cobs=where(obsTemp gt extraValues(0),countobs)
              statXYResult[i,j,k,0]=countobs
              if statType ge 1 then statXYResult[i,j,k,0]=countobs/24
              if countFin1 eq 0 then statXYResult[i,j,k,0]=!values.f_nan
              cfin1=where(finite(runTemp) eq 1,countFin1)
              cfin0=where(finite(runTemp) eq 0,countFin0)
              if countFin0 ge 1 then runTemp(cfin0)=-999
              crun=where(runTemp gt extraValues(0),countrun)
              statXYResult[i,j,k,1]=countrun
              if statType ge 1 then statXYResult[i,j,k,1]=countrun/24
              if countFin1 eq 0 then statXYResult[i,j,k,1]=!values.f_nan
            endif
            if elabCode eq 60 then begin  ;Bias
              statXYResult[i,j,k,0]=mean(obsTemp)
              statXYResult[i,j,k,1]=bias(obsTemp,runTemp)
            endif
            if elabCode eq 61 then begin  ;MeanNormalisedBias
              statXYResult[i,j,k,0]=mean(obsTemp)
              statXYResult[i,j,k,1]=mnb(obsTemp,runTemp)
            endif
            if elabCode eq 62 then begin ;Stddev 62
              statXYResult[i,j,k,0]=stddevOM(obsTemp)
              statXYResult[i,j,k,1]=stddevOM(runTemp)
            endif
            if elabCode eq 63 then begin ;CorrCoeff 63
              statXYResult[i,j,k,0]=correlate(obsTemp, obsTemp)  ; = 1 needed in plot routine
              statXYResult[i,j,k,1]=correlate(obsTemp, runTemp)
            endif
            if elabCode eq 64 then begin ;RMSE 64
              statXYResult[i,j,k,0]=rmse(obsTemp, obsTemp)  ; = 0 needed in plot routine
              statXYResult[i,j,k,1]=rmse(obsTemp, runTemp)
            endif
            if elabCode eq 65 then begin ;AOT 65        
              ExtraValues=request->getExtraValues()
              refValue=ExtraValues[0]
              obsAOT=0.
              runAOT=0.
              cfin0=where(finite(obsTemp) eq 0,countFin0)
              cfin1=where(finite(obsTemp) eq 1,countFin1)
              if countFin0 ge 1 then obsTemp(cfin0)=-999
              obsAOT=total(  (obsTemp ge refValue)*(obsTemp-refValue) )
              if countFin1 eq 0 then obsAOT=!values.f_nan
              cfin0=where(finite(runTemp) eq 0,countFin0)
              cfin1=where(finite(runTemp) eq 1,countFin1)
              if countFin0 ge 1 then runTemp(cfin0)=-999
              runAOT=total(  (runTemp ge refValue)*(runTemp-refValue) )
              if countFin1 eq 0 then runAOT=!values.f_nan
              statXYResult[i,j,k,0]=obsAOT/1000.
              statXYResult[i,j,k,1]=runAOT/1000.
            endif
            if elabCode eq 66 then begin ;SOMO 66       
              ExtraValues=request->getExtraValues()
              refValue=ExtraValues[0]
              obsSOMO=0
              runSOMO=0
              cfin0=where(finite(obsTemp) eq 0,countFin0)
              cfin1=where(finite(obsTemp) eq 1,countFin1)
              if countFin0 ge 1 then obsTemp(cfin0)=-999
              obsSOMO=total(  (obsTemp ge refValue)*(obsTemp-refValue) )
              if statType ge 1 then statXYResult[i,j,k,0]=obsSOMO/24
              if countFin1 eq 0 then obsSOMO=!values.f_nan
              cfin0=where(finite(runTemp) eq 0,countFin0)
              cfin1=where(finite(runTemp) eq 1,countFin1)
              if countFin0 ge 1 then runTemp(cfin0)=-999
              runSOMO=total(  (runTemp ge refValue)*(runTemp-refValue) )
              if statType ge 1 then statXYResult[i,j,k,1]=runSOMO/24
              if countFin1 eq 0 then runSOMO=!values.f_nan
              statXYResult[i,j,k,0]=obsSOMO/1000.
              statXYResult[i,j,k,1]=runSOMO/1000.
            endif
            if elabCode eq 67 then begin ;RDE 67
              ExtraValues=request->getExtraValues()
              refValue=ExtraValues[0]
              statXYResult[i,j,k,0]=rde(obsTemp, obsTemp,refValue)  ; = 0 needed in plot routine
              statXYResult[i,j,k,1]=rde(obsTemp, runTemp,refValue)
            endif
            if elabCode eq 68 then begin ;sigM/sigO 68
              statXYResult[i,j,k,0]=stddevOM(obsTemp)/stddevOM(obsTemp)  ; = 1 needed in plot routine
              statXYResult[i,j,k,1]=stddevOM(runTemp)/stddevOM(obsTemp)
            endif
            if elabCode eq 69 then begin ;Normalized Mean Stddev 69            
              statXYResult[i,j,k,0]=100.*(stddevOM(runTemp)-stddevOM(obsTemp))/stddevOM(obsTemp)
              statXYResult[i,j,k,1]=100.*(stddevOM(runTemp)-stddevOM(obsTemp))/stddevOM(runTemp)
            endif
            if elabCode eq 70 then begin ;Target 70
              statXYResult[i,j,k,0]=1. ; needed for plot routine
              statXYResult[i,j,k,1]=rmse(obsTemp, runTemp)/stddevOM(obsTemp)
            endif
            statXYResult(i,j,k,2)=longstat(i,j,k)
            statXYResult(i,j,k,3)=latstat(i,j,k)
            statXYResult(i,j,k,4)=altstat(i,j,k)
            statNb=statNb+1
          endif
        endif
      endfor
    endfor
  endfor

  endGE:
  if Sing eq 0 then statXYResult[0]='AllNaN'
  result->setGenericPlotInfo, statXYResult, statSymb, statNb, legoNames1, legendColors, legendSymbols

END

;****************
PRO FM_ConditionScatter, request, result
  ;****************************************************************************************************

  ; start/end index -> first/last position of "time/data" user selection (datetime selection)
  startIndex=request->getStartIndex()
  endIndex=request->getEndIndex()
  ; standard functions available for "request" object
  npar=request->getParameterNumber()
  nmod=request->getModelNumber()
  modelCodes=request->getModelCodes()
  parCodes=request->getParameterCodes()
  scenarioCodes=request->getScenarioCodes()
  nsce=request->getScenarioNumber()
  isSingleSelection=request->isSingleObsPresent()
  isGroupSelection=request->isGroupObsPresent()
  nobsS=0
  ; standard functions available for "result" object
  ; use sMonitIndexes & sRunIndexes to access rawData struct
  if isSingleSelection then begin
    obsNames=request->getSingleObsNames()
    obsCodes=request->getSingleObsCodes()
    obsShortNames=request->getSingleShortObsNames()
    nobsS=request->getSingleObsNumber()
    singleRawData=result->getSingleRawData()
    singleRDMatrix=result->getSingleRawDataCheckMatrix(sMonitIndexes, sRunIndexes)
  endif
  ngroup=0
  if isGroupSelection then begin
    allGroupStations=request->buildAllGroupNames()
    groupTitlesNumber=request->getGroupTitlesNumber()
    groupTitles=request->getGroupTitles()
;    groupStatToApply=request->getGroupStatToApply()
    groupCodes=request->getGroupCodes()
    groupNames=request->getGroupNames()
    for i=0, groupTitlesNumber-1 do begin
      print, "*************"
      print, groupTitles[i]
      print, *groupCodes[i]
      print, *groupNames[i]
      print, "*************"
    endfor
    ngroup=n_elements(allGroupStations)
    groupRawData=result->getGroupRawData()
    groupRDMatrix=result->getGroupRawDataCheckMatrix(gMonitIndexes, gRunIndexes)
  endif
  nobs=nobsS+ngroup
  
  if isSingleSelection then begin
    regNamesAll=strarr(nobsS)
    for i=0, nobsS-1 do regNamesAll[i]=request->getRegionofObs(obsCodes[i]);regionCode=request->getRegionofObs(obsCodes[i])
    regNames = regNamesAll[UNIQ(regNamesAll, SORT(regNamesAll))]
  endif  
  nreg=n_elements(regNames)

  ; access to goals & criteria table (see configuration dir)
  ; **example** **start**
  mParameter=parCodes[0] & mScalename='LOCAL' & mStatName='IOA' & timeAvgName='N/A'
  gcValues=request->getGoalsCriteriaValues(parameter=mParameter, scalename=mScalename, statname=mStatName, timeAvgName=timeAvgName, NOVALUES=NOVALUES)
  print, 'query GC for:', ' parameter=', mParameter, '**scalename=', mScalename, '**statname=', mStatName
  if keyword_set(NOVALUES) then print, 'No values' else print, gcValues
  print, '***End GC query***'
  ; **example** **end**

  ; access to extra values (thresholds, references...) user input example **start**
  ; **example** **start**
  extraValNumber=request->getExtraValuesNumber()
  if extraValNumber gt 0 then begin
    print, 'extra Values available #', extraValNumber
    print, 'ExtraValues: ', request->getExtraValues()
    extraval=request->getExtraValues()
  endif else begin
    print, 'extra Values not available'
  endelse
  obsFact=1

  mcFlags=request->getMultipleChoiceUserSelectionFlags()
  ; which are multiples?
  whichAreMultiple=where(mcFlags eq 1b, multipleChoicesNo)

  if multipleChoicesNo eq 0 then begin
    multipleChoicesNo = 1
    whichAreMultiple[0] = 3
  endif

  ; ++++++++++++++1 multiple choice+++++++++++++++++
  ; only one multiple choice section **begin**
  if multipleChoicesNo eq 1 then begin
    case whichAreMultiple[0] of
      ; Only parameters are multiple
      0:begin ;parameters section
      print, '--> Only parameters are multiple'
      mChoice1run=singleRawData[sRunIndexes].parameterCode
      test1=parCodes
      legendNames=test1
      legoNames=parCodes
      ;      statResult=fltarr(1)
      ; tek color starts from index 2(red)
      targetColors=intarr(2)
      targetPointsNo=2
      legendColors=indgen(n_elements(targetPointsNo))
      legendSymbols=strarr(2)
      statColors=indgen(2)
      statSymbols=intarr(2)
      statSymbols(*)=9
    end
  endcase
endif else begin
  if ((whichAreMultiple[0] eq 0) and (whichAreMultiple[1] eq 3)) or $
    ((whichAreMultiple[0] eq 3) and (whichAreMultiple[1] eq 0)) then begin
    mixedMode='Mixed mode: observations+parameters'
    mChoice1run=singleRawData[sRunIndexes].observedCode
    mChoice2run=singleRawData[sRunIndexes].parameterCode
    ;mChoice3obs=singleRawData[sMonitIndexes].observedCode
    legoTest1=singleRawData[sMonitIndexes].observedCode
    legoTest2=singleRawData[sMonitIndexes].parameterCode
    ;legoNames=parCodes
    ;eg   inan  is flag for treatment of legoIdx  in the "i, j cycle" below
    inan = 1
    test1=obsNames
    test2=parCodes
    legendNames=strarr(2+nreg)
    targetColors=indgen(n_elements(test1))
    targetPointsNo=n_elements(test1)*2
    legendColors=targetColors
    legendSymbols=strarr(2+nobs)
    legendSymbols(0:1)=9
    legendSymbols(2:1+nobs)=indgen(nobs) mod 13

  endif
endelse
; 2 multiple choices sections **end**

; here calc a stat!!!!++++++++1 multiple choice sections: execute statistic-related operations+++++++
; 1 multiple choice sections: execute statistic-related operations
;**begin**
if multipleChoicesNo eq 1 then begin

  statXYResult=fltarr(targetPointsNo,2)

  ; only one choice
  varCond=fix(extraval(0))
  if varCond eq 0 then Varmain=1
  if varCond eq 1 then Varmain=0
  choiceCond=(where(mChoice1run eq test1[varCond]))[0]
  choiceMain=(where(mChoice1run eq test1[Varmain]))[0]
  ; get original data (obs & run)
  obsMain=*singleRawData[sMonitIndexes[choiceMain]].observedData
  runMain=*singleRawData[sRunIndexes[choiceMain]].runData
  obsCond=*singleRawData[sMonitIndexes[choiceCond]].observedData
  runCond=*singleRawData[srunIndexes[choiceCond]].runData

  time_operations, request, result, obsMain, runMain
  time_operations, request, result, obsCond, runCond
  obsMain=obsMain[startIndex:endIndex]
  obsCond=obsCond[startIndex:endIndex]
  runMain=runMain[startIndex:endIndex]
  runCond=runCond[startIndex:endIndex]

  cc=where(obscond lt 0. or obsmain le 0. or runmain lt -900, count999)
  if count999 gt 0 then begin
    obsmain(cc)=!values.f_nan
    runmain(cc)=!values.f_nan
    obscond(cc)=!values.f_nan
  endif

  ccCond= where(obsCond ge extraval(1) and obsCond le extraval(2) and finite(obsCond) eq 1 $
    and finite(obsMain) eq 1 and finite(runMain) eq 1,countCond)

  ccCond0=where((obsCond lt extraval(1) or obsCond gt extraval(2)) and finite(obsCond) eq 1 $
    and finite(obsMain) eq 1 and finite(runMain) eq 1,countCond0)

  if countCond gt 0 then begin
    statXYResult(0,0)=mean(obsMain(ccCond))
    statXYResult(0,1)=mean(runMain(ccCond))
  endif else begin
    statXYResult(0,*)=!values.f_nan
  endelse

  if countCond0 gt 0 then begin
    statXYResult(1,0)=mean(obsMain(ccCond0))
    statXYResult(1,1)=mean(runMain(ccCond0))
  endif else begin
    statXYResult(1,*)=!values.f_nan
  endelse

  legendColors=statColors
  legendSymbols=statsymbols
  legendNames(0)=parCodes(Varmain)+' for '+strtrim(extraval(1),2)+' < '+parCodes(varCond)+' < '+strtrim(extraval(2),2)
  legendNames(1)=parCodes(Varmain)+' under remaining conditions'
endif

; 1 multiple choice section --end--
; ++++++++++++++1 multiple choice end+++++++++++++++++

; 2 multiple choices section --begin--
if multipleChoicesNo ne 1 then begin

  extLastIndex=n_elements(test1)-1
  statXYResult=fltarr(targetPointsNo, 2)
  statSymbols=intarr(targetPointsNo)
  statColors=intarr(targetPointsNo)
  k=0
  for i=0, extLastIndex do begin

    varCond=fix(extraval(0))
    if varCond eq 0 then Varmain=1
    if varCond eq 1 then Varmain=0

    choiceCond=(where(legoTest1 eq test1(i) and legoTest2 eq test2(varCond)))[0]
    choiceMain=(where(legoTest1 eq test1(i) and legoTest2 eq test2(Varmain)))[0]

    obsCond=*singleRawData[sMonitIndexes[choiceCond]].observedData
    obsMain=*singleRawData[sMonitIndexes[choiceMain]].observedData

    choiceCond=(where(mChoice1run eq test1[i] and mChoice2run eq test2[varCond]))[0]
    choiceMain=(where(mChoice1run eq test1[i] and mChoice2run eq test2[Varmain]))[0]

    runCond=*singleRawData[srunIndexes[choiceCond]].runData
    runMain=*singleRawData[srunIndexes[choiceMain]].runData

    time_operations, request, result, obsMain, runMain
    time_operations, request, result, obsCond, runCond


    obsMain=obsMain[startIndex:endIndex]
    runMain=runMain[startIndex:endIndex]
    obsCond=obsCond[startIndex:endIndex]
    runCond=runCond[startIndex:endIndex]

    cc=where(obscond lt 0. or obsmain le 0. or runmain lt -900, count999)
    if count999 gt 0 then begin
      obsmain(cc)=!values.f_nan
      runmain(cc)=!values.f_nan
      obscond(cc)=!values.f_nan
    endif

    ccCond= where(obsCond ge extraval(1) and obsCond le extraval(2) and finite(obsCond) eq 1 $
      and finite(obsMain) eq 1 and finite(runMain) eq 1,countCond)

    ccCond0=where((obsCond lt extraval(1) or obsCond gt extraval(2)) and finite(obsCond) eq 1 $
      and finite(obsMain) eq 1 and finite(runMain) eq 1,countCond0)

    if countCond gt 0 then begin
      statXYResult(k,0)=mean(obsMain(ccCond))
      statXYResult(k,1)=mean(runMain(ccCond))
    endif else begin
      statXYResult(k,*)=!values.f_nan
    endelse

    if countCond0 gt 0 then begin
      statXYResult(k+1,0)=mean(obsMain(ccCond0))
      statXYResult(k+1,1)=mean(runMain(ccCond0))
    endif else begin
      statXYResult(k+1,*)=!values.f_nan
    endelse

    statColors[k]=0
    statColors[k+1]=1
    legendColors=statColors
;    regionWhere=where(regNamesAll(i) eq regNames)
    statSymbols[k]=k mod 13
    statSymbols[k+1]=k mod 13

    legendColors(0:1)=[0,1]
    legendColors(2:1+n_elements(regNames))=-2
    legendNames(0)=parCodes(Varmain)+' for '+strtrim(extraval(1),2)+' < '+parCodes(varCond)+' < '+strtrim(extraval(2),2)
    legendNames(1)=parCodes(Varmain)+' under remaining conditions'
    legendSymbols=statSymbols
    k=k+2
  endfor
  legendSymbols(0:1)=13

endif
result->setGenericPlotInfo, statXYResult, statSymbols, statColors, legendNames, legendColors, legendSymbols
; 2 multiple choices section **end**
END
PRO FM_MultiParScatter, request, result
  ;****************************************************************************************************

  ; start/end index -> first/last position of "time/data" user selection (datetime selection)
  startIndex=request->getStartIndex()
  endIndex=request->getEndIndex()
  ; standard functions available for "request" object
  npar=request->getParameterNumber()
  nmod=request->getModelNumber()
  modelCodes=request->getModelCodes()

  parCodes=request->getParameterCodes()
  scenarioCodes=request->getScenarioCodes()
  nsce=request->getScenarioNumber()
  isSingleSelection=request->isSingleObsPresent()
  isGroupSelection=request->isGroupObsPresent()
  nobs=0
  ; standard functions available for "result" object
  ; use sMonitIndexes & sRunIndexes to access rawData struct
  if isSingleSelection then begin
    obsNames=request->getSingleObsNames()
    obsCodes=request->getSingleObsCodes()
    obsShortNames=request->getSingleShortObsNames()
    nobs=nobs+request->getSingleObsNumber()
    singleRawData=result->getSingleRawData()
    singleRDMatrix=result->getSingleRawDataCheckMatrix(sMonitIndexes, sRunIndexes)
  endif
  if isGroupSelection then begin
    allGroupStations=request->buildAllGroupNames()
    groupTitlesNumber=request->getGroupTitlesNumber()
    groupTitles=request->getGroupTitles()
    groupStatToApply=request->getGroupStatToApply()
    groupCodes=request->getGroupCodes()
    groupNames=request->getGroupNames()
    for i=0, groupTitlesNumber-1 do begin
      print, "*************"
      print, groupTitles[i]
      print, *groupCodes[i]
      print, *groupNames[i]
      print, "*************"
    endfor
    nobs=nobs+n_elements(allGroupStations)
    groupRawData=result->getGroupRawData()
    groupRDMatrix=result->getGroupRawDataCheckMatrix(gMonitIndexes, gRunIndexes)
  ;nobs=nobs+nobs=nobs+
  endif
  regNamesAll=strarr(nobs)

  ;for i=0, nobs-1 do print, obsCodes[i],'**', obsNames[i],'**', request->getRegionofObs(obsCodes[i]);regionCode=request->getRegionofObs(obsCodes[i])
  for i=0, nobs-1 do regNamesAll[i]=request->getRegionofObs(obsCodes[i]);regionCode=request->getRegionofObs(obsCodes[i])
  regNames = regNamesAll[UNIQ(regNamesAll, SORT(regNamesAll))]

  ; access to goals & criteria table (see configuration dir)
  ; **example** **start**
  mParameter=parCodes[0] & mScalename='LOCAL' & mStatName='IOA' & timeAvgName='N/A'
  gcValues=request->getGoalsCriteriaValues(parameter=mParameter, scalename=mScalename, statname=mStatName, timeAvgName=timeAvgName, NOVALUES=NOVALUES)
  print, 'query GC for:', ' parameter=', mParameter, '**scalename=', mScalename, '**statname=', mStatName
  if keyword_set(NOVALUES) then print, 'No values' else print, gcValues
  print, '***End GC query***'
  ; **example** **end**

  ; access to extra values (thresholds, references...) user input example **start**
  ; **example** **start**
  extraValNumber=request->getExtraValuesNumber()
  if extraValNumber gt 0 then begin
    print, 'extra Values available #', extraValNumber
    print, 'ExtraValues: ', request->getExtraValues()
    extraval=request->getExtraValues()
  endif else begin
    print, 'extra Values not available'
  endelse
  obsFact=1
  ; **example** **start**

  ;  rawData=result->getRawData()
  ;  rDMatrix=result->getRawDataCheckMatrix(monitIndexes, runIndexes)

  mcFlags=request->getMultipleChoiceUserSelectionFlags()
  ; which are multiples?
  whichAreMultiple=where(mcFlags eq 1b, multipleChoicesNo)

  if multipleChoicesNo eq 0 then begin
    multipleChoicesNo = 1
    whichAreMultiple[0] = 0
  endif

  ; ++++++++++++++1 multiple choice+++++++++++++++++
  ; only one multiple choice section **begin**
  if multipleChoicesNo eq 1 then begin
    case whichAreMultiple[0] of
      ; Only parameters are multiple
      0:begin ;parameters section
      print, '--> Only parameters are multiple'
      mChoice1run=SingleRawData[srunIndexes].parameterCode
      test1=parCodes
      legendNames=['OBS vs OBS',modelCodes+' vs '+modelCodes]
      legoNames=parCodes
      ;      statResult=fltarr(1)
      ; tek color starts from index 2(red)
      targetColors=intarr(2)
      targetPointsNo=2
      legendColors=indgen(n_elements(targetPointsNo))
      legendSymbols=strarr(2)
      legendColors=[0,1]
      legendSymbols=[9,9]
    end
  endcase
endif
if multipleChoicesNo eq 1 then begin

  range=endIndex-startIndex+1
  statXYResult=fltarr(range*2,2)
  statSymbols=intarr(range*2)
  statColors=intarr(range*2)
  statSymbols(*)=9
  statColors(0:range-1)=0
  statColors(range:2*range-1)=1

  choiceMain=(where(mChoice1run eq test1[0]))[0]
  choiceCond=(where(mChoice1run eq test1[1]))[0]
  ; get original data (obs & run)
  obsMain=*singleRawData[sMonitIndexes[choiceMain]].observedData
  obsCond=*singleRawData[sMonitIndexes[choiceCond]].observedData
  runMain=*singleRawData[sRunIndexes[choiceMain]].runData
  runCond=*singleRawData[srunIndexes[choiceCond]].runData

  time_operations, request, result, obsMain, obsCond
  time_operations, request, result, runMain, runCond
  obsMain=obsMain[startIndex:endIndex]
  obsCond=obsCond[startIndex:endIndex]
  runMain=runMain[startIndex:endIndex]
  runCond=runCond[startIndex:endIndex]

  cc=where(obscond le -900. or obsMain le 0., count900)
  if count900 gt 0 then begin
    obscond(cc)=!values.f_nan
    obsMain(cc)=!values.f_nan
  endif
  cc=where(runcond le -900. or runMain le -900., count900)
  if count900 gt 0 then begin
    runcond(cc)=!values.f_nan
    runMain(cc)=!values.f_nan
  endif
  ; print,correlate(obsMain,obsCond)
  statXYResult(0:range-1,0)=obsMain(*)
  statXYResult(0:range-1,1)=obsCond(*)
  statXYResult(range:2*range-1,0)=runMain(*)
  statXYResult(range:2*range-1,1)=runCond(*)

endif

result->setGenericPlotInfo, statXYResult, statSymbols, statColors, legendNames, legendColors, legendSymbols
; 2 multiple choices section **end**
END

pro FM_QQ_SC_ALLTIME, request, result

  ; start/end index -> first/last position of "time/data" user selection (datetime selection)
  startIndex=request->getStartIndex()
  endIndex=request->getEndIndex()
  ; standard functions available for "request" object
  npar=request->getParameterNumber()
  nmod=request->getModelNumber()
  modelCodes=request->getModelCodes()
  parCodes=request->getParameterCodes()
  scenarioCodes=request->getScenarioCodes()
  nsce=request->getScenarioNumber()
  isSingleSelection=request->isSingleObsPresent()
  isGroupSelection=request->isGroupObsPresent()
  isSingleSelection=request->isSingleObsPresent()
  isGroupSelection=request->isGroupObsPresent()
  elabcode=request->getElaborationCode()
  nobs=0
  ; standard functions available for "result" object
  ; use sMonitIndexes & sRunIndexes to access rawData struct
  if isSingleSelection then begin
    obsNames=request->getSingleObsNames()
    obsCodes=request->getSingleObsCodes()
    obsShortNames=request->getSingleShortObsNames()
    nobs=nobs+request->getSingleObsNumber()
    singleRawData=result->getSingleRawData()
    singleRDMatrix=result->getSingleRawDataCheckMatrix(sMonitIndexes, sRunIndexes)
  endif
  if isGroupSelection then begin
    allGroupStations=request->buildAllGroupNames()
    groupTitlesNumber=request->getGroupTitlesNumber()
    groupTitles=request->getGroupTitles()
    groupStatToApplyCode=request->getGroupStatToApplyCode()
    groupStatToApplyName=request->getGroupStatToApplyName()
    groupCodes=request->getGroupCodes()
    groupNames=request->getGroupNames()
    for i=0, groupTitlesNumber-1 do begin
      print, "*************"
      print, groupTitles[i]
      print, *groupCodes[i]
      print, *groupNames[i]
      print, "*************"
    endfor
    nobs=nobs+n_elements(allGroupStations)
    groupRawData=result->getGroupRawData()
    groupRDMatrix=result->getGroupRawDataCheckMatrix(gMonitIndexes, gRunIndexes)
  ;nobs=nobs+nobs=nobs+
  endif

  ; access to goals & criteria table (see configuration dir)
  ; **example** **start**
  mParameter=parCodes[0] & mScalename='LOCAL' & mStatName='IOA' & timeAvgName='N/A'
  gcValues=request->getGoalsCriteriaValues(parameter=mParameter, scalename=mScalename, statname=mStatName, timeAvgName=timeAvgName, NOVALUES=NOVALUES)
  print, 'query GC for:', ' parameter=', mParameter, '**scalename=', mScalename, '**statname=', mStatName
  if keyword_set(NOVALUES) then print, 'No values' else print, gcValues
  print, '***End GC query***'
  ; **example** **end**

  ; access to extra values (thresholds, references...) user input example **start**
  ; **example** **start**
  extraValNumber=request->getExtraValuesNumber()
  if extraValNumber gt 0 then begin
    print, 'extra Values available #', extraValNumber
    print, 'ExtraValues: ', request->getExtraValues()
  endif else begin
    print, 'extra Values not available'
  endelse
  obsFact=1
  ; **example** **start**

  mcFlags=request->getMultipleChoiceUserSelectionFlags()
  ; which are multiples?
  whichAreMultiple=where(mcFlags eq 1b, multipleChoicesNo)

  if multipleChoicesNo eq 0 then begin
    multipleChoicesNo = 1
    whichAreMultiple[0] = 1
  endif

  ; ++++++++++++++1 multiple choice+++++++++++++++++
  ; only one multiple choice section **begin**
  if multipleChoicesNo eq 1 then begin
    case whichAreMultiple[0] of
      ; Only parameters are multiple
      0:begin ;parameters section
      print, '--> Only parameters are multiple'
      if isSingleSelection then begin
        print, 'singles selected!'
        mChoice1runS=singleRawData[sRunIndexes].parameterCode
        test1S=parCodes
        targetPointsNo=n_elements(test1S)
        forSLastIndex=n_elements(test1S)-1
        legendNames=test1S
        targetColors=indgen(n_elements(test1S))
      endif
      if isGroupSelection then begin
        print, 'groups selected!'
        mChoice1runG=groupRawData[gRunIndexes].observedCode
        mChoice2runG=groupRawData[gRunIndexes].parameterCode
        test1G=parCodes
        targetPointsNo=n_elements(test1G)
        forGLastIndex=n_elements(test1G)-1
        legendNames=test1G
        targetColors=indgen(n_elements(test1G))
        forGLastIndex =n_elements(groupTitles)-1
        forGLastIndex2=n_elements(parCodes)-1
      endif
    end
    ; Only models are multiple
    1:begin ;models section
    print, '--> Only models are multiple'
    if isSingleSelection then begin
      print, 'singles selected!'
      mChoice1runS=singleRawData[sRunIndexes].modelCode
      test1S=modelCodes
      targetPointsNo=n_elements(test1S)
      forSLastIndex=n_elements(test1S)-1
      legendNames=test1S
      targetColors=indgen(n_elements(test1S))
    endif
    if isGroupSelection then begin
      print, 'groups selected!'
      mChoice1runG=groupRawData[gRunIndexes].observedCode
      mChoice2runG=groupRawData[gRunIndexes].modelCode
      test1G=modelCodes
      targetPointsNo=n_elements(test1G)
      forGLastIndex=n_elements(test1G)-1
      legendNames=test1G
      targetColors=indgen(n_elements(test1G))
      forGLastIndex =n_elements(groupTitles)-1
      forGLastIndex2=n_elements(modelCodes)-1
    endif
  end
  ; Only scenarios are multiple **not allowed but leave code here for safe**
  2:begin ;scenarios section
  print, '--> Only scenarios are multiple'
  if isSingleSelection then begin
    print, 'singles selected!'
    mChoice1runS=singleRawData[sRunIndexes].scenarioCode
    test1S=scenarioCodes
    targetPointsNo=n_elements(test1S)
    forSLastIndex=n_elements(test1S)-1
    legendNames=test1S
    targetColors=indgen(n_elements(test1S))
  endif
  if isGroupSelection then begin
    print, 'groups selected!'
    mChoice1runG=groupRawData[gRunIndexes].observedCode
    mChoice2runG=groupRawData[gRunIndexes].scenarioCode
    test1G=scenarioCodes
    targetPointsNo=n_elements(test1G)
    forGLastIndex=n_elements(test1G)-1
    legendNames=test1G
    targetColors=indgen(n_elements(test1G))
    forGLastIndex =n_elements(groupTitles)-1
    forGLastIndex2=n_elements(modelCodes)-1
  endif
end
; Only observations are multiple
3:begin ;observations section
print, '--> Only observations are multiple'
if isSingleSelection then begin
  print, 'singles selected!'
  mChoice1runS=singleRawData[sRunIndexes].observedCode
  test1S=obsNames
  targetPointsNo=n_elements(test1S)
  forSLastIndex=n_elements(test1S)-1
  legendNames=test1S
  targetColors=indgen(n_elements(test1S))
endif
if isGroupSelection then begin
  print, 'groups selected!'
  mChoice1runG=groupRawData[gRunIndexes].observedCode
  mChoice2runG=groupRawData[gRunIndexes].parameterCode
  test1G=parCodes
  targetPointsNo=n_elements(test1G)
  forGLastIndex=n_elements(test1G)-1
  legendNames=test1G
  targetColors=indgen(n_elements(test1G))
  forGLastIndex =n_elements(groupTitles)-1
  forGLastIndex2=0
endif
end
endcase
endif
; 2 multiple choices sections **end**


if multipleChoicesNo eq 1 then begin
  ; dimension: nb of models/scen+1 for observations; nb of points in time
  statXYResult=fltarr(targetPointsNo, endIndex-startIndex+1,2)
  statSymbols=intarr(targetPointsNo)
  statColors=intarr(targetPointsNo)

  if isSingleSelection then begin

    for i=0, forSLastIndex do begin
      ; only one choice
      choiceIdx1=(where(mChoice1runS eq test1S[i]))[0]

      obsTemp=*singleRawData[sMonitIndexes[choiceIdx1]].observedData
      runTemp=*singleRawData[sRunIndexes[choiceIdx1]].runData

      time_operations, request, result, obsTemp, runTemp
      obs_run_nan,request,result,obsTemp, runTemp

      range =n_elements(obsTemp)
      if elabcode eq 13 then begin  ;scatter all times
        statXYResult[i,0:range-1,0]=obsTemp(*)
        statXYResult[i,0:range-1,1]=runTemp(*)
      endif else begin ;QQ all times elabCode=29
        statXYResult[i,0:range-1,0]=obsTemp(sort(obsTemp))
        statXYResult[i,0:range-1,1]=runTemp(sort(runTemp))
      endelse

      statSymbols[i]=9
      statColors[i]=i

    endfor

  endif

  if isGroupSelection then begin

    for k=0,forGLastIndex2 do begin  ;loop models

      currentNames=*groupNames[0]
      validIdxs=n_elements(currentNames)
      obsGroupStatResult=fltarr(validIdxs,endIndex-startIndex+1)
      runGroupStatResult=fltarr(validIdxs,endIndex-startIndex+1)

      for j=0, validIdxs-1 do begin  ;groups loop

        choiceIdx1=(where(mChoice1runG eq currentNames(j) and mChoice2runG eq test1G(k)))[0]

;        print, 'gMonitIndexes[choiceIdx1]', gMonitIndexes[choiceIdx1]
        obsTemp=*groupRawData[gMonitIndexes[choiceIdx1]].observedData

        choiceIdx1=(where(mChoice1runG eq currentNames[j] and mChoice2runG eq test1G(k)))[0]

;        print, 'sRunIndexes[choiceIdx1]', gRunIndexes[choiceIdx1]
        runTemp=*groupRawData[gRunIndexes[choiceIdx1]].runData

        time_operations, request, result, obsTemp, runTemp
        obs_run_nan,request,result,obsTemp, runTemp

        range =n_elements(obsTemp)
        obsGroupStatResult[j,0:range-1]=obsTemp(*)
        runGroupStatResult[j,0:range-1]=runTemp(*)

      endfor

      if elabcode eq 13 or elabCode eq 29 then begin  ;scatter all times
        for jj=0,endIndex-startIndex do begin
          statXYResult(k,jj,0)=mean(obsGroupStatResult(*,jj))
          statXYResult(k,jj,1)=mean(runGroupStatResult(*,jj))
        endfor
      endif  
      if elabcode eq 29 then begin  ;QQ for 100% group
          hlp=sort(statXYResult(k,*,0))
          statXYResult(k,*,0)=statXYResult(k,hlp,0)
          hlp=sort(statXYResult(k,*,1))
          statXYResult(k,*,1)=statXYResult(k,hlp,1)
      endif 
;      endif else begin    ??
;        statXYResult(k,*,*)=!values.f_nan
;      endelse
      statSymbols[k]=9
      statColors[k]=k
      statSymbols[0]=9
      statColors[0]=0

    endfor

  endif

endif
legendColors=statColors
legendSymbols=statsymbols

result->setGenericPlotInfo, statXYResult, statSymbols, statColors, legendNames, legendColors, legendSymbols
; 2 multiple choices section **end**
end

function strsplit, stringIn, pattern, _ref_extra=extra

  ON_ERROR, 2  ; return to caller
  RETURN, (n_params() eq 1) ? STRTOK(stringIn, _STRICT_EXTRA=extra) : $
    STRTOK(stringIn, pattern, _STRICT_EXTRA=extra)

end