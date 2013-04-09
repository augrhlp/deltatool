;********************
@structure_definition
;********************
;  ;MM summer 2012 Start
;  ;for Goals & Criteria now you can use (two new columns added in elaboration.dat)
;  ;elaboration related info called OCTimeAvgName & OCStat 
;  OCTimeAvgName=request->getElaborationOCTimeAvgName()
;  OCStat=request->getElaborationOCStat()
;  ;MM summer 2012 End
function rmses, obsValues, runValues

  valNum=n_elements(obsValues)
  res=linfit(obsValues,runValues)
  runValues_lin=res(0)+res(1)*obsValues
  statisticValue=sqrt(total(((runValues_lin-obsValues)^2))/valNum)
  return, statisticValue

end
function mfb, obsValues, runValues

  valNum=n_elements(obsValues)
  statisticValue=200.*total( ( (runValues-obsValues) /(runValues+obsvalues) ) /valNum)
  return, statisticValue

end
function nmb, obsValues, runValues

  valNum=n_elements(obsValues)
  statisticValue=100.*(mean(runValues)-mean(obsValues))/mean(obsvalues)
  return, statisticValue

end
function nmsd, obsValues, runValues

  valNum=n_elements(obsValues)
  statisticValue=100.*(stddevOM(runValues)-stddevOM(obsValues))/stddevOM(obsvalues)
  return, statisticValue

end
function mnb, obsValues, runValues

  valNum=n_elements(obsValues)
  statisticValue=100.*total( ((runValues-obsValues) /obsvalues )/valNum)
  return, statisticValue

end
function mfe, obsValues, runValues

  valNum=n_elements(obsValues)
  statisticValue=200.*total( ( abs((runValues-obsValues)) /(runValues+obsvalues) ) /valNum)
  return, statisticValue

end
;*****************
function ioa, obsValues, runValues

  valNum=n_elements(obsValues)
  statisticValue=1. - valNum*rmse(obsValues, runValues)^2 / $
    total((abs(runValues-mean(obsValues)) + abs(obsValues-mean(obsValues)))^2)
  return, statisticValue

end
;******************
function fac2, obsValues, runValues

  valNum=n_elements(obsValues)
  cfac2=where(runValues lt 2.*obsValues and runValues gt 0.5*obsValues,count_fac2)
  statisticValue=100.*count_fac2/valNum
  if finite(max(obsValues)) eq 0 then statisticValue=!values.f_nan
  return, statisticValue

end
;******************
function stddevOM, obsValues

  valNum=n_elements(obsValues)
  statisticValue=sqrt( total(( (mean(obsvalues)-obsValues)^2)) /valNum)
  return, statisticValue

end
;*********************
function resilience, obsValues

  valNum=n_elements(obsValues)
  tothlp=0.
  for i=1,valNum-1 do begin
   tothlp=tothlp+(obsValues(i)-obsValues(i-1))^2
  endfor
  statisticValue=sqrt(tothlp/(valNum-1))
  return, statisticValue

end
;*****************
function stddevOWeigth, obsValues,weigth

  valNum=n_elements(obsValues)
  meanWeigth=total(weigth*obsValues)/valNum
  statisticValue=sqrt( total(weigth*(meanWeigth-obsValues)^2)/valNum)
  return, statisticValue

end

;**********************
function crmse, obsValues, runValues

  valNum=n_elements(obsValues)
  statisticValue=sqrt(total((( (runValues-mean(runValues)) - (obsValues-mean(obsValues)))^2))/valNum)
  return, statisticValue

end
;*******************
function crmseWeigth, obsValues, runValues,weigth

  valNum=n_elements(obsValues)
  meanOWeigth=total(weigth*obsValues)/valNum
  meanMWeigth=total(weigth*runValues)/valNum
  statisticValue=sqrt(total( weigth*(runValues-meanMWeigth-obsValues+meanOWeigth)^2)/valNum)
  return, statisticValue

end
;************************
function rde, obsValues, runValues, threshold

  valNum=n_elements(obsValues)
  obsSort=obsValues(sort(obsValues))
  runSort=runValues(sort(runValues))
  distObsThreshold=sqrt((obsSort-threshold)^2)
  cmin=min(distObsThreshold,min_subscript)
  if threshold gt 0. then begin
    statisticValue=100*abs(obsSort(min_subscript)-runSort(min_subscript))/threshold
  endif else begin
    statisticValue=!values.f_nan
  endelse
  return, statisticValue

end
function rdeYearly, obsValues, runValues, threshold

  valNum=n_elements(obsValues)
  obsSort=mean(obsValues)
  runSort=mean(runValues)
  if threshold gt 0. then begin
    statisticValue=100*abs(obsSort-runSort)/threshold
  endif else begin
    statisticValue=!values.f_nan
  endelse
  return, statisticValue

end
;******************************
;************************
function rpe, obsValues, runValues, threshold

  valNum=n_elements(obsValues)
  obsSort=reverse(obsValues(sort(obsValues)))
  runSort=reverse(runValues(sort(runValues)))
  if threshold gt 0 then begin
    statisticValue=100*abs(obsSort(threshold)-runSort(threshold))/obsSort(threshold)
  endif else begin
    statisticValue=0
  endelse
  return, statisticValue

end
;******************************
function bias, obsValues, runValues

  valNum=n_elements(obsValues)
  statisticValue=total(runValues-obsValues)/valNum
  return, statisticValue

end
;*******************************
function biasWeigth, obsValues, runValues,weigth

  valNum=n_elements(obsValues)
  statisticValue=total(weigth*(runValues-obsValues))/valNum
  return, statisticValue

end
;*******************************
function gme, obsValues, runValues

  valNum=n_elements(obsValues)
  statisticValue=total(abs(runValues-obsValues))/valNum
  return, statisticValue

end
; generic functions, utilities and so on...
; **begin**
function rmse, obsValues, runValues

  valNum=n_elements(obsValues)
  statisticValue=sqrt(total(((runValues-obsValues)^2))/valNum)
  return, statisticValue

end
function rmseu, obsValues, runValues

  valNum=n_elements(obsValues)
  res=linfit(obsValues,runValues)
  runValues_lin=res(0)+res(1)*obsValues
  statisticValue=sqrt(total(((runValues-runValues_lin)^2))/valNum)
  return, statisticValue

end
function sumsquare, obsValues

  valNum=n_elements(obsValues)
  statisticValue=total(obsValues^2)/valnum
  return, statisticValue

end
pro obs_run_nan, request,result,obsValues, runValues

  modelInfo=request->getModelInfo()
  year=modelInfo.year
  scale=modelInfo.scale
  frequency=modelInfo.frequency
  elabcode=request->getElaborationCode()
  startIndex=request->getStartIndex()
  endIndex=request->getEndIndex()
    modelInfo=request->getModelInfo()
    year=modelInfo.year
    if 4*(fix(year)/4) ne fix(year) then begin  ; normal year
      Feb29start=59*24
      Feb29end=Feb29start+23
      if startIndex lt Feb29start and endIndex ge FEB29end then endIndex=endIndex-24
      if startIndex ge Feb29start then begin
        endIndex=endIndex-24
        startIndex=startIndex-24
      endif  
    endif  
  ddn=request->getHourType()
  hour=request->getHourInfo() ;HourType
  start_hour_hlp=hour[0].value
  end_hour_hlp=hour[1].value
  iUseObserveModel=request->getUseObservedModel()  ; 0=0ld case; 1=no obs

;KeesC move next to lines to .dat conf file
  minDataAvail=0.75 
  minDayAvail=18 ; minimal 18 8-hour-mean values should be available per day
;KeesC 4APR2013  
  dayHourLength=12  ;fix(end_hour_hlp)-fix(start_hour_hlp)+1
  NightHourLength=24-dayHourLength
  if ddn eq 1 or abs(elabcode) eq 71 then minDayAvail=fix(dayHourLength*minDataAvail) 
  if ddn eq 2 or abs(elabcode) eq -71 then minDayAvail=fix(NightHourLength*minDataAvail) 
  
  for i=0,364 do begin
    kcobs=where(obsValues(i*24:i*24+23) gt -990,nkcobs)
    kcrun=where(runValues(i*24:i*24+23) gt -990,nkcrun)
    if nkcobs lt minDayAvail then obsValues(i*24:i*24+23)=-999
    if nkcrun lt minDayAvail then runValues(i*24:i*24+23)=-999
  endfor

  obsValues=obsValues[startIndex:endIndex]
  runValues=runValues[startIndex:endIndex]

  if 4*(year/4) ne year then begin   ;normal year
    iyear=0
    day_nb= [31,28,31,30,31,30,31,31,30,31,30,31]
    day_sum=[0,31,59,90,120,151,181,212,243,273,304,334,365]
  endif
  if 4*(year/4) eq year then begin   ;leapyear 
    iyear=1
    day_nb= [31,29,31,30,31,30,31,31,30,31,30,30]
    day_sum=[0,31,60,91,121,152,182,213,244,274,305,335,366]
  endif

  if elabcode ge 0 and elabcode ne 71 and elabcode ne 72 and elabcode ne 73 then begin
    hour=request->getHourInfo() ;HourType
    season=request->getSeasonInfo()
    start_seasons_dates_hlp=season[0].value
    end_seasons_dates_hlp=season[1].value
    res_start=strsplit(start_seasons_dates_hlp,'*',/extract)
    day_start=fix(strmid(res_start,0,2))
    month_start=fix(strmid(res_start,2,2))
    index_start_season=intarr(n_elements(day_start))
    res_end=strsplit(end_seasons_dates_hlp,'*',/extract)
    day_end=fix(strmid(res_end,0,2))
    month_end=fix(strmid(res_end,2,2))
;    if 4*(year/4) eq year and day_end eq 31 and month_end eq 12 then day_end=30
    index_end_season=intarr(n_elements(day_end))

    ysw=request->getSeasonType()  ;year, summer, winter
    ; From season: Winter is 0101-2802 & 0112-3112
    ; For leapyear should be 0101-2902 & 0112-3112
    ; If a winter period ends at 2802 make it 2902
    if 4*(year/4) eq year and ysw eq 2 then begin  ;winter
      whlp=where(month_end eq 2,wnc)  ;find Feb month
      if wnc eq 1 then begin
        if day_end(whlp[0]) eq 28 then day_end(whlp[0])=29
      endif
    endif

    ahlp=intarr(8784) & ahlp(*)=0
    for i=0,n_elements(day_start)-1 do begin
      index_start_season(i)=day_sum(month_start(i)-1)*24+(day_start(i)-1)*24
      index_end_season(i)=day_sum(month_end(i)-1)*24+(day_end(i))*24-1
      ahlp(index_start_season(i):index_end_season(i))=1
    endfor

    start_hour_hlp=hour[0].value
    end_hour_hlp=hour[1].value
    res_start=strsplit(start_hour_hlp,'*',/extract)
    res_end=strsplit(end_hour_hlp,'*',/extract)
    bhlp=intarr(8784) & bhlp(*)=0
    
;KeesC 4/APR2013    
;   if res_start[0] eq 'WD' then begin  
    if ddn eq 3 then begin  
      for i=0,364+iyear do begin
        DayYear=(julday(1,i+1,year)+1) mod 7
        if DayYear ne 0 and DayYear ne 6 then bhlp(i*24:i*24+23)=1
      endfor
    endif 
;    if res_start[0] eq 'WE' then begin
    if ddn eq 4 then begin
      for i=0,364+iyear do begin
        DayYear=(julday(1,i+1,year)+1) mod 7
        if DayYear eq 0 or DayYear eq 6 then bhlp(i*24:i*24+23)=1
      endfor
    endif
;    if res_start[0] ne 'WD' and res_start[0] ne 'WE' then begin
    if ddn ne 3 and ddn ne 4 then begin
      res_start=fix(strsplit(start_hour_hlp,'*',/extract))
      res_end=fix(strsplit(end_hour_hlp,'*',/extract))
      for j=0,n_elements(res_start)-1 do begin
        for i=0,364 do begin
          bhlp(24*i+res_start(j):24*i+res_end(j))=1
        endfor
      endfor
    endif
    ;   intersection of ahlp and bhlp
    if iyear eq 0 then begin
      ahlp=reform(ahlp(0:8759))
      bhlp=reform(bhlp(0:8759))
    endif
    ahlp=ahlp(startIndex:endIndex)
    bhlp=bhlp(startIndex:endIndex)
    ahlp=ahlp*bhlp
    cc=where(ahlp eq 1,countDaySeason)
  endif

  if abs(elabcode) eq 71 or abs(elabcode) eq 72 or abs(elabcode) eq 73 then begin

    ysw=request->getSeasonType()  ;year, summer, winter
    ddn=request->getHourType()    ;allday, day, night, WD, WE

    ahlp=intarr(8784) & ahlp(*)=0
    if elabcode eq 71 then begin  ;day
      for i=0,364+iyear do begin
        ahlp(24*i+fix(start_hour_hlp)-1:24*i+fix(end_hour_hlp)-1)=1
      endfor
    endif
    if elabcode eq -71 then begin  ;night
      for i=0,364+iyear do begin
        ahlp(24*i:24*i+fix(start_hour_hlp)-2)=1
        ahlp(24*i+fix(end_hour_hlp):24*i+23)=1
      endfor
    endif
    if elabcode eq 72 then begin  ;summer
      ahlp(day_sum(5)*24:day_sum(8)*24-1)=1
    endif
    if elabcode eq -72 then begin  ;winter
      ahlp(day_sum(0):day_sum(2)*24-1)=1
      ahlp(day_sum(11)*24:8783)=1
    endif
    if elabcode eq 73 then begin  ;weekdays
      for i=0,364+iyear do begin
        DayYear=(julday(1,i+1,year)+1) mod 7
        if DayYear ne 0 and DayYear ne 6 then ahlp(i*24:i*24+23)=1
      endfor
    endif
    if elabcode eq -73 then begin  ;weekend
      for i=0,364+iyear do begin
        DayYear=(julday(1,i+1,year)+1) mod 7
        if DayYear eq 0 or DayYear eq 6 then ahlp(i*24:i*24+23)=1
      endfor
    endif

    bhlp=intarr(8784) & bhlp(*)=0
    if abs(elabcode) eq 71 or abs(elabcode) eq 73 then begin
      if ysw eq 0 then begin  ;year
        bhlp(*)=1
      endif
      if ysw eq 1 then begin  ;summer
        bhlp(day_sum(5)*24:day_sum(8)*24-1)=1
      endif
      if ysw eq 2 then begin ; winter
        bhlp(day_sum(0):day_sum(2)*24-1)=1
        bhlp(day_sum(11)*24:8783)=1
      endif
    endif
    if abs(elabcode) eq 72 then begin
      if ddn eq 0 then begin ;allday
        bhlp(*)=1
      endif
      if ddn eq 1 then begin ;day
        for i=0,364+iyear do begin
          bhlp(24*i+7:24*i+18)=1
        endfor
      endif
      if ddn eq 2 then begin ;night
        for i=0,364+iyear do begin
          bhlp(24*i:24*i+6)=1
          bhlp(24*i+19:24*i+23)=1
        endfor
      endif
      if ddn eq 3 then begin  ;WD
        for i=0,364+iyear do begin
          DayYear=(julday(1,i+1,year)+1) mod 7
          if DayYear ne 0 and DayYear ne 6 then bhlp(i*24:i*24+23)=1
        endfor
      endif
      if ddn eq 4 then begin
        for i=0,364+iyear do begin
          DayYear=(julday(1,i+1,year)+1) mod 7
          if DayYear eq 0 or DayYear eq 6 then bhlp(i*24:i*24+23)=1
        endfor
      endif
    endif
    if iyear eq 0 then begin
      ahlp=reform(ahlp(0:8759))
      bhlp=reform(bhlp(0:8759))
    endif
    ahlp=ahlp(startIndex:endIndex)
    bhlp=bhlp(startIndex:endIndex)
    ;   intersection of ahlp and bhlp
    ahlp=ahlp*bhlp
    cc=where(ahlp eq 1,countDaySeason)
  endif
  countThreshold=float(countDaySeason)

  ccc=where(obsValues eq 0, countCCC)
  if countCCC gt 0 then obsValues(ccc)=-999.

  ;;  countThreshold=float(size(obsValues,/n_elements))/facDaySeason

  if iUseObserveModel eq 0 then idxAll=where(ahlp eq 1 and obsValues ne -999 and runValues ne -999, count)
  if iUseObserveModel eq 1 then idxAll=where(ahlp eq 1 and runValues ne -999, count)
  if count/countThreshold ge minDataAvail then begin
    obsValues=obsvalues(idxAll)
    runValues=runvalues(idxAll)
    if iUseObserveModel eq 1 then obsValues(*)=!values.f_nan 
    goto,jumpOut
  endif
  idxObs=where(obsValues ne -999, countObs)
  idxMod=where(runValues ne -999, countMod)
  if iUseObserveModel eq 0 then begin 
    if countObs/countThreshold lt minDataAvail and countMod/countThreshold ge minDataAvail then begin
      obsValues=obsvalues(idxMod)
      runValues=runvalues(idxMod)
      obsValues(*)=!values.f_nan
      goto,jumpOut
    endif
    if countObs/countThreshold ge minDataAvail and countMod/countThreshold lt minDataAvail then begin
      obsValues=obsvalues(idxObs)
      runValues=runvalues(idxObs)
      runValues(*)=!values.f_nan
      goto,jumpOut
    endif
    if countObs/countThreshold ge minDataAvail and countMod/countThreshold ge minDataAvail and $
      count/countThreshold lt minDataAvail then begin
      obsValues=obsvalues(idxMod)
      runValues=runvalues(idxMod)
      obsValues(*)=!values.f_nan
      goto,jumpOut
    endif
    if countObs/countThreshold lt minDataAvail and countMod/countThreshold lt minDataAvail then begin
      obsValues(*)=!values.f_nan
      runValues(*)=!values.f_nan
      goto,jumpOut
    endif
  endif
  if iUseObserveModel eq 1 then begin 
    if countMod/countThreshold ge minDataAvail then begin
      obsValues=obsvalues(idxMod)
      runValues=runvalues(idxMod)
      obsValues(*)=!values.f_nan
      goto,jumpOut
    endif
    if countObs/countThreshold ge minDataAvail and countMod/countThreshold ge minDataAvail and $
      count/countThreshold lt minDataAvail then begin
      obsValues=obsvalues(idxMod)
      runValues=runvalues(idxMod)
      obsValues(*)=!values.f_nan
      goto,jumpOut
    endif
    if countMod/countThreshold lt minDataAvail then begin
      obsValues(*)=!values.f_nan
      runValues(*)=!values.f_nan
      goto,jumpOut
    endif  
  endif
  
  jumpOut:

end
;*******************************
pro mypsym,number,size_symb

  CASE number OF

    0: begin ; diamond empty
      X = [-size_symb, 0, size_symb, 0, -size_symb]
      Y = [0, size_symb, 0, -size_symb, 0]
      USERSYM, X, Y,thick=2
    end

    1: begin ;square empty
      X = [-size_symb, -size_symb, size_symb,  size_symb, -size_symb]*0.75
      Y = [-size_symb,  size_symb, size_symb, -size_symb, -size_symb]*0.75
      USERSYM, X, Y,thick=2
    end

    2: begin ;triangle up empty
      X = [-size_symb,  0,  size_symb, -size_symb]*0.9
      Y = [-size_symb,  size_symb, -size_symb, -size_symb]*0.9
      USERSYM, X, Y,thick=2
    end

    3: begin ; triangle down empty
      X = [-size_symb,  0,           size_symb, -size_symb]*.9
      Y = [ size_symb,  -size_symb,  size_symb,  size_symb]*.9
      USERSYM, X, Y,thick=2
    end

    4: begin ; diamond full
      X = [-size_symb, 0, size_symb, 0, -size_symb]
      Y = [0, size_symb, 0, -size_symb, 0]
      USERSYM, X, Y,/fill
    end

    5: begin ; square full
      X = [-size_symb, -size_symb, size_symb,  size_symb, -size_symb]*.75
      Y = [-size_symb,  size_symb, size_symb, -size_symb, -size_symb]*.75
      USERSYM, X, Y,/fill
    end

    6: begin ; triangle up full
      X = [-size_symb,  0,  size_symb, -size_symb]*.9
      Y = [-size_symb,  size_symb, -size_symb, -size_symb]*.9
      USERSYM, X, Y,/fill
    end

    7: begin ; triangle down full
      X = [-size_symb,  0,           size_symb, -size_symb]*.9
      Y = [ size_symb,  -size_symb,  size_symb,  size_symb]*.9
      USERSYM, X, Y,/fill
    end

    8: begin ; + sign
      X = [0, 0, 0, size_symb, -size_symb]
      Y = [size_symb, -size_symb,0, 0,  0]
      USERSYM, X, Y,thick=2
    end

    9: begin ; circle full
      A = FINDGEN(17) * (!PI*2/16.)
      USERSYM, size_symb*COS(A), size_symb*SIN(A), /FILL
    end

    10: begin ; * sign
      X = [-size_symb, size_symb, 0, -size_symb,  size_symb,0,0, 0, 0, size_symb, -size_symb]
      Y = [-size_symb, size_symb, 0,  size_symb, -size_symb,0,size_symb, -size_symb,0, 0,  0]
      USERSYM, X, Y,thick=2
    end

    11: begin ; X sign
      X = [-size_symb, size_symb, 0, -size_symb,  size_symb]
      Y = [-size_symb, size_symb, 0,  size_symb, -size_symb]
      USERSYM, X, Y,thick=2
    end

    12: begin ; - sign
      X = [-size_symb   ,size_symb    ,size_symb     ,-size_symb    ,-size_symb]
      Y = [size_symb*0.2,size_symb*0.2,-size_symb*0.2,-size_symb*0.2,-size_symb*0.2]
      USERSYM, X, Y,thick=2
    end

    13:begin ; circle empty
      A = FINDGEN(17) * (!PI*2/16.)
      USERSYM, size_symb*1.7*COS(A), 1.7*SIN(A),thick=2,/fill
    end
  
; KeesC 20JUN2012    
    14: begin ; square line
      X = [-size_symb, -size_symb, size_symb,  size_symb, -size_symb]*.75
      Y = [-size_symb,  size_symb, size_symb, -size_symb, -size_symb]*.75
      USERSYM, X, Y, color=0
    end
      
endcase

end
function getDisclaimerText

  disclText=strarr(10)
  disclText[0]='Reserved line'
  disclText[1]='Reserved line'
  disclText[2]='Reserved line'
  disclText[3]='***********************'
  disclText[4]='Although the DELTA software has been constructed with great care,the European Commission does not accept responsability or liability whatsoever with regards to the use of this software'
  disclText[5]=''
  disclText[6]='Any decisions or applications based on DELTA are the sole responsability of the user'
  disclText[7]=''
  disclText[8]=''
  disclText[9]=''
  return, disclText
end
PRO postProcessing, request, result

  ;ToDo: you need to fill this to have a nice plot... yrange?
  util=obj_new('FMUtility')
  diagramCode=request->getDiagramCode()
  ;diagramCode=0
  ;set main title
  title=request->getDiagramName()+' - '+request->getElaborationName()
  ;set y title
  mus=request->getParameterMeasureUnits()
  ytitle=mus[0]+'#'
  for i=1, n_elements(mus)-1 do yTitle=yTitle+mus[i]+'#'
  ytitle=strmid(ytitle, 0, strlen(ytitle)-1)
  case diagramCode of
    0:begin ;bar plot section
    ;fill if there is generic actions
    ;xrange=[util->dateToHours(request->getStartDate()), util->dateToHours(request->getEndDate())]
    ;util->dateToHours(request->getStartDate())
    ;util->dateToHours(request->getEndDate())
    xTitle=''
    ;yrange=fltarr(2)
    ;no more:here to specific 'Axis' selection (map data with plot)
    ;but:pass as parameter which selections are multiple...
    request->buildBarPlotInfo, result;, multipleSelection=...
    if request->getParameterNumber() eq 1 then title=title+request->getParameterCodes()
  end
  1:begin ;Time series section, free plot (multiple parameters)
  ;xrange=[util->dateToHours(request->getStartDate()), util->dateToHours(request->getEndDate())]
  ;util->dateToHours(request->getStartDate())
  ;util->dateToHours(request->getEndDate())
  xTitle='Date/Time'
  mus=request->getParameterMeasureUnits()
  ;Always one year of tickmarks!!!
  xLabels=util->buildDateAxisTickMark(request->getFirstStandardDate(), '1H', request->getStandardInterval())
  yrange=fltarr(2)
  yrange[0]=min([result->getRawMonMinVal(),result->getRawRunMinVal()]) & yrange[1]=max([result->getRawMonMaxVal(),result->getRawRunMaxVal()])
  if request->getParameterNumber() eq 1 then title=title+request->getParameterCodes()
end

endcase
obj_destroy, util
;standard way to fill plot info!!!

;  if indic lt 200 then barsD1D2(index,*)=bars
;  if ((indic ge 200 and indic le 204) or indic eq 206) and indic ne 202 then $
;    plotD1D2(index,*,*)=tseries
;  if indic eq 202 then begin
;    if index eq 0 then obsallD1=obsallh
;    if index eq 1 then obsallD2=obsallh
;    if index eq 0 then varallD1=varallh
;    if index eq 1 then varallD2=varallh
;  endif
;noplot:
;  widget_control,ind_txt,set_value=indic0+unitm
;  varallD12=varallD12sav
;  obsallD12=obsallD12sav
;  varallD12sav=1.
;  obsallD12sav=1.
;  barsh=0
;  indhlp=0
END
function median, arrValues

  valNum=n_elements(arrValues)
  arrValues=arrValues(sort(arrvalues))
  statisticValue=arrValues(fix(valNum/2.))
  return, statisticValue

end
;***********************

PRO runningAverage, request, result

  stop
  runningMeanInfo=request.runningMeanInfo
  if runningMeanInfo.template eq 'hh' then hours=fix(runningMeanInfo.value)

  iyear=0
  yrhrs=8760
  if 4*(year/4) eq year then begin   ;leapyear
    iyear=1  
    yrhrs=8784
  endif
  datas=n_elements(dataResult)
  bigMatrix=fltarr(datas, 2, yrhrs, /NO)
  monitIdxs=intarr(datas)
  runIdxs=intarr(datas)
  j=0 & k=0
  for i=0, n_elements(dataResult)-1 do begin
    if ptr_valid(dataResult[i].observedData) then begin
      bigMatrix[i, 0, *]=*dataResult[i].observedData
      monitIdxs[j]=i
      j++
    endif
    if ptr_valid(dataResult[i].runData) then begin
      bigMatrix[i, 1, *]=*dataResult[i].runData
      runIdxs[k]=i
      k++
    endif
  endfor
  parMax=max([k, j])
  monitIdxs=monitIdxs[0:j-1]
  runIdxs=runIdxs[0:k-1]
  bigMatrix[0:j-1, 0, *]=bigMatrix[monitIdxs, 0, *] ;
  bigMatrix[0:k-1, 1, *]=bigMatrix[runIdxs, 1, *] ;
  bigMatrix=bigMatrix[0:parMax-1, *, *]
  hlpT=fltarr(parMax, 2, yrhrs, 2)
  hlp1=finite(bigMatrix)
  hlp2=bigMatrix
  ;[1 obs+n models, statNumbers, hours]
  for ih=1,hours-1 do begin
    hlp1=hlp1+shift(finite(bigMatrix),0,0,ih)
    hlpT(*,*,*,0)=hlp2
    hlp2=0
    hlpT(*,*,*,1)=shift(bigMatrix,0,0,ih)
    hlp2=total(hlpT,4,/nan)
  endfor
  chlp=where(hlp1 eq 0,cnth)
  if cnth ge 1 then hlp1(chlp)=!values.f_nan
  for n=0,yrhrs-1 do begin
    bigMatrix[*,*,n]=hlp2[*,*,n]/hlp1[*,*,n]
  endfor
  obsvar=0
  hlpT=0 & hlp1=0 & hlp2=0
  for i=0, n_elements(monitIdxs)-1 do begin
    ;if ptr_valid(dataResult[i].observedData) then begin
    ptr_free, dataResult[monitIdxs[i]].observedData
    dataResult[monitIdxs[i]].observedData=ptr_new(bigMatrix[i, 0, *], /NO_COPY)
  endfor
  for i=0, n_elements(runIdxs)-1 do begin
    ;if ptr_valid(dataResult[i].observedData) then begin
    ptr_free, dataResult[runIdxs[i]].runData
    dataResult[runIdxs[i]].runData=ptr_new(bigMatrix[i, 1, *], /NO_COPY)
  endfor
  bigMatrix=0

END
PRO preProcessing, request, result

  ; day,daytime,night, year, summer,winter
  dtu=obj_new('DateTimeUtility')

  sInfo=request->getSplitSeasonInfo()
  hInfo=request->getSplitHourInfo()

  hPeriods=n_elements(hInfo)
  sPeriods=n_elements(sInfo)
  startH=dtu->getAbsoluteHours(hInfo[0:hPeriods/2-1].value, template=hInfo[0].template)
  endH=dtu->getAbsoluteHours(hInfo[hPeriods/2:hPeriods-1].value, template=hInfo[hPeriods/2].template)
  startS=dtu->getAbsoluteHours(sInfo[0:sPeriods/2-1].value, template=sInfo[0].template)
  endS=dtu->getAbsoluteHours(sInfo[sPeriods/2:sPeriods-1].value, template=sInfo[sPeriods/2].template)
  if not(dtu->containHour(sInfo[0].template)) then startS[*]=startS[*]-23
  if not(dtu->containHour(sInfo[sPeriods/2].template)) then endS[*]=endS[*]
  if not(dtu->containHour(hInfo[0].template)) then startH[*]=startH[*]-23
  if not(dtu->containHour(hInfo[hPeriods/2].template)) then endH[*]=endH[*]
  obj_destroy, dtu
  allYearHours=bytarr(8760)
  result->applySeasonPeriodSelection, startS, endS
  result->applyHourPeriodSelection, startH, endH
END

pro time_operations, request, result, obsTemp, runTemp

  ; selection of averaging time information
  ;************************************

  modelInfo=request->getModelInfo()
  year=modelInfo.year
  scale=modelInfo.scale
  frequency=modelInfo.frequency
  startIndex=request->getStartIndex()
  endIndex=request->getEndIndex()
    modelInfo=request->getModelInfo()
    year=modelInfo.year
    if 4*(fix(year)/4) ne fix(year) then begin  ; normal year
      Feb29start=59*24
      Feb29end=Feb29start+23
      if startIndex lt Feb29start and endIndex ge FEB29end then endIndex=endIndex-24
      if startIndex ge Feb29start then begin
        endIndex=endIndex-24
        startIndex=startIndex-24
      endif  
    endif  
  elabcode=request->getElaborationCode()
  min08Avail=6 ; minimal 8-hours values available
  minDayAvail=18 ; minimal 18 8-hour-mean values should be available per day

  if 4*(year/4) ne year then begin   ;normal year
    iyear=0
    yrhrs=8760
    day_nb= [31,28,31,30,31,30,31,31,30,31,30,31]
    day_sum=[0,31,59,90,120,151,181,212,243,273,304,334,365]
  endif
  if 4*(year/4) eq year then begin   ;leapyear - no 31 dec
    iyear=1
    yrhrs=8784
    day_nb= [31,29,31,30,31,30,31,31,30,31,30,31]
    day_sum=[0,31,60,91,121,152,182,213,244,274,305,335,366]
  endif
  hourStat=request->getGroupByTimeInfo() ;HourType
  flag_average=hourStat[0].value
  if flag_average eq 'preserve' then begin
  ;    no action
  endif
  if flag_average eq '08' then begin
    flag_average=fix(flag_average)
    ahlp=fltarr(yrhrs,flag_average)
    bhlp=fltarr(yrhrs,flag_average)
    for i=0,flag_average-1 do begin
      ahlp(*,i)=shift(obsTemp,i)
      bhlp(*,i)=shift(runTemp,i)
      if i ge 1 then ahlp(0:i-1,i)=-999
      if i ge 1 then bhlp(0:i-1,i)=-999
    endfor
    iahlp=intarr(yrhrs,flag_average) & iahlp(*,*)=1
    ibhlp=intarr(yrhrs,flag_average) & ibhlp(*,*)=1
    kcobs=where(ahlp lt -990,nkcobs)
    kcrun=where(bhlp lt -990,nkcrun)
    if nkcobs ge 1 then begin
      ahlp(kcobs)=!values.f_nan
      iahlp(kcobs)=0
    endif
    if nkcrun ge 1 then begin
      bhlp(kcrun)=!values.f_nan
      ibhlp(kcrun)=0
    endif
    tiahlp=total(iahlp,2) ;,/integer)
    tibhlp=total(ibhlp,2) ;,/integer)
    kcvalobs=where(tiahlp lt min08Avail,nkcvalobs)  ; not sufficient hours for 8h avg
    kcvalrun=where(tibhlp lt min08Avail,nkcvalrun)
    if nkcvalobs ge 1 then tiahlp(fix(kcvalobs))=!values.f_nan
    if nkcvalrun ge 1 then tibhlp(fix(kcvalrun))=!values.f_nan
    obsTemp=total(ahlp,2,/nan)/tiahlp   ; total for not nan values divided by number of not nan value
    runTemp=total(bhlp,2,/nan)/tibhlp
    kcobs=where(finite(obsTemp) eq 0,nkcobs)
    kcrun=where(finite(runTemp) eq 0,nkcrun)
    if nkcobs ge 1 then obsTemp(kcobs)=-999
    if nkcrun ge 1 then runTemp(kcrun)=-999
  endif

  ; selection of statistical operator information
  ;************************************
  statType=request->getGroupByStatInfo() ;HourType

  if statType eq 0 then begin  ; preserve
  endif
  if statType ge 1 then begin  ; at least 18 values per day, otherwise -999
    for i=0,364+iyear do begin
      kcobs=where(obsTemp(i*24:i*24+23) gt -990,nkcobs)
      kcrun=where(runTemp(i*24:i*24+23) gt -990,nkcrun)
      if nkcobs lt minDayAvail then obsTemp(i*24:i*24+23)=-999
      if nkcrun lt minDayAvail then runTemp(i*24:i*24+23)=-999
    endfor
  endif
  cobs=where(obsTemp eq -999 or obsTemp eq -8888,countobs)  ; KeesC 8888
  crun=where(runTemp eq -999,countrun)
  if countobs gt 0 then obsTemp(cobs)=!values.f_nan
  if countrun gt 0 then runTemp(crun)=!values.f_nan
  if statType eq 0 then begin  ; preserve
  endif
  if statType eq 1 then begin  ; MEAN
    for i=0,364+iyear do begin
      obsTemp(i*24:i*24+23)=mean(obsTemp(i*24:i*24+23),/nan)
    endfor
    for i=0,364+iyear do begin
      runTemp(i*24:i*24+23)=mean(runTemp(i*24:i*24+23),/nan)
    endfor
  endif
  if statType eq 2 then begin  ; MAX
    for i=0,364+iyear do obsTemp(i*24:i*24+23)=max(obsTemp(i*24:i*24+23),/nan)
    for i=0,364+iyear do runTemp(i*24:i*24+23)=max(runTemp(i*24:i*24+23),/nan)
  endif
  if statType eq 3 then begin  ; MIN
    for i=0,364+iyear do obsTemp(i*24:i*24+23)=min(obsTemp(i*24:i*24+23),/nan)
    for i=0,364+iyear do runTemp(i*24:i*24+23)=min(runTemp(i*24:i*24+23),/nan)
  endif
  cc=where(finite(obstemp) eq 0,count)
  if count gt 0 then obstemp(cc)=-999
  cc=where(finite(runtemp) eq 0,count)
  if count gt 0 then runtemp(cc)=-999
  ;getGroupByStatInfo end

  if elabcode ge 0 and elabcode ne 71 and elabcode ne 72 and elabcode ne 73 then begin
    ; For elabcode 71,72,73 the full time series is needed for the calculation of
    ; Day-Night
    ; Summer-Winter
    ; WeekDays-WeekEnd
    ; selection of seasonal information
    ;************************************

    ;season input --> modification of start and end indexes
    season=request->getSeasonInfo()
    start_seasons_dates_hlp=season[0].value
    end_seasons_dates_hlp=season[1].value
    res_start=strsplit(start_seasons_dates_hlp,'*',/extract)
    day_start=fix(strmid(res_start,0,2))
    month_start=fix(strmid(res_start,2,2))
    index_start_season=intarr(n_elements(day_start))
    res_end=strsplit(end_seasons_dates_hlp,'*',/extract)
    day_end=fix(strmid(res_end,0,2))
    month_end=fix(strmid(res_end,2,2))
    index_end_season=intarr(n_elements(day_end))
    ysw=request->getSeasonType()  ;year, summer, winter
    ; From season: Winter is 0101-2802 & 0112-3112
    ; For leapyear should be 0101-2902 & 0112-3112
    if 4*(year/4) eq year and ysw eq 2 then begin  ;winter
      whlp=where(month_end eq 2,wnc)  ;find Feb month
      if wnc eq 1 then begin
        if day_end(whlp[0]) eq 28 then day_end(whlp[0])=29
      endif
    endif

    for i=0,n_elements(day_start)-1 do begin
      index_start_season(i)=day_sum(month_start(i)-1)*24+(day_start(i)-1)*24
      index_end_season(i)=day_sum(month_end(i)-1)*24+(day_end(i))*24-1
      index_end_season(i)=min([index_end_season(i),yrhrs-1])
    endfor

    ahlp=intarr(yrhrs) & ahlp(*)=0
    for i=0,n_elements(day_start)-1 do ahlp(index_start_season(i):index_end_season(i))=1

    ;take season into account
    cc=where(ahlp eq 0,count)
    if count gt 0 then obsTemp(cc)=-999
    if count gt 0 then runTemp(cc)=-999

    ; selection of hour (day-night) information
    ;************************************

    hour=request->getHourInfo() ;HourType
    start_hour_hlp=hour[0].value
    end_hour_hlp=hour[1].value
    res_start=strsplit(start_hour_hlp,'*',/extract)
    res_end=strsplit(end_hour_hlp,'*',/extract)
    ahlp=intarr(yrhrs) & ahlp(*)=0
    ddn=request->getHourType()    ;allday, day, night, WD, WE

;KeesC 4APR2013
;    if res_start[0] eq 'WD' then begin  
    if ddn eq 3 then begin
      for i=0,364+iyear do begin
        DayYear=(julday(1,i+1,year)+1) mod 7
        if DayYear ne 0 and DayYear ne 6 then ahlp(i*24:i*24+23)=1
      endfor
    endif
;    if res_start[0] eq 'WE' then begin
    if ddn eq 4 then begin
      for i=0,364+iyear do begin
        DayYear=(julday(1,i+1,year)+1) mod 7
        if DayYear eq 0 or DayYear eq 6 then ahlp(i*24:i*24+23)=1
      endfor
    endif
;    if res_start[0] ne 'WD' and res_start[0] ne 'WE' then begin
    if ddn ne 3 and ddn ne 4 then begin
      res_start=fix(strsplit(start_hour_hlp,'*',/extract))
      res_end=fix(strsplit(end_hour_hlp,'*',/extract))
      for j=0,n_elements(res_start)-1 do begin
        for i=0,364+iyear do begin
          ahlp(24*i+res_start(j):24*i+res_end(j))=1
        endfor
      endfor
    endif

    ; take day/night into account
    cc=where(ahlp eq 0,count)
    if count gt 0 then obsTemp(cc)=-999
    if count gt 0 then runTemp(cc)=-999
  endif

  if abs(elabcode) eq 71 or abs(elabcode) eq 72 or abs(elabcode) eq 73 then begin

    hour=request->getHourInfo() ;HourType
    start_hour_hlp=hour[0].value
    end_hour_hlp=hour[1].value
    
    ysw=request->getSeasonType()  ;year, summer, winter
    ddn=request->getHourType()    ;allday, day, night, WD, WE
 
    ahlp=intarr(yrhrs) & ahlp(*)=0
    if elabcode eq 71 then begin  ;day
      for i=0,364+iyear do begin
        ahlp(24*i+fix(start_hour_hlp)-1:24*i+fix(end_hour_hlp)-1)=1
      endfor
    endif
    if elabcode eq -71 then begin  ;night
      for i=0,364+iyear do begin
        ahlp(24*i:24*i+fix(start_hour_hlp)-2)=1
        ahlp(24*i+fix(end_hour_hlp):24*i+23)=1
      endfor
    endif
    if elabcode eq 72 then begin  ;summer
      ahlp(day_sum(5)*24:day_sum(8)*24-1)=1
    endif
    if elabcode eq -72 then begin  ;winter
      ahlp(day_sum(0):day_sum(2)*24-1)=1
      ahlp(day_sum(11)*24:yrhrs-1)=1
    endif
    if elabcode eq 73 then begin  ;weekdays
      for i=0,364+iyear do begin
        DayYear=(julday(1,i+1,year)+1) mod 7
        if DayYear ne 0 and DayYear ne 6 then ahlp(i*24:i*24+23)=1
      endfor
    endif
    if elabcode eq -73 then begin  ;weekend
      for i=0,364+iyear do begin
        DayYear=(julday(1,i+1,year)+1) mod 7
        if DayYear eq 0 or DayYear eq 6 then ahlp(i*24:i*24+23)=1
      endfor
    endif

    bhlp=intarr(yrhrs) & bhlp(*)=0
    if abs(elabcode) eq 71 or abs(elabcode) eq 73 then begin
      if ysw eq 0 then begin  ;year
        bhlp(*)=1
      endif
      if ysw eq 1 then begin  ;summer
        bhlp(day_sum(5)*24:day_sum(8)*24-1)=1
      endif
      if ysw eq 2 then begin ; winter
        bhlp(day_sum(0):day_sum(2)*24-1)=1
        bhlp(day_sum(11)*24:yrhrs-1)=1
      endif
    endif
    if abs(elabcode) eq 72 then begin
      if ddn eq 0 then begin ;allday
        bhlp(*)=1
      endif
;      if ddn eq 1 then begin ;day
;        for i=0,364+iyear do begin
;          bhlp(24*i+7:24*i+18)=1
;        endfor
;      endif
;      if ddn eq 2 then begin ;night
;        for i=0,364+iyear do begin
;          bhlp(24*i:24*i+6)=1
;          bhlp(24*i+19:24*i+23)=1
;        endfor
;      endif
;      if ddn eq 3 then begin  ;WD
;        for i=0,364+iyear do begin
;          DayYear=(julday(1,i+1,year)+1) mod 7
;          if DayYear ne 0 and DayYear ne 6 then bhlp(i*24:i*24+23)=1
;        endfor
;      endif
;      if ddn eq 4 then begin
;        for i=0,364+iyear do begin
;          DayYear=(julday(1,i+1,year)+1) mod 7
;          if DayYear eq 0 or DayYear eq 6 then bhlp(i*24:i*24+23)=1
;        endfor
;      endif

    endif
    ahlp=ahlp*bhlp
    cc=where(ahlp eq 0,count)
    if count gt 0 then obsTemp(cc)=-999
    if count gt 0 then runTemp(cc)=-999
  endif
end