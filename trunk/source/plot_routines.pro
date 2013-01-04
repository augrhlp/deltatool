;********************
@structure_definition
;********************
;  ;MM summer 2012 Start
;  ;for Goals & Criteria now you can use (inside elaboration.dat)
;  ;elaboration related info called OCTimeAvgName & OCStat 
;  OCTimeAvgName=request->getElaborationOCTimeAvgName()
;  OCStat=request->getElaborationOCStat()
;  ;MM summer 2012 End
FUNCTION CIRCLE, xcenter, ycenter, radius

  points = (2 * !PI / 99.0) * FINDGEN(100)
  x = xcenter + radius * COS(points )
  y = ycenter + radius * SIN(points )
  RETURN, TRANSPOSE([[x],[y]])
  
END
PRO FM_PlotBars, plotter, request, result

  plotter->wsetMainDataDraw
  plotInfo=result->getPlotInfo()
  targetInfo=result->getGenericPlotInfo()
  DEVICE,decomposed=0
  LOADCT,39
  mytek_color;, 0, 32
  !p.color=0
  !y.charsize=1.5
  allDataXY=targetInfo->getXYS()
  if string(allDataXY[0]) eq 'AllNaN' then begin
    res=dialog_message(['No validated stations - all MOD/OBS NaN',' '],/information)
    goto,noplot
  endif
  dims=size(allDataXY,/dimensions)
  npar=dims(0) & nmod=dims(1) & nsce=dims(2) & nobs=dims(3)  ;  !nobs is number of validated stations&groups
  nstat=nobs
  legNames=targetInfo->getLegendNames()  ;long name
  allDataSymbol=targetInfo->getSymbols()
  allDataColor=targetInfo->getColors()
  modelCodes=request->getModelCodes()
  groupTitles=request->getGroupTitles()
  parCodes=request->getParameterCodes()
  modelCodes=request->getModelCodes()
  scenarioCodes=request->getScenarioCodes()
  elabcode=request->getElaborationCode()
  elabname=request->getElaborationName()
  mus=request->getParameterMeasureUnits()
  ;MM summer 2012 Start
  ;for Goals & Criteria now you can use
  ;elaboration related info called OCTimeAvgName & OCStat 
  OCTimeAvgName=request->getElaborationOCTimeAvgName()
  OCStat=request->getElaborationOCStat()
  ;MM summer 2012 End
  nmulti=npar*nsce*nmod
  ifree=reform(legNames(0,5))
  iUseObserveModel=request->getUseObservedModel()  ; 0=0ld case; 1=no obs
  
  choices=[npar,nmod,nsce,nstat]
  
  !y.range=[min([0,min(allDataXY,/nan)])*1.1, max([0,max(allDataXY,/nan)])*1.1]
  obsbar=1
  if total(where(elabCode eq [2,3,4,5,7,8,14,23,24,28,30,33,54])) ge 0 then begin
    allDataXY(*,*,*,*,0)=0.
    obsbar=0
    if ifree eq '1101' then begin
      allDataXY(*,*,*,*,0)=(!y.range(1)-!y.range(0))/50.
      obsbar=1
    endif
  endif
  if strmid(ifree,2,1) eq '1' then begin
    allDataXY(*,*,*,*,0)=0.
    obsbar=0
  endif
  if ifree eq '1011' or ifree eq '0111' then begin
    allDataXY(*,*,*,*,0)=(!y.range(1)-!y.range(0))/50.
    obsbar=1
  endif
  
  if ifree eq '1000' then begin
    plotVarObs=reform(allDataXY(*,*,0,0,0))
    plotVarMod=reform(allDataXY(*,*,0,0,1))
    nBars=npar & nSubBars=1 & nDots=1
    ntxt1=0 & ntxt2=3 & ntxt3=1
  endif
  if ifree eq '0100' then begin
    plotVarObs=transpose(allDataXY(*,*,*,*,0),[0,1])
    plotVarMod=transpose(allDataXY(*,*,*,*,1),[0,1])
    nBars=1 & nSubBars=1 & nDots=nmod
    ntxt1=0 & ntxt2=3 & ntxt3=1
  endif
  if ifree eq '0010' then begin
    plotVarObs=reform(allDataXY(*,0,*,0,0))
    plotVarMod=reform(allDataXY(*,0,*,0,1))
    nBars=nsce & nSubBars=1 & nDots=1
    ntxt1=2 & ntxt2=0 & ntxt3=1
  endif
  if ifree eq '0001' then begin
    plotVarObs=reform(allDataXY(*,0,0,*,0))
    plotVarMod=reform(allDataXY(*,0,0,*,1))
    nBars=nstat & nSubBars=1 & nDots=1
    ntxt1=3 & ntxt2=0 & ntxt3=1
  endif
  if ifree eq '1100' then begin
    plotVarObs=reform(allDataXY(*,*,0,0,0),npar,nmod)
    plotVarMod=reform(allDataXY(*,*,0,0,1),npar,nmod)
    nBars=npar & nSubBars=1 & nDots=nmod
    ntxt1=0 & ntxt2=1 & ntxt3=1
  endif
  if ifree eq '1010' then begin
    plotVarObs=reform(allDataXY(*,0,*,0,0),npar,nsce)
    plotVarMod=reform(allDataXY(*,0,*,0,1),npar,nsce)
    nBars=npar & nSubBars=1 & nDots=nsce
    ntxt1=0 & ntxt2=2 & ntxt3=2
  endif
  if ifree eq '1001' then begin
    plotVarObs=reform(allDataXY(*,0,0,*,0),npar,nstat)
    plotVarMod=reform(allDataXY(*,0,0,*,1),npar,nstat)
    plotVarObs=transpose(plotVarObs)
    plotVarMod=transpose(plotVarMod)
    nBars=nstat & nSubBars=npar & nDots=1
    ntxt1=3 & ntxt2=0 & ntxt3=0
    ; KeesC
    if total(where(elabCode eq [2,3,4,5,7,8,14,23,24,28,30,33,54])) ge 0 then begin
      nBars=nstat & nSubBars=1 & nDots=npar
      ntxt1=3 & ntxt2=0 & ntxt3=0
    endif
  endif
  if ifree eq '0110' then begin
    plotVarObs=reform(allDataXY(0,*,*,0,0),nmod,nsce)
    plotVarMod=reform(allDataXY(0,*,*,0,1),nmod,nsce)
    ;       plotVarMod(1,*)=plotVarMod(0,*)*.9   ; for test
    plotVarObs=transpose(plotVarObs)
    plotVarMod=transpose(plotVarMod)
    nBars=nsce & nSubBars=1 & nDots=nmod
    ntxt1=2 & ntxt2=1  & ntxt3=1
  endif
  if ifree eq '0101' then begin
    plotVarObs=reform(allDataXY(0,*,0,*,0),nmod,nstat)
    plotVarMod=reform(allDataXY(0,*,0,*,1),nmod,nstat)
    plotVarObs=transpose(plotVarObs)
    plotVarMod=transpose(plotVarMod)
    nBars=nstat & nSubBars=1 & nDots=nmod
    ntxt1=3 & ntxt2=0 & ntxt3=1
    ;KeesC
    if total(where(elabCode eq [2,3,4,5,7,8,14,23,24,28,30,33,54])) ge 0 then begin
      nBars=nstat & nSubBars=1 & nDots=nmod
      ntxt1=3 & ntxt2=0 & ntxt3=1
    endif
  endif
  if ifree eq '0011' then begin
    plotVarObs=reform(allDataXY(0,0,*,*,0),nsce,nstat)
    plotVarMod=reform(allDataXY(0,0,*,*,1),nsce,nstat)
    plotVarObs=transpose(plotVarObs)
    plotVarMod=transpose(plotVarMod)
    nBars=nstat & nSubBars=1 & nDots=nsce
    ntxt1=3 & ntxt2=2 & ntxt3=2
  endif
  if ifree eq '1101' then begin
    plotVarObs=reform(allDataXY(*,*,0,*,0),npar,nmod,nstat)
    plotVarMod=reform(allDataXY(*,*,0,*,1),npar,nmod,nstat)
    plotVarObs=transpose(plotVarObs,[2,0])
    plotVarMod=transpose(plotVarMod,[2,0,1])
    nBars=nstat & nSubBars=npar & nDots=nmod
    ntxt1=3 & ntxt2=0 & ntxt3=1
  endif
  if ifree eq '1011' then begin
    plotVarObs=reform(allDataXY(*,0,*,*,0),npar,nsce,nstat)
    plotVarMod=reform(allDataXY(*,0,*,*,1),npar,nsce,nstat)
    plotVarObs=transpose(plotVarObs,[2,0])
    plotVarMod=transpose(plotVarMod,[2,0,1])
    nBars=nstat & nSubBars=npar & nDots=nsce
    ntxt1=3 & ntxt2=0 & ntxt3=2
  endif
  if ifree eq '1110' then begin
    plotVarObs=reform(allDataXY(*,*,*,0,0),npar,nmod,nsce)
    plotVarMod=reform(allDataXY(*,*,*,0,1),npar,nmod,nsce)
    plotVarObs=transpose(plotVarObs,[0,2])
    plotVarMod=transpose(plotVarMod,[0,2,1])
    nBars=npar & nSubBars=nsce & nDots=nmod
    ntxt1=0 & ntxt2=2 & ntxt3=1
  endif
  if ifree eq '0111' then begin
    plotVarObs=reform(allDataXY(0,*,*,*,0),nmod,nsce,nstat)
    plotVarMod=reform(allDataXY(0,*,*,*,1),nmod,nsce,nstat)
    plotVarObs=transpose(plotVarObs,[2,0])
    plotVarMod=transpose(plotVarMod,[2,1,0])
    ;      plotVarMod(*,*,1)=plotVarMod(*,*,1)*.9   ; for test
    nBars=nstat & nSubBars=nsce & nDots=nmod
    ntxt1=3 & ntxt2=2 & ntxt3=1
  endif
  
  choices2=['PARAMETERS','MODELS','SCENARIOS','STATIONS']
  xtitle=choices2(ntxt1)
  
  if ntxt1 ge 0 then longBarNames1=reform(legNames(*,ntxt1))
  if ntxt2 ge 0 then longBarNames2=reform(legNames(*,ntxt2))
  if ntxt3 ge 0 then longBarNames3=reform(legNames(*,ntxt3))
  ;if ifree eq '0001' and nstat gt 12 then longBarNames1=strcompress(indgen(nstat)+1,/remove_all)
  
  colors=intarr(n_elements(plotVarObs))
  colors[*]=15
  musstr=''
  for i=0,n_elements(mus)-1 do musstr=musstr+'['+mus(i)+'] '
  ytitle='Units '+musstr
  if elabCode eq 2 or elabCode eq 14 then ytitle='Units [1] '
  if elabCode eq 9  then ytitle='Units [Number of days] '
  if elabCode eq 8  or elabCode eq 30 or elabCode eq 33 $
    or elabCode eq 23 or elabCode eq 24 or elabCode eq 3 then ytitle='Units [%] '
  if elabCode eq 26 then ytitle='Units [mg/m3*hrs] '
  if elabCode eq 27 then ytitle='Units [mg/m3*days] '
  
  recognizeRangeX=nbars*0.5
  recognizeRangeY=(max([0,max(allDataXY,/nan)])-min([0,min(allDataXY,/nan)]))*0.01
  nhlp=nsubbars*ndots
  ;  if nsubbars ge 2 then begin
  recognizeHighLight=bytarr(nBars*nhlp*2)
  recognizeRegionEdges=ptrarr(nBars*nhlp*2)
  recognizeNames=strarr(nBars*nhlp*2)
  recognizeValues=strarr(nBars*nhlp*2)
  
  for i =0,nBars*nhlp*2-1 do begin
    recognizePoint=fltarr(4,2)
    recognizePoint[0,*]=[-recognizeRangeX, -recognizeRangeX]
    recognizePoint[1,*]=[-recognizeRangeX, recognizeRangeX]
    recognizePoint[2,*]=[+recognizeRangeX, recognizeRangeX]
    recognizePoint[3,*]=[+recognizeRangeX, -recognizeRangeX]
    recognizePoint=transpose(recognizePoint)
    normRecognizePoint=convert_coord(recognizePoint, /DATA, /TO_NORMAL)
    normRecognizePoint=transpose(normRecognizePoint)
    normRecognizePoint=normRecognizePoint[*, 0:1]
    recognizePointPtr=ptr_new(normRecognizePoint, /NO_COPY)
    recognizeHighLight[i]=0b
    recognizeRegionEdges[i]=recognizePointPtr
  endfor
  
  recognizeValues(*)='NOVAL'
  recognizeNames(*)='NOVAL'
  
  tickV=fltarr(nsubbars*nBars)
  for isubbar=0,nsubbars-1 do begin
    plotHlp=reform(plotVarObs(0:nBars-1,isubbar))
    chlp=where(finite(plotHlp) eq 0,nchlp)
    if nchlp ge 1 then plotHlp(chlp)=0
    colors[*]=15-isubbar
    bo=.5+isubbar
    mybar_plot,plotHlp,nsubbars,barnames=strmid(longBarNames1,0,7),colors=colors,background=255,$
      ytitle=ytitle,outline=1,tickV0,request,result,title=elabname,$
      baroffset=bo,overplot=(isubbar gt 0),xtitle=xtitle
    tickV[isubbar*nBars:isubbar*nBars+nBars-1]=tickV0    
    if nchlp ge 1 then begin
      xyouts,tickV0(chlp),float(replicate('1.',nchlp))*recognizeRangeY,replicate('',nchlp),$
        charsize=1.5,charthick=1.5,orientation=90
    endif
  endfor
  colors[*]=15
  
  if nBars gt 1 then recognizeRangeX=(tickV(1)-tickV(0))/(nsubbars+0.75)/2.  ; 0.75 = bs in barplot
  if nBars eq 1 then recognizeRangeX=0.25
  for i=0,nBars-1 do begin
    for isubbar=0,nsubbars-1 do begin
      recognizePoint=fltarr(4,2)
      j=i+isubbar*nBars
      recognizePoint[0,*]=[tickV(j)-recognizeRangeX, min([0,plotVarObs(i,isubbar)])]
      recognizePoint[1,*]=[tickV(j)-recognizeRangeX, max([0,plotVarObs(i,isubbar)])]
      recognizePoint[2,*]=[tickV(j)+recognizeRangeX, max([0,plotVarObs(i,isubbar)])]
      recognizePoint[3,*]=[tickV(j)+recognizeRangeX, min([0,plotVarObs(i,isubbar)])]
      recognizePoint=transpose(recognizePoint)
      normRecognizePoint=convert_coord(recognizePoint, /DATA, /TO_NORMAL)
      normRecognizePoint=transpose(normRecognizePoint)
      normRecognizePoint=normRecognizePoint[*, 0:1]
      recognizePointPtr=ptr_new(normRecognizePoint, /NO_COPY)
      recognizeHighLight[j]=0b
      recognizeRegionEdges[j]=recognizePointPtr
      ihlp=isubbar
      recognizeNames[j]=LongBarnames2[ihlp]
      if obsbar eq 1 then begin
        recognizeNames[j]=recognizeNames[j]+'- OBS'
        if ifree eq '1100' then recognizeNames[j]='OBS'
        if ifree eq '0101' then recognizeNames[j]='OBS'
        if ifree eq '0001' then begin
          longBarNames4=reform(legNames(*,ntxt1))
          recognizeNames[j]=longBarNames4[j]
 ;         if nstat gt 12 then recognizeNames[j]=strcompress(j+1,/remove_all)+'='+recognizeNames[j]
        endif
      endif
      recognizeValues[j]=strtrim(plotVarObs[i, isubbar], 2)
    endfor
  endfor
  
  recognizeRangeX=0.03
  kg=nBars
  mypsym,9,1
  if nsubbars ge 2 then kg=nsubbars*nBars
  for isubbar=0,nsubbars-1 do begin
    if nsubbars ge 2 then begin
      for idot=0,nDots-1 do begin
        if nDots eq 1 then begin
          plotHlp=reform(plotVarMod(0:nBars-1,isubbar))
          colarr=intarr(nBars) & colarr(*)=isubbar+2
        endif
        if nDots ge 2 then begin
          plotHlp=reform(plotVarMod(0:nBars-1,isubbar,idot))
          colarr=intarr(nBars) & colarr(*)=idot+2
        endif
        ;        colarr=intarr(nBars) & colarr(*)=isubbar+2
        ;        plotHlp=reform(plotVarMod(0:nBars-1,isubbar))
        chlp=where(finite(plotHlp) eq 0,nchlp)
        if nchlp ge 1 then colarr(chlp)=1
        plots,tickV(isubbar*nBars:isubbar*nBars+nBars-1),plotHlp,psym=8,symsize=2,color=colarr  ; isubbar+2
        if obsbar eq 0 or ifree eq '1011' or ifree eq '0111' or $
          (ifree eq '1101' and total(where(elabCode eq [2,3,4,5,7,8,23,24,28,30,33,54])) ge 0) then begin
          if ifree eq '1101' or ifree eq '1011' or ifree eq '1110' or ifree eq '0111' then begin
            col2=15-isubbar
          endif else begin
            col2=0
          endelse
          for ibar=0,nBars-1 do begin
            top=plotHLP(ibar)
            bot=0.    ;min([!y.range(0),0.],/nan)
            oplot,[tickV[isubbar*nBars+ibar],tickV[isubbar*nBars+ibar]],[bot,top],linestyle=2,color=col2
          endfor
        endif
      endfor
    endif else begin
      for idot=0,nDots -1 do begin
        oplot,tickV,plotVarMod(0:nBars-1,idot),psym=8,color=idot+2,symsize=2
        chlp=where(finite(plotVarMod(0:nBars-1,idot)) eq 0,nchlp)        
        if nchlp ge 1 then begin
          xyouts,tickV(chlp),float(replicate('1.',nchlp))*recognizeRangeY,$
            replicate('',nchlp),charsize=1.5,charthick=1.5,orientation=90
        endif
      endfor 
      if obsbar eq 0 or iUseObserveModel eq 1 then begin
        for ibar=0,nBars-1 do begin
          top=max(plotVarMod(ibar,*),/nan)
          bot=0.  ;min([!y.range(0),0.],/nan)
          if ifree eq '1101' or ifree eq '1011' or ifree eq '1110' or ifree eq '0111' then begin
            col2=15-isubbar
          endif else begin
            col2=0
          endelse
          oplot,[tickV[ibar],tickV[ibar]],[bot,top],linestyle=2,color=col2
        endfor
      endif
    endelse
    for i=0,nBars-1 do begin
      recognizePoint=fltarr(4,2)
      if nsubbars ge 2 then begin
        j=isubbar*nBars+i
      endif else begin
        j=i
      endelse
      ; here isubbar/idot
      for idot=0,ndots-1 do begin
        recognizePoint=fltarr(4,2)
        if ifree eq '1101' or ifree eq '1011' or ifree eq '1110' or ifree eq '0111' then begin
          recognizePoint[0,*]=[tickV(j)-recognizeRangeX, plotVarMod(i,isubbar,idot)-recognizeRangeY]
          recognizePoint[1,*]=[tickV(j)-recognizeRangeX, plotVarMod(i,isubbar,idot)+recognizeRangeY]
          recognizePoint[2,*]=[tickV(j)+recognizeRangeX, plotVarMod(i,isubbar,idot)+recognizeRangeY]
          recognizePoint[3,*]=[tickV(j)+recognizeRangeX, plotVarMod(i,isubbar,idot)-recognizeRangeY]
        endif else begin
          recognizePoint[0,*]=[tickV(j)-recognizeRangeX, plotVarMod(i,isubbar+idot)-recognizeRangeY]
          recognizePoint[1,*]=[tickV(j)-recognizeRangeX, plotVarMod(i,isubbar+idot)+recognizeRangeY]
          recognizePoint[2,*]=[tickV(j)+recognizeRangeX, plotVarMod(i,isubbar+idot)+recognizeRangeY]
          recognizePoint[3,*]=[tickV(j)+recognizeRangeX, plotVarMod(i,isubbar+idot)-recognizeRangeY]
        endelse
        recognizePoint=transpose(recognizePoint)
        ;        print,j,' ',recognizePoint
        normRecognizePoint=convert_coord(recognizePoint, /DATA, /TO_NORMAL)
        normRecognizePoint=transpose(normRecognizePoint)
        normRecognizePoint=normRecognizePoint[*, 0:1]
        recognizePointPtr=ptr_new(normRecognizePoint, /NO_COPY)
        recognizeHighLight[kg]=0b
        recognizeRegionEdges[kg]=recognizePointPtr
        ihlp=isubbar
        addstr=''
        if ifree eq '0100' then ihlp=idot
        if ifree eq '1001' then ihlp=isubbar
        if ifree eq '1001' and obsbar eq 0 then ihlp=idot
        if ifree eq '0101' then ihlp=idot
        if ifree eq '0011' or ifree eq '1010' then begin
          ihlp=idot
          addstr='_'+modelCodes[0]
        endif
        if ifree eq '1100' then ihlp=idot
        if ifree eq '0110' then ihlp=idot
        if ifree eq '1101' or ifree eq '1011' or ifree eq '1110' or ifree eq '0111' then begin
          recognizeValues[kg]=strtrim(plotVarMod[i,isubbar,idot], 2)
          if ifree eq '1101' or ifree eq '1011' then addstr='_'+parCodes(isubbar)
          if ifree eq '1110' or ifree eq '0111' then addstr='_'+scenarioCodes(isubbar)
          ihlp=idot
        endif else begin
          recognizeValues[kg]=strtrim(plotVarMod[i,isubbar+idot], 2)
        endelse
        recognizeNames[kg]=LongBarnames3(ihlp)+addstr
        kg++
      endfor
    endfor
  endfor
  ;  endif
  
  rInfo = obj_new("RecognizeInfo", recognizeNames, recognizeValues, recognizeHighLight, recognizeRegionEdges)
  plotInfo->setRecognizeInfo, rInfo
  
  noplot:
  
  ;  !y.range=0
  !y.charsize=0
END

PRO FM_PlotBarsLegend, plotter, request, result

  targetInfo=result->getGenericPlotInfo()
  allDataXY=targetInfo->getXYS()
  if string(allDataXY[0]) eq 'AllNaN' then goto,jumpend
  diagramCode=request->getDiagramCode()
  ;  plotter->plotBarsLegend, request, result
  if diagramCode eq 0 then begin
    legendGenericBuildDiag0,request,result,plotter
  endif else begin
    legendGenericBuild,request,result,plotter
  endelse
  legendInfo,request,result,plotter
  jumpend:
END
PRO FM_PlotDynamicEvaluation, plotter,request,result
  ;PRO FM_PlotBugle, plotter, request, result, allDataXY, allDataColor, allDataSymbol

  !y.range=0
  plotter->wsetMainDataDraw
  
  tpInfo=result->getGenericPlotInfo()
  
  allDataXY=tpInfo->getXYS()
  allDataSymbol=tpInfo->getSymbols()
  allDataColor=tpInfo->getColors()
  elabName=request->getelaborationName()
  elabcode=request->getElaborationCode()
  plotInfo=result->getPlotInfo()
  legNames=tpInfo->getLegendNames()
  nobs=request->getSingleObsNumber()
  npar=request->getParameterNumber()
  nmod=request->getModelNumber()
  obsNames=request->getSingleObsNames()
  
  nobs=n_elements(allDataXY(*,0))
  
  nMulti=nobs*npar*nmod   ;8
  
  recognizeHighLight=bytarr(nobs)   ; 8
  recognizeRegionEdges=ptrarr(nobs)
  recognizeNames=strarr(nobs)
  recognizeValues=strarr(nobs)
  recognizePoint=fltarr(4,2)
  
  npoints=n_elements(allDataXY(*,0))
  
  DEVICE,DECOMPOSE=0
  LOADCT,39
  mytek_color;, 0, 32
  
  if elabcode eq 71 then begin
    ntxt1='Delta_OBS (Day-Night)'
    titleY='Formule (Day-Night)'
  endif
  if elabcode eq 72 then begin
    ntxt1='Delta_OBS (Summer-Winter)'
    titleY='Formule (Summer-Winter)'
  endif
  if elabcode eq 73 then begin
    ntxt1='Delta_OBS (WeekDays-WeekEnd)'
    titleY='Formule (WeekDays-WeekEnd)'
  endif
  
  maxxAxis=max(allDataXY(*,0),/nan)*1.1
  maxxAxis=max([maxxAxis,0])
  if finite(maxxAxis) eq 0 then maxxAxis=100
  minxAxis=min(allDataXY(*,0),/nan)*1.1
  minxAxis=min([minxAxis,0])
  if finite(minxAxis) eq 0 then minxAxis=-100
  
  maxyAxis=max(allDataXY(*,1),/nan)*1.1
  maxyAxis=max([maxyAxis,0])
  if finite(maxyAxis) eq 0 then maxyAxis=100
  minyAxis=min(allDataXY(*,1),/nan)*1.1
  minyAxis=min([minyAxis,0])
  if finite(minyAxis) eq 0 then minyAxis=-100
  
  Xaxis=max([abs(minxAxis),abs(maxxAxis)])
  Yaxis=max([abs(minyAxis),abs(maxyAxis)])
  XYaxis=max([Xaxis,Yaxis])
  if XYaxis eq 0 then begin  ; Ex: Day-Night for PM10 which is daily values
    XYaxis=1.
  endif
  Xaxis=XYaxis & Yaxis=XYaxis
  
  recognizeRangeX=(Xaxis+Xaxis)*0.01
  recognizeRangeY=(Yaxis+Yaxis)*0.01
  
  plot, [-Xaxis,Xaxis], color=0,/nodata, xtitle=ntxt1,ytitle=titleY, title='DynamicEvaluation', charsize=1, background=255,$
    yrange=[-Yaxis,Yaxis],xrange=[-Xaxis,Xaxis],xstyle=1,ystyle=1, position=plotter->getPosition(), noerase=plotter->getOverplotKeyword(0)
  adummy=fltarr(10) & adummy(*)=1.  
  CheckCriteria, request, result, 'OU', criteria, adummy,alpha,criteriaOrig,LV  ;hourly/daily
   PolfX=fltarr(6)
   PolfY=fltarr(6)
;   if criteriaOrig gt 0. then begin
;     vY=2.*(criteriaOrig/100.)*sqrt(alpha*LV^2)
;     PolfX(0)=-Xaxis   & PolfY(0)=-Xaxis
;     PolfX(1)=-Xaxis   & PolfY(1)=-Xaxis+vY
;     PolfX(2)=Xaxis-vY & PolfY(2)=Xaxis
;     PolfX(3)=Xaxis    & PolfY(3)=Xaxis
;     PolfX(4)=Xaxis    & PolfY(4)=Xaxis-vY
;     PolfX(5)=-Xaxis+vY       & PolfY(5)=-Xaxis
;    polyfill,PolfX,PolfY,/data,color=8
;    oplot,[PolfX(1),PolfX(2)],[PolfY(1),PolfY(2)],linestyle=2,color=0,thick=2
;    oplot,[PolfX(5),PolfX(4)],[PolfY(5),PolfY(4)],linestyle=2,color=0,thick=2
;  endif
  
;  plot, [-Xaxis,Xaxis], color=0,/nodata, xtitle=ntxt1,ytitle=titleY, title='DynamicEvaluation', charsize=1, background=255,$
;    yrange=[-Yaxis,Yaxis],xrange=[-Xaxis,Xaxis],xstyle=1,ystyle=1, position=plotter->getPosition(), noerase=plotter->getOverplotKeyword(0)
  oplot,[-Xaxis,Xaxis],[0,0],color=0,linestyle=2
  oplot,[0,0],[-Yaxis,Yaxis],color=0,linestyle=2
  oplot,[-Xaxis,Xaxis],[-Yaxis,Yaxis],color=0,linestyle=3
  xyouts,-0.97*Xaxis,-0.1*Yaxis,'dMod<dObs',color=0,charsize=2
  xyouts,0.57*Xaxis,0.05*Yaxis,'dMod<dObs',color=0,charsize=2
  xyouts,-0.43*Xaxis,-0.95*Yaxis,'dMod>dObs',color=0,charsize=2
  xyouts,0.02*Xaxis,0.89*Yaxis,'dMod>dObs',color=0,charsize=2
  
  for iObs=0, npoints-1 do begin
    mypsym,allDataSymbol[iObs],1
    plots, allDataXY[iObs, 0], allDataXY[iObs, 1], psym=8, color=2+allDataColor[iObs], symsize=1.5
    recognizePoint[0,*]=[allDataXY[iObs, 0]-recognizeRangeX,allDataXY[iObs, 1] -recognizeRangeY]
    recognizePoint[1,*]=[allDataXY[iObs, 0]-recognizeRangeX,allDataXY[iObs, 1] +recognizeRangeY]
    recognizePoint[2,*]=[allDataXY[iObs, 0]+recognizeRangeX,allDataXY[iObs, 1] +recognizeRangeY]
    recognizePoint[3,*]=[allDataXY[iObs, 0]+recognizeRangeX,allDataXY[iObs, 1] -recognizeRangeY]
    recognizePoint1=transpose(recognizePoint)
    normRecognizePoint=convert_coord(recognizePoint1, /DATA, /TO_NORMAL)
    normRecognizePoint=transpose(normRecognizePoint)
    normRecognizePoint=normRecognizePoint[*, 0:1]
    recognizePointPtr=ptr_new(normRecognizePoint, /NO_COPY)
    recognizeRegionEdges[iobs]=recognizePointPtr
    recognizeNames[iobs]=legNames[iobs]
    recognizeValues[iobs]=strcompress(allDataXY(iObs, 0),/remove_all)+'/'+strcompress(allDataXY(iObs, 1),/remove_all)
  endfor
  
  rInfo = obj_new("RecognizeInfo", recognizeNames, recognizeValues, recognizeHighLight, recognizeRegionEdges)
  plotInfo->setRecognizeInfo, rInfo
  
END
PRO FM_PLOTDYNAMICEVALUATIONLEGEND, plotter, request, result
  targetInfo=result->getGenericPlotInfo()
  allDataXY=targetInfo->getXYS()
  if string(allDataXY[0]) eq 'AllNaN' then goto,jumpend
  legendGenericBuild,request,result,plotter
  legendInfo,request,result,plotter
  jumpend:
END
PRO FM_PlotCategory, plotter, request, result

  plotter->wsetMainDataDraw
  plotInfo=result->getPlotInfo()
  targetInfo=result->getGenericPlotInfo()
  DEVICE,decomposed=0
  LOADCT,39
  mytek_color;, 0, 32
  !p.color=0
  !y.range=0
;  resPoscript=plotter->currentDeviceIsPostscript()
  targetInfo=result->getGenericPlotInfo()
  allDataXY=targetInfo->getXYS()
  if string(allDataXY[0]) eq 'AllNaN' then begin
    plot,indgen(10),/nodata ,color=255,background=255
    xyouts,1,5,'No valid stations or groups selected',charsize=2,charthick=2,/data,color=0
    goto,jumpend
  endif
  tpInfo=result->getGenericPlotInfo()
  
  DataXYAll=tpInfo->getXYS()
  DataSymbolAll=tpInfo->getSymbols()
  DataColorAll=tpInfo->getColors()
  legNamesAll=tpInfo->getLegendNames()
  elabcode=request->getElaborationCode()
  
  parCodes=request->getParameterCodes()
  npar=request->getParameterNumber()
  nmod=request->getModelNumber()
  
  ccGroup = where(DataSymbolAll eq 13 and finite(DataXYAll(*,0)) eq 1, countGroup)
  ccSing = where(DataSymbolAll ne 13 and finite(DataXYAll(*,0)) eq 1 , countSing)
  
  ; KeesC put the groups first
  if countGroup ge 1 and countSing ge 1 then begin
    hlp0=reform(DataXYAll(*,0))
    hlp1=[hlp0([ccGroup]),hlp0([ccSing])]
    DataXYAll(*,0)=hlp1
    DataSymbolAll=[DataSymbolAll(ccGroup),DataSymbolAll(ccSing)]
    legNamesAll=[legNamesAll(ccGroup),legNamesAll(ccSing)]
    ccGroup=ccGroup-min(ccGroup)
    ccSing=ccSing+max(ccGroup)
  endif
  
  legNamesReco=legNamesAll
  if countSing gt 0 then begin
    for i=countGroup,countGroup+countSing-1 do begin
      res=strsplit(legNamesAll(i),'&',/extract)
      legNamesAll(i)=res(0)
      legNamesReco(i)=res(1)
    endfor
  endif
  
  dataNb=n_elements(DataXYAll(*,0))
  
  if (dataNb - countGroup) gt 0 then begin
    for ig=0,countGroup-1 do begin
      cg=where(legNamesAll(ig) eq legNamesAll(countGroup:dataNb-1),countCG)
      if countCG gt 0 then legNamesAll(ig)=legNamesAll(ig)+'G'
    endfor
  endif
  
  regNames = legNamesAll[UNIQ(legNamesAll, SORT(legNamesAll))]
  
  legnames=strarr(n_elements(regNames))
  allDataXY=fltarr(dataNb,2)
  allDatacolor=intarr(dataNb)
  allDataSymbol=intarr(dataNb)
  
  if countGroup gt 0 then begin
    legnames(0:countGroup-1)=legNamesAll(0:countGroup-1)
    allDataXY(ccGroup,0)=DataXYAll(ccGroup,0)
    allDataXY(ccGroup,1)=ccGroup+0.5
    allDataSymbol(ccGroup)=13
    allDatacolor(ccGroup)=2
  endif
  ccSingle = where(DataSymbolAll ne 13, countSingle)
  
  if countSingle gt 0 then begin
    legNamesAllS=reform(legNamesAll(ccSingle))
    regNamesSingle = legNamesAllS[UNIQ(legNamesAllS, SORT(legNamesAllS))]
    legnames(countGroup:n_elements(regNames)-1)=regNamesSingle
    for i=0,dataNb-countGroup-1 do begin
      creg=where(legNamesAllS(i) eq regNamesSingle)
      allDataXY(i+countGroup,0)=DataXYAll(i+countGroup,0)
      allDataXY(i+countGroup,1)=countGroup+creg+0.5
      allDataSymbol(i+countGroup)=9
      allDatacolor(i+countGroup)=4
    endfor
  endif
  
  maxx=max(allDataXY(*,0),/nan)
  minn=min(allDataXY(*,0),/nan)
  recognizeRangeX=(Maxx-Minn)*0.1
  if elabcode eq 10 or elabcode eq 75 then begin
    title='Bias Indicator'
    ;    CheckCriteria, request, result, 'OU', criteria, obsTemp, 0,alpha,criteriaOrig,LV,nobsAv
    criteria=1
    !x.range=[-3,3]
    polyMin=-Criteria
    polyMax=Criteria
    ;    recognizeRangeX=1
    xtitle='Bias/2U'
  endif
  if elabcode eq 18 or elabcode eq 77 then begin
    title='Standard Deviation Indicator'
    criteria=1.
    !x.range=[-3,3]
    polyMin=-Criteria
    polyMax=Criteria
    ;    recognizeRangeX=1
    xtitle='(SigM-SigO)/2U'
  endif
  if elabcode eq 11 or elabCode eq 76 then begin
    title='Correlation Indicator'
    criteria=1
    !x.range=[0,3]
    polyMin=0
    polyMax=1
    ;    recognizeRangeX=0.05
    xtitle='(1-R)/(2*U/sigO)^2'
  endif
  recognizeRangeY=n_elements(legnames)*0.01
  
  
  ahlp=fltarr(n_elements(legnames)) & ahlp(*)=0.
  ccc=intarr(n_elements(legnames)) & ccc(*)=1
  
  ;if resPoscript eq 1 then !position=plotter->getPosition()
  
  if n_elements(legNames) eq 1 then begin
    legNamesPrint=[' ',' ',legnames(0),' ',' ',' ']
    allDataXY(*,1)=allDataXY(*,1)-0.1
  endif else begin
    legNamesPrint=legnames
  endelse
  
  bar_plot,ahlp,barnames=legNamesPrint,title=title,/rotate,colors=ccc,background=255,xtitle=xtitle
  if criteria gt 0 then begin
    polyfill,[polyMin,polyMin,polyMax,polyMax],[0,n_elements(legnames),n_elements(legnames),0],color=175
    plots,[polyMin,polyMin],[0,n_elements(legnames)],color=0,/data,linestyle=2,thick=2
    plots,[polyMax,polyMax],[0,n_elements(legnames)],color=0,/data,linestyle=2,thick=2
  endif
  
  recognizeHighLight=bytarr(dataNb)
  recognizeRegionEdges=ptrarr(dataNb) ; coords (normalized standard)
  recognizeNames=strarr(dataNb)
  recognizeValues=strarr(dataNb)
  recognizePoint=fltarr(4,2)
  
  for ii=0,dataNb-1 do begin
    mypsym,allDataSymbol(ii),1
    plots,alldataXY(ii,0),allDataXY(ii,1),psym=8,color=2,symsize=1
    recognizePoint[0,*]=[alldataXY(ii,0)-recognizeRangeX, allDataXY(ii,1)-recognizeRangeY]
    recognizePoint[1,*]=[alldataXY(ii,0)-recognizeRangeX, allDataXY(ii,1)+recognizeRangeY]
    recognizePoint[2,*]=[alldataXY(ii,0)+recognizeRangeX, allDataXY(ii,1)+recognizeRangeY]
    recognizePoint[3,*]=[alldataXY(ii,0)+recognizeRangeX, allDataXY(ii,1)-recognizeRangeY]
    recognizePoint1=transpose(recognizePoint)
    normRecognizePoint=convert_coord(recognizePoint1, /DATA, /TO_NORMAL)
    normRecognizePoint=transpose(normRecognizePoint)
    normRecognizePoint=normRecognizePoint[*, 0:1]
    recognizePointPtr=ptr_new(normRecognizePoint, /NO_COPY)
    recognizeHighLight[ii]=0b
    recognizeRegionEdges[ii]=recognizePointPtr
    recognizeNames[ii]=legNamesReco[ii]
    recognizeValues[ii]=strtrim(alldataXY(ii,0), 2)
  endfor
  
  cc=where(abs(allDataXY(*,0)) lt Criteria,countGood)
  cc=where(finite(allDataXY(*,0)) eq 1,countAll)
  xyouts,110,n_elements(legnames)*.9,strtrim(fix(100.*float(countgood)/countall),2)+'%',/data,charsize=2,color=0
  
  rInfo = obj_new("RecognizeInfo", recognizeNames, recognizeValues, recognizeHighLight, recognizeRegionEdges)
  plotInfo->setRecognizeInfo, rInfo
  ;; End of JRC original plot target routine
  
  jumpend:
  
  !x.range=0
END
PRO FM_PlotCategoryLegend, plotter, request, result

  plotter->wsetInfoDataDraw
  white=obj_new('Color', 200, 200, 200)
  black=obj_new('Color', 0, 0, 0)
  whiteL=white->AsLongTrueColor()
  blackL=black->AsLongTrueColor()
  obj_destroy, white
  obj_destroy, black
  DEVICE, DECOMPOSED=1
  plotter->erase, whiteL
  
END
;  ****** comment begin EG  6dec2010
;PRO FM_PlotTimeSeries, plotter, request, result
;
;  plotter->plotTimeSeries, request, result
;
;END
;
PRO FM_PlotTimeSeries, plotter, request, result, allDataXY, allDataColor, allDataSymbol
  !y.range=0
  plotter->wsetMainDataDraw
  
  mus=request->getParameterMeasureUnits()
  tpInfo=result->getGenericPlotInfo()
  startIndex=request->getStartIndex()
  endIndex=request->getEndIndex()
  
  allDataXY=tpInfo->getXYS()
  allDataSymbol=tpInfo->getSymbols()
  allDataColor=tpInfo->getColors()
  
  DEVICE,DECOMPOSE=0
  LOADCT,39
  !p.charsize=1.5
  ; use "tek" color table...
  tek_color;, 0, 32
  
  yrange=[min(alldataXY,/nan),max(alldataXY,/nan)]
  
  plot,alldataXY(0,*),color=0,background=255,xrange=[0,n_elements(alldataXY(0,*))],$
    xstyle=1,yrange=yrange, position=plotter->getPosition(),xtitle='Hours',$
    ytitle='Units '+mus
  for i=0,n_elements(allDataXY(*,0))-2 do oplot,alldataXY(i+1,*),color=alldataColor(i+1)+2
  
END

PRO FM_PlotTimeSeriesLegend, plotter, request, result

  targetInfo=result->getGenericPlotInfo()
  allDataXY=targetInfo->getXYS()
  if string(allDataXY[0]) eq 'AllNaN' then goto,jumpend
  plotter->wsetInfoDataDraw
  white=obj_new('Color', 200, 200, 200)
  black=obj_new('Color', 0, 0, 0)
  whiteL=white->AsLongTrueColor()
  blackL=black->AsLongTrueColor()
  obj_destroy, white
  obj_destroy, black
  DEVICE, DECOMPOSED=1
  plotter->erase, whiteL
  ;erase, whiteL
  
  DEVICE,DECOMPOSE=0
  LOADCT,39
  ; use "tek" color table...
  tek_color;, 0, 32
  
  legoWidth=.012
  legoHeight=.05
  startX=.01
  maxWidth=0
  
  targetInfo=Result->getGenericPlotInfo()
  colors=targetInfo->getColors()
  symbols=targetInfo->getSymbols()
  legColors=targetInfo->getLegendColors()
  legNames=targetInfo->getLegendNames()
  legSyms=targetInfo->getLegendSymbols()
  legoSequenceNo=n_elements(legNames)
  
  symbolSequenceNo=n_elements(legNames)
  legColors(1:symbolSequenceNo-1)=legColors(1:symbolSequenceNo-1)+2
  
  for i=0, symbolSequenceNo-1 do begin
    jheight = i MOD 9
    startx = .10*fix(i/9)
    startY=1.-((jheight+1)*legoHeight*2)
    thisStartX=startX+maxWidth+legoWidth+.02
    lego=[[thisStartX,startY], [thisStartX,startY+legoHeight], [thisStartX+legoWidth,startY+legoHeight], [thisStartX+legoWidth,startY], [thisStartX,startY]]
    mypsym,legsyms[i],1
    legendCoordsPlots=[thisStartX+(legoWidth/2), startY+.002]
    legendCoordsXYouts=[thisStartX+legoWidth+.01, startY+.002]
    legendCoordsPlots=plotter->legendNormalize(legendCoordsPlots)
    legendCoordsXYouts=plotter->legendNormalize(legendCoordsXYouts)
    plots, legendCoordsPlots[0], legendCoordsPlots[1], psym=8, color=legColors[i], /NORM,symsize=1.
    xyouts, legendCoordsXYouts[0], legendCoordsXYouts[1], strmid(legNames[i],0,7), COLOR=0, /NORM, charsize=.8, charthick=.8,  WIDTH=textWidth
  endfor
  
  legendInfo,request,result,plotter
  
  jumpend:
END

PRO FM_PlotScatter, plotter, request, result

  !y.range=0
  plotter->wsetMainDataDraw
  plotInfo=result->getPlotInfo()
  DEVICE,decomposed=0
  LOADCT,39
  ; use "tek" color table...
  mytek_color;, 0, 32
  targetInfo=result->getGenericPlotInfo()
  legNames=targetInfo->getLegendNames()
  allDataXY=targetInfo->getXYS()
  if string(allDataXY[0]) eq 'AllNaN' then begin
    plot,indgen(10),/nodata ,color=255,background=255
    xyouts,1,5,'No valid stations or groups selected',charsize=2,charthick=2,/data,color=0
    goto,jumpend
  endif
  allDataSymbol=targetInfo->getSymbols()
  allDataColor=targetInfo->getColors()
  modelCodes=request->getModelCodes()
  nobs=request->getSingleObsNumber()

  groupTitles=request->getGroupTitles()
  npar=request->getParameterNumber()
  nmod=request->getModelNumber()
  nsce=request->getScenarioNumber()
  elabName=request->getelaborationName()
  mus=request->getParameterMeasureUnits()
  parCodes=request->getParameterCodes()
  elabcode=request->getElaborationCode()
  obsNames=request->getSingleObsNames()
  psFact=plotter->getPSCharSizeFactor()
  dims=size(allDataXY,/dimensions)
  nobs=dims(0)/npar/nmod/nsce
  ; KeesC 31AUG2012
  totalStationNb=nobs
  nmulti=npar*nsce*nmod*nobs
  if elabCode ne 50 then begin
    maxAxis=max([0,max(allDataXY,/nan)])*1.2
    minAxis=min([0,min(allDataXY,/nan)])
  endif
  if elabCode eq 50 then begin
    maxAxis=max([0,max(allDataXY(nmulti/2:nmulti-1,*),/nan)])*1.2
    minAxis=min([0,min(allDataXY(nmulti/2:nmulti-1,*),/nan)])
  endif
  if maxAxis eq 0 and minAxis eq 0 then maxAxis=1.
  recognizeRange=(maxAxis-minAxis)*0.01
  cc=where(finite(allDataXy(*,0)) eq 1 and finite(allDataXy(*,1)) eq 1,validStationNb)
  validStationNb=validStationNb/npar/nmod/nsce
  isGroupSelection=request->isGroupObsPresent()
  ngroups = 0
  if isGroupSelection then begin
    groupTitles=request->getGroupTitles()
    ngroups=n_elements(groupTitles)
  endif
  
   if n_elements(parCodes) eq 1 and isGroupSelection eq 0 then begin
  adummy=fltarr(10) & adummy(*)=1.
  ;if elabCode ne 21 then begin ; hourly values
     CheckCriteria, request, result, 'OU', criteria, adummy,alpha,criteriaOrig,LV  ;hourly/daily
     if Criteria(0) ne -1 then begin
        UrLV=criteriaOrig(0)
        alpha=criteriaOrig(1)
        Neff=criteriaOrig(2)
        Nnp=criteriaOrig(3)
        LV=criteriaOrig(4)
     endif 
   ;endif
;   if elabCode eq 21 then begin ; yearly values
;     CheckCriteria, request, result, 'OU', criteria, adummy,alpha,criteriaOrig,LV  ;hourly/daily
;     if Criteria(0) ne -1 then begin
;        UrLV=criteriaOrig(0)
;        alpha=criteriaOrig(1)
;        Neff=criteriaOrig(2)
;        Nnp=criteriaOrig(3)
;        LV=criteriaOrig(4)
;     endif 
;   endif
   
   endif else begin
   criteria=0
   endelse
  ;if elabCode eq 21 then CheckCriteria, request, result, 'OU', criteria, adummy, 1,alpha,criteriaOrig,LV,nobsAv  ;yearly
  
  musstr=''
  for i=0,n_elements(mus)-1 do musstr=musstr+'/'+mus(i)
  if elabCode eq 6 then begin
    xtitle='OBS '+musstr
    ytitle='MOD '+musstr
  endif
  if elabCode eq 50 then begin
    xtitle=modelCodes(0)+'/'+mus[0]
    ytitle=modelCodes(1)+'/'+'EmisUnits'
  endif
  if elabCode eq 56 then begin
    xtitle='OBS '+musstr
    ytitle='MOD '+musstr
  endif
  if elabCode eq 57 then begin
    xtitle=parCodes(0)+'/'+mus[0]
    ytitle=parCodes(1)+'/'+mus[1]
  endif
  plot, indgen(max([fix(maxaxis),fix(abs(minAxis))])),color=0, /nodata, xtitle=xtitle,ytitle=ytitle, title='Scatter PLOT', $
    charsize=1.5*psfact, background=255,xrange=[minAxis,maxAxis],$
    yrange=[minAxis,maxAxis],xstyle=1,ystyle=1, position=plotter->getPosition(), noerase=plotter->getOverplotKeyword(0)
  if elabCode ne 50 and elabcode ne 57 and criteria[0] gt 0 then begin
    critPolyfill  =fltarr(1000,2)
    critPolyfill05=fltarr(1000,2)
    crit_ii=fltarr(1000)
    for ii=0,999 do begin
       iix=float(ii)*float(maxAxis)/1000.
       crit_ii(ii)=UrLV/100.*sqrt( (1.-alpha)*float(iix)^2/float(Neff) +alpha*LV^2/float(Nnp))
       critPolyfill(ii,1)=min([iix+2.*crit_ii(ii),MaxAxis])
       critPolyfill(ii,0)=max([iix-2.*crit_ii(ii),MinAxis])
       critPolyfill05(ii,1)=min([iix+crit_ii(ii),MaxAxis])
       critPolyfill05(ii,0)=max([iix-crit_ii(ii),MinAxis])
    endfor
    xx=findgen(1000)*maxAxis/1000.
    xx(fix(maxAxis+1))=min([xx(maxAxis+1),maxAxis])
    polyfill,[xx,xx(999),reverse(xx)],$
      [critPolyfill(*,1),critPolyfill(999,0),reverse(critPolyfill(*,0))],$
      /data,color=8
    oplot,xx,critPolyfill05(*,1),linestyle=2,color=0,thick=2
    oplot,xx,critPolyfill(*,1),color=0,thick=2
    oplot,xx,critPolyfill05(*,0),linestyle=2,color=0,thick=2
    oplot,xx,critPolyfill(*,0),color=0,thick=2
  endif
  plots,[minAxis,maxAxis],[minAxis,maxAxis],color=0,/data
  xyouts,minAxis+recognizeRange*4,maxAxis-recognizeRange*4,'Valid/selected stations/groups: '+strtrim(validStationNb,2)+'/'+strtrim(totalStationNb,2),/data,color=0
  
;  if criteria gt 0 and nmod eq 1 and isGroupSelection eq 0 then begin
;  
;    coords1=[0.15, 0.80]
;    coords2=[0.15, 0.90]
;    coords3=[0.50, 0.90]
;    coords4=[0.50, 0.80]
;    
;    polyfill,[coords1[0],coords2[0],coords3[0],coords4[0]],[coords1[1],coords2[1],coords3[1],coords4[1]],color=15,/normal
;    coords=[-recognizeRange+recognizeRange*0.10,recognizeRange-recognizeRange*0.15]
;    psFact=plotter->getPSCharSizeFactor()
;    xyouts,coords[0],coords[1],'Stations within Crit (T=1):',color=3,/data,charthick=3,charsize=1.3*psFact
;  endif
  
  recognizeRange=(maxAxis-minAxis)*0.01
  
  size_alldataXY=size(allDataXY)
  if size_alldataXY(0) eq 2 then begin    ; scatter mean
  
    if elabcode eq 6 or elabCode eq 50 or elabcode eq 56 or elabcode eq 57 then begin
      if elabCode eq 50 then begin
        allDataXY=allDataXY[nmulti/2:nmulti-1,*] 
        nmulti=nmulti/2
      endif  
      ;      nObs=n_elements(allDataColor)
      recognizeHighLight=bytarr(nmulti)
      recognizeRegionEdges=ptrarr(nmulti) ; coords (normalized standard)
      recognizeNames=strarr(nmulti)
      recognizeValues=strarr(nmulti)
      legNames=targetInfo->getLegendNames()
      
      for iobs=0,nmulti-1 do begin
        ipar=iobs-(iobs/npar)*npar
        istat=(iobs/npar)
        mypsym,allDataSymbol[iobs],1
        plots, allDataXY[iObs, 0], allDataXY[iObs, 1], psym=8, color=2+allDataColor[iobs], symsize=1.5
        recognizePoint=fltarr(4,2)
        recognizePoint[0,*]=[allDataXY[iObs, 0]-recognizeRange, allDataXY[iObs, 1]-recognizeRange]
        recognizePoint[1,*]=[allDataXY[iObs, 0]-recognizeRange, allDataXY[iObs, 1]+recognizeRange]
        recognizePoint[2,*]=[allDataXY[iObs, 0]+recognizeRange, allDataXY[iObs, 1]+recognizeRange]
        recognizePoint[3,*]=[allDataXY[iObs, 0]+recognizeRange, allDataXY[iObs, 1]-recognizeRange]
        recognizePoint=transpose(recognizePoint)
        normRecognizePoint=convert_coord(recognizePoint, /DATA, /TO_NORMAL)
        normRecognizePoint=transpose(normRecognizePoint)
        normRecognizePoint=normRecognizePoint[*, 0:1]
        recognizePointPtr=ptr_new(normRecognizePoint, /NO_COPY)
        recognizeHighLight[iObs]=0b
        recognizeRegionEdges[iObs]=recognizePointPtr
        if elabCode eq 57 then begin
          recognizeNames[iObs]='Hour = '+strcompress(iobs,/remove_all)
        endif
        if elabCode eq 50 then begin
          recognizeNames[iObs]='Hour = '+strcompress(iobs,/remove_all)
        endif
        if elabCode eq 56 then begin
          recognizeNames[iObs]=obsnames(istat)
        endif
        if elabCode ne 56 and elabCode ne 57 then begin
          recognizeNames[iObs]=legNames[iObs]
        endif
        recognizeValues[iObs]=strtrim(allDataXY[iObs, 0], 2)+'/'+strtrim(allDataXY[iObs, 1], 2)
      endfor
    endif
    
  endif else begin  ; scatter all values plotted
  
    nObs_param=size_alldataXY(1)
    nvalues=size_alldataXY(2)
    
    for iobs=0, nObs_param-1 do begin
      for ival=0,nvalues-1 do begin
        mypsym,allDataSymbol[iObs],1
        plots, allDataXY[iObs,ival,0], allDataXY[iObs,ival,1], psym=8, color=2+allDataColor[iObs], symsize=1
      endfor
      obshlp=reform(allDataXY(iObs,*,0))
      modhlp=reform(allDataXY(iObs,*,1))
      ck1 = where(finite(obshlp) eq 1 and finite(modhlp) eq 1 and obshlp ne 0., countCK1 )
      if countCK1 gt 0 then begin
        varObsR=reform(obshlp(ck1))
        varModR=reform(modhlp(ck1))
        R2=correlate(varObsR,varModR)^2
      endif else begin
        R2=!values.f_nan
      endelse
      xyouts,0.80,0.30-iobs*0.05,'R2 = '+strtrim(R2,2),/normal,color=2+allDataColor(iobs),charthick=2,charsize=1.3
    endfor
  endelse
  ;  endif
  if elabcode eq 6 or elabCode eq 50 or elabCode eq 56 or elabCode eq 57 then begin
    rInfo = obj_new("RecognizeInfo", recognizeNames, recognizeValues, recognizeHighLight, recognizeRegionEdges)
    plotInfo->setRecognizeInfo, rInfo
  endif
  
  jumpend:
  
END

PRO FM_PlotScatterLegend, plotter, request, result
  targetInfo=result->getGenericPlotInfo()
  allDataXY=targetInfo->getXYS()
  if string(allDataXY[0]) eq 'AllNaN' then goto,jumpend
  legendGenericBuild,request,result,plotter
  legendInfo,request,result,plotter
  jumpend:
END

PRO FM_PlotGoogleEarth, plotter, request, result

  print,'Start FM_PlotGoogleEarth'
  
  plotter->wsetMainDataDraw
  tpInfo=result->getGenericPlotInfo()
  
  allDataXY=tpInfo->getXYS()           ; eliminatie nog niet gedaan
  if string(allDataXY[0]) eq 'AllNaN' then goto, EndGE   ; nstat+1,npar+1,nmod+1,5
  
  allDataXYdim=size(allDataXY,/dimensions)
  nobs=allDataXYdim(0)-1 & npar=allDataXYdim(1)-1 & nmod=allDataXYdim(2)-1
  ;  nobs = number of non-Validated stations
  statcheck=indgen(nobs)
  for istat=0,nobs-1 do begin
    for ipar=0,npar-1 do begin
      for imod=0,nmod-1 do begin
        if finite(allDataXY(istat,ipar,imod,0)) eq 0 or finite(allDataXY(istat,ipar,imod,1)) eq 0 then $
          statcheck(istat)=-1
      endfor
    endfor
  endfor
  statValidate=where(statcheck ne -1,nstat)
  statValidatePlus=[statValidate,nobs]
  if nstat eq 0 then begin
    rsult=dialog_message(['GoogleEarth not available for current choice',' ',$
      'No Validated stations selected'])
    goto,endGE
  endif
  allDataXY=reform(allDataXY(statValidatePlus,*,*,*))
  allDataSymbol=tpInfo->getSymbols()   ; eliminatie ok;  nstat+1,npar+1,nmod+1,5
  allDataSymbol=reform(allDataSymbol(statValidatePlus,*,*,*))
  ;  allDataColor=tpInfo->getColors()     ; 60 = nstat*npar*nmod
  obsNames=request->getSingleObsNames()  ; elim ok; nstat
  obsNames=obsNames(statValidate)
  modCodes=request->getModelCodes()     ; nmod
  ;  nobs=request->getSingleObsNumber()    ;nstat
  parCodes=request->getParameterCodes()  ;npar
  elabName=request->getelaborationName()  ; 1
  npar=request->getParameterNumber()  ;npar
  nmod=request->getModelNumber()  ;nmod
  statType=request->getGroupByStatInfo() ;HourType
  categoryInfo=request->getSingleObsCatInfos()   ; 4*nstat
  categoryInfo=categoryInfo(*,statValidate)
  mus=request->getParameterMeasureUnits()   ; units of parameters
  mus='   ['+mus+']'
  ; OBS - PAR - MOD
  elabcode=request->getElaborationCode() ;ElaborationCode 58 or 59, ...
  obsValues=reform(allDataXY(*,*,*,0))   ;722    1->7
  modValues=reform(allDataXY(*,*,*,1))
  if elabcode eq 59 then begin
    obsValues=round(obsValues)
    modValues=round(modValues)
  endif
  if elabCode eq 61 or elabCode eq 69 then begin
    modValues=round(modValues)
  endif
  
  obsLongitudes=reform(allDataXY(*,*,*,2))
  obsLatitudes=reform(allDataXY(*,*,*,3))
  obsAltitudes=reform(allDataXY(*,*,*,4))
  obsshortnames=reform(allDataSymbol(*,*,*,0))  ;722   0->6
  obsCodes=reform(allDataSymbol(*,*,*,1))
  obsRegions=reform(categoryInfo(0,*))
  obsGMT=reform(allDataSymbol(*,*,*,3))
  stationType=reform(categoryInfo(1,*))
  Layer=reform(categoryInfo(2,*))
  Source=reform(categoryInfo(3,*))
  ;  nstat=n_elements(uniq(obsNames,sort(obsNames)))
  obsRegions2=obsRegions(uniq(obsRegions,sort(obsRegions)))
  nObsRegions=n_elements(obsRegions2)
  
  ;  mParameter=parCodes[0] & mScalename='LOCAL' & mStatName='IOA' & timeAvgName='N/A'
  ;  gcValues=request->getGoalsCriteriaValues(parameter=mParameter, scalename=mScalename, statname=mStatName, timeAvgName=timeAvgName, NOVALUES=NOVALUES)
  
  iObsRun=1   ; Obs independent of Run
  if elabCode eq 63 or elabCode eq 64 or elabCode eq 67 or elabCode eq 68 or elabCode eq 70 then iObsRun=0
  ; Not 60,61,69
  ; Mean 58
  ; unitVar is indicator unit
  limValue=9999
  unitVar=strarr(npar)
  if elabCode eq 58 then begin
    unitVar=mus  ;'   [ug/m3]'
    ExtraValues=request->getExtraValues()
    limValue=ExtraValues[0]
  endif
  ; ExcDays 59
  if elabCode eq 59 then begin
    unitVar(*)='  [Days]'
    if statType eq 0 then unitVar(*)='  [Hrs]'
    ExtraValues=request->getExtraValues()
    refValue=ExtraValues[0] & limValue=ExtraValues[1]
  endif
  ; Bias 60
  if elabCode eq 60 then begin
    unitVar=mus  ;'   [ug/m3]'
  endif
  ; MFBias 61
  if elabCode eq 61 then begin
    unitVar(*)='   [%]'
  endif
  ; Stddev 62
  if elabCode eq 62 then begin
    unitVar=mus  ;'   [ug/m3]'
  endif
  ; CorrCoeff 63
  if elabCode eq 63 then begin
    unitVar(*)=' '
  endif
  ; RMSE 64  -  -
  if elabCode eq 64 then begin
    unitVar=mus  ;'   [ug/m3]'
  endif
  ; AOT 65
  if elabCode eq 65 then begin
    unitVar='   [mg/m3*hrs]'
    ExtraValues=request->getExtraValues()
    refValue=ExtraValues[0] & limValue=ExtraValues[1]
  endif
  ;  SOMO 66
  if elabCode eq 66 then begin
    unitVar='   [mg/m3*days]'
    ExtraValues=request->getExtraValues()
    refValue=ExtraValues[0] & limValue=ExtraValues[1]
  endif
  ; RDEx 67  -  -
  if elabCode eq 67 then begin
    unitVar(*)='   [%]'
    ExtraValues=request->getExtraValues()
    refValue=ExtraValues[0] & limValue=ExtraValues[1]
  endif
  ; SigM/sigO 68
  if elabCode eq 68 then begin
    unitVar(*)=' '
    refValue=1.
  endif
  if elabCode eq 69 then begin
    unitVar(*)='   [%] '
    refValue=0
  endif
  ; Target
  if elabCode eq 70 then begin
    unitVar(*)='   '
  endif
  unitVarObs=unitVar
  if elabCode eq 61 or elabCode eq 69 then unitVarObs=mus  ; '  [ug/m3]'
  
  if npar eq 1 then begin
    if finite(min(obsValues(0:nstat-1,0,0),/nan)) eq 0 or finite(max(obsValues(0:nstat-1,0,0),/nan)) eq 0 or $
      finite(min(modValues(0:nstat-1,0,0),/nan)) eq 0 or finite(max(modValues(0:nstat-1,0,0),/nan)) eq 0 then begin
      res=dialog_message(['No validated stations - all NaN',' '],/information)
      goto,endGE
    endif
    if total(elabCode eq [58,59,62,65,66]) eq 1 then begin
      minValue=min(obsValues(0:nstat-1,0,0),/nan)
      maxValue=max(obsValues(0:nstat-1,0,0),/nan)
    endif
    if total(elabCode eq [60,61,63,64,67,68,69,70]) eq 1 then begin
      minValue=min(modValues(0:nstat-1,0,0),/nan)
      maxValue=max(modValues(0:nstat-1,0,0),/nan)
    endif
    pincolors=['ltblu','blue','grn','ylw','pink','red']
    npincolors=n_elements(pincolors)
    if minValue eq maxValue then begin
      npincolors=1
      pincolors=['red']
    endif
    if limValue gt maxValue then begin
      ick=0
      valrange=1.*(maxValue-minValue)/npincolors
    endif
    if limValue gt minValue and limValue le maxValue then begin
      ick=1
      maxValue=limValue
      valrange=1.*(maxValue-minValue)/npincolors
    endif
    if limValue le minValue then begin
      ick=2
      valrange=-999
    endif
  endif
  
  startIndex=request->getStartIndex()
  endIndex=request->getEndIndex()
  
  ;  DEVICE,DECOMPOSE=0
  LOADCT,39
  tek_color;, 0, 32
  
  fname='google.kml'
  request->openDataDumpFile, fname
  fsm=obj_new('FMFileSystemManager')
  PATHDUMP=fsm->getDumpDir()
  PATHDUMP=PATHDUMP+'\'
  
  Doc_name=strtrim(fix(nObsRegions),2)+' REGIONS'
  
  ; DOCUMENT
  txthlp=string( '<?xml version="1.0" encoding="UTF-8"?>', $
    ;  '<kml xmlns="http://earth.google.com/kml/2.2">', $
    '<kml xmlns="http://www.opengis.net/kml/2.2">', $
    '<Document>', $
    '<name>', strupcase(Doc_name), '</name>', $
    '<open>1</open>')
  request->writeDataDumpFileRecord, txthlp
  ;.................................................................
  txthlp=string('<Style id="black_pin">',$
    '<IconStyle>',$
    '<Icon>',$
    '<href>http://labs.google.com/ridefinder/images/mm_20_black.png</href>',$
    ;    '<href>http://maps.google.com/mapfiles/kml/pushpin/wht-pushpin.png</href>',$
    '</Icon>',$
    '</IconStyle>',$
    '</Style>')
  request->writeDataDumpFileRecord, txthlp
  txthlp=string('<Style id="red_pin">',$
    '<IconStyle>',$
    '<Icon>',$
    '<scale>2.0</scale>',$
    '<href>http://maps.google.com/mapfiles/kml/pushpin/red-pushpin.png</href>',$
    '</Icon>',$
    '</IconStyle>',$
    '</Style>')
  request->writeDataDumpFileRecord, txthlp
  txthlp=string('<Style id="pink_pin">',$
    '<IconStyle>',$
    '<Icon>',$
    '<href>http://maps.google.com/mapfiles/kml/pushpin/pink-pushpin.png</href>',$
    '</Icon>',$
    '</IconStyle>',$
    '</Style>')
  request->writeDataDumpFileRecord, txthlp
  txthlp=string('<Style id="ylw_pin">',$
    '<IconStyle>',$
    '<Icon>',$
    '<href>http://maps.google.com/mapfiles/kml/pushpin/ylw-pushpin.png</href>',$
    '</Icon>',$
    '</IconStyle>',$
    '</Style>')
  request->writeDataDumpFileRecord, txthlp
  txthlp=string('<Style id="grn_pin">',$
    '<IconStyle>',$
    '<Icon>',$
    '<href>http://maps.google.com/mapfiles/kml/pushpin/grn-pushpin.png</href>',$
    '</Icon>',$
    '</IconStyle>',$
    '</Style>')
  request->writeDataDumpFileRecord, txthlp
  txthlp=string('<Style id="ltblu_pin">',$
    '<IconStyle>',$
    '<Icon>',$
    '<href>http://maps.google.com/mapfiles/kml/pushpin/ltblu-pushpin.png</href>',$
    '</Icon>',$
    '</IconStyle>',$
    '</Style>')
  request->writeDataDumpFileRecord, txthlp
  txthlp=string('<Style id="blue_pin">',$
    '<IconStyle>',$
    '<Icon>',$
    '<href>http://maps.google.com/mapfiles/kml/pushpin/blue-pushpin.png</href>',$
    '</Icon>',$
    '</IconStyle>',$
    '</Style>')
  request->writeDataDumpFileRecord, txthlp
  txthlp=string('<StyleMap id="msn_placemark_circle">',$
    '<Pair>',$
    '<key>normal</key>',$
    '<styleUrl>#sn_placemark_circle</styleUrl>',$
    '</Pair>',$
    '<Pair>',$
    '<key>highlight</key>',$
    '<styleUrl>#sh_placemark_circle_highlight','</styleUrl>',$
    '</Pair>',$
    '</StyleMap>',$
    '<Style id="sh_placemark_circle_highlight">',$
    '<IconStyle>',$
    '<scale>1</scale>',$
    '<Icon>',$
    '<href>http://maps.google.com/mapfiles/kml/shapes/placemark_circle_highlight.png</href>',$
    '</Icon>',$
    '</IconStyle>',$
    '<ListStyle>',$
    '</ListStyle>',$
    '</Style>',$
    '<Style id="sn_placemark_circle">',$
    '<IconStyle>',$
    '<scale>1</scale>',$
    '<Icon>',$
    '<href>http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png</href>',$
    '</Icon>',$
    '</IconStyle>',$
    '<ListStyle>',$
    '</ListStyle>',$
    '</Style>')
  request->writeDataDumpFileRecord, txthlp
  txthlp=string('<style id="Labelsize">',$
    '<LabelStyle>',$
    '<scale>.5</scale>',$
    '</LabelStyle>',$
    '</style>')
  request->writeDataDumpFileRecord, txthlp
  
  for iReg=0,nObsRegions-1 do begin
  
    hlp=where(obsRegions eq obsRegions2(ireg),nreg2)
    
    latzoom=(max(obsLatitudes(hlp,*,*),/nan)+min(obsLatitudes(hlp,*,*),/nan))/2.
    lonzoom=(max(obsLongitudes(hlp,*,*),/nan)+min(obsLongitudes(hlp,*,*),/nan))/2.
    extension=100000.
    
    txthlp=string('<LookAt>', $
      '<longitude>',lonzoom,'</longitude>', $
      '<latitude>',latzoom,'</latitude>', $
      '<range>',extension,'</range>', $
      '<tilt>',25,'</tilt>', $
      '<heading>',0,'</heading>', $
      '</LookAt>')
    request->writeDataDumpFileRecord, txthlp
    
    ; FOLDER 1
    Folder1=strupcase(obsRegions2(ireg))+': '+strtrim(nreg2,2)+' STATIONS'
    ;  printf,77,'<Folder>','<name>',Folder1,'</name>','<open>0</open>'
    txthlp=string('<Folder>','<name>',Folder1,'</name>','<open>0</open>')
    request->writeDataDumpFileRecord, txthlp
    
    print,'Number of non-validated stations = ',nreg2
    for istat =0,nstat-1 do begin
    
      if  obsRegions(istat) ne obsRegions2(ireg) then goto,noValStat
      
      parstring=parCodes(0)
      if npar ge 2 then begin
        for ipar=1,npar-1 do begin
          parstring=parstring+'*'+parCodes(ipar)
        endfor
      endif
      obsModString=''
      ;      modelString=''
      for ipar=0,npar-1 do begin
        obsHLPtxt='OBS'  ; this is the standard
        if elabCode eq 60 or elabCode eq 61 then obsHLPtxt='MeanOBS !!'  ; for Bias ans MBias
        if elabCode eq 69 then obsHLPtxt='StddevOBS !!'    ; for MFS
        pChlp=parCodes(ipar)+'*'
        if npar eq 1 then pChlp=''
        if finite(obsValues(istat,ipar,0)) eq 0 then goto,noValStat
        if iObsRun eq 0 then begin
          obsHLP='--'
        endif else begin
          obsHlp=strtrim(reform(obsValues(istat,ipar,0)),2)
        endelse
        ObsModString=ObsModString+'<tr><td>'+pChlp+obsHLPtxt+'</td>,<td>'+$
          obsHLP+unitVarObs[ipar]+'</td></tr>'
        for imod=0,nmod-1 do begin
          modHlp=reform(modValues(*,ipar,imod))
          plusje=''
          if (elabCode eq 60 or elabCode eq 61 or elabCode eq 69) and modHlp(istat) gt 0. then plusje='+'
          pChlp=parCodes(ipar)+'*'
          if npar eq 1 then pChlp=''
          if finite(modHlp(istat)) eq 0 then goto,noValStat
          ObsModString=ObsModstring+'<tr><td>'+PChlp+modCodes(imod)+$
            '</td>,<td>'+plusje+strtrim(modHlp(istat),2)+unitVar[ipar]+'</td></tr>'
        endfor
      endfor
      
      data_source='<a href="http://aqm.jrc.it/DELTA">JRC DELTA_TOOL</a>'
      
      txthlp=string('<Placemark>', $
        '<name>',obsCodes(istat),'</name>', $
        '<visibility>1</visibility>',$
        '<description>', $
        '<![CDATA[', $
        '<table><tbody><th>', $
        '<tr><td><table border="1">', $
        '<tbody>', $
        '<tr><td width=170>Data source</td>', $
        '<td width=220>',data_source,'</td></tr>', $
        '<tr><td>StatCode</td>', $
        '<td>', obsCodes(istat),'</td></tr>', $
        '<tr><td>StatName</td>', $
        '<td>',obsNames(istat),'</td></tr>', $
        '<tr><td>StatNameAbb</td>', $
        '<td>', obsshortnames(istat),'</td></tr>', $
        '<tr><td>Region</td>',$
        '<td>',obsRegions(istat),'</td></tr>', $
        '<tr><td>Longitude</td>',$
        '<td>',strtrim(obsLongitudes(istat),2),'</td></tr>', $
        '<tr><td>Latitude</td>',$
        '<td>',strtrim(obsLatitudes(istat),2),'</td></tr>', $
        '<tr><td>Altitude</td>',$
        '<td>',round(obsaltitudes(istat)),'    [m]','</td></tr>', $
        '<tr><td>GMT</td>',$
        '<td>',obsGMT(istat),'</td></tr>', $
        '<tr><td>Type</td>',$
        '<td>',stationType(istat),'</td></tr>', $
        '<tr><td>Layer</td>',$
        '<td>',Layer(istat),'</td></tr>', $
        '<tr><td>OBS Source</td>',$
        '<td>',Source(istat),'</td></tr>', $
        '<tr><td>Variables</td>',$
        '<td>','TEST VARS','</td></tr>', $
        '</th></tbody></table></td></tr>', $
        '<table><tbody><th>', $
        '<tr><td><table border="0">', $
        '<tbody>', $
        '</th></tbody></table></td></tr>', $
        '<tr><td><table border="1">', $
        '<tbody>', $
        '<tr><td width=170>OBS period</td>',$
        '<td width=220>from ',strtrim(startIndex,2),' to ',strtrim(endIndex,2),'   [hrs]','</td></tr>', $
        '<tr><td>Specs</td>',$
        '<td>',parstring,'</td></tr>', $
        '<tr><td>Indicator</td>',$
        '<td>',elabname(0),'</td></tr>')
      request->writeDataDumpFileRecord, txthlp
      txthlp=string(obsModstring)
      request->writeDataDumpFileRecord, txthlp
      ;      txthlp=string(modelstring)
      ;      request->writeDataDumpFileRecord, txthlp
      txthlp=string('</th></tbody>', $
        '</table></td></tr>', $
        '</th></tbody>', $
        '</table><br />', $
        ']]>', $
        '</description>', $
        '<styleUrl>#msn_placemark_circle','</styleUrl>',$
        '<Point>', $
        '<coordinates>')
      request->writeDataDumpFileRecord, txthlp
      txthlp=string(strtrim(obsLongitudes(istat),2),',',strtrim(obsLatitudes(istat),2),',',$
        strtrim(obsAltitudes(istat),2), $
        '</coordinates>', $
        '</Point>')
      request->writeDataDumpFileRecord, txthlp
      
      ; ***********
      ; part: Color pins
      if npar eq 1 then begin
        if total(elabCode eq [58,59,62,65,66]) then PinValue=obsValues(istat,0,0)
        if total(elabCode eq [60,61,63,64,67,68,69,70]) eq 1 then PinValue=modValues(istat,0,0)
        if (ick eq 1 and PinValue gt limValue) or ick eq 2 then begin
          txthlp=string('<styleUrl>#black_pin','</styleUrl>','<Point>','<coordinates>')
          request->writeDataDumpFileRecord, txthlp
        endif
        if ick eq 0 or (ick eq 1 and PinValue le limValue) then begin
          for icol=0,npincolors-1 do begin
            eps0=.0 & eps5=.0
            if icol eq 0 then eps0=.001
            if icol eq npincolors-1 then eps5=.001
            if PinValue gt minValue+icol*valrange-eps0 and PinValue le minValue+(icol+1.)*valrange+eps5 then begin
              colhlp=pincolors(icol)
              txthlp=string('<styleUrl>#'+colhlp+'_pin','</styleUrl>','<Point>','<coordinates>')
              request->writeDataDumpFileRecord, txthlp
            endif
          endfor
        endif
        txthlp=string(strtrim(obsLongitudes(istat),2),',',strtrim(obsLatitudes(istat),2),',',$
          strtrim(obsAltitudes(istat),2),'</coordinates>','</Point>')
        request->writeDataDumpFileRecord, txthlp
        txthlp=string('</Placemark>')
        request->writeDataDumpFileRecord, txthlp
        
      endif else begin
        txthlp=string('</Placemark>')
        request->writeDataDumpFileRecord, txthlp
      endelse
      ; ***********
      noValStat:
    endfor    ; istat
    
    ; FOLDER 1 END
    txthlp=string('</Folder>')   ;Folder1
    request->writeDataDumpFileRecord, txthlp
    
  endfor  ; END nRegions
  
  if npar eq 1 then begin
  
    ; FOLDER 2
    legend_var='OBS'+unitVar[0]
    if total(elabCode eq [60,61,63,64,67,68,69,70]) eq 1 then legend_var=modCodes(0)+mus[0]
    Folder2='LEGEND -- '+legend_var
    txthlp=string('<Folder>','<name>',Folder2,'</name>','<open>1</open>')
    request->writeDataDumpFileRecord, txthlp
    
    if ick eq 0 or ick eq 1 then begin
      for icol=0,npincolors-1 do begin
        minV=strtrim(minValue+icol*valrange,2)
        maxV=strtrim(minValue+(icol+1.)*valrange,2)
        txthlp=string('<Placemark>')
        request->writeDataDumpFileRecord, txthlp
        colhlp=pincolors(icol)
        txthlp=string('<styleUrl>#'+colhlp+'_pin','</styleUrl>')
        request->writeDataDumpFileRecord, txthlp
        txthlp=string('<name>','<![CDATA[',minV,'  <=  ',maxV,']]>','</name>')
        request->writeDataDumpFileRecord, txthlp
        txthlp=string('</Placemark>')
        request->writeDataDumpFileRecord, txthlp
      endfor
      if limValue lt 9000 then begin
        limV=strtrim(limValue,2)
        txthlp=string('<Placemark>')
        request->writeDataDumpFileRecord, txthlp
        txthlp=string('<styleUrl>#black_pin','</styleUrl>')
        request->writeDataDumpFileRecord, txthlp
        txthlp=string('<name>','<![CDATA[  >=  ',limV,' (LimValue)]]>','</name>')
        request->writeDataDumpFileRecord, txthlp
        txthlp=string('</Placemark>')
        request->writeDataDumpFileRecord, txthlp
      endif
    endif
    if ick eq 2 and limValue lt 9000 then begin
      limV=strtrim(limValue,2)
      txthlp=string('<Placemark>')
      request->writeDataDumpFileRecord, txthlp
      txthlp=string('<styleUrl>#black_pin','</styleUrl>')
      request->writeDataDumpFileRecord, txthlp
      txthlp=string('<name>','<![CDATA[  >=  ',limV,' (LimValue)]]>','</name>')
      request->writeDataDumpFileRecord, txthlp
      txthlp=string('</Placemark>')
      request->writeDataDumpFileRecord, txthlp
    endif
    ;    endif
    txthlp=string('</Folder>')   ;Folder2
    request->writeDataDumpFileRecord, txthlp
  ; FOLDER 2 END
  ;*************************************************************************
  endif   ; npar eq 1
  
  ; DOCUMENT END
  txthlp=string('</Document></kml>')
  request->writeDataDumpFileRecord, txthlp
  
  request->closeDataDumpFile
  
  GE_path=request->getGoogleEarthLocation()
  
  ;SPAWN, ['"C:\Program Files (x86)\Google\Google Earth\client\googleearth.exe"',PATHDUMP+fname], /NOSHELL, /NOWAIT
  SPAWN, [GE_path,PATHDUMP+fname], /NOSHELL, /NOWAIT
  
  endGE:
END

;
PRO FM_PlotGoogleEarthLegend, plotter, request, result

;  plotter->plot2DLegend, request, result

END

PRO FM_PlotTaylor, plotter, request, result
  !y.range=0
  xmarg0_sav=!x.margin(0)
  plotter->wsetMainDataDraw
  DEVICE,DECOMPOSE=0
  LOADCT,39
  ; use "tek" color table...
  mytek_color;, 0, 32
  plotInfo=result->getPlotInfo()
  targetInfo=result->getGenericPlotInfo()
  legNames=targetInfo->getLegendNames()
  tpInfo=result->getGenericPlotInfo()
  nobs=request->getSingleObsNumber()
  npar=request->getParameterNumber()
  nmod=request->getModelNumber()
  obsNames=request->getSingleObsNames()
  elabcode=request->getElaborationCode()  
  allDataXY=tpInfo->getXYS()
  if string(allDataXY[0]) eq 'AllNaN' then begin
    plot,indgen(10),/nodata,color=255,background=255
    xyouts,1,5,'No valid stations or groups selected',charsize=2,charthick=2,/data,color=0
    goto,notay
  endif
  allDataSymbol=tpInfo->getSymbols()
  allDataColor=tpInfo->getColors()
  
  adummy=fltarr(10) & adummy(*)=1.
  CheckCriteria, request, result, 'OU', criteriaOU, adummy,alpha,criteriaOrig,LV
  
  nmodels=n_elements(alldataxy(*,0))
  tay_std=fltarr(nmodels+1) & tay_cor=fltarr(nmodels+1)
  tay_std(0)=1 & tay_cor(0)=0
  tay_std(1:nmodels)=alldataxy(*,0)  ; normalized in elaboration
  tay_cor(1:nmodels)=alldataxy(*,1)
  cols=allDataColor
  
  ;  pi=3.14159
  a=max([tay_std(1:nmodels)],/nan) & b=tay_std(0)
  max_s=max([a,b,2],/nan)
  
  recognizeRange=max_s*0.01
  recognizeHighLight=bytarr(nmodels+1)
  recognizeRegionEdges=ptrarr(nmodels+1)
  recognizeNames=strarr(nmodels+1)
  recognizeValues=strarr(nmodels+1)
  recognizePoint=fltarr(4,2)
  
  nota=1
  if finite(max_s) eq 0 or max_s eq 0. then nota=0
  
  x181=fltarr(181)
  theta181=fltarr(181)
  
  if nota eq 0 then max_s=1.
  
  for i=0,180 do begin
    hk=i*!pi/360.
    x181(i)=max_s*cos(hk)    ; max_s --> 0
    theta181(i)=max_s*sin(hk)  ; 0 --> max_s
  endfor
  
  ytit='SigmaM/sigmaO'
  if elabcode eq 14 then ytit='SigmaM/(2*OU*O)'
  
  taymap=fltarr(nmodels+1,3) & taymap(*,*)=!values.f_nan
  
  !x.margin(0)=25
  plot,x181,theta181,xrange=[0,1.1*max_s],yrange=[0,1.1*max_s],xstyle=9,ystyle=9, background=255,$
    xtitle=ytit, ytitle=ytit,color=0, position=plotter->getPosition(), $
    noerase=plotter->getOverplotKeyword(0),/isotropic
    
  if criteriaOU gt 0 and elabCode eq 14 then begin
    points = (!PI / 99.0) * FINDGEN(100)
    xcenter=1
    ycenter=0
    radius=1
    x = xcenter + radius * COS(points )
    y = ycenter + radius * SIN(points )
    polyfill,TRANSPOSE([[x],[y]]),/data,thick=2,color=160
  endif
  
  for k=1,5 do begin
    i=k*max_s/5
    oplot,i*x181/max_s,i*theta181/max_s,linestyle=0,color=0
  endfor
  ; dotted line
  oplot,max(tay_std(0),/nan)*x181/max_s,tay_std(0)*theta181/max_s,linestyle=1,color=0
  
  ;radial lines
  for i=1,4 do begin
    cc=2.*i/10.
    ca=acos(cc)*360./!pi
    oplot,[0,x181(ca)],[0,theta181(ca)],linestyle=0,color=0
  endfor
  
  for i=0,8 do begin
    cc=2.*i/10.
    if i eq 5 then cc=0.9
    if i eq 6 then cc=0.95
    if i eq 7 then cc=0.99
    if i eq 8 then cc=1.0
    ca=acos(cc)*360./!pi
    oplot,[x181(ca),x181(ca)+max_s*.01*cos(ca*!pi/180.)],$
      [theta181(ca),theta181(ca)+max_s*.01*sin(ca*!pi/180.)],color=0
    xtxt=x181(ca)+.02*max_s*cos(ca*!pi/180.)
    ytxt=theta181(ca)+.02*max_s*sin(ca*!pi/180.)
    xyouts,xtxt,ytxt,strmid(strtrim(string(cc),2),0,4),charsize=1.,color=0
  endfor
  
  xyouts,.8*max_s,0.85*max_s,'Corr_Coef',charsize=1.5,orientation=-45.,$
    alignment=.5,color=0
    
  if nota eq 0 then goto,notay
  
  x=fltarr(361)
  theta=fltarr(361)
  
  for i=0,360 do begin
    hk=i*!pi/360.
    x(i)=cos(hk)
    theta(i)=sin(hk)
  endfor
  xhlp=x
  thetahlp=theta
  
  sd_obs=tay_std(0)
  
  if finite(sd_obs) eq 0 then goto,notay
  
  for k=1,5 do begin
    i=max_s*k/5
    for j=0,360 do begin
      if (sd_obs+i*x(j))^2+(i*theta(j))^2 ge max_s^2 or sd_obs+i*x(j) le 0. then begin
        xhlp(j)=!values.f_nan
        thetahlp(j)=!values.f_nan
      endif else begin
        jlab=j
      endelse
    endfor
    oplot,sd_obs+i*xhlp,i*thetahlp,linestyle=0,color=0
    if jlab eq 360 then begin
      xyouts,sd_obs+i*xhlp(360),.02*max_s,charsize=1.,strmid(strtrim(string(i),2),0,3), $
        color=0
    endif else begin
      xyouts,.02*max_s,.01*max_s+i*thetahlp(jlab),strmid(strtrim(string(i),2),0,3),charsize=1.,$
        color=0
    endelse
  endfor
  
  plots,tay_std(0),0,psym=2,color=0,thick=1.5,symsize=2.
  taymap(0,0)=tay_std(0)
  taymap(0,1)=0.
  taymap(0,2)=tay_std(0)
  
  recognizePoint[0,*]=[tay_std(0)-recognizeRange, -recognizeRange]
  recognizePoint[1,*]=[tay_std(0)-recognizeRange, recognizeRange]
  recognizePoint[2,*]=[tay_std(0)+recognizeRange, recognizeRange]
  recognizePoint[3,*]=[tay_std(0)+recognizeRange, -recognizeRange]
  recognizePoint1=transpose(recognizePoint)
  normRecognizePoint=convert_coord(recognizePoint1, /DATA, /TO_NORMAL)
  normRecognizePoint=transpose(normRecognizePoint)
  normRecognizePoint=normRecognizePoint[*, 0:1]
  recognizePointPtr=ptr_new(normRecognizePoint, /NO_COPY)
  recognizeHighLight[0]=0b
  recognizeRegionEdges[0]=recognizePointPtr
  recognizeNames[0]='All Stations'
  recognizeValues[0]='Stddev-normalized='+strtrim(allDataXY[0, 0], 2)
  
  
  for nm=1,nmodels do begin
    ca_mod=90.-acos(tay_cor(nm))*180./!pi
    x_mod=tay_std(nm)*sin(ca_mod*!pi/180.)
    y_mod=tay_std(nm)*cos(ca_mod*!pi/180.)
    taymap(nm,0)=x_mod
    taymap(nm,1)=y_mod  ;sd_obs
    taymap(nm,2)= sqrt( (x_mod-sd_obs)^2.+y_mod*y_mod )
    mypsym,allDataSymbol[nm-1],1
    plots, x_mod, y_mod, psym=8, color=2+allDataColor[nm-1], symsize=1.5
    ;    plots,x_mod,y_mod,psym=allDataSymbol(nm),color=cols(nm)+2,thick=1.5,symsize=1.5
    ahlp=1.+((x_mod-sd_obs)/y_mod)^2
    
    recognizePoint[0,*]=[x_mod-recognizeRange, y_mod-recognizeRange]
    recognizePoint[1,*]=[x_mod-recognizeRange, y_mod+recognizeRange]
    recognizePoint[2,*]=[x_mod+recognizeRange, y_mod+recognizeRange]
    recognizePoint[3,*]=[x_mod+recognizeRange, y_mod-recognizeRange]
    recognizePoint1=transpose(recognizePoint)
    normRecognizePoint=convert_coord(recognizePoint1, /DATA, /TO_NORMAL)
    normRecognizePoint=transpose(normRecognizePoint)
    normRecognizePoint=normRecognizePoint[*, 0:1]
    recognizePointPtr=ptr_new(normRecognizePoint, /NO_COPY)
    recognizeHighLight[nm]=0b
    recognizeRegionEdges[nm]=recognizePointPtr
    nmmod=fix(nm mod nmod)
    recognizeNames[nm]=legNames[nm-1]
    recognizeValues[nm]=strtrim(tay_std(nm), 2)+'/'+strtrim(tay_cor(nm), 2)+'/'+$
      strtrim(taymap(nm,2),2)
  endfor
  rInfo = obj_new("RecognizeInfo", recognizeNames, recognizeValues, recognizeHighLight, recognizeRegionEdges)
  plotInfo->setRecognizeInfo, rInfo
  
  notay:
  !x.margin(0)=xmarg0_sav
END

PRO FM_PlotTaylorLegend, plotter, request, result
  targetInfo=result->getGenericPlotInfo()
  allDataXY=targetInfo->getXYS()
  if string(allDataXY[0]) eq 'AllNaN' then goto,jumpend
  legendGenericBuild,request,result,plotter
  legendInfo,request,result,plotter
  jumpend:
END

PRO testNormOnPlot, plotter

  ;test working of plotNormalize on 4 rectangle
  coords1=[0.0, 0.0]
  coords1=plotter->plotNormalize(coords1)
  coords2=[0.5, 0.0]
  coords2=plotter->plotNormalize(coords2)
  coords3=[0.5, 0.5]
  coords3=plotter->plotNormalize(coords3)
  coords4=[0.0, 0.5]
  coords4=plotter->plotNormalize(coords4)
  polyfill,[coords1[0],coords2[0],coords3[0],coords4[0]],[coords1[1],coords2[1],coords3[1],coords4[1]],color=15,/norm
  
  coords1=[0.0, 0.5]
  coords1=plotter->plotNormalize(coords1)
  coords2=[0.5, 0.5]
  coords2=plotter->plotNormalize(coords2)
  coords3=[0.5, 1.]
  coords3=plotter->plotNormalize(coords3)
  coords4=[0., 1.]
  coords4=plotter->plotNormalize(coords4)
  polyfill,[coords1[0],coords2[0],coords3[0],coords4[0]],[coords1[1],coords2[1],coords3[1],coords4[1]],color=16,/norm
  
  coords1=[0.5, 0.0]
  coords1=plotter->plotNormalize(coords1)
  coords2=[1., 0.0]
  coords2=plotter->plotNormalize(coords2)
  coords3=[1., .5]
  coords3=plotter->plotNormalize(coords3)
  coords4=[0.5, .5]
  coords4=plotter->plotNormalize(coords4)
  polyfill,[coords1[0],coords2[0],coords3[0],coords4[0]],[coords1[1],coords2[1],coords3[1],coords4[1]],color=17,/norm
  
  coords1=[0.5, 0.5]
  coords1=plotter->plotNormalize(coords1)
  coords2=[1., 0.5]
  coords2=plotter->plotNormalize(coords2)
  coords3=[1., 1.]
  coords3=plotter->plotNormalize(coords3)
  coords4=[0.5, 1.]
  coords4=plotter->plotNormalize(coords4)
  polyfill,[coords1[0],coords2[0],coords3[0],coords4[0]],[coords1[1],coords2[1],coords3[1],coords4[1]],color=18,/norm
  
END

PRO FM_PlotTarget, plotter, request, result, allDataXY, allDataColor, allDataSymbol
  !y.range=0
  plotter->wsetMainDataDraw
  DEVICE,DECOMPOSE=0
  LOADCT,3
  mytek_color;, 0, 32
  tpInfo=result->getGenericPlotInfo()
  allDataXY=tpInfo->getXYS()
  if string(allDataXY[0]) eq 'AllNaN' then begin
    plot,indgen(10),/nodata,color=255,background=255
    xyouts,1,5,'No valid stations or groups selected',charsize=2,charthick=2,/data,color=0
    goto,jumpend
  endif
  allDataSymbol=tpInfo->getSymbols()
  allDataColor=tpInfo->getColors()
  plotInfo=result->getPlotInfo()
  targetInfo=result->getGenericPlotInfo()
  legNames=targetInfo->getLegendNames()
  parCodes=request->getParameterCodes()
  npar=request->getParameterNumber()
  nmod=request->getModelNumber()
  nsce=request->getScenarioNumber()
  elabName=request->getElaborationName()
  elabcode=request->getElaborationCode()
  obsNames=request->getSingleObsNames()
  groupTitles=request->getGroupTitles()
  isGroupSelection=request->isGroupObsPresent()
  
  npoints=n_elements(allDataXY(*,0))
  
  cc=where(finite(allDataXY(*,0)) eq 1,countValidStations)
  countValidStations=countValidStations/(npar*nmod*nsce)
  
  adummy=fltarr(10) & adummy(*)=1.
  CheckCriteria, request, result, 'OU', criteria, adummy,alpha,criteriaOrig,LV
  ;  criteria=criteria/2.
  
  hourStat=request->getGroupByTimeInfo() ;HourType
  flag_average=hourStat[0].value
  
  recognizeHighLight=bytarr(npoints)   ; 8
  recognizeRegionEdges=ptrarr(npoints)
  recognizeNames=strarr(npoints)
  recognizeValues=strarr(npoints)
  recognizePoint=fltarr(4,2)
  
  dims=get_screen_size(RESOLUTION=resolution)
   
  if (criteria eq 0 or countValidStations eq 0) then begin
    plot,indgen(10),/nodata,color=255,background=255
    xyouts,1,9,'No Diagram available for current choice',charsize=2,charthick=2,/data,color=0
    xyouts,2,7,'Check criteria availability for selected parameter',charsize=1.5,charthick=1,/data,color=0
    xyouts,2,6,'Only available for 8hmax O3 daily PM10 and hourly NO2',charsize=1.5,charthick=1,/data,color=0
    xyouts,2,5,'If group selected only worst statistics works',charsize=1.5,charthick=1,/data,color=0
    xyouts,2,4,'Might be no valid stations or groups selected',charsize=1.5,charthick=1,/data,color=0
    xyouts,2,3,'For annual MQO entire year should be selected',charsize=1.5,charthick=1,/data,color=0
    goto,jumpend
  endif
  
  maxAxis=max([max(abs(allDataXY),/nan),1.5])
  plotRange=maxAxis + maxAxis*.4
  if finite(maxAxis) eq 0 then plotRange=1
  if finite(maxAxis) eq 0 then maxAxis=1
  
  facSize=min([plotRange*.45,1])
  
  plot, indgen(10), color=0,xrange=[-plotRange,plotRange], yrange=[-plotRange,plotRange], xstyle=1,$
    ystyle=1,/nodata, title='TARGET PLOT', charsize=facSize, background=255, position=plotter->getPosition(), noerase=plotter->getOverplotKeyword(0)
  ; plot referring circles (4)
  if criteria gt 0 then begin
    POLYFILL, CIRCLE(0, 0, 1), /data, thick=2, color=8
    plots, CIRCLE(0, 0, 1), /data, thick=2, color=0
    plots, CIRCLE(0, 0, 0.5), /data, thick=2, color=0,linestyle=2
  endif else begin
    plots, CIRCLE(0, 0, 1), /data, thick=2, color=0
  endelse
  xyouts,0.1,1,'T=1',color=0,/data,charthick=3,charsize=facSize*1.3
  
  if criteria gt 0 and nmod eq 1 and isGroupSelection eq 0 then begin
  
    coords1=[-plotRange+plotRange*0.05, plotrange-plotRange*0.25]
    coords2=[-plotRange*0.15, plotrange-plotRange*0.25]
    coords3=[-plotRange*0.15, plotrange-plotRange*0.05]
    coords4=[-plotRange+plotRange*0.05, plotrange-plotRange*0.05]
    
    polyfill,[coords1[0],coords2[0],coords3[0],coords4[0]],[coords1[1],coords2[1],coords3[1],coords4[1]],color=15,/data
    coords=[-plotRange+plotRange*0.10,plotrange-plotRange*0.15]
    psFact=plotter->getPSCharSizeFactor()
    xyouts,coords[0],coords[1],'Stations within Crit (T=1):',color=3,/data,charthick=3,charsize=facSize*1.3*psFact
  endif
  
  fixedLabels=strarr(4)
  fixedLabels[0]='BIAS'
  fixedLabels[1]='CRMSE'
  fixedLabels[2]='MU > OU'
  
  ;if elabCode eq 81 then fixedLabels[3]='SigM > SigO          SigO > SigM'
    xfac=0.1
    ;PHILTH 27032012  Change of axis in test target diagram
    plots,[-plotRange,-xfac],[-plotRange,-xfac],/data,color=0,thick=2
    plots,[-plotRange,-xfac],[plotRange,xfac],/data,color=0,thick=2
    plots,[plotRange,xfac],[-plotRange,-xfac],/data,color=0,thick=2
    plots,[plotRange,xfac],[plotRange,xfac],/data,color=0,thick=2
    ;      plots,[1+xfac,plotRange],[-1,-1],/data,color=0,thick=2
    ;      plots,[-plotRange,-1-xfac],[-1,-1],/data,color=0,thick=2
    ;      plots,[1+xfac,plotRange],[1,1],/data,color=0,thick=2
    ;      plots,[-plotRange,-1-xfac],[1,1],/data,color=0,thick=2
    ;      plots,[1,1],[1+xfac,plotRange],/data,color=0,thick=2
    ;      plots,[-1,-1],[1+xfac,plotRange],/data,color=0,thick=2
    ;      plots,[1,1],[-1-xfac,-plotRange],/data,color=0,thick=2
    ;      plots,[-1,-1],[-1-xfac,-plotRange],/data,color=0,thick=2
    xyouts, -plotRange*0.1, plotRange*0.85, 'BIAS > 0',charthick=2, color=0,/data,charsize=facSize*1.5
    xyouts, -plotRange*0.1, -plotRange*0.95, 'BIAS < 0',charthick=2, color=0,/data,charsize=facSize*1.5
    ;      xyouts, -plotRange*0.95, plotRange*0.85, 'BIAS > 0 / R',charthick=2, color=0,/data,charsize=facSize*1.5
    ;      xyouts, plotRange*0.60, plotRange*0.85, 'BIAS > 0 / SD',charthick=2, color=0,/data,charsize=facSize*1.5
    ;      xyouts, -plotRange*0.95, -plotRange*0.95, 'BIAS < 0 / R',charthick=2, color=0,/data,charsize=facSize*1.5
    ;      xyouts, plotRange*0.60, -plotRange*0.95, 'BIAS < 0 / SD',charthick=2, color=0,/data,charsize=facSize*1.5
    xyouts, -plotRange*0.95, -plotRange*0.05, 'R',charthick=2, color=0,/data,charsize=facSize*1.5
    xyouts, plotRange*0.85, -plotRange*0.05, 'SD',charthick=2, color=0,/data,charsize=facSize*1.5
  
  posLabels=fltarr(4,2)
  posLabels[0, *]=[0.1, 1.3]
  posLabels[1, *]=[1.2,0.1]
  posLabels[2, *]=[0.9,0.9]
  posLabels[3, *]=[-plotRange*0.35, -plotRange+plotRange*0.05]
  
  orientLabels=fltarr(4)
  orientLabels[0]=90
  orientLabels[1]=0
  orientLabels[2]=45
  orientLabels[3]=0
  
  thickLabels=fltarr(4)
  thickLabels[0]=1.
  thickLabels[1]=1.
  thickLabels[2]=2.
  thickLabels[3]=2.
  
  axisXLine=[[-plotRange,0], [plotRange,0]]
  axisYLine=[[0,-plotRange], [0,plotRange]]
  
  plots, axisXLine, /data, color=0
  plots, axisYLine, /data, color=0
  
  for i=0, 1 do begin
    xyouts, posLabels[i,0], posLabels[i,1], fixedLabels[i], orient=orientLabels[i], $
      charthick=thickLabels[i], color=0,/data,charsize=facSize*1.2
  endfor
  
  recognizeRange=plotRange*0.02
  
  
  nobsStart=0
  for iObs=nobsStart, npoints-1 do begin
    mypsym,allDataSymbol[iObs],1
    plots, allDataXY[iObs, 0], allDataXY[iObs, 1], psym=8, color=2+allDataColor[iObs], symsize=1.2*facSize  ;2*/maxAxis
    recognizePoint[0,*]=[allDataXY[iObs, 0]-recognizeRange,allDataXY[iObs, 1] -recognizeRange]
    recognizePoint[1,*]=[allDataXY[iObs, 0]-recognizeRange,allDataXY[iObs, 1] +recognizeRange]
    recognizePoint[2,*]=[allDataXY[iObs, 0]+recognizeRange,allDataXY[iObs, 1] +recognizeRange]
    recognizePoint[3,*]=[allDataXY[iObs, 0]+recognizeRange,allDataXY[iObs, 1] -recognizeRange]
    recognizePoint1=transpose(recognizePoint)
    normRecognizePoint=convert_coord(recognizePoint1, /DATA, /TO_NORMAL)
    normRecognizePoint=transpose(normRecognizePoint)
    normRecognizePoint=normRecognizePoint[*, 0:1]
    recognizePointPtr=ptr_new(normRecognizePoint, /NO_COPY)
    recognizeRegionEdges[iobs]=recognizePointPtr
    recognizeNames[iobs]=legNames[iobs]
    recognizeValues[iobs]=strcompress(allDataXY(iObs, 0),/remove_all)+','+strcompress(allDataXY(iObs, 1),/remove_all)
  endfor
  
  if criteria gt 0 and nmod eq 1 and isGroupSelection eq 0 then begin
    cc=where(finite(allDataXY[*, 0]) eq 1,countValidStations)
    if countValidStations gt 0 then begin
      radius = sqrt(allDataXY[cc, 0]^2+allDataXY[cc, 1]^2)
      ccCrit=where(radius le 1,countCritPerc)
      percentageCrit=fix(100.*float(countCritPerc)/float(countValidStations))
      xyouts,-plotRange*0.33,plotrange-plotRange*0.17,strtrim(percentageCrit,2)+'%',/data,charsize=2*facSize,charthick=2,color=0
    endif
  endif
  
  ;  endelse
  
  rInfo = obj_new("RecognizeInfo", recognizeNames, recognizeValues, recognizeHighLight, recognizeRegionEdges)
  plotInfo->setRecognizeInfo, rInfo
  
  jumpend:
  
END

PRO FM_PlotSoccer, plotter, request, result, allDataXY, allDataColor, allDataSymbol
  !y.range=0
  plotter->wsetMainDataDraw
  DEVICE,DECOMPOSE=0
  LOADCT,39
  ; use "tek" color table...
  mytek_color;, 0, 32
  tpInfo=result->getGenericPlotInfo()
  allDataXY=tpInfo->getXYS()
  if string(allDataXY[0]) eq 'AllNaN' then begin
    plot,indgen(10),/nodata,color=255,background=255
    xyouts,1,5,'No valid stations or groups selected',charsize=2,charthick=2,/data,color=0
    goto,jumpend
  endif
  allDataSymbol=tpInfo->getSymbols()
  allDataColor=tpInfo->getColors()
  legNames=tpInfo->getLegendNames()
  plotInfo=result->getPlotInfo()
  parCodes=request->getParameterCodes()
  npar=request->getParameterNumber()
  nmod=request->getModelNumber()
  elabcode=request->getElaborationCode()
  
  npoints=n_elements(allDataXY(*,0))
  recognizeHighLight=bytarr(npoints)   ; 8
  recognizeRegionEdges=ptrarr(npoints)
  recognizeNames=strarr(npoints)
  recognizeValues=strarr(npoints)
  recognizePoint=fltarr(4,2)
  
  ; KeesC 30MAR2012: obsTemp undefined, changed into adummy
  adummy=fltarr(10) & adummy(*)=1.
  CheckCriteria, request, result, 'RMSE', criteriaRMSE, adummy,alpha,criteriaOrig,LV
  CheckCriteria, request, result, 'BIAS', criteriaBIAS, adummy,alpha,criteriaOrig,LV
  criteriaRMSE=criteriaRMSE(0)
  criteriaBIAS=criteriaBIAS(0)
  ;  CheckCriteria, request, result, 'RMSE', criteriaRMSE, obsTemp, 0,alpha,criteriaOrig,LV,nobsAv
  ;  CheckCriteria, request, result, 'BIAS', criteriaBIAS, obsTemp, 0,alpha,criteriaOrig,LV,nobsAv
  
  if elabcode ne 20 and elabCode ne 22 and elabCode ne 34 then begin
  
    maxxAxis=max(abs(allDataXY(*,0)),/nan)*2
    maxyAxis=max(allDataXY(*,1),/nan)*2
    recognizeRangeX=(maxxAxis)*0.01
    recognizeRangeY=(maxyAxis)*0.01
    
    plot, indgen(10), color=0,xrange=[-maxxAxis,maxxAxis], yrange=[0,maxyAxis], xstyle=1,ystyle=1,/nodata, $
      xtitle='BIAS',ytitle='RMSE',title='Soccer PLOT', charsize=1, background=255, position=plotter->getPosition(), noerase=plotter->getOverplotKeyword(0)
    ; plot referring circles (4)
    if criteriaBIAS gt 0 and criteriaRMSE gt 0 then begin
      polyfill, [-CriteriaBias,-CriteriaBias,CriteriaBias,CriteriaBias,-CriteriaBias],$
        [    0,         CriteriaRmse,CriteriaRMSE,0,0],/data,color=160
      PLOTS, [CriteriaBias,CriteriaBias],[0,CriteriaRmse],/data,color=0,thick=2,linestyle=2
      PLOTS, [-CriteriaBias,-CriteriaBias],[0,CriteriaRmse],/data,color=0,thick=2,linestyle=2
      PLOTS, [-CriteriaBias, CriteriaBias],[CriteriaRmse,CriteriaRmse],/data,color=0,thick=2,linestyle=2
    endif
    
    nObs=n_elements(allDataColor)
    
    for iObs=0, nObs-1 do begin
      mypsym,allDataSymbol[iObs],1
      plots, allDataXY[iObs, 0], allDataXY[iObs, 1], psym=8, color=2+allDataColor[iObs], symsize=1.5
      recognizePoint[0,*]=[allDataXY[iObs, 0]-recognizeRangeX,allDataXY[iObs, 1] -recognizeRangeY]
      recognizePoint[1,*]=[allDataXY[iObs, 0]-recognizeRangeX,allDataXY[iObs, 1] +recognizeRangeY]
      recognizePoint[2,*]=[allDataXY[iObs, 0]+recognizeRangeX,allDataXY[iObs, 1] +recognizeRangeY]
      recognizePoint[3,*]=[allDataXY[iObs, 0]+recognizeRangeX,allDataXY[iObs, 1] -recognizeRangeY]
      recognizePoint1=transpose(recognizePoint)
      normRecognizePoint=convert_coord(recognizePoint1, /DATA, /TO_NORMAL)
      normRecognizePoint=transpose(normRecognizePoint)
      normRecognizePoint=normRecognizePoint[*, 0:1]
      recognizePointPtr=ptr_new(normRecognizePoint, /NO_COPY)
      recognizeRegionEdges[iobs]=recognizePointPtr
      recognizeNames[iobs]=legNames[iobs]
      recognizeValues[iobs]=strcompress(allDataXY(iObs, 0),/remove_all)+'/'+strcompress(allDataXY(iObs, 1),/remove_all)
    endfor
    
    rInfo = obj_new("RecognizeInfo", recognizeNames, recognizeValues, recognizeHighLight, recognizeRegionEdges)
    plotInfo->setRecognizeInfo, rInfo
    
  endif else begin  ;Soccer AQ
  
    maxxAxis=3.
    maxyAxis=3.
    recognizeRangeX=(maxxAxis)*0.01
    recognizeRangeY=(maxyAxis)*0.01
    
    plot, indgen(10), color=0,xrange=[-maxxAxis,maxxAxis], yrange=[-maxyAxis,maxyAxis], xstyle=1,ystyle=1,/nodata, $
      xtitle=' ',ytitle=' ',title='Soccer AQ PLOT', charsize=1, background=255, position=plotter->getPosition(), noerase=plotter->getOverplotKeyword(0)
    polyfill, [0,0,1,1,0],[-1,1,1,-1,-1],/data,color=160
    
    ;plots,[-maxxAxis,maxxAxis],[0,0],/data,color=0
    plots,[-maxxAxis,maxxAxis],[1,1],/data,color=0
    plots,[-maxxAxis,maxxAxis],[-1,-1],/data,color=0
    plots,[0,0],[-maxyAxis,maxyAxis],/data,color=0
    plots,[1,1],[-maxyAxis,maxyAxis],/data,color=0
    plots,[-1,-1],[-maxyAxis,maxyAxis],/data,color=0
    
    xyouts,0.2,2.7,'Bias+',/data,color=0,charsize=1.2
    xyouts,1.5,2.7,'Bias+ / R',/data,color=0,charsize=1.2
    xyouts,1.8,0.1,'R',/data,color=0,charsize=1.2
    xyouts,1.5,-2.7,'Bias- / R',/data,color=0,charsize=1.2
    xyouts,0.2,-2.7,'Bias-',/data,color=0,charsize=1.2
    xyouts,-0.6,0.1,'SD',/data,color=0,charsize=1.2
    xyouts,-0.9,2.7,'SD / Bias+',/data,color=0,charsize=1.2
    xyouts,-0.9,-2.7,'SD / Bias-',/data,color=0,charsize=1.2
    xyouts,-2.9,2.7,'SD / Bias+ / R',/data,color=0,charsize=1.2
    xyouts,-2.9,-2.7,'SD / Bias- / R',/data,color=0,charsize=1.2
    xyouts,-2.9,0.1,'SD / R',/data,color=0,charsize=1.2
    
    
    nObs=n_elements(allDataColor)
    
    for iObs=0, nObs-1 do begin
      mypsym,allDataSymbol[iObs],1
      plots, allDataXY[iObs, 0], allDataXY[iObs, 1], psym=8, color=2+allDataColor[iObs], symsize=1.5
      recognizePoint[0,*]=[allDataXY[iObs, 0]-recognizeRangeX,allDataXY[iObs, 1] -recognizeRangeY]
      recognizePoint[1,*]=[allDataXY[iObs, 0]-recognizeRangeX,allDataXY[iObs, 1] +recognizeRangeY]
      recognizePoint[2,*]=[allDataXY[iObs, 0]+recognizeRangeX,allDataXY[iObs, 1] +recognizeRangeY]
      recognizePoint[3,*]=[allDataXY[iObs, 0]+recognizeRangeX,allDataXY[iObs, 1] -recognizeRangeY]
      recognizePoint1=transpose(recognizePoint)
      normRecognizePoint=convert_coord(recognizePoint1, /DATA, /TO_NORMAL)
      normRecognizePoint=transpose(normRecognizePoint)
      normRecognizePoint=normRecognizePoint[*, 0:1]
      recognizePointPtr=ptr_new(normRecognizePoint, /NO_COPY)
      recognizeRegionEdges[iobs]=recognizePointPtr
      recognizeNames[iobs]=legNames[iobs]
      recognizeValues[iobs]=strcompress(allDataXY(iObs, 0),/remove_all)+'/'+strcompress(allDataXY(iObs, 1),/remove_all)
    endfor
    
    rInfo = obj_new("RecognizeInfo", recognizeNames, recognizeValues, recognizeHighLight, recognizeRegionEdges)
    plotInfo->setRecognizeInfo, rInfo
    
  endelse
  
  jumpend:
  
END

;PRO FM_PlotTarget, plotter, request, result, allDataXY, allDataColor, allDataSymbol
;  !y.range=0
;  plotter->wsetMainDataDraw
;
;  tpInfo=result->getGenericPlotInfo()
;
;  allDataXY=tpInfo->getXYS()
;  allDataSymbol=tpInfo->getSymbols()
;  allDataColor=tpInfo->getColors()
;  plotInfo=result->getPlotInfo()
;  targetInfo=result->getGenericPlotInfo()
;  legNames=targetInfo->getLegendNames()
;
;
;  parCodes=request->getParameterCodes()
;  npar=request->getParameterNumber()
;  nmod=request->getModelNumber()
;  nsce=request->getScenarioNumber()
;  elabName=request->getElaborationName()
;  elabcode=request->getElaborationCode()
;  obsNames=request->getSingleObsNames()
;  groupTitles=request->getGroupTitles()
;  isGroupSelection=request->isGroupObsPresent()
;
;  npoints=n_elements(allDataXY(*,0))
;
;  cc=where(finite(allDataXY(*,0)) eq 1,countValidStations)
;  countValidStations=countValidStations/(npar*nmod*nsce)
;
;  adummy=fltarr(10) & adummy(*)=1.
;  CheckCriteria, request, result, 'OU', criteria, adummy, 0,alpha,criteriaOrig,LV,nobsAv
;  ;  criteria=criteria/2.
;
;  hourStat=request->getGroupByTimeInfo() ;HourType
;  flag_average=hourStat[0].value
;
;  recognizeHighLight=bytarr(npoints)   ; 8
;  recognizeRegionEdges=ptrarr(npoints)
;  recognizeNames=strarr(npoints)
;  recognizeValues=strarr(npoints)
;  recognizePoint=fltarr(4,2)
;
;  dims=get_screen_size(RESOLUTION=resolution)
;
;  DEVICE,DECOMPOSE=0
;  LOADCT,3
;  mytek_color;, 0, 32
;
;  if (criteria eq 0 or countValidStations eq 0) and elabCode eq 55 then begin
;    plot,indgen(10),/nodata,color=255,background=255
;    xyouts,1,9,'No Diagram available for current choice',charsize=2,charthick=2,/data,color=0
;    xyouts,2,7,'Check criteria availability for selected parameter',charsize=1.5,charthick=1,/data,color=0
;    xyouts,2,6,'Only available for 8hmax O3 daily PM10 and hourly NO2',charsize=1.5,charthick=1,/data,color=0
;    xyouts,2,5,'If group selected only worst statistics works',charsize=1.5,charthick=1,/data,color=0
;    xyouts,2,4,'Might be no valid stations or groups selected',charsize=1.5,charthick=1,/data,color=0
;    xyouts,2,3,'For annual MQO entire year should be selected',charsize=1.5,charthick=1,/data,color=0
;    goto,jumpend
;  endif
;
;    maxxAxis=max(allDataXY(*,0),/nan)*1.1
;    if finite(maxxAxis) eq 0 then maxxAxis=100
;    minxAxis=0
;    minyAxis=-200
;    maxyAxis=200
;
;    ymax=max([max(abs(alldataXY(*,1)),/nan)*1.2,1.5])
;    recognizeRangeX=(maxxAxis-minxAxis)*0.01
;    recognizeRangeY=(ymax+ymax)*0.01
;
;    plot, indgen(fix(maxxaxis)), color=0,/nodata, xtitle='Average Observed concentration',ytitle='Bias/2*OU*mean(0)', title='MQO PLOT Annual average', charsize=1, background=255,$
;      yrange=[-ymax,ymax],xrange=[minxAxis,maxxAxis],xstyle=1,ystyle=1, position=plotter->getPosition(), noerase=plotter->getOverplotKeyword(0)
;    if criteria gt 0 then polyfill,[0,0,maxxaxis,maxxaxis,0],[-1,1,1,-1,-1],/data,color=8
;    plots,[0,maxxaxis],[0,0],color=0,/data
;    plots,[0,maxxaxis],[0.5,0.5],color=0,/data,linestyle=2
;    plots,[0,maxxaxis],[-0.5,-0.5],color=0,/data,linestyle=2
;    plots,[0,maxxaxis],[1,1],color=0,/data,thick=2
;    plots,[0,maxxaxis],[-1,-1],color=0,/data,thick=2
;
;    plotrangeX=maxxAxis-minxAxis
;    plotRangeY=ymax+ymax
;    facSize=min([plotRangeX*.45,plotRangeY*.45,1])
;
;    if criteria gt 0 and nmod eq 1 and isGroupSelection eq 0 then begin
;
;      coords1=[plotRangeX*0.02, ymax-plotRangeY*0.03]
;      coords2=[plotRangeX*0.5, ymax-plotRangeY*0.03]
;      coords3=[plotRangeX*0.5, ymax-plotRangeY*0.15]
;      coords4=[plotRangeX*0.02, ymax-plotRangeY*0.15]
;
;      polyfill,[coords1[0],coords2[0],coords3[0],coords4[0]],[coords1[1],coords2[1],coords3[1],coords4[1]],color=15,/data
;      coords=[plotRangeX*0.04,ymax-plotrangeY*0.10]
;      xyouts,coords[0],coords[1],'Stations within Crit (T=1):',color=0,/data,charthick=3,charsize=facSize*1.3
;    endif
;
;    for iObs=0, npoints-1 do begin
;      mypsym,allDataSymbol[iObs],1
;      plots, allDataXY[iObs, 0], allDataXY[iObs, 1], psym=8, color=2+allDataColor[iObs], symsize=1.5,/data
;      recognizePoint[0,*]=[allDataXY[iObs, 0]-recognizeRangeX,allDataXY[iObs, 1] -recognizeRangeY]
;      recognizePoint[1,*]=[allDataXY[iObs, 0]-recognizeRangeX,allDataXY[iObs, 1] +recognizeRangeY]
;      recognizePoint[2,*]=[allDataXY[iObs, 0]+recognizeRangeX,allDataXY[iObs, 1] +recognizeRangeY]
;      recognizePoint[3,*]=[allDataXY[iObs, 0]+recognizeRangeX,allDataXY[iObs, 1] -recognizeRangeY]
;      recognizePoint1=transpose(recognizePoint)
;      normRecognizePoint=convert_coord(recognizePoint1, /DATA, /TO_NORMAL)
;      normRecognizePoint=transpose(normRecognizePoint)
;      normRecognizePoint=normRecognizePoint[*, 0:1]
;      recognizePointPtr=ptr_new(normRecognizePoint, /NO_COPY)
;      recognizeRegionEdges[iobs]=recognizePointPtr
;      recognizeNames[iobs]=legNames[iobs]
;      recognizeValues[iobs]=strcompress(allDataXY(iObs, 0),/remove_all)+'/'+strcompress(allDataXY(iObs, 1),/remove_all)
;    endfor
;
;    if criteria gt 0 and nmod eq 1 and isGroupSelection eq 0 then begin
;      cc=where(finite(allDataXY[*, 0]) eq 1,countValidStations)
;      if countValidStations gt 0 then begin
;        radius = abs(allDataXY[cc, 1])
;        ccCrit=where(radius le 1,countCritPerc)
;        percentageCrit=fix(100.*float(countCritPerc)/float(countValidStations))
;        xyouts,plotRangeX*0.4,ymax-plotrangeY*0.10,strtrim(percentageCrit,2)+'%',/data,charsize=2*facSize,charthick=2,color=0
;      endif
;    endif
;
;  endif else begin  ; target/OU for hourly/daily values
;
;    maxAxis=max([max(abs(allDataXY),/nan),1.5])
;    plotRange=maxAxis + maxAxis*.4
;    if finite(maxAxis) eq 0 then plotRange=1
;    if finite(maxAxis) eq 0 then maxAxis=1
;
;    facSize=min([plotRange*.45,1])
;
;    plot, indgen(10), color=0,xrange=[-plotRange,plotRange], yrange=[-plotRange,plotRange], xstyle=1,$
;      ystyle=1,/nodata, title='TARGET PLOT', charsize=facSize, background=255, position=plotter->getPosition(), noerase=plotter->getOverplotKeyword(0)
;    ; plot referring circles (4)
;    if criteria gt 0 and elabcode eq 55 then begin
;      POLYFILL, CIRCLE(0, 0, 1), /data, thick=2, color=8
;      plots, CIRCLE(0, 0, 1), /data, thick=2, color=0
;      plots, CIRCLE(0, 0, 0.5), /data, thick=2, color=0,linestyle=2
;    endif else begin
;      plots, CIRCLE(0, 0, 1), /data, thick=2, color=0
;    endelse
;    xyouts,0.1,1,'T=1',color=0,/data,charthick=3,charsize=facSize*1.3
;
;    if criteria gt 0 and nmod eq 1 and isGroupSelection eq 0 then begin
;
;      coords1=[-plotRange+plotRange*0.1, plotrange-plotRange*0.25]
;      coords2=[-plotRange*0.05, plotrange-plotRange*0.25]
;      coords3=[-plotRange*0.05, plotrange-plotRange*0.05]
;      coords4=[-plotRange+plotRange*0.1, plotrange-plotRange*0.05]
;
;      polyfill,[coords1[0],coords2[0],coords3[0],coords4[0]],[coords1[1],coords2[1],coords3[1],coords4[1]],color=15,/data
;      coords=[-plotRange+plotRange*0.15,plotrange-plotRange*0.15]
;      xyouts,coords[0],coords[1],'Stations within Crit (T=1):',color=3,/data,charthick=3,charsize=facSize*1.3
;    endif
;
;    fixedLabels=strarr(4)
;    if elabcode eq 55 then begin
;      fixedLabels[0]='BIAS'
;      fixedLabels[1]='CRMSE'
;      fixedLabels[2]='MU > OU'
;    endif else begin
;      fixedLabels[2]='MEF < 0'
;      fixedLabels[0]='BIAS/SigO'
;      fixedLabels[1]='CRMSE/SigO'
;    endelse
;    fixedLabels[3]='SigM > SigO          SigO > SigM'
;
;    posLabels=fltarr(4,2)
;    posLabels[0, *]=[0.1, 1.3]
;    posLabels[1, *]=[1.2,0.1]
;    posLabels[2, *]=[0.9,0.9]
;    posLabels[3, *]=[-plotRange*0.35, -plotRange+plotRange*0.05]
;
;    orientLabels=fltarr(4)
;    orientLabels[0]=90
;    orientLabels[1]=0
;    orientLabels[2]=45
;    orientLabels[3]=0
;
;    thickLabels=fltarr(4)
;    thickLabels[0]=1.
;    thickLabels[1]=1.
;    thickLabels[2]=2.
;    thickLabels[3]=2.
;
;    axisXLine=[[-plotRange,0], [plotRange,0]]
;    axisYLine=[[0,-plotRange], [0,plotRange]]
;
;    plots, axisXLine, /data, color=0
;    plots, axisYLine, /data, color=0
;
;    for i=0, n_elements(fixedLabels)-1 do begin
;      ;      coords=plotter->plotNormalize([posLabels[i,0], posLabels[i,1]])
;      xyouts, posLabels[i,0], posLabels[i,1], fixedLabels[i], orient=orientLabels[i], $
;        charthick=thickLabels[i], color=0,/data,charsize=facSize*1.2
;    endfor
;
;    recognizeRange=plotRange*0.02
;
;
;    nobsStart=0
;    for iObs=nobsStart, npoints-1 do begin
;      mypsym,allDataSymbol[iObs],1
;      plots, allDataXY[iObs, 0], allDataXY[iObs, 1], psym=8, color=2+allDataColor[iObs], symsize=1.2*facSize  ;*7/maxAxis
;      recognizePoint[0,*]=[allDataXY[iObs, 0]-recognizeRange,allDataXY[iObs, 1] -recognizeRange]
;      recognizePoint[1,*]=[allDataXY[iObs, 0]-recognizeRange,allDataXY[iObs, 1] +recognizeRange]
;      recognizePoint[2,*]=[allDataXY[iObs, 0]+recognizeRange,allDataXY[iObs, 1] +recognizeRange]
;      recognizePoint[3,*]=[allDataXY[iObs, 0]+recognizeRange,allDataXY[iObs, 1] -recognizeRange]
;      recognizePoint1=transpose(recognizePoint)
;      normRecognizePoint=convert_coord(recognizePoint1, /DATA, /TO_NORMAL)
;      normRecognizePoint=transpose(normRecognizePoint)
;      normRecognizePoint=normRecognizePoint[*, 0:1]
;      recognizePointPtr=ptr_new(normRecognizePoint, /NO_COPY)
;      recognizeRegionEdges[iobs]=recognizePointPtr
;      recognizeNames[iobs]=legNames[iobs]
;      recognizeValues[iobs]=strcompress(allDataXY(iObs, 0),/remove_all)+'/'+strcompress(allDataXY(iObs, 1),/remove_all)
;    endfor
;
;    if criteria gt 0 and nmod eq 1 and isGroupSelection eq 0 then begin
;      cc=where(finite(allDataXY[*, 0]) eq 1,countValidStations)
;      if countValidStations gt 0 then begin
;        radius = sqrt(allDataXY[cc, 0]^2+allDataXY[cc, 1]^2)
;        ccCrit=where(radius le 1,countCritPerc)
;        percentageCrit=fix(100.*float(countCritPerc)/float(countValidStations))
;        xyouts,-plotRange*0.24,plotrange-plotRange*0.17,strtrim(percentageCrit,2)+'%',/data,charsize=2*facSize,charthick=2,color=0
;      endif
;    endif
;
;  endelse
;
;  rInfo = obj_new("RecognizeInfo", recognizeNames, recognizeValues, recognizeHighLight, recognizeRegionEdges)
;  plotInfo->setRecognizeInfo, rInfo
;
;  jumpEnd:
;
;END
;
;PRO FM_PlotSoccer, plotter, request, result, allDataXY, allDataColor, allDataSymbol
;  !y.range=0
;  plotter->wsetMainDataDraw
;
;  tpInfo=result->getGenericPlotInfo()
;
;  allDataXY=tpInfo->getXYS()
;  allDataSymbol=tpInfo->getSymbols()
;  allDataColor=tpInfo->getColors()
;  legNames=tpInfo->getLegendNames()
;  plotInfo=result->getPlotInfo()
;  parCodes=request->getParameterCodes()
;  npar=request->getParameterNumber()
;  nmod=request->getModelNumber()
;
;  npoints=n_elements(allDataXY(*,0))
;  recognizeHighLight=bytarr(npoints)   ; 8
;  recognizeRegionEdges=ptrarr(npoints)
;  recognizeNames=strarr(npoints)
;  recognizeValues=strarr(npoints)
;  recognizePoint=fltarr(4,2)
;
;  CheckCriteria, request, result, 'RMSE', criteriaRMSE, obsTemp, 0,alpha,criteriaOrig,LV,nobsAv
;  CheckCriteria, request, result, 'BIAS', criteriaBIAS, obsTemp, 0,alpha,criteriaOrig,LV,nobsAv
;
;  DEVICE,DECOMPOSE=0
;  LOADCT,39
;  ; use "tek" color table...
;  mytek_color;, 0, 32
;
;  maxxAxis=max(abs(allDataXY(*,0)),/nan)*2
;  maxyAxis=max(allDataXY(*,1),/nan)*2
;  recognizeRangeX=(maxxAxis)*0.01
;  recognizeRangeY=(maxyAxis)*0.01
;
;  plot, indgen(10), color=0,xrange=[-maxxAxis,maxxAxis], yrange=[0,maxyAxis], xstyle=1,ystyle=1,/nodata, $
;    xtitle='BIAS',ytitle='RMSE',title='Soccer PLOT', charsize=1, background=255, position=plotter->getPosition(), noerase=plotter->getOverplotKeyword(0)
;  ; plot referring circles (4)
;  if criteriaBIAS gt 0 and criteriaRMSE gt 0 then begin
;    polyfill, [-CriteriaBias,-CriteriaBias,CriteriaBias,CriteriaBias,-CriteriaBias],$
;      [    0,         CriteriaRmse,CriteriaRMSE,0,0],/data,color=160
;    PLOTS, [CriteriaBias,CriteriaBias],[0,CriteriaRmse],/data,color=0,thick=2,linestyle=2
;    PLOTS, [-CriteriaBias,-CriteriaBias],[0,CriteriaRmse],/data,color=0,thick=2,linestyle=2
;    PLOTS, [-CriteriaBias, CriteriaBias],[CriteriaRmse,CriteriaRmse],/data,color=0,thick=2,linestyle=2
;  endif
;
;  nObs=n_elements(allDataColor)
;
;  for iObs=0, nObs-1 do begin
;    mypsym,allDataSymbol[iObs],1
;    plots, allDataXY[iObs, 0], allDataXY[iObs, 1], psym=8, color=2+allDataColor[iObs], symsize=1.5
;    recognizePoint[0,*]=[allDataXY[iObs, 0]-recognizeRangeX,allDataXY[iObs, 1] -recognizeRangeY]
;    recognizePoint[1,*]=[allDataXY[iObs, 0]-recognizeRangeX,allDataXY[iObs, 1] +recognizeRangeY]
;    recognizePoint[2,*]=[allDataXY[iObs, 0]+recognizeRangeX,allDataXY[iObs, 1] +recognizeRangeY]
;    recognizePoint[3,*]=[allDataXY[iObs, 0]+recognizeRangeX,allDataXY[iObs, 1] -recognizeRangeY]
;    recognizePoint1=transpose(recognizePoint)
;    normRecognizePoint=convert_coord(recognizePoint1, /DATA, /TO_NORMAL)
;    normRecognizePoint=transpose(normRecognizePoint)
;    normRecognizePoint=normRecognizePoint[*, 0:1]
;    recognizePointPtr=ptr_new(normRecognizePoint, /NO_COPY)
;    recognizeRegionEdges[iobs]=recognizePointPtr
;    recognizeNames[iobs]=legNames[iobs]
;    recognizeValues[iobs]=strcompress(allDataXY(iObs, 0),/remove_all)+'/'+strcompress(allDataXY(iObs, 1),/remove_all)
;  endfor
;
;  rInfo = obj_new("RecognizeInfo", recognizeNames, recognizeValues, recognizeHighLight, recognizeRegionEdges)
;  plotInfo->setRecognizeInfo, rInfo
;
;END

PRO FM_PlotTable2, plotter, request, result
  !y.range=0
  plotter->wsetMainDataDraw
  modelInfo=request->getModelInfo()
  frequency=modelInfo.frequency
  DEVICE,DECOMPOSE=0
  LOADCT,39
  mytek_color;, 0, 32
  !p.background=255
  tpInfo=result->getGenericPlotInfo()
  plotInfo=result->getPlotInfo() 
  allDataXY=tpInfo->getXYS()
  if string(allDataXY[0]) eq 'AllNaN' then begin
    plot,indgen(10),/nodata,color=255,background=255
    xyouts,1,5,'No valid stations or groups selected',charsize=2,charthick=2,/data,color=0
    goto,jumpend
  endif
  allDataSymbol=tpInfo->getSymbols()
  allDataColors=tpInfo->getColors()
  legNames=tpInfo->getLegendNames()
  elabcode=request->getElaborationCode()
  psFact=plotter->getPSCharSizeFactor()
  allDataAxis=tpInfo->getLegendcolors()
  isGroupSelection=request->isGroupObsPresent()
  dims=get_screen_size(RESOLUTION=resolution)
  facSize=1
  parCodes=request->getParameterCodes()
  legsymbols=tpInfo->getLegendSymbols()
  hourStat=request->getGroupByTimeInfo() ;HourType
  flag_average=hourStat[0].value
  
  adummy=fltarr(10) & adummy(*)=1.
  CheckCriteria, request, result, 'OU', criteria, adummy,alpha,criteriaOrig,LV
  if criteria eq 0 then begin
    plot,indgen(10),/nodata,color=255,background=255
    xyouts,1,9,'No Diagram available for current choice',charsize=2,charthick=2,/data,color=0
    xyouts,2,7,'Check criteria availability for selected parameter',charsize=1.5,charthick=1,/data,color=0
    xyouts,2,6,'Only available for 8hmax O3 daily/yearly PM10 and hourly/yearly NO2',charsize=1.5,charthick=1,/data,color=0
    xyouts,2,5,'If group selected only worst statistics works',charsize=1.5,charthick=1,/data,color=0
    xyouts,2,4,'Might be no valid stations or groups selected',charsize=1.5,charthick=1,/data,color=0
    xyouts,2,3,'For annual MQO entire year should be selected',charsize=1.5,charthick=1,/data,color=0
    goto,jumpend
  endif
  
  if strupcase(frequency) eq 'HOUR' then begin  ;Hourly/daily values
  
    statis=['Mean','Exc','NMB','R','NMSD','Rspace','NMSDspace','RDE']
    nvar=8
    xmax=1 &  xmin=0 & ymin=0 & ymax=1
    plot, indgen(1),color=255,/nodata,xrange=[0,1],xstyle=1,position=plotter->getPosition(), noerase=plotter->getOverplotKeyword(0)
    
    plots,[xmin,xmax],[ymax,ymax],/data,thick=2,color=0
    plots,[xmin-0.05,xmax],[ymin+0.07,ymin+0.07],/data,thick=2,color=0
    plots,[xmin,xmin],[ymin+0.07,ymax],/data,thick=2,color=0
    plots,[xmax,xmax],[ymin+0.07,ymax],/data,thick=2,color=0
    plots,[xmin-0.05,xmin-0.05],[ymin+0.07,ymax-0.14],/data,thick=2,color=0
    
    plots,[xmin,xmax],[ymax-0.08,ymax-0.08],/data,thick=2,color=0
    plots,[xmin-0.05,xmax],[ymax-0.14,ymax-0.14],/data,thick=2,color=0
    
    plots,[xmin,xmin+0.15],[ymax-0.235,ymax-0.235],/data,thick=2,color=0
    plots,[xmin-0.05,xmax],[ymax-0.33,ymax-0.33],/data,thick=2,color=0
    plots,[xmin,xmin+0.15],[ymax-0.425,ymax-0.425],/data,thick=2,color=0
    plots,[xmin,xmin+0.15],[ymax-0.52,ymax-0.52],/data,thick=2,color=0
    plots,[xmin-0.05,xmax],[ymax-0.615,ymax-0.615],/data,thick=2,color=0
    plots,[xmin,xmin+0.15],[ymax-0.71,ymax-0.71],/data,thick=2,color=0
    plots,[xmin-0.05,xmax],[ymax-0.805,ymax-0.805],/data,thick=2,color=0
    
    plots,[xmin+0.10,xmin+0.10],[ymin+0.07,ymax-0.14],/data,thick=2,color=0
    plots,[xmin+0.15,xmin+0.15],[ymin+0.07,ymax-0.08],/data,thick=2,color=0
    
    xyouts,xmin+0.20,ymax-0.05,'SUMMARY STATISTICS',charsize=1.5*facsize*psFact,/data,charthick=2,color=0
    xyouts,xmin+0.52,ymax-0.05,'Nb of stations/groups: '+strtrim(allDataAxis(1),2)+' valid / '+strtrim(allDataAxis(0),2)+' selected',/data,charsize=1.0*facSize*psFact,color=0
    xyouts,xmin+0.01,ymax-0.12,'INDICATOR',/data,charsize=1.2*facSize*psFact,color=0,charthick=2.3
    
    minVal=[0,    0, -2, 0, -2,  0, -2,  0]
    maxVal=[100,100,  2, 2,  2,  2,  2,100]
    fillint1=['20',  '20',  '-1.2', '0.4','-1.2','0.4','-1.2','20']
    fillint2=['40',  '40',  '-0.4', '0.8','-0.4','0.8','-0.4','40']
    fillint3=['60',  '60',  ' 0.4', '1.2',' 0.4','1.2', '0.4','60']
    fillint4=['80',  '80',  ' 1.2', '1.6',' 1.2','1.6', '1.2','80']
    units=  ['ug/m3','days',  '%',  ' ',' ',' ',' ','%']
    
    recognizeRange=0.01
    recognizeHighLight=bytarr(8*n_elements(allDataSymbol))
    recognizeRegionEdges=ptrarr(8*n_elements(allDataSymbol)) ; coords (normalized standard)
    recognizeNames=strarr(8*n_elements(allDataSymbol))
    recognizeValues=strarr(8*n_elements(allDataSymbol))
    recognizePoint=fltarr(4,2)
    kg=0
    
    for ii=0,7 do begin
    
      if ii ge 2 and ii ne 7 then begin
        res1=strsplit(legnames(ii),' ',/extract)
        xyouts,xmin+0.01,ymax-0.18-ii*0.095,res1(0),/data,charsize=1.2*facSize*psFact,color=3,charthick=2.3
        xyouts,xmin+0.05,ymax-0.22-ii*0.095,res1(1),/data,charsize=1.0*facSize*psFact,color=3,charthick=2.3
      endif else begin
        xyouts,xmin+0.01,ymax-0.20-ii*0.095,legnames(ii),/data,charsize=1.2*facSize*psFact,color=3,charthick=2.3
      endelse
      xyouts,xmin+0.25,ymax-0.22-ii*0.095,strtrim(minVal(ii),2),/data,charsize=0.9*facSize*psFact,charthick=2.3,alignment=0.5,color=0
      xyouts,xmax-0.10,ymax-0.22-ii*0.095,strtrim(maxVal(ii),2),/data,charsize=0.9*facSize*psFact,charthick=2.3,alignment=0.5,color=0
      
      xyouts,xmin+0.25+(xmax-0.10-xmin-0.25)*.2,ymax-0.22-ii*0.095,fillint1(ii),/data,charsize=0.9*facSize*psFact,charthick=2.3,alignment=0.5,color=0
      xyouts,xmin+0.25+(xmax-0.10-xmin-0.25)*.4,ymax-0.22-ii*0.095,fillint2(ii),/data,charsize=0.9*facSize*psFact,charthick=2.3,alignment=0.5,color=0
      xyouts,xmin+0.25+(xmax-0.10-xmin-0.25)*.6,ymax-0.22-ii*0.095,fillint3(ii),/data,charsize=0.9*facSize*psFact,charthick=2.3,alignment=0.5,color=0
      xyouts,xmin+0.25+(xmax-0.10-xmin-0.25)*.8,ymax-0.22-ii*0.095,fillint4(ii),/data,charsize=0.9*facSize*psFact,charthick=2.3,alignment=0.5,color=0
      
      ;      if ii eq 2 then criteria=criteriaOU*200
      criteria=0
      if ii eq 2 or ii eq 3 or ii eq 4 or ii eq 5 or ii eq 6 then criteria=1.
      if ii eq 7 then criteria=50.
      
      fillValMin=[0,0,-criteria,0, -criteria,0, -criteria,    0]
      fillValMax=[0,0, criteria,1,  criteria,1,  criteria,criteria]
      
      xminfill=xmin+0.25+(xmax-0.10-xmin-0.25)*(fillValMin(ii)-minVal(ii))/(maxVal(ii)-minVal(ii))
      xmaxfill=xmin+0.25+(xmax-0.10-xmin-0.25)*(fillValMax(ii)-minVal(ii))/(maxVal(ii)-minVal(ii))
      if criteria gt 0 then polyfill,[xminfill,xminfill,xmaxfill,xmaxfill],[ymax-0.20-ii*0.095,ymax-0.16-ii*0.095,ymax-0.16-ii*0.095,ymax-0.20-ii*0.095],color=160,/data
      plots,[xmin+0.25,xmax-0.10],[ymax-0.20-ii*0.095,ymax-0.20-ii*0.095],/data,thick=2,color=0
      plots,[xmin+0.25,xmax-0.10],[ymax-0.16-ii*0.095,ymax-0.16-ii*0.095],/data,thick=2,color=0
      plots,[xmax-0.10,xmax-0.05],[ymax-0.20-ii*0.095,ymax-0.20-ii*0.095],/data,thick=2,color=0,linestyle=2
      plots,[xmax-0.10,xmax-0.05],[ymax-0.16-ii*0.095,ymax-0.16-ii*0.095],/data,thick=2,color=0,linestyle=2
      if ii le 1 or ii eq 3 or ii eq 5 or ii eq 7 then begin
        plots,[xmin+0.25,xmin+0.25],[ymax-0.20-ii*0.095,ymax-0.16-ii*0.095],/data,thick=2,color=0
      endif else begin
        plots,[xmin+0.2,xmin+0.25],[ymax-0.20-ii*0.095,ymax-0.20-ii*0.095],/data,thick=2,color=0,linestyle=2
        plots,[xmin+0.2,xmin+0.25],[ymax-0.16-ii*0.095,ymax-0.16-ii*0.095],/data,thick=2,color=0,linestyle=2
        plots,[xmin+0.2,xmin+0.2],[ymax-0.20-ii*0.095,ymax-0.16-ii*0.095],/data,thick=2,color=0,linestyle=2
      endelse
      plots,[xmax-0.05,xmax-0.05],[ymax-0.20-ii*0.095,ymax-0.16-ii*0.095],/data,thick=2,color=0,linestyle=2
      
      symsizesymbol=1
      
      for ip=0,n_elements(allDataSymbol)-1 do begin
        mypsym,allDataSymbol(ip),symsizesymbol
        xposDot=xmin+0.25+(xmax-0.10-xmin-0.25)*(allDataXY(ip,ii)-minVal(ii))/(maxVal(ii)-minVal(ii))
        cc=where(allDataXY(*,ii) gt maxVal(ii),countXpos)
        if allDataXY(ip,ii) gt maxVal(ii) gt 0 then xposDot=xmax-0.07
        if allDataXY(ip,ii) lt minVal(ii) gt 0 then xposDot=xmin+0.22
        plots,xposDot,ymax-0.18-ii*0.095,psym=8,color=allDataColors(ip),symsize=1,/data
        recognizePoint[0,*]=[xposDot-recognizeRange, ymax-0.18-ii*0.095-recognizeRange]
        recognizePoint[1,*]=[xposDot-recognizeRange, ymax-0.18-ii*0.095+recognizeRange]
        recognizePoint[2,*]=[xposDot+recognizeRange, ymax-0.18-ii*0.095+recognizeRange]
        recognizePoint[3,*]=[xposDot+recognizeRange, ymax-0.18-ii*0.095-recognizeRange]
        recognizePoint1=transpose(recognizePoint)
        normRecognizePoint=convert_coord(recognizePoint1, /DATA, /TO_NORMAL)
        normRecognizePoint=transpose(normRecognizePoint)
        normRecognizePoint=normRecognizePoint[*, 0:1]
        recognizePointPtr=ptr_new(normRecognizePoint, /NO_COPY)
        recognizeHighLight[kg]=0b
        recognizeRegionEdges[kg]=recognizePointPtr
        recognizeNames[kg]=legSymbols[ip]
        recognizeValues[kg]=strtrim(allDataXY(ip,ii), 2)
        kg++
        xyouts,xmax-0.08,ymax-0.22-ii*0.095,units(ii),/data,color=0,charsize=psFact
      endfor
      
      if ii eq 2 or ii eq 3 or ii eq 5 or ii eq 4 or ii eq 6 then begin
        cc=where(abs(allDataXY(*,ii)) le criteria,countC)
      endif
      if ii eq 7 then cc=where(abs(allDataXY(*,ii)) le 50.,countC)
      color_indic=250
      
      if (ii eq 2 or ii eq 3 or ii eq 4 or ii eq 7) and criteria gt 0 then begin
        if countC/float(allDataAxis(1)) lt 0.9 and countC/float(allDataAxis(1)) gt .75 then color_indic=210
        if countC/float(allDataAxis(1)) ge 0.9 then color_indic=160
        mypsym,9,1
        if isGroupSelection ne 1 then plots,xmin+0.12,ymax-0.18-ii*0.095,psym=8,color=color_indic,symsize=3,/data
      endif
      
      if (ii eq 5 or ii eq 6) and criteria gt 0 and n_elements(allDataSymbol) gt 1 then begin
        if countC/float(allDataAxis(1)) lt 0.9 and countC/float(allDataAxis(1)) gt .75 then color_indic=210
        if countC/float(allDataAxis(1)) ge 0.9 then color_indic=160
        mypsym,9,1
        if isGroupSelection ne 1 then plots,xmin+0.12,ymax-0.18-ii*0.095,psym=8,color=color_indic,symsize=3,/data
      endif 
      
      
    endfor
    
    xyouts,xmin-0.03,ymax-0.20,'O',/data,charsize=1.5*psFact,color=3,charthick=2
    xyouts,xmin-0.027,ymax-0.25,'B',/data,charsize=1.5*facsize*psFact,color=3,charthick=2
    xyouts,xmin-0.03,ymax-0.30,'S',/data,charsize=1.5*facsize*psFact,color=3,charthick=2
    
    xyouts,xmin-0.03,ymax-0.40,'T',/data,charsize=1.5*facsize*psFact,color=3,charthick=2
    xyouts,xmin-0.027,ymax-0.45,'I',/data,charsize=1.5*facsize*psFact,color=3,charthick=2
    xyouts,xmin-0.03,ymax-0.50,'M',/data,charsize=1.5*facsize*psFact,color=3,charthick=2
    xyouts,xmin-0.03,ymax-0.55,'E',/data,charsize=1.5*facsize*psFact,color=3,charthick=2
    ;  ;
    xyouts,xmin-0.03,ymax-0.66,'S',/data,charsize=1.5*facsize*psFact,color=3,charthick=2
    xyouts,xmin-0.03,ymax-0.69,'P',/data,charsize=1.5*facsize*psFact,color=3,charthick=2
    xyouts,xmin-0.03,ymax-0.72,'A',/data,charsize=1.5*facsize*psFact,color=3,charthick=2
    xyouts,xmin-0.03,ymax-0.75,'C',/data,charsize=1.5*facsize*psFact,color=3,charthick=2
    xyouts,xmin-0.03,ymax-0.78,'E',/data,charsize=1.5*facsize*psFact,color=3,charthick=2
    ;  ;
    xyouts,xmin-0.03,ymax-0.84,'A',/data,charsize=1.5*facsize*psFact,color=3,charthick=2
    xyouts,xmin-0.03,ymax-0.87,'Q',/data,charsize=1.5*facsize*psFact,color=3,charthick=2
    xyouts,xmin-0.03,ymax-0.90,'D',/data,charsize=1.5*facsize*psFact,color=3,charthick=2
    
    
  ;******************YEARLY*********************************************
    
  endif else begin  ;yearly values
  
    statis=['Mean','MNB','Rspace','MFSspace','RDE']
    xmax=1 &  xmin=0 & ymin=0 & ymax=1
    plot, indgen(1),color=255,/nodata,xrange=[0,1],xstyle=1,position=plotter->getPosition(), noerase=plotter->getOverplotKeyword(0)
    
    plots,[xmin,xmax],[ymax,ymax],/data,thick=2,color=0
    plots,[xmin-0.05,xmax],[ymin+0.07,ymin+0.07],/data,thick=2,color=0
    plots,[xmin,xmin],[ymin+0.07,ymax],/data,thick=2,color=0
    plots,[xmax,xmax],[ymin+0.07,ymax],/data,thick=2,color=0
    plots,[xmin-0.05,xmin-0.05],[ymin+0.07,ymax-0.14],/data,thick=2,color=0
    
    plots,[xmin,xmax],[ymax-0.08,ymax-0.08],/data,thick=2,color=0
    plots,[xmin-0.05,xmax],[ymax-0.14,ymax-0.14],/data,thick=2,color=0
    
    plots,[xmin-0.05,xmax],[ymax-0.30,ymax-0.30],/data,thick=2,color=0
    plots,[xmin-0.05,xmax],[ymax-0.46,ymax-0.46],/data,thick=2,color=0
    plots,[xmin,xmin+0.15],[ymax-0.62,ymax-0.62],/data,thick=2,color=0
    plots,[xmin-0.05,xmax],[ymax-0.78,ymax-0.78],/data,thick=2,color=0
    
    plots,[xmin+0.10,xmin+0.10],[ymin+0.07,ymax-0.14],/data,thick=2,color=0
    plots,[xmin+0.15,xmin+0.15],[ymin+0.07,ymax-0.08],/data,thick=2,color=0
    
    xyouts,xmin+0.10,ymax-0.05,'SUMMARY Yearly STATISTICS',charsize=1.5*facsize*psFact,/data,charthick=2,color=0
    xyouts,xmin+0.52,ymax-0.05,'Nb of stations/groups: '+strtrim(allDataAxis(1),2)+' valid / '+strtrim(allDataAxis(0),2)+' selected',/data,charsize=1.0*facsize*psFact,color=0
    xyouts,xmin+0.01,ymax-0.12,'INDICATOR',/data,charsize=1.2*facsize*psFact,color=0,charthick=2.3
    
    minVal=[0, -2, 0,-2,  0]
    maxVal=[100,2, 2, 2,100]
    fillint1=['20','-1.2','0.4','-1.2','20']
    fillint2=['40','-0.4','0.8','-0.4','40']
    fillint3=['60',' 0.4','1.2',' 0.4','60']
    fillint4=['80',' 1.2','1.6',' 1.2','80']
    units=['ug/m3','%',' ',' ','%']
    
    cyear=[0,2,5,6,7]
    legnames=reform(legnames(cyear))
    allDataXY=allDataXY(*,cyear)
    
    deltaY=0.17
    
    recognizeRange=0.01
    recognizeHighLight=bytarr(5*n_elements(allDataSymbol))
    recognizeRegionEdges=ptrarr(5*n_elements(allDataSymbol)) ; coords (normalized standard)
    recognizeNames=strarr(5*n_elements(allDataSymbol))
    recognizeValues=strarr(5*n_elements(allDataSymbol))
    recognizePoint=fltarr(4,2)
    kg=0
    
    for ii=0,4 do begin
    
      adummy=fltarr(10) & adummy(*)=1.
      CheckCriteria, request, result, 'OU', criteria, adummy,alpha,criteriaOrig,LV
      
      ;      if ii eq 1 then criteria=criteriaOU*200
      if ii eq 1 or ii eq 2 or ii eq 3 then criteria=1.
      if ii eq 4 then criteria=50.
      
      if ii ge 1 and ii ne 4 then begin
        res1=strsplit(legnames(ii),' ',/extract)
        xyouts,xmin+0.01,ymax-0.18-ii*deltaY,res1(0),/data,charsize=1.2*facsize*psFact,color=3,charthick=2.3
        xyouts,xmin+0.05,ymax-0.22-ii*deltaY,res1(1),/data,charsize=1.0*facsize*psFact,color=3,charthick=2.3
      endif else begin
        xyouts,xmin+0.01,ymax-0.20-ii*deltaY,legnames(ii),/data,charsize=1.2*facsize*psFact,color=3,charthick=2.3
      endelse
      xyouts,xmin+0.25,ymax-0.22-ii*deltaY,strtrim(minVal(ii),2),/data,charsize=0.9*facsize*psFact,charthick=2.3,alignment=0.5,color=0
      xyouts,xmax-0.10,ymax-0.22-ii*deltaY,strtrim(maxVal(ii),2),/data,charsize=0.9*facsize*psFact,charthick=2.3,alignment=0.5,color=0
      
      xyouts,xmin+0.25+(xmax-0.10-xmin-0.25)*.2,ymax-0.22-ii*deltaY,fillint1(ii),/data,charsize=0.9*facsize*psFact,charthick=2.3,alignment=0.5,color=0
      xyouts,xmin+0.25+(xmax-0.10-xmin-0.25)*.4,ymax-0.22-ii*deltaY,fillint2(ii),/data,charsize=0.9*facsize*psFact,charthick=2.3,alignment=0.5,color=0
      xyouts,xmin+0.25+(xmax-0.10-xmin-0.25)*.6,ymax-0.22-ii*deltaY,fillint3(ii),/data,charsize=0.9*facsize*psFact,charthick=2.3,alignment=0.5,color=0
      xyouts,xmin+0.25+(xmax-0.10-xmin-0.25)*.8,ymax-0.22-ii*deltaY,fillint4(ii),/data,charsize=0.9*facsize*psFact,charthick=2.3,alignment=0.5,color=0
      
      fillValMin=[0,-1,  0,   -1,    0]
      fillValMax=[0, 1,  1,    1,criteria]
      
      xminfill=xmin+0.25+(xmax-0.10-xmin-0.25)*(fillValMin(ii)-minVal(ii))/(maxVal(ii)-minVal(ii))
      xmaxfill=xmin+0.25+(xmax-0.10-xmin-0.25)*(fillValMax(ii)-minVal(ii))/(maxVal(ii)-minVal(ii))
      if criteria gt 0 then polyfill,[xminfill,xminfill,xmaxfill,xmaxfill],[ymax-0.20-ii*deltaY,ymax-0.16-ii*deltaY,ymax-0.16-ii*deltaY,ymax-0.20-ii*deltaY],color=160,/data
      plots,[xmin+0.25,xmax-0.10],[ymax-0.20-ii*deltaY,ymax-0.20-ii*deltaY],/data,thick=2,color=0
      plots,[xmin+0.25,xmax-0.10],[ymax-0.16-ii*deltaY,ymax-0.16-ii*deltaY],/data,thick=2,color=0
      if ii eq 0 or ii eq 2 or ii eq 4 then begin
        plots,[xmin+0.25,xmin+0.25],[ymax-0.20-ii*deltaY,ymax-0.16-ii*deltaY],/data,thick=2,color=0
      endif else begin
        plots,[xmin+0.2,xmin+0.25],[ymax-0.20-ii*deltaY,ymax-0.20-ii*deltaY],/data,thick=2,color=0,linestyle=2
        plots,[xmin+0.2,xmin+0.25],[ymax-0.16-ii*deltaY,ymax-0.16-ii*deltaY],/data,thick=2,color=0,linestyle=2
        plots,[xmin+0.2,xmin+0.2],[ymax-0.20-ii*deltaY,ymax-0.16-ii*deltaY],/data,thick=2,color=0,linestyle=2
      endelse
      plots,[xmax-0.10,xmax-0.05],[ymax-0.20-ii*deltaY,ymax-0.20-ii*deltaY],/data,thick=2,color=0,linestyle=2
      plots,[xmax-0.10,xmax-0.05],[ymax-0.16-ii*deltaY,ymax-0.16-ii*deltaY],/data,thick=2,color=0,linestyle=2
      plots,[xmax-0.05,xmax-0.05],[ymax-0.20-ii*deltaY,ymax-0.16-ii*deltaY],/data,thick=2,color=0,linestyle=2
      
      symsizesymbol=1
      
      for ip=0,n_elements(allDataSymbol)-1 do begin
        mypsym,allDataSymbol(ip),symsizesymbol
        xposDot=xmin+0.25+(xmax-0.10-xmin-0.25)*(allDataXY(ip,ii)-minVal(ii))/(maxVal(ii)-minVal(ii))
        cc=where(allDataXY(*,ii) gt maxVal(ii),countXpos)
        if allDataXY(ip,ii) gt maxVal(ii) gt 0 then xposDot=xmax-0.07
        if allDataXY(ip,ii) lt minVal(ii) gt 0 then xposDot=xmin+0.22
        plots,xposDot,ymax-0.18-ii*deltaY,psym=8,color=allDataColors(ip),symsize=1,/data
        
        recognizePoint[0,*]=[xposDot-recognizeRange, ymax-0.18-ii*deltaY-recognizeRange]
        recognizePoint[1,*]=[xposDot-recognizeRange, ymax-0.18-ii*deltaY+recognizeRange]
        recognizePoint[2,*]=[xposDot+recognizeRange, ymax-0.18-ii*deltaY+recognizeRange]
        recognizePoint[3,*]=[xposDot+recognizeRange, ymax-0.18-ii*deltaY-recognizeRange]
        recognizePoint1=transpose(recognizePoint)
        normRecognizePoint=convert_coord(recognizePoint1, /DATA, /TO_NORMAL)
        normRecognizePoint=transpose(normRecognizePoint)
        normRecognizePoint=normRecognizePoint[*, 0:1]
        recognizePointPtr=ptr_new(normRecognizePoint, /NO_COPY)
        recognizeHighLight[kg]=0b
        recognizeRegionEdges[kg]=recognizePointPtr
        recognizeNames[kg]=legSymbols[ip]
        recognizeValues[kg]=strtrim(allDataXY(ip,ii), 2)
        xyouts,xmax-0.082,ymax-0.22-ii*deltaY,units(ii),/data,color=0,charsize=psFact
        kg++
      endfor
      
      countC=0
      if ii eq 1 or ii eq 2 or ii eq 3 then cc=where(abs(allDataXY(*,ii)) le criteria,countC)
      ;      if ii eq 2 then cc=where(abs(allDataXY(*,ii)) ge criteria,countC)
      if ii eq 4 then cc=where(abs(allDataXY(*,ii)) le criteria,countC)
      color_indic=250
      if countC/float(allDataAxis(1)) lt 0.9 and countC/float(allDataAxis(1)) gt .75 then color_indic=210
      if countC/float(allDataAxis(1)) ge 0.9 then color_indic=160
      mypsym,9,1
      if ii ge 1 and criteria gt 0 then begin
        if isGroupSelection ne 1 then plots,xmin+0.12,ymax-0.18-ii*deltaY,psym=8,color=color_indic,symsize=3,/data
      endif
    endfor
    
    xyouts,xmin-0.03,ymax-0.20,'O',/data,charsize=1.5*facsize*psFact,color=3,charthick=2
    xyouts,xmin-0.027,ymax-0.24,'B',/data,charsize=1.5*facsize*psFact,color=3,charthick=2
    xyouts,xmin-0.03,ymax-0.28,'S',/data,charsize=1.5*facsize*psFact,color=3,charthick=2
    
    xyouts,xmin-0.03,ymax-0.345,'T',/data,charsize=1.5*facsize*psFact,color=3,charthick=2
    xyouts,xmin-0.027,ymax-0.38,'I',/data,charsize=1.5*facsize*psFact,color=3,charthick=2
    xyouts,xmin-0.03,ymax-0.415,'M',/data,charsize=1.5*facsize*psFact,color=3,charthick=2
    xyouts,xmin-0.03,ymax-0.45,'E',/data,charsize=1.5*facsize*psFact,color=3,charthick=2
    ;  ;
    xyouts,xmin-0.03,ymax-0.53,'S',/data,charsize=1.5*facsize*psFact,color=3,charthick=2
    xyouts,xmin-0.03,ymax-0.58,'P',/data,charsize=1.5*facsize*psFact,color=3,charthick=2
    xyouts,xmin-0.03,ymax-0.63,'A',/data,charsize=1.5*facsize*psFact,color=3,charthick=2
    xyouts,xmin-0.03,ymax-0.68,'C',/data,charsize=1.5*facsize*psFact,color=3,charthick=2
    xyouts,xmin-0.03,ymax-0.73,'E',/data,charsize=1.5*facsize*psFact,color=3,charthick=2
    ;  ;
    xyouts,xmin-0.03,ymax-0.83,'A',/data,charsize=1.5*facsize*psFact,color=3,charthick=2
    xyouts,xmin-0.03,ymax-0.87,'Q',/data,charsize=1.5*facsize*psFact,color=3,charthick=2
    xyouts,xmin-0.03,ymax-0.91,'D',/data,charsize=1.5*facsize*psFact,color=3,charthick=2
    
  endelse
  
  rInfo = obj_new("RecognizeInfo", recognizeNames, recognizeValues, recognizeHighLight, recognizeRegionEdges)
  plotInfo->setRecognizeInfo, rInfo
  
  jumpend:
  
END

PRO FM_PlotTargetLegend, plotter, request, result
  targetInfo=result->getGenericPlotInfo()
  allDataXY=targetInfo->getXYS()
  if string(allDataXY[0]) eq 'AllNaN' then goto,jumpend
  legendGenericBuild,request,result,plotter
  legendInfo,request,result,plotter
  jumpend:
END

PRO FM_PlotSoccerLegend, plotter, request, result

  legendGenericBuild,request,result,plotter
  legendInfo,request,result,plotter
  
END

PRO FM_PlotBugle, plotter, request, result, allDataXY, allDataColor, allDataSymbol

  !y.range=0
  plotter->wsetMainDataDraw
  DEVICE,DECOMPOSE=0
  LOADCT,39
  mytek_color;, 0, 32
  tpInfo=result->getGenericPlotInfo() 
  allDataXY=tpInfo->getXYS()
  if string(allDataXY[0]) eq 'AllNaN' then begin
    plot,indgen(10),/nodata,color=255,background=255
    xyouts,1,5,'No valid stations or groups selected',charsize=2,charthick=2,/data,color=0
    goto,jumpend
  endif
  allDataSymbol=tpInfo->getSymbols()
  allDataColor=tpInfo->getColors()
  elabName=request->getelaborationName()
  elabcode=request->getElaborationCode()
  plotInfo=result->getPlotInfo()
  legNames=tpInfo->getLegendNames()
  nobs=request->getSingleObsNumber()
  npar=request->getParameterNumber()
  nmod=request->getModelNumber()
  obsNames=request->getSingleObsNames()
  nobs=n_elements(allDataXY(*,0))
  nMulti=nobs*npar*nmod   ;8
  recognizeHighLight=bytarr(nobs)   ; 8
  recognizeRegionEdges=ptrarr(nobs)
  recognizeNames=strarr(nobs)
  recognizeValues=strarr(nobs)
  recognizePoint=fltarr(4,2)
  npoints=n_elements(allDataXY(*,0))
  adummy=fltarr(10) & adummy(*)=1.
  
;  if elabcode eq 16 then begin  ;NMB
;  
;    CheckCriteria, request, result, 'OU', criteria, adummy, 0,alpha,criteriaOrig,LV,nobsAv
;    maxxAxis=max(allDataXY(*,0),/nan)*1.1
;    if finite(maxxAxis) eq 0 then maxxAxis=100
;    minxAxis=0
;    minyAxis=-200
;    maxyAxis=200
;    
;    ymax=max([max(abs(alldataXY(*,1)),/nan)*1.2,1.1*maxxaxis])
;    recognizeRangeX=(maxxAxis-minxAxis)*0.01
;    recognizeRangeY=(ymax+ymax)*0.01
;    
;    plot, indgen(2), color=0,/nodata, xtitle='2*OU',ytitle='Mean Bias', title='Buggle PLOT', charsize=1, background=255,$
;      yrange=[-ymax,ymax],xrange=[minxAxis,maxxAxis],xstyle=1,ystyle=1, position=plotter->getPosition(), noerase=plotter->getOverplotKeyword(0)
;    if criteria gt 0 then begin
;      polyfill,[0,maxxaxis,maxxaxis,0],[0,maxxaxis,-maxxaxis,0],/data,color=160
;    endif
;    plots,[0,maxxaxis],[0,0],color=0,/data
;    for iObs=0, npoints-1 do begin
;      mypsym,allDataSymbol[iObs],1
;      plots, allDataXY[iObs, 0], allDataXY[iObs, 1], psym=8, color=2+allDataColor[iObs], symsize=1.5
;      recognizePoint[0,*]=[allDataXY[iObs, 0]-recognizeRangeX,allDataXY[iObs, 1] -recognizeRangeY]
;      recognizePoint[1,*]=[allDataXY[iObs, 0]-recognizeRangeX,allDataXY[iObs, 1] +recognizeRangeY]
;      recognizePoint[2,*]=[allDataXY[iObs, 0]+recognizeRangeX,allDataXY[iObs, 1] +recognizeRangeY]
;      recognizePoint[3,*]=[allDataXY[iObs, 0]+recognizeRangeX,allDataXY[iObs, 1] -recognizeRangeY]
;      recognizePoint1=transpose(recognizePoint)
;      normRecognizePoint=convert_coord(recognizePoint1, /DATA, /TO_NORMAL)
;      normRecognizePoint=transpose(normRecognizePoint)
;      normRecognizePoint=normRecognizePoint[*, 0:1]
;      recognizePointPtr=ptr_new(normRecognizePoint, /NO_COPY)
;      recognizeRegionEdges[iobs]=recognizePointPtr
;      recognizeNames[iobs]=legNames[iobs]
;      recognizeValues[iobs]=strcompress(allDataXY(iObs, 0),/remove_all)+'/'+strcompress(allDataXY(iObs, 1),/remove_all)
;    endfor
;    
;  endif
  
  if elabcode eq 25 or elabCode eq 79 or elabCode eq 32 then begin  ;NMSD
  
    CheckCriteria, request, result, 'OU', criteria, adummy,alpha,criteriaOrig,LV
    
    maxxAxis=max(allDataXY(*,0),/nan)*1.1
    if finite(maxxAxis) eq 0 then maxxAxis=100
    minxAxis=0
    minyAxis=-200
    maxyAxis=200
    
    ymax=max([max(abs(alldataXY(*,1)),/nan)*1.2,1.5])
    recognizeRangeX=(maxxAxis-minxAxis)*0.01
    recognizeRangeY=(ymax+ymax)*0.01
    
    plot, indgen(fix(maxxaxis+1)), color=0,/nodata, xtitle='RMSU / SigO',ytitle='NMSD', title='MPC PLOT', charsize=1, background=255,$
      yrange=[-ymax,ymax],xrange=[minxAxis,maxxAxis],xstyle=1,ystyle=1, position=plotter->getPosition(), noerase=plotter->getOverplotKeyword(0)
    if criteria gt 0 then begin
      xx=fltarr(101) & yy1=fltarr(101) & yy2=fltarr(101) & yy1_05=fltarr(101) & yy2_05=fltarr(101)
      for i=0,100 do begin
        xx(i)=minxAxis+i*(maxxAxis-minxAxis)/100.
        yy1(i)= min([ 200.*xx(i), ymax])
        yy2(i)= max([-200.*xx(i),-ymax])
        yy1_05(i)= min([ 100.*xx(i), ymax])
        yy2_05(i)= max([-100.*xx(i),-ymax])
      endfor
      polyfill,[xx,reverse(xx)],[yy2,reverse(yy1)],/data,color=160
      oplot,xx,yy2_05,linestyle=2,thick=2,color=0
      oplot,xx,yy1_05,linestyle=2,thick=2,color=0
      oplot,xx,yy2,thick=2,color=0
      oplot,xx,yy1,thick=2,color=0
    endif
    plots,[0,maxxaxis],[0,0],color=0,/data
    for iObs=0, npoints-1 do begin
      mypsym,allDataSymbol[iObs],1
      plots, allDataXY[iObs, 0], allDataXY[iObs, 1], psym=8, color=2+allDataColor[iObs], symsize=1.5
      recognizePoint[0,*]=[allDataXY[iObs, 0]-recognizeRangeX,allDataXY[iObs, 1] -recognizeRangeY]
      recognizePoint[1,*]=[allDataXY[iObs, 0]-recognizeRangeX,allDataXY[iObs, 1] +recognizeRangeY]
      recognizePoint[2,*]=[allDataXY[iObs, 0]+recognizeRangeX,allDataXY[iObs, 1] +recognizeRangeY]
      recognizePoint[3,*]=[allDataXY[iObs, 0]+recognizeRangeX,allDataXY[iObs, 1] -recognizeRangeY]
      recognizePoint1=transpose(recognizePoint)
      normRecognizePoint=convert_coord(recognizePoint1, /DATA, /TO_NORMAL)
      normRecognizePoint=transpose(normRecognizePoint)
      normRecognizePoint=normRecognizePoint[*, 0:1]
      recognizePointPtr=ptr_new(normRecognizePoint, /NO_COPY)
      recognizeRegionEdges[iobs]=recognizePointPtr
      recognizeNames[iobs]=legNames[iobs]
      recognizeValues[iobs]=strcompress(allDataXY(iObs, 0),/remove_all)+'/'+strcompress(allDataXY(iObs, 1),/remove_all)
    endfor
    
  endif
  
  if elabcode eq 15 or elabCode eq 78 or elabCode eq 16 then begin  ;R
  
    CheckCriteria, request, result, 'OU', criteria, adummy,alpha,criteriaOrig,LV
    
    maxxAxis=max(allDataXY(*,0),/nan)*1.1
    if finite(maxxAxis) eq 0 then maxxAxis=1
    minxAxis=0
    minyAxis=-1
    maxyAxis=1
    
    ymax=1.
    ymin=min([ 0,min(allDataXY(*, 1))])
    recognizeRangeX=(maxxAxis-minxAxis)*0.01
    recognizeRangeY=(ymax)*0.01
    
    plot, indgen(fix(maxxaxis+1)), color=0,/nodata, xtitle='RMSU / sigO',ytitle='Correlation', title='MPC PLOT', charsize=1, background=255,$
      yrange=[ymin,ymax],xrange=[minxAxis,maxxAxis],xstyle=1,ystyle=1, position=plotter->getPosition(), noerase=plotter->getOverplotKeyword(0)
    if criteria gt 0 then begin
      xx=fltarr(101) & yy=fltarr(101) & yy05=fltarr(101)
      for i=0,100 do begin
        xx(i)=minxAxis+i*(maxxAxis-minxAxis)/100.
        yy(i)=1.-2.*xx(i)^2
        yy05(i)=1.-0.5*xx(i)^2
        if yy(i) lt ymin then yy(i)=ymin
      endfor
      polyfill,[xx,xx(100),xx(0)],[yy,ymax,ymax],/data,color=160
      oplot,xx,yy,color=0,thick=2
      oplot,xx,yy05,color=0,thick=2,linestyle=2
    endif
    
    for iObs=0, npoints-1 do begin
      mypsym,allDataSymbol[iObs],1
      plots, allDataXY[iObs, 0], allDataXY[iObs, 1], psym=8, color=2+allDataColor[iObs], symsize=1.5
      recognizePoint[0,*]=[allDataXY[iObs, 0]-recognizeRangeX,allDataXY[iObs, 1] -recognizeRangeY]
      recognizePoint[1,*]=[allDataXY[iObs, 0]-recognizeRangeX,allDataXY[iObs, 1] +recognizeRangeY]
      recognizePoint[2,*]=[allDataXY[iObs, 0]+recognizeRangeX,allDataXY[iObs, 1] +recognizeRangeY]
      recognizePoint[3,*]=[allDataXY[iObs, 0]+recognizeRangeX,allDataXY[iObs, 1] -recognizeRangeY]
      recognizePoint1=transpose(recognizePoint)
      normRecognizePoint=convert_coord(recognizePoint1, /DATA, /TO_NORMAL)
      normRecognizePoint=transpose(normRecognizePoint)
      normRecognizePoint=normRecognizePoint[*, 0:1]
      recognizePointPtr=ptr_new(normRecognizePoint, /NO_COPY)
      recognizeRegionEdges[iobs]=recognizePointPtr
      recognizeNames[iobs]=legNames[iobs]
      recognizeValues[iobs]=strcompress(allDataXY(iObs, 0),/remove_all)+'/'+strcompress(allDataXY(iObs, 1),/remove_all)
    endfor
    
  endif
  
  rInfo = obj_new("RecognizeInfo", recognizeNames, recognizeValues, recognizeHighLight, recognizeRegionEdges)
  plotInfo->setRecognizeInfo, rInfo
  
  jumpend:
  
END

PRO FM_PlotBugleLegend, plotter, request, result
  targetInfo=result->getGenericPlotInfo()
  allDataXY=targetInfo->getXYS()
  if string(allDataXY[0]) eq 'AllNaN' then goto,jumpend
  legendGenericBuild,request,result,plotter
  legendInfo,request,result,plotter
  jumpend:
END

PRO FM_PlotQQ, plotter, request, result
  !y.range=0
  plotter->wsetMainDataDraw
  plotInfo=result->getPlotInfo()
  DEVICE,DECOMPOSE=0
  LOADCT,39
  ; use "tek" color table...
  tek_color;, 0, 32
  tpInfo=result->getGenericPlotInfo()
  targetInfo=result->getGenericPlotInfo()
  legNames=targetInfo->getLegendNames()  ;long name
  allDataXY=tpInfo->getXYS()
  if string(allDataXY[0]) eq 'AllNaN' then begin
    plot,indgen(10),/nodata,color=255,background=255
    xyouts,1,5,'No valid stations or groups selected',charsize=2,charthick=2,/data,color=0
    goto,jumpend
  endif
  allDataSymbol=tpInfo->getSymbols()
  allDataColor=tpInfo->getColors()
  maxAxis=max(allDataXY,/nan)*1.2
  minAxis=min([0,min(allDataXY,/nan)])
  if finite(maxAxis) eq 0 then maxAxis=100.
  if finite(minAxis) eq 0 then minAxis=0.
  ;print, maxAxis > 2
  
  plot, indgen(fix(maxaxis)), color=0,/nodata, xtitle='OBSERVED',ytitle='MODELLED', title='QQ PLOT', charsize=1, background=255,xrange=[minAxis,maxAxis],$
    yrange=[minAxis,maxAxis],xstyle=1,ystyle=1, position=plotter->getPosition(), noerase=plotter->getOverplotKeyword(0)
    
  plots,[minAxis,maxAxis],[minAxis,maxAxis],color=0,/data
  oplot,indgen(fix(maxaxis)),color=0
  oplot,indgen(fix(maxaxis))/2.,color=0,linestyle=2
  oplot,indgen(fix(maxaxis))*2,color=0,linestyle=2
  
  size_alldataXY=size(allDataXY)
  if size_alldataXY(0) eq 2 then begin
    nObs=n_elements(allDataColor)
    
    for iObs=0, nObs-1 do begin
      mypsym,allDataSymbol[iObs],1
      plots, allDataXY[iObs, 0], allDataXY[iObs, 1], psym=8, color=2+allDataColor[iObs], symsize=1.5
      
    ;    xyouts, allDataXY[iObs, 0], allDataXY[iObs, 1], allDataSymbol[iObs], color=2+allDataColor[iObs], /data, charthick=3, charsize=1.2, align=0.5
    endfor
  endif else begin
    nObs_param=size_alldataXY(1)
    nvalues=size_alldataXY(2)
    recognizeRange=(maxAxis-minAxis)*0.01
    recognizeHighLight=bytarr(nvalues*nobs_param)
    recognizeRegionEdges=ptrarr(nvalues*nobs_param) ; coords (normalized standard)
    recognizeNames=strarr(nvalues*nobs_param)
    recognizeValues=strarr(nvalues*nobs_param)
    
    for iVal=0, nvalues-1 do begin
      for iobs=0, nObs_param-1 do begin
        mypsym,allDataSymbol[iObs],1
        plots, allDataXY[iObs,ival, 0], allDataXY[iObs,ival, 1], psym=8, color=2+allDataColor[iObs], symsize=1.5
        
        recognizePoint=fltarr(4,2)
        ; first point lower left corner, clockwise!
        recognizePoint[0,*]=[allDataXY[iObs,ival, 0]-recognizeRange, allDataXY[iObs,ival, 1]-recognizeRange]
        recognizePoint[1,*]=[allDataXY[iObs,ival, 0]-recognizeRange, allDataXY[iObs,ival, 1]+recognizeRange]
        recognizePoint[2,*]=[allDataXY[iObs,ival, 0]+recognizeRange, allDataXY[iObs,ival, 1]+recognizeRange]
        recognizePoint[3,*]=[allDataXY[iObs,ival, 0]+recognizeRange, allDataXY[iObs,ival, 1]-recognizeRange]
        
        recognizePoint=transpose(recognizePoint)
        normRecognizePoint=convert_coord(recognizePoint, /DATA, /TO_NORMAL)
        normRecognizePoint=transpose(normRecognizePoint)
        normRecognizePoint=normRecognizePoint[*, 0:1]
        recognizePointPtr=ptr_new(normRecognizePoint, /NO_COPY)
        acc=ival*nobs_param+iobs
        recognizeHighLight[acc]=0b
        recognizeRegionEdges[acc]=recognizePointPtr
        recognizeNames[acc]=legNames[iObs]
        recognizeValues[acc]=strtrim(allDataXY[iObs,ival, 0], 2)+'/'+strtrim(allDataXY[iObs,ival, 1], 2)
      endfor
    endfor
    rInfo = obj_new("RecognizeInfo", recognizeNames, recognizeValues, recognizeHighLight, recognizeRegionEdges)
    plotInfo->setRecognizeInfo, rInfo
  endelse
  
  jumpend:
  
END

PRO FM_PlotQQLegend, plotter, request, result

  targetInfo=result->getGenericPlotInfo()
  allDataXY=targetInfo->getXYS()
  if string(allDataXY[0]) eq 'AllNaN' then goto,jumpend
  plotter->wsetInfoDataDraw
  white=obj_new('Color', 200, 200, 200)
  black=obj_new('Color', 0, 0, 0)
  whiteL=white->AsLongTrueColor()
  blackL=black->AsLongTrueColor()
  obj_destroy, white
  obj_destroy, black
  DEVICE, DECOMPOSED=1
  plotter->erase, whiteL
  
  DEVICE,DECOMPOSE=0
  LOADCT,39
  ; use "tek" color table...
  tek_color;, 0, 32
  
  legoWidth=.012
  legoHeight=.05
  startX=.01
  maxWidth=0
  
  targetInfo=Result->getGenericPlotInfo()
  colors=targetInfo->getColors()
  symbols=targetInfo->getSymbols()
  legColors=targetInfo->getLegendColors()
  legNames=targetInfo->getLegendNames()
  legSyms=targetInfo->getLegendSymbols()
  legoSequenceNo=n_elements(legNames)
  modelCodes=request->getModelCodes()
  
  symbolSequenceNo=n_elements(legNames)
  
  allDataXY=targetInfo->getXYS()
  
  symbolSequenceNo=n_elements(legNames)
  
  ind=0
  if n_elements(modelCodes) gt 1 then begin ; many models
    for i=0, symbolSequenceNo-1 do begin
      jheight = i MOD 9
      startx = .10*fix(i/9)
      startY=1.-((jheight+1)*legoHeight*2)
      thisStartX=startX+maxWidth+legoWidth+.02
      lego=[[thisStartX,startY], [thisStartX,startY+legoHeight], [thisStartX+legoWidth,startY+legoHeight], [thisStartX+legoWidth,startY], [thisStartX,startY]]
      mypsym,legSyms[i],1
      coords=[thisStartX+(legoWidth/2), startY+.002]
      coords=plotter->legendNormalize(coords)
      plots, coords[0], coords[1], psym=8, color=2+legColors[i], /NORM, symsize=1.
      coords=[thisStartX+legoWidth+.01, startY+.002]
      coords=plotter->legendNormalize(coords)
      xyouts, coords[0], coords[1], strmid(legNames[i],0,7), COLOR=0, /NORM, charsize=.8, charthick=.8,  WIDTH=textWidth
    endfor
    
  endif else begin
    for i=0, symbolSequenceNo-1 do begin ;only one model
      if finite(alldataXY(i,0)) eq 1 and finite(allDataXy(i,1)) eq 1 and ind le 62 then begin
        jheight = ind MOD 9
        startx = .10*fix(ind/9)
        startY=1.-((jheight+1)*legoHeight*2)
        thisStartX=startX+maxWidth+legoWidth+.02
        lego=[[thisStartX,startY], [thisStartX,startY+legoHeight], [thisStartX+legoWidth,startY+legoHeight], [thisStartX+legoWidth,startY], [thisStartX,startY]]
        mypsym,legSyms[i],1
        coords=[thisStartX+(legoWidth/2), startY+.002]
        coords=plotter->legendNormalize(coords)
        plots, coords[0], coords[1], psym=8, color=2+legColors[i], /NORM, symsize=1.
        coords=[thisStartX+legoWidth+.01, startY+.002]
        coords=plotter->legendNormalize(coords)
        xyouts, coords[0], coords[1], strmid(legNames[i],0,7), COLOR=0, /NORM, charsize=.8, charthick=.8,  WIDTH=textWidth
        ind++
      endif
    endfor
  endelse
  legendInfo,request,result,plotter
  jumpend:
END

PRO FM_PlotTable2legend, plotter, request, result

  plotter->wsetInfoDataDraw
  psFact=plotter->getPSCharSizeFactor()
  resPoscript=plotter->currentDeviceIsPostscript()
  white=obj_new('Color', 200, 200, 200)
  black=obj_new('Color', 0, 0, 0)
  whiteL=white->AsLongTrueColor()
  blackL=black->AsLongTrueColor()
  obj_destroy, white
  obj_destroy, black
  DEVICE, DECOMPOSED=1
  plotter->erase, whiteL
  
  DEVICE,DECOMPOSE=0
  LOADCT,39
  ; use "tek" color table...
  mytek_color;, 0, 32
  
  legoWidth=.012
  legoHeight=.05
  startX=.01
  maxWidth=0
  
  targetInfo=Result->getGenericPlotInfo()
  allDataXY=targetInfo->getXYS()
  if string(allDataXY[0]) eq 'AllNaN' then goto,jumpend
  cc=where(finite(allDataXY(*,0)) eq 1, countCC)
  if countCC eq 0 then goto,jumpEnd
  legcolorsPrint=targetInfo->getColors()
  symbols=targetInfo->getSymbols()
  isGroupSelection=request->isGroupObsPresent()
  legSyms=targetInfo->getLegendSymbols()
  
  ;  ccfin=where(symbols eq 13,countGR)
  
  ;  if countGR gt 0 then begin
  legNamesPrint=strmid(legSyms,0,7)
  legoSequenceNo=n_elements(legNamesPrint)
  symbolSequenceNo=n_elements(legNamesPrint)
  
  for i=0, min([62,symbolSequenceNo-1]) do begin
    jheight = i MOD 9
    startx = .10*fix(i/9)
    if resPoscript then startx = startx+0.1
    startY=1.-((jheight+1)*legoHeight*2)
    thisStartX=startX+maxWidth+legoWidth+.02
    lego=[[thisStartX,startY], [thisStartX,startY+legoHeight], [thisStartX+legoWidth,startY+legoHeight], [thisStartX+legoWidth,startY], [thisStartX,startY]]
    mypsym,symbols[i],1
    coords=[thisStartX+(legoWidth/2), startY+.002]
    coords=plotter->legendNormalize(coords)
    plots, coords[0], coords[1], psym=8, color=legColorsPrint[i], /NORM, symsize=1.
    coords=[thisStartX+legoWidth+.01, startY+.002]
    coords=plotter->legendNormalize(coords)
    xyouts, coords[0], coords[1] , legNamesPrint[i], COLOR=0, /NORM, charsize=.8, charthick=.8,  WIDTH=textWidth
  endfor
  
  jumpEnd:

END


PRO LEGENDINFO,request,result,plotter

  elabCode=request->getelaborationCode()
  psFact=plotter->getPSCharSizeFactor()
  resPoscript=plotter->currentDeviceIsPostscript()
  
  coord1=[0.75,0]
  coord1=plotter->legendNormalize(coord1)
  coord2=[0.75,1]
  if resPoscript then coord2=[0.75,0.75]
  coord2=plotter->legendNormalize(coord2)
  
  plots,[coord1[0], coord2[0]],[coord1[1], coord2[1]],/normal,thick=2,color=0
  
  if ~plotter->currentDeviceIsPostscript() then dims=get_screen_size(RESOLUTION=resolution) else resolution=0.035
  facSize=resolution[0]/0.035
  
  startIndex=request->getStartIndex()
  endIndex=request->getEndIndex()
  coords=[0.77,0.9]
  if resPoscript then coords(1)=coords(1)-0.15
  coords=plotter->legendNormalize(coords)
  xyouts,coords[0],coords[1],'Strt/end Ind: '+ strtrim(startIndex+1,2)+'-'+strtrim(endIndex+1,2),/normal,charsize=facSize*psFact,color=0
  obsNames=request->getSingleObsNames()
  if n_elements(obsNames) eq 1 then begin
    coords=[0.77,0.8]
    if resPoscript then coords(1)=coords(1)-0.13
    coords=plotter->legendNormalize(coords)
    xyouts,coords[0],coords[1],'Station: '+ strtrim(obsNames,2),/normal,charsize=facSize*psFact,color=0
  endif
  modelCodes=request->getModelCodes()
  if n_elements(modelCodes) eq 1 then begin
    coords=[0.77,0.7]
    if resPoscript then coords(1)=coords(1)-0.11
    coords=plotter->legendNormalize(coords)
    xyouts,coords[0],coords[1],'Model (s): '+ modelCodes,/normal,charsize=facSize*psFact,color=0
  endif
  parCodes=request->getParameterCodes()
  if n_elements(parCodes) eq 1 then begin
    coords=[0.77,0.6]
    if resPoscript then coords(1)=coords(1)-0.09
    coords=plotter->legendNormalize(coords)
    xyouts,coords[0],coords[1],'Parameter: '+ parCodes,/normal,charsize=facSize*psFact,color=0
  endif
  scenarioCodes=request->getScenarioCodes()
  if n_elements(scenarioCodes) eq 1 then begin
    coords=[0.77,0.5]
    if resPoscript then coords(1)=coords(1)-0.07
    coords=plotter->legendNormalize(coords)
    xyouts,coords[0],coords[1],'Scen: '+ scenarioCodes,/normal,charsize=facSize*psFact,color=0
  endif
  extraValNumber=request->getExtraValuesNumber()
  if extraValNumber gt 0 then extraVal=request->getExtraValues()
  if extraValNumber eq 0 then extraValPrint='No'
  if extraValNumber eq 1 then extraValPrint=strmid(strtrim(extraVal(0),2),0,3)
  if extraValNumber eq 2 then extraValPrint=strmid(strtrim(extraVal(0),2),0,3)+'/'+strmid(strtrim(extraVal(1),2),0,3)
  if extraValNumber eq 3 then extraValPrint=strmid(strtrim(extraVal(0),2),0,3)+'/'+$
    strmid(strtrim(extraVal(1),2),0,3)+'/'+$
    strmid(strtrim(extraVal(2),2),0,3)
  coords=[0.77,0.4]
  if resPoscript then coords(1)=coords(1)-0.05
  coords=plotter->legendNormalize(coords)
  xyouts,coords[0],coords[1],'Extra Values: '+ extraValPrint,/normal,charsize=facSize*psFact,color=0
  
  ysw=request->getseasontype()
  if ysw eq 0 then season_ch='Year'
  if ysw eq 1 then season_ch='Summer'
  if ysw eq 2 then season_ch='Winter
  if elabCode eq 72 then season_ch='Summer-Winter'
  coords=[0.77,0.3]
  if resPoscript then coords(1)=coords(1)-0.03
  coords=plotter->legendNormalize(coords)
  xyouts,coords[0],coords[1],'Season: '+ season_ch,/normal,charsize=facSize*psFact,color=0
  
  ddn=request->getHourType()
  if ddn eq 0 then hourType='All 24h'
  if ddn eq 1 then hourType='Day'
  if ddn eq 2 then hourType='Night'
  if ddn eq 3 then hourType='WeekDays'
  if ddn eq 4 then hourType='WeekEnd'
  if elabCode eq 71 then hourType='Day-Night'
  if elabCode eq 73 then hourType='WeekDays-WeekEnd'
  
  coords=[0.77,0.2]
  if resPoscript then coords(1)=coords(1)-0.01
  coords=plotter->legendNormalize(coords)
  xyouts,coords[0],coords[1],'Day hours: '+ hourType,/normal,charsize=facSize*psFact,color=0
  
  timeAverage='Preserved'
  hourStat=request->getGroupByTimeInfo() ;HourType
  if hourStat(0).value eq '08' then timeAverage='8h'
  if hourStat(0).value eq '01' then timeAverage='1h'
  if hourStat(0).value eq 'Year' then timeAverage='All period'
  coords=[0.77,0.1]
  if resPoscript then coords(1)=coords(1)+0.01
  coords=plotter->legendNormalize(coords)
  xyouts,coords[0],coords[1],'Time Average: '+ timeAverage,/normal,charsize=facSize*psFact,color=0
  
  statTypeChoice='N/A'
  statType=request->getGroupByStatInfo() ;HourType
  if statType eq 0 then statTypeChoice='preserved'
  if statType eq 1 then statTypeChoice='Mean'
  if statType eq 2 then statTypeChoice='Max'
  if statType eq 3 then statTypeChoice='Min'
  coords=[0.77,0]
  if resPoscript then coords(1)=coords(1)+0.03
  coords=plotter->legendNormalize(coords)
  xyouts,coords[0],coords[1],'Daily stats: '+ statTypeChoice,/normal,charsize=facSize*psFact,color=0
  
END
pro tvellipse, rmax, rmin, xc, yc, pos_ang, color, DATA = data, $
    NPOINTS = npoints, COLOR=thecolor, MAJOR=major, MINOR=minor, $
    _Extra = _extra
  ;+
  ; NAME:
  ;      TVELLIPSE
  ;
  ; PURPOSE:
  ;      Draw an ellipse on the current graphics device.
  ;
  ; CALLING SEQUENCE:
  ;      TVELLIPSE, rmax, rmin, xc, yc, [ pos_ang, color, COLOR= ,/DATA, NPOINTS=
  ;                                        LINESTYLE=, THICK=, /MAJOR, /MINOR ]
  ; INPUTS:
  ;       RMAX,RMIN - Scalars giving the semi-major and semi-minor axes of
  ;                   the ellipse
  ; OPTIONAL INPUTS:
  ;       XC,YC - Scalars giving the position on the TV of the ellipse center
  ;               If not supplied (or if XC, YC are negative and /DATA is not set),
  ;               and an interactive graphics device (e.g. not postscript) is set,
  ;               then the user will be prompted for X,Y
  ;       POS_ANG - Position angle of the major axis, measured counter-clockwise
  ;                 from the X axis.  Default is 0.
  ;       COLOR - Scalar  giving intensity level to draw ellipse.   The color
  ;               can be specified either with either this parameter or with the
  ;               COLOR keyword.   Default is !P.COLOR
  ;
  ; OPTIONAL KEYWORD INPUT:
  ;        COLOR - Intensity value used to draw the circle, overrides parameter
  ;               value.  Default = !P.COLOR
  ;        /DATA - if this keyword is set and non-zero, then the ellipse radii and
  ;               X,Y position center are interpreted as being in DATA
  ;               coordinates.   Note that the data coordinates must have been
  ;               previously defined (with a PLOT or CONTOUR call).
  ;        NPOINTS - Number of points to connect to draw ellipse, default = 120
  ;                  Increase this value to improve smoothness
  ;        /MAJOR - Plot a line along the ellipse's major axis
  ;        /MINOR - Plot a line along the ellipse's minor axis
  ;
  ;               Any keyword recognized by PLOTS is also recognized by TVELLIPSE.
  ;               In particular, the color, linestyle, thickness and clipping of
  ;               the ellipses are controlled by the  COLOR, LINESTYLE, THICK and
  ;               NOCLIP keywords.  (Clipping is turned off by default, set
  ;               NOCLIP=0 to activate it.)
  ;
  ; RESTRICTIONS:
  ;        TVELLIPSE does not check whether the ellipse is within the boundaries
  ;        of the window.
  ;
  ;        The ellipse is evaluated at NPOINTS (default = 120) points and
  ;        connected by straight lines, rather than using the more sophisticated
  ;        algorithm used by TVCIRCLE
  ;
  ;        TVELLIPSE does not accept normalized coordinates.
  ;
  ;        TVELLIPSE is not vectorized; it only draws one ellipse at a time
  ;
  ; EXAMPLE:
  ;        Draw an ellipse of semi-major axis 50 pixels, minor axis 30
  ;        pixels, centered on (250,100), with the major axis inclined 25
  ;        degrees counter-clockwise from the X axis.  Use a double thickness
  ;        line and device coordinates (default)
  ;
  ; IDL> tvellipse,50,30,250,100,25,thick=2
  ;
  ; NOTES:
  ;        Note that the position angle for TVELLIPSE (counter-clockwise from
  ;        the X axis) differs from the astronomical position angle
  ;        (counter-clockwise from the Y axis).
  ;
  ; REVISION HISTORY:
  ;        Written  W. Landsman STX          July, 1989
  ;        Converted to use with a workstation.  M. Greason, STX, June 1990
  ;        LINESTYLE keyword, evaluate at 120 points,  W. Landsman HSTX Nov 1995
  ;        Added NPOINTS keyword, fixed /DATA keyword W. Landsman HSTX Jan 1996
  ;        Check for reversed /DATA coordinates  P. Mangiafico, W.Landsman May 1996
  ;        Converted to IDL V5.0   W. Landsman   September 1997
  ;        Work correctly when X & Y data scales are unequal  December 1998
  ;        Removed cursor input when -ve coords are entered with /data
  ;        keyword set  P. Maxted, Keele, 2002
  ;        Use _EXTRA keywords including NOCLIP  W. Landsman October 2006
  ;        Add plotting of major and minor axes and /MAJOR, /MINOR keywords;
  ;        fixed description of RMAX,RMIN (semi-axes).  J. Guerber Feb. 2007
  ;-
  On_error,2                              ;Return to caller
  
  if N_params() lt 2 then begin
    print,'Syntax - TVELLIPSE, rmax, rmin, xc, yc, [pos_ang, color, COLOR=,'
    print,'              NPOINTS=, LINESTYLE=, THICK=, /DATA, /MAJOR, /MINOR ]'
    print,'              any other keywords accepted by PLOTS'
    return
  endif
  
  if N_params() lt 4 then $
    cursor, xc, yc, /DEVICE, /NOWAIT      ;Get unroamed,unzoomed coordinates
    
  if ( (xc LT 0) or (yc LT 0)) and not(keyword_set(data)) then begin
    message,'Position cursor in window ' + strtrim(!D.WINDOW,2) + $
      ' -- then hit mouse button',/INF
    cursor, xc, yc, /DEVICE, /WAIT
    message,'Ellipse is centered at (' + strtrim(xc,2) + ',' + $
      strtrim(yc,2) + ')',/INF
  endif
  
  if N_params() LT 5 then pos_ang = 0.    ;Default position angle
  if N_Elements(TheColor) EQ 0 then begin
    IF N_Elements( Color ) eq 0 THEN Color = !P.COLOR
  endif else color = TheColor
  
  if not keyword_set(NPOINTS) then npoints = 120   ;Number of points to connect
  
  phi = 2*!pi*(findgen(npoints)/(npoints-1))       ;Divide circle into Npoints
  ang = pos_ang/!RADEG                           ;Position angle in radians
  cosang = cos(ang)
  sinang = sin(ang)
  
  x =  rmax*cos(phi)              ;Parameterized equation of ellipse
  y =  rmin*sin(phi)
  
  xprime = xc + x*cosang - y*sinang    ;Rotate to desired position angle
  yprime = yc + x*sinang + y*cosang
  
  if keyword_set(data) then $
    plots, xprime, yprime, /DATA, COLOR=color,thick=3, _STRICT_Extra = _extra else $
    plots, round(xprime), round(yprime),  COLOR=color, /DEVICE,  $
    _STRICT_Extra = _extra
    
  if keyword_set(major) then begin
    xmaj = xc + [rmax,-rmax]*cosang  ; rot & transl points (rmax,0),(-rmax,0)
    ymaj = yc + [rmax,-rmax]*sinang
    if keyword_set(data) then $
      plots, xmaj, ymaj, /DATA, COLOR=color, _STRICT_Extra=_extra  $
    else   plots, round(xmaj), round(ymaj), $
    /DEVICE, COLOR=color, _STRICT_Extra=_extra
endif

if keyword_set(minor) then begin
  xmin = xc - [rmin,-rmin]*sinang  ; rot & transl points (0,rmin),(0,-rmin)
  ymin = yc + [rmin,-rmin]*cosang
  if keyword_set(data) then $
    plots, xmin, ymin, /DATA, COLOR=color, _STRICT_Extra=_extra  $
  else   plots, round(xmin), round(ymin), $
  /DEVICE, COLOR=color, _STRICT_Extra=_extra
endif

return
end

pro mytek_color, Start_index, Ncolors

  common colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr
  
  if n_elements(ncolors) le 0 then ncolors = 32
  if n_elements(start_index) le 0 then start_index = 0
  
  if n_elements(r_orig) lt (ncolors + start_index) then begin
    r_orig = bytscl(indgen(256), max=!d.table_size-1, min = 0)
    g_orig = r_orig
    b_orig = r_orig
  endif
  
; KeesC 20JUN2012:
  r = bytscl([ 0,100,100,0,100,0,100,100,0,   60,0,0,55,100,55,70, $   
    100,75,45,17,25,50,75,100,67,40,17,17,17,45,75,90])
  g = bytscl([ 0,100,0,0,50,100,0,100,100,    83,100,50,0,0,55,70, $
    100,100,100,100,83,67,55,33,90,90,90,67,50,33,17,9])
  b = bytscl([ 0,100,0,100,0,100,83,0,0,      0,60,100,83,55,55,70, $
    33,45,60,75,83,83,83,90,45,55,67,90,100,100,100,100])
    
  if ncolors lt 32 then begin   ;Trim?
    r = r[0:ncolors-1]
    g = g[0:ncolors-1]
    b = b[0:ncolors-1]
  endif
  s = start_index < (256 - ncolors) ;Never over top
  
  r_orig[s] = r
  g_orig[s] = g
  b_orig[s] = b
  
  tvlct, r_orig, g_orig, b_orig
  r_curr = r_orig
  g_curr = g_orig
  b_curr = b_orig
end
;*************
pro CheckCriteria, request, result, statistics, criteria, obsTimeSeries,alpha,criteriaOrig,LV
  startIndex=request->getStartIndex()
  endIndex=request->getEndIndex()
  parCodes=request->getParameterCodes()
  modelInfo=request->getModelInfo()
  year=modelInfo.year
  scale=modelInfo.scale
  scale='ALL'
  frequency=modelInfo.frequency
  scaleName=strupcase(scale)
  ;MM summer 2012 End
  hourStat=request->getGroupByTimeInfo() ;HourType
  flag_average=hourStat[0].value
  statType=request->getGroupByStatInfo() ;HourType
  isGroupSelection=request->isGroupObsPresent()
  isSingleSelection=request->isSingleObsPresent()
  elabcode=request->getElaborationCode()
  ;MM summer 2012 Start
  ;for Goals & Criteria now you can use
  ;elaboration related info called OCTimeAvgName & OCStat 
  OCTimeAvgName=request->getElaborationOCTimeAvgName()
  OCStat=request->getElaborationOCStat()
  ;MM summer 2012 End
  GroupModeOKmode=0
  if isSingleSelection then GroupModeOKmode=1
  if isGroupSelection then GroupModeOKmode=request->getGroupStatToApplyCode()
  
  ; initial values
  LV=0
  alpha=0
  criteriaOrig=0
  criteria=0
  Neff=1
  Nnp=1
  
  ;if more than one pollutant or group mode statistic ne 90 percentile, no criteria found
  if n_elements(parCodes) gt 1 or GroupModeOKmode ne 1 then begin
    goto,jumpEnd
  endif
  
  ; put user choices into Criteria langage (goalsandcriteria.dat file)
  if flag_average eq 'preserve' then flag_average='P'
  if flag_average eq '08' then flag_average='8H'
  
  if statType eq 0 then flagDailyStat='P'
  if statType eq 1 then flagDailyStat='MEAN'
  if statType eq 2 then flagDailyStat='MAX'
  if statType eq 3 then flagDailyStat='MIN'
  
  dailyStatOp=flag_average+flagDailyStat
  ; MM summer 2012 Start
  ; You can replace all these with request->get
  ; OCTimeAvgName=request->getElaborationOCTimeAvgName()
  ;OCStat=request->getElaborationOCStat()
  ; MM summer 2012 End
  if strupcase(frequency) eq 'YEAR' then begin  ;annual averages
    if parcodes[0] eq 'PM10' then dailyStatOp='PMEAN'
    if parcodes[0] eq 'NO2'  then dailyStatOp='PP'
    if parcodes[0] eq 'O3'   then dailyStatOp='N/A'
  endif
  
  FlagAll=0
  
  ; request criteria: check existence
  Criteria=request->getGoalsCriteriaValues(parameter=parCodes[0], scalename=scalename, statname=statistics, timeAvgName=dailyStatOp, NOVALUES=NOVALUES)
  if Criteria(0) eq -1 then begin
     Criteria=request->getGoalsCriteriaValues(parameter=parCodes[0], scalename=scalename, statname=statistics, timeAvgName='ALL', NOVALUES=NOVALUES)
     if criteria(0) gt 0 then FlagAll=1
  endif
  if Criteria(0) ne -1 and statistics eq 'OU' then begin
    UrLV=criteria(0)
    alpha=criteria(1)
    Neff=criteria(2)
    Nnp=criteria(3)
    LV=criteria(4)
    if strupcase(frequency) eq 'HOUR' then criteria(2:3)=1
  endif
  CriteriaOrig=criteria
  if keyword_set(NOVALUES) then UrLV=0
  
  ; calculation of absolute error
  
;  nobsAv=endIndex-startIndex+1
;  if statType eq 1 or statType eq 2 or statType eq 3 then nobsAv=nobsAv/24.
;  if statType eq 0 and (parcodes eq 'PM10' or parCodes eq 'PM25') then nobsAv=nobsAv/24.
;  if nobsAv lt 1 then nobsAv=1
;  if frequency eq 'HOUR' then nobsAv=1
  
  if statistics eq 'OU' and criteria(0) ne -1 then begin 
     if strupcase(frequency) eq 'HOUR' then criteria=UrLV/100.*sqrt( (1.-alpha)*(stddevOM(obsTimeSeries)^2+mean(obsTimeSeries)^2)+alpha*LV^2)
     if strupcase(frequency) eq 'YEAR' then criteria=UrLV/100.*sqrt( (1.-alpha)*(stddevOM(obsTimeSeries)^2+mean(obsTimeSeries)^2)/Neff +alpha*LV^2/Nnp)
     if (Neff eq -999 or Nnp eq -999) and strupcase(frequency) eq 'YEAR' then criteria=-1
     ;if parcodes[0] eq 'O3' and strupcase(frequency) eq 'HOUR' then criteria=criteria/1.43
  endif
  jumpend:
;**********************
end
pro ObsModCriteriaPercentile, request, result, obsTimeSeries,modTimeSeries
  startIndex=request->getStartIndex()
  endIndex=request->getEndIndex()
  parCodes=request->getParameterCodes()
  modelInfo=request->getModelInfo()
  year=modelInfo.year
  scale=modelInfo.scale
  scale='ALL'
  frequency=modelInfo.frequency
  scaleName=strupcase(scale)
  ;MM summer 2012 End
  hourStat=request->getGroupByTimeInfo() ;HourType
  flag_average=hourStat[0].value
  statType=request->getGroupByStatInfo() ;HourType
  isGroupSelection=request->isGroupObsPresent()
  isSingleSelection=request->isSingleObsPresent()
  elabcode=request->getElaborationCode()
  OCTimeAvgName=request->getElaborationOCTimeAvgName()
  OCStat=request->getElaborationOCStat()
  GroupModeOKmode=0
  if isSingleSelection then GroupModeOKmode=1
  if isGroupSelection then GroupModeOKmode=request->getGroupStatToApplyCode()
  
  ; initial values
  LV=0
  alpha=0
  criteriaOrig=0
  criteria=0
  Neff=1
  Nnp=1
  
  ;if more than one pollutant or group mode statistic ne 90 percentile, no criteria found
  if n_elements(parCodes) gt 1 or GroupModeOKmode ne 1 then begin
    goto,jumpEnd
  endif
  
  ; put user choices into Criteria langage (goalsandcriteria.dat file)
  if flag_average eq 'preserve' then flag_average='P'
  if flag_average eq '08' then flag_average='8H'
  
  if statType eq 0 then flagDailyStat='P'
  if statType eq 1 then flagDailyStat='MEAN'
  if statType eq 2 then flagDailyStat='MAX'
  if statType eq 3 then flagDailyStat='MIN'
  
  dailyStatOp=flag_average+flagDailyStat
  
  if strupcase(frequency) eq 'YEAR' then begin  ;annual averages
    if parcodes[0] eq 'PM10' then dailyStatOp='PMEAN'
    if parcodes[0] eq 'NO2'  then dailyStatOp='PP'
    if parcodes[0] eq 'O3'   then dailyStatOp='N/A'
  endif
  
  FlagAll=0
  
  ; request criteria: check existence
  Criteria=request->getGoalsCriteriaValues(parameter=parCodes[0], scalename=scalename, statname='OU', timeAvgName=dailyStatOp, NOVALUES=NOVALUES)
  if Criteria(0) eq -1 then begin
     Criteria=request->getGoalsCriteriaValues(parameter=parCodes[0], scalename=scalename, statname='OU', timeAvgName='ALL', NOVALUES=NOVALUES)
     if criteria(0) gt 0 then FlagAll=1
  endif
  if Criteria(0) ne -1 then begin
    UrLV=criteria(0)
    alpha=criteria(1)
    Neff=criteria(2)
    Nnp=criteria(3)
    LV=criteria(4)
    if strupcase(frequency) eq 'HOUR' then criteria(2:3)=1
  endif
  CriteriaOrig=criteria
  if keyword_set(NOVALUES) then UrLV=0
  
  if criteria(0) ne -1 then begin 
     criteria=UrLV/100.*sqrt( (1.-alpha)*obsTimeSeries^2+alpha*LV^2)
     diffhlp=abs(modTimeSeries-obsTimeSeries)/criteria
     res=sort(diffhlp)
     percentile=0.90
     nFin=fix(n_elements(res)*percentile)-1
     obsTimeSeries=reform(obsTimeSeries(res(0:nfin)))
     modTimeSeries=reform(modTimeSeries(res(0:nfin)))
  endif
  jumpend:
;**********************
end

pro legendGenericBuild,request,result,plotter
  plotter->wsetInfoDataDraw
  psFact=plotter->getPSCharSizeFactor()
  resPoscript=plotter->currentDeviceIsPostscript()
  white=obj_new('Color', 200, 200, 200)
  black=obj_new('Color', 0, 0, 0)
  whiteL=white->AsLongTrueColor()
  blackL=black->AsLongTrueColor()
  obj_destroy, white
  obj_destroy, black
  DEVICE, DECOMPOSED=1
  plotter->erase, whiteL
  
  DEVICE,DECOMPOSE=0
  LOADCT,39
  ; use "tek" color table...
  mytek_color;, 0, 32
  
  legoWidth=.012
  legoHeight=.05
  startX=.01
  maxWidth=0
  
  targetInfo=result->getGenericPlotInfo()
  allDataXY=targetInfo->getXYS()
  if finite(min(allDataXY,/nan)) eq 0 or  finite(max(allDataXY,/nan)) eq 0 then goto, noplot
  allDataColor=targetInfo->getColors()
  colors=targetInfo->getColors()
  symbols=targetInfo->getSymbols()
  legColors=targetInfo->getLegendColors()
  legNames=targetInfo->getLegendNames()   ;long names
  legSyms=targetInfo->getLegendSymbols()
  legoSequenceNo=n_elements(legNames)
  modelCodes=request->getModelCodes()
  obsNames=request->getSingleObsNames()
  scenarioCodes=request->getScenarioCodes()
  npar=request->getParameterNumber()
  nmod=request->getModelNumber()
  nsce=request->getScenarioNumber()
  nobsS=request->getSingleObsNumber()
  groupTitles=request->getGroupTitles()
  gTstr=strcompress(groupTitles,/remove_all)
  nobsG=0
  if gTstr[0] ne '-1' then nobsG=n_elements(groupTitles)
  nobs=nobsS+nobsG
  parCodes=request->getParameterCodes()
  diagramCode=request->getDiagramCode()
  elabcode=request->getElaborationCode()
  
  ;  allDataXY=targetInfo->getXYS()
  
  nmulti=npar*nmod*nsce*nobs
  
  legendnames1=strarr(n_elements(legNames)) & legendnames2=strarr(n_elements(legNames))
  for i=0,n_elements(legNames)-1 do begin
    res=strsplit(legNames(i),'&',/extract)
    legendnames1(i)=res(0)
    if n_elements(res) gt 1 then legendnames2(i)=res(1)
  endfor
  legNamesPrint=legNames
  legcolorsPrint=legColors
  if legendnames2[0] ne '' then begin
    b = UNIQ(legendNames2, SORT(legendNames2))
    legNamesPrint=legendnames2(b)
    legcolorsPrint=legColors(b)
  endif
  symbolSequenceNo=n_elements(legNamesPrint)
  if elabCode eq 56 then symbolSequenceNo=2
  
  if (elabCode ge 71 and elabCode le 73) and $
    (npar gt 1 and nmod gt 1 and nsce eq 1 and nobs eq 1) then begin
    symbolSequenceNo=npar*nmod
    legNamesPrint2=strarr(symbolSequenceNo)
    legColorsPrint=intarr(npar*nmod)
    for ipar=0,npar-1 do begin
      for imod=0,nmod-1 do begin
        legNamesPrint2(ipar*nmod+imod)=legendnames1(ipar)+'*'+legNamesPrint(imod)
      endfor
    endfor
    legNamesPrint=legNamesPrint2
    legColorsPrint=indgen(npar*nmod)
  endif
  
  ind=0
  ;  for i=0, nmulti-1 do begin  ; min([symbolSequenceNo-1,62,n_elements(legSyms)-1]) do begin
  for i=0, min([symbolSequenceNo-1,44,n_elements(legSyms)-1]) do begin
    jheight = i MOD 9
    startx = .14*fix(i/9)
    startY=1.-((jheight+1)*legoHeight*2)
    thisStartX=startX+maxWidth+legoWidth+.02
    lego=[[thisStartX,startY], [thisStartX,startY+legoHeight], [thisStartX+legoWidth,startY+legoHeight], [thisStartX+legoWidth,startY], [thisStartX,startY]]
    mypsym,legSyms[i],1
    coords=[thisStartX+(legoWidth/2), startY+.002]
    coords=plotter->legendNormalize(coords)
    plots, coords[0], coords[1], psym=8, color=2+legColorsPrint[i], /NORM, symsize=1.
    coords=[thisStartX+legoWidth+.01, startY+.002]
    coords=plotter->legendNormalize(coords)
    xyouts, coords[0], coords[1] , strmid(legNamesPrint[i],0,13), COLOR=0, /NORM, charsize=.8, charthick=.8,  WIDTH=textWidth
  endfor
  
  noplot:
end
;**************
pro legendGenericBuildDiag0,request,result,plotter

  plotter->wsetInfoDataDraw
  white=obj_new('Color', 200, 200, 200)
  black=obj_new('Color', 0, 0, 0)
  whiteL=white->AsLongTrueColor()
  blackL=black->AsLongTrueColor()
  obj_destroy, white
  obj_destroy, black
  DEVICE, DECOMPOSED=1
  plotter->erase, whiteL
  DEVICE,DECOMPOSE=0
  LOADCT,39
  ; use "tek" color table...
  mytek_color;, 0, 32
  legoWidth=.012
  legoHeight=.05
  startX=.01
  maxWidth=0
  
  targetInfo=Result->getGenericPlotInfo()
  colors=targetInfo->getColors()
  symbols=targetInfo->getSymbols()
  legColors=targetInfo->getLegendColors()
  legNames=targetInfo->getLegendNames()   ;long names
  legSyms=targetInfo->getLegendSymbols()
  ;  legoSequenceNo=n_elements(legNames)
  scenarioCodes=request->getScenarioCodes()
  npar=request->getParameterNumber()
  nmod=request->getModelNumber()
  nsce=request->getScenarioNumber()
  ;  nobsS=request->getSingleObsNumber()
  ;  groupNames=request->getGroupNames()
  ;  nobsG=n_elements(groupNames)
  ;  nobs=nobsS+nobsG
  modelCodes=request->getModelCodes()
  parCodes=request->getParameterCodes()
  diagramCode=request->getDiagramCode()
  elabcode=request->getElaborationCode()
  
  ifree=reform(legNames(0,5))
  if diagramCode eq 0 then begin  ;barplot
    statName=legNames(0,3)
    if ifree eq '1000' then begin
      legPrint=strarr(1)
      legPrint(0)=modelCodes+'-'+scenarioCodes+'-'+statName
      legColors2=legColors & legSyms2=legSyms & legPrint2=['OBS']
    endif
    if ifree eq '0100' then begin
      legPrint=modelCodes+'-'+parCodes+'-'+scenarioCodes+'-'+statName
      legColors2=legColors & legSyms2=legSyms & legPrint2=['OBS']
    endif
    if ifree eq '0010' then begin
      legPrint=strarr(1)
      legPrint(0)=parCodes+'-'+modelCodes+'-'+statName
      legColors2=legColors & legSyms2=legSyms & legPrint2=['']
    endif
    if ifree eq '0001' then begin
      legPrint=strarr(1)
      legPrint(0)=parCodes+'-'+modelCodes+'-'+scenarioCodes
      legColors2=legColors & legSyms2=[9] & legPrint2=['OBS']
    endif
    if total(where(elabCode eq [2,3,4,5,7,8,23,24,28,30,33,54])) ge 0  then legPrint2=['']
    ;    legprint=legprint(0:0)
    if ifree eq '1100' then begin
      legPrint=modelCodes & legColors2=legColors(0,*) & legSyms2=legSyms(0,*) & legPrint2=['OBS']
      if total(where(elabCode eq [2,3,4,5,7,8,23,24,28,30,33,54])) ge 0  then legPrint2=['']
    endif
    if ifree eq '1010' then begin
      legPrint=scenarioCodes  & legColors2=legColors(0,*) & legSyms2=legSyms(0,*) & legPrint2=['OBS']
      legPrint2=['']
    endif
    if ifree eq '1001' then begin
      legPrint=parCodes & legColors2=legColors(*,0) & legSyms2=legSyms(*,0) & legPrint2='OBS-'+legPrint
      if total(where(elabCode eq [2,3,4,5,7,8,23,24,28,30,33,54])) ge 0  then legPrint2=['']
    endif
    if ifree eq '0110' then begin
      legPrint=modelCodes & legColors2=legColors(0,*) & legSyms2=legSyms(0,*) & legPrint2='OBS-2009'
      legPrint2=['']
    endif
    if ifree eq '0101' then begin
; KeesC 20JUN2012: (0,*) changed into (*,0)    
      legPrint=modelCodes & legColors2=legColors(*,0) & legSyms2=legSyms(*,0) & legPrint2='OBS'
      if total(where(elabCode eq [2,3,4,5,7,8,23,24,28,30,33,54])) ge 0  then legPrint2=['']
    endif
    if ifree eq '0011' then begin
      legPrint=scenarioCodes & legColors2=legColors(*,0) & legSyms2=legSyms(*,0) & legPrint2='OBS'
      legPrint2=['']
    endif
    if ifree eq '1011' then begin
      legPrint=scenarioCodes & legColors2=legColors(0,0,*,0) & legSyms2=legSyms(0,0,*,0) & legPrint2=parCodes
    endif
    if ifree eq '1101' then begin
      legPrint=modelCodes & legColors2=legColors(0,*,0,0) & legSyms2=legSyms(0,*,0,0) & legPrint2=parCodes+'-OBS'
      if total(where(elabCode eq [2,3,4,5,7,8,23,24,28,30,33,54])) ge 0 then legPrint2=parCodes
    endif
    if ifree eq '0111' then begin
      legPrint=modelCodes & legColors2=legColors(0,*,0,0) & legSyms2=legSyms(0,*,0,0) & legPrint2=scenarioCodes
    endif
    
    for i=0, n_elements(legprint)-1 do begin
      jheight = i MOD 9
      startx = .10*fix(i/9)
      startY=1.-((jheight+1)*legoHeight*2)
      thisStartX=startX+maxWidth+legoWidth+.02
      lego=[[thisStartX,startY], [thisStartX,startY+legoHeight], [thisStartX+legoWidth,startY+legoHeight], [thisStartX+legoWidth,startY], [thisStartX,startY]]
      mypsym,legSyms2[i],1.5
      coords=[thisStartX+(legoWidth/2), startY+.002]
      coords=plotter->legendNormalize(coords)
      plots, coords[0], coords[1], psym=8, color=2+i, /NORM, symsize=1.
      coords=[thisStartX+legoWidth+.01, startY+.002]
      coords=plotter->legendNormalize(coords)
      xyouts, coords[0], coords[1] , legPrint[i], COLOR=0, /NORM, charsize=.8, charthick=.8,  WIDTH=textWidth
    endfor
    
    if legprint2[0] ne '' then begin
      for i=0, n_elements(legprint2)-1 do begin
        jheight = i MOD 9
        startx = .30 + .10*fix(i/9)
        startY=1.-((jheight+1)*legoHeight*2)
        thisStartX=startX+maxWidth+legoWidth+.02
        lego=[[thisStartX,startY], [thisStartX,startY+legoHeight], [thisStartX+legoWidth,startY+legoHeight], [thisStartX+legoWidth,startY], [thisStartX,startY]]
        coords=[thisStartX+(legoWidth/2), startY+.002]
        coords=plotter->legendNormalize(coords)
        mypsym,5,1.5
        plots, coords[0], coords[1], psym=8, color=15-i, /NORM, symsize=1.
        mypsym,14,1.5
        plots,coords[0],coords[1],psym=8,color=0,/norm,symsize=1.   
        coords=[thisStartX+legoWidth+.01, startY+.002]
        coords=plotter->legendNormalize(coords)
        xyouts, coords[0], coords[1]-.5*legoHeight , legPrint2[i], COLOR=0, /NORM, charsize=.8, charthick=.8,  WIDTH=textWidth
      endfor
    endif
  endif
  
end

pro mybar_plot,values,nsubbars,baselines=baselines,colors=colors,barnames=barnamesIn, $
    title=title,xtitle=xtitle,ytitle=ytitle,baserange=baserange, $
    barwidth=barwidth,barspace=barspaceIn,baroffset=baroffset, $
    outline=outline,overplot=overplot,background=background, $
    rotate=rotate,tickv,request,result
    
  compile_opt idl2
  
  on_error, 2
  
  elabcode=request->getElaborationCode()
  
  if (n_params(d) eq 0) then begin  ;Print call & return if no parameters
    print,'bar_test,values,baselines=baselines,colors=colors,barnames=barnames,$'
    print,' title=title,xtitle=xtitle,ytitle=ytitle,baserange=baserange, $'
    print,' barwidth=barwidth,barspace=barspace,baroffset=baroffset, $'
    print,' outline=outline,overplot=overplot,background=background, $'
    print,' rotate=rotate'
    return
  endif
  
  nbars=n_elements(values)    ; Determine number of bars
  ; Baselines (bars extend from baselines through values); default=0
  if not(keyword_set(baselines)) then baselines=intarr(nbars)
  ; Default colors spaced evenly in current color table
  if not(keyword_set(colors)) then $
    colors=fix(((!d.n_colors < 256)/float(nbars))*(Lindgen(nbars)+0.5))
  ; Labels for the individual bars; none by default
  barnames = (N_Elements(barnamesIn) gt 0) ? barnamesIn : strarr(nbars)+' '
  ; Main title
  if not(keyword_set(title)) then title=''
  ; Centered title under X-axis
  if not(keyword_set(xtitle)) then xtitle=''
  ; Title for Y-axis
  if not(keyword_set(ytitle)) then ytitle=''
  ; Fraction (0-1) of full X range to use
  ;  if not(keyword_set(baserange)) then baserange=1.0
  ; Space betw. bars, taken from nominal bar widths; default is none
  ;  barspace = (N_Elements(barspaceIn) gt 0) ? Float(barspaceIn) : 0.2
  ; Bar width scaling factor, relative to nominal
  ;  if not(keyword_set(barwidth)) then barwidth=1.0 - barspace-barspace/nbars
  ;  if nbars eq 1 then barwidth=0.6/nsubbars
  ; Initial X offset, in scaled bar widths; default is none
  if not(keyword_set(baroffset)) then baroffset=barspace/barwidth
  ;  if nbars eq 1 then baroffset=0.3
  ; Outline of bars; default is none
  outline = keyword_set(outline)
  ; Overplot (do not erase the existing display); default is to create new plot
  overplot = keyword_set(overplot)
  ; Background color index; defaults to 0 (usually black) if not specified
  if not(keyword_set(background)) then background=0
  ; Rotate (make horizontal bars); default is vertical bars
  rotate = keyword_set(rotate)
  
  mnBB = MIN(baselines, MAX=mxB, /NAN)
  mnVV = MIN(values, MAX=mxV, /NAN)
  range=[mnBB < mnVV, $    ;Minimum of bases & values
    mxB > mxV]      ;Maximum of bases & values
    
  if (!y.range[0] eq 0) and (!y.range[1] eq 0) $  ;Determine range for Y-axis
    then yrange=range $
  else yrange=!y.range                 ;Or, use range specified
  xrange=!x.range           ;Axis perpend. to bars
  ;   if xrange(1)-xrange(0) eq 0 then !x.range=[0,1]
  ;   xrange=!x.range
  xticks=1
  xtickname=strarr(2)+' '
  yticks=0
  ytickname=strarr(1)+''
  
  if (overplot eq 0) then $        ;Create new plot, no data
    if n_elements(values) eq 1 then begin
    plot,[values],/nodata,title=title,xtitle=xtitle,ytitle=ytitle, $
      noerase=overplot,yrange=yrange,xticks=xticks, background=background,/data,$
      yticks=yticks,ytickname=ytickname,ystyle=1,xtickname=xtickname
  endif else begin
    plot,[values],/nodata,title=title,xtitle=xtitle,ytitle=ytitle, $
      noerase=overplot,xrange=xrange,yrange=yrange,xticks=xticks, $
      xtickname=xtickname,yticks=yticks,ytickname=ytickname, $
      xstyle=1,ystyle=1,/data,background=background
  endelse
  ; Polyfill Criteria
  if elabcode eq 2 then statistics='R'
  if elabcode eq 7 then statistics='IOA'
  if elabcode eq 8 then statistics='RDE'
  if elabcode eq 23 then statistics='MNB'
  if elabcode eq 28 then statistics='Target'
  if elabcode eq 30 then statistics='RPE'
  if elabcode eq 54 then statistics='MFS'
  if total(where(elabCode eq [2,7,8,23,28,30,54])) ge 0 then begin
    ;  KeesC 28MAR2012: obsTemp changed into values
    CheckCriteria, request, result, statistics, criteria, values,alpha,criteriaOrig,LV
  endif else begin
    criteria=0
  endelse
  if criteria[0] gt 0 then begin
    ymin=max([criteria[0],yrange[0]])
    ymax=min([1,yrange[1]])
    if elabcode eq 2 or elabcode eq 7 then polyfill,[0,0,nbars-1,nbars-1,0],[ymin,ymax,ymax,ymin,ymin],color=8,/data
    ymin=0
    ymax=min([criteria[0],yrange[1]])
    if elabcode eq 8 or elabcode eq 30 or elabcode eq 28 then polyfill,[0,0,nbars-1,nbars-1,0],[ymin,ymax,ymax,ymin,ymin],/data,color=8
    ymin=max([-criteria[0],yrange[0]])
    ymax=min([criteria[0],yrange[1]])
    if elabcode eq 23 or elabcode eq 54 then polyfill,[0,0,nbars-1,nbars-1,0],[ymin,ymax,ymax,ymin,ymin],/data,color=8
  endif
  plots,[0,nbars-1],[0,0],/data,color=0
  
  ;Vertical bars
  base_win=!x.window          ;Window range in X
  scal_fact=!y.s          ;Scaling factors
  tick_scal_fact=!x.s           ;Tick scaling factors
  
  ;  winrange=baserange*(base_win[1]-base_win[0])     ;Normal. window range
  ;  barsize=barwidth*winrange/nbars        ;Normal. bar width
  ;  winoffset=base_win[0]+(baroffset*barsize)    ;Normal. first offset
  bases=scal_fact[0]+(scal_fact[1]*baselines)    ;Baselines, in normal coor.
  normal=scal_fact[0]+(scal_fact[1]*values)    ;Values, in normal coor.
  ;  bsp=barspace*(winrange/nbars)
  ;  barstart=Lindgen(nbars)*(barsize+bsp) ;Coor. at left edges
  ;Tick coor. (centered)
  
  br=0.98 ;base_win[1]-base_win[0]
  bs=0.75
  barsize=br*(base_win[1]-base_win[0])/(nbars*nsubbars+(nbars-.1)*bs)
  winoffset=base_win[0]+(baroffset*barsize)*br
  if nbars eq 1 and nsubbars eq 1 then begin
    winoffset=base_win[0]+.2
    barsize=.45
  endif
  barstart=Lindgen(nbars)*(barsize)*(nsubbars+bs)
  tickv=winoffset+barstart+(0.5*barsize)
  for i=0,nbars-1 do begin         ;Draw the bars
    width=winoffset+[barstart[i],barstart[i], $     ;Compute bar width
      (barstart[i]+barsize),(barstart[i]+barsize)]
    length=[bases[i],normal[i],normal[i],bases[i]]  ;Compute bar length
    if (rotate) then begin        ;Horizontal bars
      x=length             ;X-axis is "length" axis
      y=width            ;Y-axis is "width" axis
    endif else begin          ;Vertical bars
      x=width            ;X-axis is "width" axis
      y=length             ;Y-axis is "length" axis
    endelse
    polyfill,x,y,color= colors[i],/normal      ;Polyfill with color
    if (outline) then plots,x,y,/normal       ;Outline using !p.color
  endfor
  
  tickv=(tickv-tick_scal_fact[0])/tick_scal_fact[1]  ;Locations of the ticks
  tickv2par=tickv + nbars*barsize/3.
  
  if nbars le 60 then begin ; ctickname limited to 60 in axis
    if (rotate) then begin           ;Label the bars (Y-axis)
      axis,yaxis=0,ystyle=1,yticks=(nbars-1),ytickv=tickv,ytickname=barnames,yticklen=0.0
    endif else begin             ;Label the bars (X-axis)
      if overplot eq 0 then begin
        if nbars gt 1 then begin
          if nsubbars ge 2 then begin
            axis,xaxis=0,xstyle=1,xticks=(nbars-1),xtickv=tickv2par,xtickname=barnames,xticklen=0.00001
            axis,xaxis=0,xstyle=1,xticks=(nbars-1),xtickv=tickv,xtickname=replicate(' ',nbars),xticklen=0.0
          endif else begin
            axis,xaxis=0,xstyle=1,xticks=(nbars-1),xtickv=tickv,xtickname=barnames,xticklen=0.0
          endelse
        endif
        if nbars eq 1 then begin
          if nsubbars ge 2 then begin
            axis,xaxis=0,xstyle=1,xticks=nbars,xtickv=tickv2par,xtickname=barnames,xticklen=0.00001
            axis,xaxis=0,xstyle=1,xticks=nbars,xtickv=tickv,xtickname=replicate(' ',nbars),xticklen=0.0
          endif else begin
            axis,xaxis=0,xstyle=1,xticks=nbars,xtickv=tickv,xtickname=barnames,xticklen=0.0
          endelse
        endif
      endif
      if overplot eq 1 then begin
        if nbars gt 1 then axis,xaxis=0,xstyle=1,xticks=(nbars-1),xtickname=replicate(' ',nbars),xtickv=tickv,xticklen=0.0
        if nbars eq 1 then axis,xaxis=0,xstyle=1,xticks=nbars,xtickname=replicate(' ',nbars),xtickv=tickv,xticklen=0.0
      endif
    endelse
  endif
  
end
