FUNCTION Plotter::getPSCharSizeFactor

 if self->currentDeviceIsPostscript() then return, self.mainView->getPSCharSizeFactor() else return, 1.

END

PRO Plotter::postScriptFixing

  FIXPS, self->getLastPostScriptFileName(), /A4
  
END

PRO Plotter::setLastPostScriptFileName, fullFileName

  self.lastPostScriptFileName=fullFileName
  
END

FUNCTION Plotter::getLastPostScriptFileName

  return, self.lastPostScriptFileName
  
END

PRO Plotter::erase, color

  if ~self->currentDeviceIsPostscript() then erase, color
  
END

FUNCTION Plotter::sizeCorrection, recArrays, X=X, Y=Y, SYMBOL=SYMBOL

  print, '*****'
  for i=0, n_elements(recArrays)-1 do begin
    if ptr_valid(recArrays[i]) then begin
      thisRec=recArrays[i]
      help, *thisRec
      ;print, *recArrays
      print, '*****'
      print, 'width', (*thisRec)[3,0]-(*thisRec)[0,0]
      print, '*****'
      xDiff=abs((*thisRec)[3,0]-(*thisRec)[0,0])
      yDiff=abs((*thisRec)[2,1]-(*thisRec)[0,1])
      minDiff=0.01
      maxDiff=0.05
      if xDiff lt minDiff and keyword_set(X) then begin
        xCenter=mean((*thisRec)[*,0])
        (*thisRec)[0:1,0]=xCenter-minDiff/2
        (*thisRec)[2:3,0]=xCenter+minDiff/2
      endif
      if yDiff lt minDiff and keyword_set(Y) then begin
        yCenter=mean((*thisRec)[*,1])
        (*thisRec)[0,1]=yCenter-minDiff/2
        (*thisRec)[3,1]=yCenter-minDiff/2
        (*thisRec)[1,1]=yCenter+minDiff/2
        (*thisRec)[2,1]=yCenter+minDiff/2
      endif
      if yDiff gt maxDiff and keyword_set(SYMBOL) then begin
        yCenter=mean((*thisRec)[*,1])
        (*thisRec)[0,1]=yCenter-maxDiff/2
        (*thisRec)[3,1]=yCenter-maxDiff/2
        (*thisRec)[1,1]=yCenter+maxDiff/2
        (*thisRec)[2,1]=yCenter+maxDiff/2
      endif
      if xDiff gt maxDiff and keyword_set(SYMBOL) then begin
        xCenter=mean((*thisRec)[*,0])
        (*thisRec)[0:1,0]=xCenter-maxDiff/2
        (*thisRec)[2:3,0]=xCenter+maxDiff/2
      endif
    endif else begin
        polygon=fltarr(4,2)
        polygon[*,0]=[0.,0.,0.,0.]
        polygon[*,1]=[0.,0.,0.,0.]
        recArrays[i]=ptr_new(polygon, /NO_COPY)
    endelse
  endfor
  return, recArrays
  
END

PRO Plotter::setCurrentDevice, deviceName

  self.currentDeviceName=deviceName
  
END

FUNCTION Plotter::currentDeviceIsPostscript

  return, self.currentDeviceName eq 'PS'
  
END

FUNCTION Plotter::getOverplotKeyword, originalValue

  if self->currentDeviceIsPostscript() then return, 1
  return, originalValue
  
END

FUNCTION Plotter::deviceIsOpen

  return, self.deviceIsOpen
  
END

FUNCTION Plotter::getPosition

  ;print, !P.POSITION
  if  self->currentDeviceIsPostscript() then begin
    if self.orientation eq 'LANDSCAPE' then begin
      position=self.position
      position[1]=self.legendSpaceYNorm*(position[3]-position[1])+position[1]
    endif else begin
      ;'PORTRAIT'
      position=self.position
      position[0]=position[0]+0.1
      ;position[2]=position[2]+position[2]/10
      position[1]=self.legendSpaceYNorm*(position[3]-position[1])+position[1]
      ;position[0]=
      print, position
    endelse
    print, 'position', position
  endif
  return, position
  
END

FUNCTION Plotter::legendNormalize, coords

  if self->currentDeviceIsPostscript() then begin
    normalizeY=self.legendSpaceYNorm*(self.position[3]-self.position[1])
    ;normalizeY=(self.legendSpaceYNorm*.75)*(self.position[3]-self.position[1])
    offSetY=self.position[1]
    
    normalizeX=self.position[2]-self.position[0]
    offSetX=self.position[0]
    
    ;if size(coords, /N_DIMENSIONS) eq 0 then coords=(coords-correction*correction)*correction else coords[1,*]=(coords[1,*]-correction*correction)*correction
    if size(coords, /N_DIMENSIONS) eq 0 then begin
      coords[0]=coords[0]*normalizeX+offSetX
      coords[1]=coords[1]*normalizeY+offSetY
    endif else begin
      coords[0,*]=coords[0,*]*normalizeX+offSetX
      coords[1,*]=coords[1,*]*normalizeY+offSetY
    endelse
    print, coords
    
  endif else begin
    ; no correction
    correction=0
  endelse
  return, coords
  
END

FUNCTION Plotter::plotNormalize, coords

  if self->currentDeviceIsPostscript() then begin
    ;normalizeY=(self.legendSpaceYNorm*.75)*(self.position[3]-self.position[1])
    normalizeY=(self.position[3]-self.position[1])*(1.-self.legendSpaceYNorm)
    legendEnd=self->legendNormalize([0.,1.])
    offSetY=legendEnd[1]
    
    normalizeX=self.position[2]-self.position[0]
    offSetX=self.position[0]
    
    ;if size(coords, /N_DIMENSIONS) eq 0 then coords=(coords-correction*correction)*correction else coords[1,*]=(coords[1,*]-correction*correction)*correction
    if size(coords, /N_DIMENSIONS) eq 0 then begin
      coords[0]=coords[0]*normalizeX+offSetX
      coords[1]=coords[1]*normalizeY+offSetY
    endif else begin
      coords[0,*]=coords[0,*]*normalizeX+offSetX
      coords[1,*]=coords[1,*]*normalizeY+offSetY
    endelse
    
  endif else begin
    ; no correction
    correction=0
  endelse
  return, coords
  
END

PRO Plotter::openDevice, deviceName, fileName, orientation, pageBreak, location

  if deviceName eq 'PS' then begin
    ;self.previousPMulti=!P.MULTI[0:2]
    ;!P.MULTI=[0,1,2]
    ;self.currentPMulti=!P.MULTI[0:2]
  
    if ~self.deviceIsOpen then begin
      self.previousDeviceName=!D.NAME
      set_plot, deviceName
      self->setCurrentDevice, deviceName
      self.deviceIsOpen=1
      self.orientation=orientation
      fsm=obj_new('FMFileSystemManager')
      psExtension=fsm->getPSExtension()
      saveDir=fsm->getSaveDir(/WITH)
      if orientation eq 'LANDSCAPE' then begin
          device, /LANDSCAPE
      endif else begin
          device, /PORTRAIT
          ;check right definition of A4 size...
          DEVICE,/INCHES,XSIZE=8.3,SCALE_FACTOR=1. 
          DEVICE,/INCHES,YSIZE=11.7,SCALE_FACTOR=1. 
      endelse
      if fileName ne '' then fileName=fsm->setExtension(fileName, psExtension)
      repeat begin
        filter=['*'+psExtension]
        fix_filter='*'+psExtension
        fullFileName=dialog_pickfile(DEFAULT_EXTENSION=psExtension, $
          DIALOG_PARENT=self.mainView->getTopBase(), $
          FILTER=filter, FIX_FILTER=fix_filter, $
          GET_PATH=path, PATH=saveDir, $
          file=fileName, $
          TITLE='Postscript selection', /OVERWRITE_PROMPT, /WRITE)
      endrep until fullFileName ne ''
      fullFileName=fsm->setExtension(fullFileName, psExtension)
      ;fullFileName=dir+fileName
      self->setLastPostScriptFileName, fullFileName
      device, /ENCAPSULATED, BITS_PER_PIXEL=24, file=fullFileName, /COLOR
    endif
    self.position=location
    print, '!D.NAME', 'self.currentDeviceName', 'self.previousDeviceName'
    print, !D.NAME, self.currentDeviceName, self.previousDeviceName
    return
  endif
  
  ; IMAGE and standards (X, WIN)
  if deviceName ne 'PS' then begin
    self.deviceIsOpen=1
    self.previousDeviceName=!D.NAME
    self->setCurrentDevice, deviceName
    print, '!D.NAME', 'self.currentDeviceName', 'self.previousDeviceName'
    print, !D.NAME, self.currentDeviceName, self.previousDeviceName
    return
  endif
  
END

PRO Plotter::closeDevice, pageBreak, fileName, printOrientation

  ;!p.position=[0.,0.,0.,0.]
  if self->currentDeviceIsPostscript() then begin
    if pageBreak eq 'TRUE' then erase ;newPage
    ;if pageBreak eq 'FALSE' then ;do nothing
    if pageBreak eq 'CLOSE' then begin
      device, /CLOSE_FILE
      self.deviceIsOpen=0
      set_plot, self.previousDeviceName
      self->setCurrentDevice, self.previousDeviceName
      print, 'printOrientation: ', printOrientation
      self->postScriptFixing
      ;if printOrientation eq 'LANDSCAPE' then self->postScriptFixing
    endif
    print, '!D.NAME', 'self.currentDeviceName', 'self.previousDeviceName'
    print, !D.NAME, self.currentDeviceName, self.previousDeviceName
    return
  endif
  
  if self.currentDeviceName eq 'IMAGE' then begin
    if pageBreak eq 'TRUE' then self.mainView->saveImage
    if pageBreak eq 'CLOSE' then begin
      self.mainView->saveImage, fileName
      self.deviceIsOpen=0
      set_plot, self.previousDeviceName
      self->setCurrentDevice, self.previousDeviceName
    endif
    print, '!D.NAME', 'self.currentDeviceName', 'self.previousDeviceName'
    print, !D.NAME, self.currentDeviceName, self.previousDeviceName
    return
  endif
  
  if ~self->currentDeviceIsPostscript() then begin
    set_plot, self.previousDeviceName
    self->setCurrentDevice, self.previousDeviceName
    self.deviceIsOpen=0
    print, '!D.NAME', 'self.currentDeviceName', 'self.previousDeviceName'
    print, !D.NAME, self.currentDeviceName, self.previousDeviceName
    return
  endif
  
END

PRO Plotter::saveImage

  self.mainView->saveImage
  
END

PRO Plotter::wsetInfoDataDraw

  if ~self->currentDeviceIsPostscript() then self.mainView->wsetInfoDataDraw
  
END

PRO Plotter::wsetMainDataDraw

  if ~self->currentDeviceIsPostscript() then self.mainView->wsetMainDataDraw
  DEVICE, /DECOMPOSE
  
END

FUNCTION Plotter::buildXRange

  return, self->getStartIndex()+indgen(self->getEndIndex()-self->getStartIndex())
  
END

PRO Plotter::setStartIndex, value

  self.startIndex=value
  
END

FUNCTION Plotter::getStartIndex

  return, self.startIndex
  
END

PRO Plotter::setEndIndex, value

  self.endIndex=value
  
END

FUNCTION Plotter::getEndIndex

  return, self.endIndex
  
END

PRO Plotter::plotAll, request, result

  ;ToDo: call procedures from "diagram.dat" config file
  ; plot procedures may be found in "plot_routines.pro" file
  call_procedure, request.plotRoutine, self, request, result
  ;self->closePage
  call_procedure, request.plotRoutine+'Legend', self, request, result
;self->closePage
;Previous version (using internal objects)
;  call_method, request.plotRoutine, self, request, result
;  call_method, request.plotRoutine+'Legend', self, request, result
  
END

PRO Plotter::plotStandardLegend, request, result

  self.mainView->wsetInfoDataDraw
  
END

PRO Plotter::setInfoDataDrawArea

  if ~self->currentDeviceIsPostscript() then self.mainView->wsetInfoDataDraw
  DEVICE,/DECOMPOSE
  
END

PRO Plotter::setMainDataDrawArea

  if ~self->currentDeviceIsPostscript() then self.mainView->wsetMainDataDraw
  DEVICE,/DECOMPOSE
  
END

PRO Plotter::plotBarsLegend, request, result

  self->setInfoDataDrawArea
  white=obj_new('Color', 200, 200, 200)
  black=obj_new('Color', 0, 0, 0)
  whiteL=white->AsLongTrueColor()
  blackL=black->AsLongTrueColor()
  obj_destroy, white
  obj_destroy, black
  DEVICE, DECOMPOSED=1
  self->erase, whiteL
  ;  if ~self->currentDeviceIsPostscript() then begin
  ;    erase, whiteL
  ;    offset=0.
  ;  ;self.yoffset=0
  ;  endif else begin
  ;    offset=0.
  ;  ;self.yoffset=self.postscriptYoffset
  ;  endelse
  bpInfo=Result->getBarPlotInfo()
  plotInfo=result->getPlotInfo()
  legoSequenceNo=bpInfo->getLegoSequenceNumber()
  legoSequenceNames=bpInfo->getLegoSequenceNames()
  legoNames=bpInfo->getLegoNames()
  
  psyms=[2,1,4,5,6,7]
  legoWidth=.05
  legoHeight=.05
  startX=.01
  maxWidth=0
  
  for i=0, legoSequenceNo-1 do begin
    startY=1.-((i+1)*legoHeight*2)
    lego=[[startX,startY], [startX,startY+legoHeight], [startX+legoWidth,startY+legoHeight], [startX+legoWidth,startY]]
    if result->HaveObs() then begin
      legoCoords=self->legendNormalize(lego)
      polyfill, legoCoords, color=self->getLegoColorAtIndex(i), /NORM
      xyOutsCoord=self->legendNormalize([startX+legoWidth+.01, startY+.002])
      xyouts, xyOutsCoord[0], xyOutsCoord[1], 'Observations', COLOR=blackl, /NORM, charsize=.8, charthick=.8,  WIDTH=textWidth
      maxWidth=max([textWidth, maxWidth])
    endif
  ;xyouts, startX+legoWidth+.01, startY+.002, legoSequenceNames[i], COLOR=blackl, /NORM, charsize=.8, charthick=.8,  WIDTH=textWidth
  endfor
  
  ;  if criteria gt 0 then plots,[
  
  symbolSequenceNo=bpInfo->getSymbolSequenceNumber()
  if symbolSequenceNo gt 0 then begin
    symbolSequenceNames=bpInfo->getSymbolSequenceNames()
    symbolPSym=bpInfo->getPSymUsed()
    symbolPSymNo=n_elements(symbolPSym)
  endif
  
  for i=0, symbolSequenceNo-1 do begin
    startY=1.-((i+1)*legoHeight*2)
    thisStartX=startX+maxWidth+legoWidth+.02
    lego=[[thisStartX,startY], [thisStartX,startY+legoHeight], [thisStartX+legoWidth,startY+legoHeight], [thisStartX+legoWidth,startY]]
    legoCoords=self->legendNormalize(lego)
    plots, legoCoords, color=self->getSymbolColorAtIndex(i), psym=symbolPSym[i mod symbolPSymNo], /NORM
    xyOutsCoords=self->legendNormalize([thisStartX+legoWidth+.01, startY+.002])
    xyouts, xyOutsCoords[0], xyOutsCoords[1], symbolSequenceNames[i], COLOR=blackl, /NORM, charsize=.8, charthick=.8,  WIDTH=textWidth
  endfor
  
END

PRO Plotter::plotLegoAndSymbols, outLegoCoords, outSymbolCoords, legoValues, barName, symbolValues=symbolValues, $
    title=title, valuesRange=valuesRange, legonames=legonames, $
    legoColors=legoColors, symbolColors=symbolColors, baselines=baselines,$
    xtitle=xtitle,ytitle=ytitle,baserange=baserange, outTickPos=outTickPos, $
    barwidth=barwidth,barspace=barspaceIn,baroffset=baroffset, $
    outline=outline,overplot=overplot,background=background, $
    rotate=rotate, usedPsym=usedPsym, FIRST=FIRST, DRAWAXIS=DRAWAXIS, tickvs=tickvs
    
  nbars=n_elements(legoValues)    ; Determine number of bars
  allVals=legoValues
  legoNans=finite(legovalues)
  nSymbols=0
  if keyword_set(symbolValues) then begin
    nSymbols=n_elements(symbolValues)
    symbolNans=finite(symbolValues)
    ; Default sym colors spaced evenly in current color table
    ;    if not(keyword_set(symbolColors)) then $
    ;      symbolColors=256-fix(((!d.n_colors < 256)/float(nColors+1))*(Lindgen(nColors+1)+0.5))
    allVals=[allVals, reform(symbolValues, n_elements(symbolValues))]
    outSymbolCoords=ptrarr(nSymbols)
  endif
  nColors=1+nSymbols
  ; Baselines (bars extend from baselines through values); default=0
  if not(keyword_set(baselines)) then baselines=intarr(nbars)
  ; Default colors spaced evenly in current color table
  ;  if not(keyword_set(legoColors)) then $
  ;    legoColors=fix(((!d.n_colors < 256)/float(nColors+1))*(Lindgen(nColors+1)+0.5))
  ; Labels for the individual bars; none by default
  ;barnames = (N_Elements(barnamesIn) gt 0) ? barnamesIn : strarr(nbars)+' '
  ; Main title
  if not(keyword_set(title)) then title=''
  ; Centered title under X-axis
  if not(keyword_set(xtitle)) then xtitle=''
  ; Title for Y-axis
  if not(keyword_set(ytitle)) then ytitle=''
  ; Fraction (0-1) of full X range to use
  if not(keyword_set(baserange)) then baserange=1.0
  ; Space betw. bars, taken from nominal bar widths; default is none
  barspace = (N_Elements(barspaceIn) gt 0) ? Float(barspaceIn) : 0.2
  ; Bar width scaling factor, relative to nominal
  if not(keyword_set(barwidth)) then barwidth=1.0 - barspace - barspace / nbars
  ; Initial X offset, in scaled bar widths; default is none
  if not(keyword_set(baroffset)) then baroffset=barspace/barwidth
  ; Outline of bars; default is none
  outline = keyword_set(outline)
  ; Overplot (do not erase the existing display); default is to create new plot
  overplot = keyword_set(overplot)
  ; Background color index; defaults to 0 (usually black) if not specified
  if not(keyword_set(background)) then background=0
  ; Rotate (make horizontal bars); default is vertical bars
  rotate = keyword_set(rotate)
  xRange=[0.,1.]
  mnB = MIN(baselines, MAX=mxB, /NAN)
  mnV = MIN(allVals, MAX=mxV, /NAN)
  
  range=[mnB < mnV, $    ;Minimum of bases & values
    mxB > mxV]      ;Maximum of bases & values
    
  if (rotate) then begin           ;Horizontal bars
    if (!x.range[0] eq 0) and (!x.range[1] eq 0) $  ;Determine range for X-axis
      then xrange=valuesRange $
    else xrange=valuesRange;!x.range         ;Or, use range specified
    if (!y.range[0] eq 0) and (!y.range[1] eq 0) $  ;Plot will calculate
      then $                                       ; defaults for X, but not
      yrange = [0., n_elements(values)] $         ; for Ys, so fill in here.
    else $
      yrange=!y.range          ;Axis perpend. to bars
    yticks=1            ;Suppress ticks in plot
    ytickname=strarr(2)+' '
    xticks=0
    xtickname=strarr(1)+''
  endif else begin           ;Vertical bars
    if (!y.range[0] eq 0) and (!y.range[1] eq 0) $  ;Determine range for Y-axis
      then yrange=valuesRange $
    else yrange=valuesRange;!y.range                 ;Or, use range specified
    xrange=!x.range           ;Axis perpend. to bars
    xticks=1            ;Suppress ticks in plot
    xtickname=strarr(2)+' '
    yticks=0
    ytickname=strarr(1)+''
  endelse
  if (keyword_set(FIRST)) then begin        ;Create new plot, no data
  
    plot,valuesRange,/nodata,title=title,xtitle=xtitle,ytitle=ytitle, $
      ;noerase=overplot,xrange=xrange,yrange=yrange,xticks=xticks, $
      noerase=1,xrange=xrange,yrange=yrange,xticks=xticks, $
      xtickname=xtickname,yticks=yticks,ytickname=ytickname, $
      xstyle=1,ystyle=1,/data,background=background,color=0, position=self->getPosition()
  endif
  if (rotate) then begin           ;Horizontal bars
    base_win=!y.window          ;Window range in Y
    scal_fact=!x.s          ;Scaling factors
    tick_scal_fact=!y.s           ;Tick scaling factors
  endif else begin           ;Vertical bars
    base_win=!x.window          ;Window range in X
    scal_fact=!y.s          ;Scaling factors
    tick_scal_fact=!x.s           ;Tick scaling factors
  endelse
  winrange=baserange*(base_win[1]-base_win[0])     ;Normal. window range
  barsize=barwidth*winrange/nbars        ;Normal. bar width
  winoffset=base_win[0]+(baroffset*barsize)    ;Normal. first offset
  bases=scal_fact[0]+(scal_fact[1]*baselines)    ;Baselines, in normal coor.
  normal=scal_fact[0]+(scal_fact[1]*legoValues)    ;Values, in normal coor.
  if keyword_set(symbolValues) then begin
    symNormal=scal_fact[0]+(scal_fact[1]*symbolValues)    ;SymValues, in normal coor.
    usedPsym=[2,1,4,5,6,7]
    symBarPos=nbars/2
  endif
  barstart=Lindgen(nbars)*(barsize+barspace*(winrange/nbars)) ;Coor. at left edges
  tickv=winoffset+barstart+(0.5*barsize)       ;Tick coor. (centered)
  print, '-->', barsize
  if not(keyword_set(FIRST)) then begin
    for i=0,nbars-1 do begin         ;Draw the bars
      width=winoffset+[barstart[i],barstart[i], $     ;Compute bar width
        (barstart[i]+barsize),(barstart[i]+barsize)]
      length=[bases[i],normal[i],normal[i],bases[i]]  ;Compute bar length
      idxs=where(width[*] gt 1., count)
      if count gt 0 then width[idxs]=.95
      if keyword_set(symbolValues) then begin
        widthMiddle=winoffset+(barstart[i]+barsize)/2
        lengthMiddle=symNormal[*]
      endif
      if (rotate) then begin        ;Horizontal bars
        x=length             ;X-axis is "length" axis
        y=width            ;Y-axis is "width" axis
        if legonans eq 1 then begin
          outLegoCoordsV=[[x],[y]]
          polyfill,x,y,color=colors[i],/normal       ;Polyfill with color
        endif
        outTickPos=(x[0]+x[1])/2
        if keywod_set(symbolValues) then begin
          xSym=lengthMiddle             ;X-axis is "length" axis
          ySym=(width[0]+width[2])/2            ;Y-axis is "width" axis
          if i eq symBarPos then begin
            ;print, 'nSymbols-->', nSymbols
            for j=0, nSymbols-1 do begin
              if symbolNans[j] eq 1 then begin
                outSymbolCoords=ptr_new([xSym[j], ySym], /NO_COPY)
                plots, xSym[j], ySym,color=symbolColors[j], /normal, psym=usedPsym[j mod 6], SYMSIZE=1.
              endif
            endfor
          endif
        endif
      endif else begin          ;Vertical bars
        x=width            ;X-axis is "width" axis
        y=length            ;Y-axis is "length" axis
        outLegoCoordsV=[[x],[y]]
        legoDims=(x[3]-x[1])/2
        if legoNans eq 1 then begin
          polyfill,x,y,color=legoColors[i],/normal       ;Polyfill with color
        endif
        ;test text
        plots, [(x[0]+x[2])/2, (x[0]+x[2])/2], [y[0], y[0]-.02], /NORMAL, color=0
        xyouts, x[0], y[0]-.04, barname, /NORMAL, ALIGN=0., COLOR=0
        
        outTickPos=(x[0]+x[1])/2
        if keyword_set(symbolValues) then begin
          xSym=(width[0]+width[2])/2            ;X-axis is "width" axis
          ySym=lengthMiddle             ;Y-axis is "length" axis
          if i eq symBarPos then begin
            ;print, 'nSymbols-->', nSymbols
            for j=0, nSymbols-1 do begin
              if symbolNans[j] eq 1 then begin
                symX=outLegoCoordsV[*,0]
                ;symX=outTickPos
                outSymbolCoords[j]=ptr_new([[symX],[ySym[j]-legoDims, ySym[j]+legoDims, ySym[j]+legoDims, ySym[j]-legoDims]], /NO_COPY)
                plots, xSym, ySym[j],color=symbolColors[j], /normal, psym=usedPsym[j mod 6], thick=3,SYMSIZE=1.5
              endif
            endfor
          endif
        endif
      endelse
      if (outline) and (legoNans eq 1) then begin
        plots,x,y,/normal       ;Outline using !p.color
      endif
    endfor
  endif
  outLegoCoords=ptr_new(outLegoCoordsV, /NO_COPY)
  
;  if (keyword_set(DRAWAXIS)) then begin
;    if (rotate) then begin           ;Label the bars (Y-axis)
;      axis,yaxis=0,ystyle=1,yticks=(nbars-1),ytickv=tickvs,ytickname=legonames, $
;        yticklen=0.0, /NORMAL
;    endif else begin             ;Label the bars (X-axis)
;      legonumbers=n_elements(legonames)
;      tickvs[legonumbers-1]=outTickPos
;      axis,xaxis=0,xstyle=1,xticks=legonumbers-1,xtickv=tickvs,xtickname=legonames, $
;        xticklen=0.0, /NORMAL
;    endelse
;  endif
  
END

PRO Plotter::plotBars, request, result

  self->setMainDataDrawArea
  self->setStartIndex, request->getStartPlotIndex()
  self->setEndIndex, request->getEndPlotIndex()
  plotNo=result->getRawElementNumber()
  plotInfo=result->getPlotInfo()
  ;  singleRawData=result->getSingleRawData()
  ;  singlerDMatrix=result->getSingleRawDataCheckMatrix(monitIndexes, runIndexes)
  
  elabcode=request->getElaborationCode()
  if elabcode eq 2 then statistics='R'
  if elabcode eq 7 then statistics='IOA'
  if elabcode eq 8 then statistics='RDE'
  if elabcode eq 23 then statistics='NMB'
  if elabcode eq 28 then statistics='Target'
  if elabcode eq 30 then statistics='RPE'
  if elabcode eq 54 then statistics='MFS'
  if total(where(elabCode eq [2,7,8,23,28,30,54])) ge 0 then begin
; KeesC 30MAR2012: undefined obsTemp  
;    CheckCriteria, request, result, statistics, criteria, obsTemp, 0,alpha,criteriaOrig,LV,nobsAv
; changed into dummy
  adummy=fltarr(10) & adummy(*)=1.
    CheckCriteria, request, result, statistics, criteria, adummy, 0,alpha,criteriaOrig,LV,nobsAv
  endif else begin
    criteria=0
  endelse
  
  title=plotInfo->getTitle()
  xTitle=plotInfo->getXTitle()
  yTitle=plotInfo->getYTitle()
  xRange=plotInfo->getXRange()
  yRange=plotInfo->getYRange()
  xTicks=plotInfo->getxTicks()
  xTickNumber=plotInfo->getXTickNumber()
  xTickNames=plotInfo->getXTickNames(self->getStartIndex(), self->getEndIndex(), /SUBSET)
  yTicks=plotInfo->getyTicks()
  yTickNames=plotInfo->getyTickNames()
  xStyle=plotInfo->getxStyle()
  yStyle=plotInfo->getyStyle()
  ;position=plotInfo->getyStyle()
  xCharSize=plotInfo->getXCharSize()
  yCharSize=plotInfo->getYCharSize()
  charsize=plotInfo->getcharSize()
  
  bpInfo=Result->getBarPlotInfo()
  symbolSequencesNo=0
  if bpInfo->hasSymbolValues() then begin
    symbolSequencesNo=bpInfo->getSymbolSequenceNumber()
    symbolColors=self->buildSymbolColors(symbolSequencesNo)
    symbolValues=bpInfo->getSymbolValues()
  endif
  legoSequenceNo=bpInfo->getLegoSequenceNumber()
  legoNumbers=bpInfo->getLegoNumbers()
  legoColors=self->buildLegoColors(legoSequenceNo)
  valuesRange=bpInfo->getValuesRange()
  legoValues=bpInfo->getLegoValues()
  legoNames=bpInfo->getLegoNames()
  
  white=obj_new('Color', 255, 255, 255)
  whiteL=white->AsLongTrueColor()
  obj_destroy, white
  ;self.mainView->wsetMainDataDraw
  DEVICE, DECOMPOSED=1
  ;    DEVICE,DECOMPOSE=0
  LOADCT,39
  ; use "tek" color table...
  ;    mytek_color;, 0, 32
  ;Make axes black:
  ;!P.COLOR=0
  self->erase, whiteL
  ;if ~self->currentDeviceIsPostscript() then erase, whiteL
  
  ;BAR_PLOT_EXTRA, legoValues[0:legoSeries-1,0], symbolValues[0:symbolSeries-1,0], COLORS=legoColors, BACKGROUND=255, $
  ;BARWIDTH=0.73, BARSPACE=0.05, BAROFFSET=0*(1.4*legoNumbers), barName=legoNames[0], $
  ;BASERANGE=1./legoNumbers, valuesRange=valuesRange, symColors=symbolColors
  ;FOR I = 1, legoNumbers-1 DO BEGIN
  ;BAR_PLOT_EXTRA, legoValues[0:legoSeries-1,i], symbolValues[0:symbolSeries-1,i], COLORS=extraColors, BACKGROUND=255, $
  ;BARWIDTH=0.73, BARSPACE=0.05, BAROFFSET=i*(1.4*legoSeries), barName=legoNames[i], $
  ;OVER=(I GT 0), BASERANGE=1./legoNumbers, valuesRange=valuesRange, symColors=extraSymColors
  ;ENDFOR
  i=0
  ; FM V2 To be filled with right information...
  recognizeHighLight=bytarr((symbolSequencesNo+1)*legonumbers)
  recognizeRegionEdges=ptrarr((symbolSequencesNo+1)*legonumbers) ; coords (normalized standard)
  recognizeNames=strarr((symbolSequencesNo+1)*legonumbers)
  recognizeValues=strarr((symbolSequencesNo+1)*legonumbers)
  ;recognizeHighLight: ptr_new(), $
  ;
  
  if bpInfo->hasSymbolValues() then thisSymbolValues=symbolValues[0:symbolSequencesNo-1,i]
  ;recognizeValues[0]=strcompress(legoValues[0], /REMOVE)
  ;recognizeNames[0]=legoNames[i]
  self->plotLegoAndSymbols, outLegoCoords, outSymbolCoords, legoValues[0:legoSequenceNo-1, i], legoNames[i], symbolValues=thisSymbolValues, $
    title=title, yTitle=yTitle, valuesRange=valuesRange, legoColors=legoColors, symbolColors=symbolColors, $
    BAROFFSET=i*(1.*legoNumbers), BASERANGE=1./legoNumbers, BARWIDTH=1./legoNumbers, BARSPACE=0.05, $
    BACKGROUND=whiteL, /FIRST
    
  if criteria gt 0 then begin
    symbolColorsTest=self->buildSymbolColors(10)
    if elabcode eq 2 or elabcode eq 7 then polyfill,[0,0,1,1,0],[criteria,min([1,valuesRange(1)]),min([1,valuesRange(1)]),criteria,criteria],/data,color=symbolColorsTest(1)
    if elabcode eq 8 or elabcode eq 30 or elabcode eq 28 then polyfill,[0,0,1,1,0],[0,min([criteria,abs(valuesRange(1))]),min([criteria,abs(valuesRange(1))]),0,0],/data,color=symbolColorsTest(1)
    if elabcode eq 23 or elabcode eq 54 then polyfill,[0,0,1,1,0],[-min([criteria,abs(valuesRange(0))]),min([criteria,abs(valuesRange(1))]),min([criteria,abs(valuesRange(1))]),-min([criteria,abs(valuesRange(0))]),-min([criteria,abs(valuesRange(0))])],/data,color=symbolColorsTest(1)
  endif
  
  ;recognizeRegionEdges[0]=ptr_new(outLegoCoords, /NO_COPY)
  saveOutTick=fltarr(legonumbers)
  symbolNames=bpInfo->getSymbolSequenceNames()
  ;lastIndex=0
  for i=0, legoNumbers-1 do begin
    if bpInfo->hasSymbolValues() then begin
      symIndex=(n_elements(thisSymbolValues)+1)*i
      nameIndex=n_elements(thisSymbolValues)*i
      thisSymbolValues=symbolValues[0:symbolSequencesNo-1,i]
      ;thisSymbolNames=symbolNames[0:symbolSequencesNo-1,i]
      ;thisSymbolNames=symbolNames[0, symIndex:symIndex+symbolSequencesNo-1]
      thisSymbolNames=symbolNames[0, nameIndex:nameIndex+symbolSequencesNo-1]
      recognizeValues[symIndex:symIndex+n_elements(thisSymbolValues)]=strcompress(([reform(legoValues[i]), thisSymbolValues]), /REMOVE)
      recognizeNames[symIndex:symIndex+n_elements(thisSymbolValues)]=reform([["OBS - "+legoNames[i]], [legoNames[i]+" - "+strcompress(thisSymbolNames, /REMOVE_ALL)]])
    endif else begin
      recognizeValues[i]=strcompress(reform(legoValues[i]), /REMOVE)
      recognizeNames[i]=["OBS - "+reform(legoNames[i])]
    endelse
    self->plotLegoAndSymbols, outLegoCoords, outSymbolCoords, legoValues[0:legoSequenceNo-1,i], legoNames[i], symbolValues=thisSymbolValues, $
      title=title, yTitle=yTitle, valuesRange=valuesRange, legoColors=legoColors, symbolColors=symbolColors, $
      BAROFFSET=i*(1.*legoNumbers), BASERANGE=1./legoNumbers, BARWIDTH=1./legoNumbers, BARSPACE=0.05, $
      BACKGROUND=whiteL, OVERPLOT=self->getOverplotKeyword(1), DRAWAXIS=(i eq legoNumbers-1), legonames=legonames, outTickPos=outTickPos, tickvs=saveOutTick
    if bpInfo->hasSymbolValues() then begin
      ;help, [outLegoCoords, outSymbolCoords]
      ;help, *outLegoCoords
      if n_elements(outSymbolCoords) eq 0 then begin
        myCoords=*outLegoCoords
        outSymbolCoords=ptr_new(myCoords, /NO_COPY)
      endif
      outLegoCoords=self->sizeCorrection(outLegoCoords, /X)
      outSymbolCoords=self->sizeCorrection(outSymbolCoords, /X, /Y, /SYMBOL)
      recognizeRegionEdges[symIndex:symIndex+n_elements(thisSymbolValues)]=[outLegoCoords, outSymbolCoords]
    endif else begin
      ;help, outLegoCoords
      ;print, *outLegoCoords
      outLegoCoords=self->sizeCorrection(outLegoCoords, /X)
      recognizeRegionEdges[i]=outLegoCoords
    endelse
    saveOutTick[i]=outTickPos
  endfor
  ;criteria set for RDE
  ;  plots,[0,1],[50,50],/data,color=150,thick=2
  ;  print, "recognizeValues"
  ;  help, recognizeValues
  ;  print, recognizeValues
  ;  print, "***************"
  ;  print, "recognizeNames"
  ;  help, recognizeNames
  ;  print, recognizeNames
  ;  print, "***************"
  ;  print, "recognizeRegionEdges"
  ;  help, recognizeRegionEdges
  ;  print, recognizeRegionEdges
  ;  print, "***************"
  
  rInfo = obj_new("RecognizeInfo", recognizeNames, recognizeValues, recognizeHighLight, recognizeRegionEdges)
  ;plotInfo->setRecognizeHighLight,
  ;plotInfo->setRecognizeValues,
  ;plotInfo->setRecognizeNames,
  ;plotInfo->setRecognizeRegionEdges,
  plotInfo->setRecognizeInfo, rInfo
  
  print, '...draw done...'
  
END

PRO Plotter::plotTimeSeriesLegend, request, result

  self.mainView->wsetInfoDataDraw
  white=obj_new('Color', 200, 200, 200)
  black=obj_new('Color', 0, 0, 0)
  whiteL=white->AsLongTrueColor()
  blackL=black->AsLongTrueColor()
  obj_destroy, white
  obj_destroy, black
  ;self.mainView->wsetMainDataDraw
  DEVICE, DECOMPOSED=1
  ;Make axes black:
  ;!P.COLOR=0
  self->erase, whiteL
  plotNo=result->rawElementNumber()
  plotInfo=result->getPlotInfo()
  title=plotInfo->getTitle()
  ;  rawData=result->getRawData()
  ;  rDMatrix=result->getRawDataCheckMatrix(monitIndexes, runIndexes)
  singleRawData=result->getSingleRawData()
  singleRDMatrix=result->getSingleRawDataCheckMatrix(sMonitIndexes, sRunIndexes)
  groupRawData=result->getGroupRawData()
  groupRDMatrix=result->getGroupRawDataCheckMatrix(gMonitIndexes, gRunIndexes)
  psyms=[2,1,4,5,6,7]
  legoWidth=.05
  legoHeight=.05
  startX=.01
  maxWidth=-1
  
  if monitIndexes[0] ne -1 then begin
    i=0
    obsNames=rawData.observedCode
    toPlotNames=obsNames[uniq(obsNames, sort(obsNames))]
    mElems=n_elements(toPlotNames)
    toPlotIndexes=intarr(mElems)
    title=title+rawData[[monitIndexes[i]]].parameterCode
    plotInfo->setTitle, title
    for j=0, mElems-1 do toPlotIndexes[j]=(where(toPlotNames[j] eq obsNames))[0]
    name=toPlotNames[i];rawData[monitIndexes[i]].observedCode
    penColor=self->getLegoColorAtIndex(i)
    BACKGROUND=whiteL
    psym=psyms[0]
    startY=1.-((i+1)*legoHeight*2)
    lego=[[startX,startY], [startX,startY+legoHeight], [startX+legoWidth,startY+legoHeight], [startX+legoWidth,startY]]
    plots, lego, color=penColor, psym=psym, /NORM, SYMSIZE=1.5
    xyouts, startX+legoWidth+.01, startY+.002, name, COLOR=blackl, /NORM, charsize=.8, charthick=.8,  WIDTH=textWidth
    maxWidth=max([textWidth, maxWidth])
    for i=1, mElems-1 do begin
      penColor=self->getLegoColorAtIndex(i)
      name=toPlotNames[i]
      psym=psyms[i mod 6]
      startY=1.-((i+1)*legoHeight*2)
      lego=[[startX,startY], [startX,startY+legoHeight], [startX+legoWidth,startY+legoHeight], [startX+legoWidth,startY]]
      plots, lego, color=penColor, psym=psym, /NORM, SYMSIZE=1.5
      xyouts, startX+legoWidth+.01, startY+.002, name, COLOR=blackl, /NORM, charsize=.8, charthick=.8,  WIDTH=textWidth
      maxWidth=max([textWidth, maxWidth])
    endfor
  endif
  
  if runIndexes[0] ne -1 then begin
    rElems=n_elements(runIndexes)
    for i=0, rElems-1 do begin
      penColor=self->getSymbolColorAtIndex(i)
      name=rawData[runIndexes[i]].scenarioCode+' '+rawData[runIndexes[i]].modelCode+' '+rawData[runIndexes[i]].observedCode
      psym=psyms[i mod 6]
      startY=1.-((i+1)*legoHeight*2)
      thisStartX=startX+maxWidth+legoWidth+.02
      lego=[[thisStartX,startY], [thisStartX,startY+legoHeight], [thisStartX+legoWidth,startY+legoHeight], [thisStartX+legoWidth,startY]]
      plots, lego, color=penColor, psym=psym, /NORM, SYMSIZE=1.5
      xyouts, thisStartX+legoWidth+.01, startY+.002, name, COLOR=blackl, /NORM, charsize=.8, charthick=.8,  WIDTH=textWidth
    endfor
  endif
  
END

PRO Plotter::plotTimeSeries, request, result

  self.mainView->wsetMainDataDraw
  device, decomposed=1
  white=obj_new('Color', 200, 200, 200)
  black=obj_new('Color', 0, 0, 0)
  whiteL=white->AsLongTrueColor()
  blackL=black->AsLongTrueColor()
  obj_destroy, white
  obj_destroy, black
  
  self->setStartIndex, request->getStartPlotIndex()
  self->setEndIndex, request->getEndPlotIndex()
  
  isSingleSelection=request->isSingleObsPresent()
  isGroupSelection=request->isGroupObsPresent()
  
  plotNo=result->getrawElementNumber()
  plotInfo=result->getPlotInfo()
  
  if isSingleSelection then begin
  singleRawData=result->getSingleRawData()
  singleRDMatrix=result->getSingleRawDataCheckMatrix(sMonitIndexes, sRunIndexes)
  endif
  if isGroupSelection then begin
  groupRawData=result->getGroupRawData()
  groupRDMatrix=result->getGroupRawDataCheckMatrix(gMonitIndexes, gRunIndexes)
  endif
  
  psym=[2,1,4,5,6,7]
  title=plotInfo->getTitle()
  xTitle=plotInfo->getXTitle()
  yTitle=plotInfo->getYTitle()
  xRange=plotInfo->getXRange()
  yRange=plotInfo->getYRange()
  xTicks=plotInfo->getxTicks()
  xTickNumber=plotInfo->getXTickNumber()
  xTickNames=plotInfo->getXTickNames(self->getStartIndex(), self->getEndIndex(), /SUBSET)
  yTicks=plotInfo->getyTicks()
  yTickNames=plotInfo->getyTickNames()
  xStyle=plotInfo->getxStyle()
  yStyle=plotInfo->getyStyle()
  position=plotInfo->getyStyle()
  xCharSize=plotInfo->getXCharSize()
  yCharSize=plotInfo->getYCharSize()
  charsize=plotInfo->getcharSize()
  
  if smonitIndexes[0] ne -1 then begin
    i=0
    obsNames=singleRawData.observedCode
    toPlotIndexes=uniq(obsNames[sort(obsNames)])
    mElems=n_elements(toPlotIndexes)
    values=*singleRawData[toPlotIndexes[0]].observedData
    penColor=self->getLegoColorAtIndex(i)
    overplot=0 & noerase=0
    plotInfo->setTitle, title
    plot, values[self->getStartIndex():self->getEndIndex()], color=penColor, title=title, xtitle=xtitle, ytitle=ytitle, yrange=yrange, $
      xstyle=1, xtickname=xTickNames, xTicks=xTickNumber, $;, xrange=[self->getStartIndex(),self->getEndIndex()], $
      xcharsize=.7, BACKGROUND=whiteL, psym=-psym[i];, ymargin=[.25,.75], xmargin=[.25,.75]
    ;noerase=(overplot or noerase),xrange=xrange,yrange=yrange,xticks=xticks, $
    ;xtickname=xtickname,yticks=yticks,ytickname=ytickname, $
    ;xstyle=1,ystyle=1,background=background, position=position, $
    ;XCHARSIZE=XCHARSIZE, YCHARSIZE=YCHARSIZE, charsize=charsize, color=penColor ; ,/data
    for i=1, mElems-1 do begin
      penColor=self->getLegoColorAtIndex(i)
      values=*rawData[toPlotIndexes[i]].observedData
      oplot, values[self->getStartIndex():self->getEndIndex()], color=penColor, psym =-psym[i mod 6]
    endfor
  endif
  
  if srunIndexes[0] ne -1 then begin
    rElems=n_elements(runIndexes)
    for i=0, rElems-1 do begin
      penColor=self->getSymbolColorAtIndex(i)
      values=*rawData[runIndexes[i]].runData
      oplot, values[self->getStartIndex():self->getEndIndex()], color=penColor, psym =-psym[i mod 6]
    endfor
  endif
  print, '...draw done...'
  
END

PRO Plotter::plotScatterLegend, request, result

  self->plotStandardLegend, request, result
  self.mainView->wsetInfoDataDraw
  
END

PRO Plotter::plotScatter, request, result

  ;To be implemented
  print, '...draw done...'
  
END

PRO Plotter::plotTaylorLegend, request, result

  ;To be implemented
  print, '...draw done...'
  
END

PRO Plotter::plotTaylor, request, result

  ;To be implemented
  print, '...draw done...'
  
END

PRO Plotter::plot2DLegend, request, result

  ;To be implemented
  print, '...draw done...'
  
END

PRO Plotter::plot2D, request, result

  ;To be implemented
  print, '...draw done...'
  
END

PRO Plotter::plotUndefLegend, request, result

  ;To be implemented
  print, '...draw done...'
  
END

PRO Plotter::plotUndef, request, result

  ;To be implemented
  print, '...draw done...'
  
END

FUNCTION Plotter::getCodes

  thisList=*self.list
  return, thisList[*].code
  
END

FUNCTION Plotter::getNames

  thisList=*self.list
  return, thisList[*].name
  
END

FUNCTION Plotter::getIdlRoutineCodes

  thisList=*self.list
  return, thisList[*].idlRoutineCode
  
END

FUNCTION Plotter::getMultipleChoices

  thisList=*self.list
  return, thisList[*].multipleChoice
  
END

FUNCTION Plotter::getDescriptions

  thisList=*self.list
  return, thisList[*].description
  
END

PRO Plotter::streamPrint

  print, '***********************'
  print, '**Start of<',OBJ_CLASS(self),'>**'
  
  print, '**** fileName:<', self.fileName,'>'
  thisList=*self.list
  for i=0, n_elements(thisList)-1 do begin
    print, '**element n.<', strcompress(i, /REMOVE),'>'
    print, '**** code:<', thisList[i].code,'>'
    print, '**** IDLRoutineCode:<', thisList[i].idlRoutineCode,'>'
    print, '**** multipleChoice:<', thisList[i].multipleChoice,'>'
    print, '**** description:<', thisList[i].description,'>'
    print, '**** name:<', thisList[i].name,'>'
    print, '**'
  endfor
  
  print, '***********************'
  print, '**End of:<',OBJ_CLASS(self),'>**'
  
END

FUNCTION  Plotter::getLegoColorAtIndex, index

  return, self.legoColors[index mod n_elements(self.legoColors)]
  
END

FUNCTION  Plotter::getCurrentLegoColor

  return, self->getLegoColorAtIndex(self.legoColorIndex)
  
END

FUNCTION  Plotter::getNextLegoColor

  self.legoColorIndex++
  return, self->getCurrentLegoColor()
  
END

FUNCTION  Plotter::getSymbolColorAtIndex, index

  return, self.symbolColors[index mod n_elements(self.symbolColors)]
  
END

FUNCTION  Plotter::getCurrentSymbolColor

  return, self->getSymbolColorAtIndex(self.symbolColorIndex)
  
END

FUNCTION  Plotter::getNextSymbolColor

  self.symbolColorIndex++
  return, self->getCurrentSymbolColor()
  
END

FUNCTION Plotter::buildLegoColors, howMany

  return, self.legoColors[indgen(howMany)]
  
END

FUNCTION Plotter::buildSymbolColors, howMany

  return, self.symbolColors[indgen(howMany)]
  
END

PRO Plotter::configureColor

  legoColors=lonarr(15)
  symbolColors=lonarr(15)
  testColor=obj_new('Color', 255, 0, 0)
  legoColors[0]=testColor->AsLongTrueColor();red
  testColor->setRGB, 0, 255, 0 & legoColors[1]=testColor->AsLongTrueColor();green
  testColor->setRGB, 0, 0, 255 & legoColors[2]=testColor->AsLongTrueColor();blue
  testColor->setRGB, 255, 255, 0 & legoColors[3]=testColor->AsLongTrueColor();comp1
  testColor->setRGB, 255, 0, 255 & legoColors[4]=testColor->AsLongTrueColor();comp2
  testColor->setRGB, 0, 255, 255 & legoColors[5]=testColor->AsLongTrueColor();comp3
  testColor->setRGB, 128, 128, 0 & legoColors[6]=testColor->AsLongTrueColor();comp4
  testColor->setRGB, 128, 0, 128 & legoColors[7]=testColor->AsLongTrueColor();comp5
  testColor->setRGB, 0, 128, 128 & legoColors[8]=testColor->AsLongTrueColor();comp6
  testColor->setRGB, 128, 128, 255 & legoColors[9]=testColor->AsLongTrueColor();comp4
  testColor->setRGB, 128, 255, 128 & legoColors[10]=testColor->AsLongTrueColor();comp5
  testColor->setRGB, 255, 128, 128 & legoColors[11]=testColor->AsLongTrueColor();comp6
  testColor->setRGB, 128, 128, 128 & legoColors[12]=testColor->AsLongTrueColor();comp7
  testColor->setRGB, 0, 255, 0 & legoColors[13]=testColor->AsLongTrueColor();comp6
  testColor->setRGB, 0, 0, 255 & legoColors[14]=testColor->AsLongTrueColor();comp7
  self.legoColors=legoColors
  self.symbolColors=reverse(legoColors)
  self.symbolColorsIndex=0
  self.legoColorsIndex=0
  obj_destroy, testColor
  
END

PRO Plotter::fillDataFromFile, fileName

  self.fileName=fileName
  
  ERROR=0
  catch, error_status
  
  if error_status NE 0 THEN BEGIN
    ERROR=1
    catch, /CANCEL
    errMsg=dialog_message('problem with file: <'+fileName+'> check existence or read permission.', /ERROR)
    return
  endif
  
  openr, unit, fileName, /GET_LUN
  
  bufferString=''
  i=0
  ;plotTypes=getFMPlotType()
  while not(eof(unit)) do begin
    readf, unit, bufferString
    i++
    checkFirst=strmid(bufferString, 0,1)
    check1=(strpos(checkFirst, '[')+1) > 0
    check2=(strpos(checkFirst, ';')+1) > 0
    check3=(strpos(checkFirst, '#')+1) > 0
    null=strlen(strcompress(bufferString, /REMOVE)) eq 0
    ; #, ; is a comment
    ; void string string is discarded
    ; [ header is discarded
    if (check1+check2+check3) gt 0 or null then begin
;      print, 'Discard row', i
;      print, bufferString
    endif else begin
      info=strsplit(bufferString, ';', /EXTRACT)
      if n_elements(info) eq 5 then begin
        ;thisPlotType=getFMPlotType()
        thisPlotType.code=fix(info[0])
        thisPlotType.name=info[1]
        thisPlotType.idlRoutineCode=fix(info[2])
        thisPlotType.multipleChoice=info[3]
        thisPlotType.description=info[4]
        plotTypes=[plotTypes, thisPlotType]
      endif else begin
        print, 'Bad conf file at line', i, bufferString
      endelse
    endelse
  endwhile
  close, unit & free_lun, unit
  self.list=ptr_new(plotTypes[1:*], /NO_COPY)
  
END

PRO Plotter::cleanUp

  self -> Object :: cleanUp
  
END

FUNCTION Plotter::init, mainView

  if not (self -> Object :: init()) then return , 0
  self.mainView=mainView
  self.legendSpaceYNorm=0.25
  self->configureColor
  return , 1
  
END

PRO Plotter__Define

  Struct = { Plotter , $
    mainView: obj_new(), $
    lastPostScriptFileName: '', $
    startIndex: 0l, $
    endIndex: 0l, $
    deviceIsOpen: 0, $
    orientation: '', $
    position: fltarr(4), $
    legendSpaceYNorm: 0., $
    previousDeviceName: '', $
    currentDeviceName: '', $
    symbolColors: lonarr(15), $
    legoColors: lonarr(15), $
    symbolColorsIndex: 0, $
    legoColorsIndex: 0, $
    Inherits Object $
    }
    
END