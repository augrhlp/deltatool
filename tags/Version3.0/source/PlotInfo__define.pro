;****************************************************************************************
PRO PlotInfo::setXLabels, list

  self.xLabels=ptr_new(list, /NO_COPY)
  
END

FUNCTION PlotInfo::getXLabels, list

  if ptr_valid(self.xLabels) then return, *self.xLabels else return, -1
  
END

FUNCTION PlotInfo::buildXTickNames, xLabels, xTicksNumber, xrange

  xTickNames=''
  if n_elements(xLabels) ne 0 then begin
    xTickNumber=self->getXTickNumber()
    ;tickIdxs=indgen(xTickNumber+1)*((n_elements(xLabels)-1)/xTickNumber)
    ;xrange=[0., 8759.]
    tickIdxs=xrange[0]+indgen(xTickNumber+1)*((xrange[1]-xrange[0])/xTickNumber)
    ;print, '****>', tickIdxs
    xTickNames=xLabels[tickIdxs]
    for i=0, n_elements(xTickNames)-1 do begin
      length=strlen(xTickNames[i])
      if length gt 8 then xTickNames[i]=strmid(xTickNames[i], 0, length/2)+'!C'+strmid(xTickNames[i], length/2, length)
    endfor
  ; if userRangeInfo[0] eq 1 then begin
  ;   yMin=userRangeInfo[1]
  ;   yMax=userRangeInfo[2]
  ; endif
  endif
  return, xTickNames
  
END

FUNCTION PlotInfo::getXTickNumber

  return, 4
  
END

FUNCTION PlotInfo::getPenColorAtIndex, index

  return, self.penColor[index mod n_elements(self.penColor)]
  
END

FUNCTION PlotInfo::getPenColor

  return, self.penColor
  
END

PRO PlotInfo::setPenColor, list

  self.penColor=list
  
END

FUNCTION PlotInfo::getTitle

  return, self.title
  
END

PRO PlotInfo::setTitle, value

  self.title=value
  
END

FUNCTION PlotInfo::getXTitle

  return, '!C!C'+self.xTitle
  
END

PRO PlotInfo::setXTitle, value

  self.xTitle=value
  
END

FUNCTION PlotInfo::getYTitle

  return, self.yTitle
  
END

PRO PlotInfo::setYTitle, value

  self.yTitle=value
  
END

FUNCTION PlotInfo::getXRange

  return, self.xRange
;return, [0., 8759.]
  
END

PRO PlotInfo::setXRange, value

  self.xRange=value
  
END

FUNCTION PlotInfo::getYRange

  return, self.yRange
  
END

PRO PlotInfo::setYRange, value

  self.yRange=value
  
END

FUNCTION PlotInfo::getXTicks

  return, self.xTicks
  
END

PRO PlotInfo::setXTicks, value

  self.xTicks=value
  
END

FUNCTION PlotInfo::getYTicks

  return, self.yTicks
  
END

PRO PlotInfo::setYTicks, value

  self.yTicks=value
  
END

FUNCTION PlotInfo::getXTickNames, startIndex, endIndex, SUBSET=SUBSET

  if not(keyword_set(SUBSET)) then xRange=self.xRange else xRange=[startIndex, endIndex]
  if ptr_valid(self.xLabels) then begin
    allLabels=self->getXLabels()
    tickNames=self->buildXTickNames(allLabels, self.xTicks, xRange)
    return, tickNames
  endif
  return, [-1]
  
END

PRO PlotInfo::setXTickNames, list

  self.yTickNames=ptr_new(list, /NO_COPY)
  
END

FUNCTION PlotInfo::getYTickNames

  if ptr_valid(self.yTickNames) then return, *self.yTickNames else return, [-1]
  
END

PRO PlotInfo::setYTickNames, list

  self.yTickNames=ptr_new(list, /NO_COPY)
  
END

FUNCTION PlotInfo::getYStyle

  return, self.yStyle
  
END

PRO PlotInfo::setYStyle, value

  self.yStyle=value
  
END

FUNCTION PlotInfo::getXStyle

  return, self.xStyle
  
END

PRO PlotInfo::setXStyle, value

  self.xStyle=value
  
END

PRO PlotInfo::setXCharSize, value

  self.xCharSize=value
  
END

FUNCTION PlotInfo::getXCharSize

  return, self.xCharSize
  
END

PRO PlotInfo::setYCharSize, value

  self.yCharSize=value
  
END

FUNCTION PlotInfo::getYCharSize

  return, self.yCharSize
  
END

PRO PlotInfo::setCharSize, value

  self.charSize=value
  
END

FUNCTION PlotInfo::getCharSize

  return, self.charSize
  
END

PRO PlotInfo::setRecognizeInfo, recognizeInfo

  if obj_valid(self.recognizeInfo) then obj_destroy, self.recognizeInfo
  self.recognizeInfo=recognizeInfo
  
END

FUNCTION PlotInfo::getRecognizeInfo

  return, self.recognizeInfo
  
END

;****************************************************************************************

;****************************************************************************************

; Fair mode 2 - introduce cleanup (missed in previous version)

FUNCTION PlotInfo::cleanUp

  ptr_free, self.xLabels
  ptr_free, self.xTickNames
  ptr_free, self.yTickNames
  ptr_free, self.recognizeInfo
  
END

FUNCTION PlotInfo::init

  if not self -> Object::init() then return , 0
  red=[255,0,0]
  green=[0,255,0]
  blue=[0,0,255]
  yellow=[255,255,0]
  cyan=[0,255,255]
  purple=[255,0,255]
  sampleColor=obj_new('Color', red[0], red[1], red[2])
  self.penColor[0]=sampleColor->asLongTrueColor()
  sampleColor->setRGB, green[0], green[1], green[2]
  self.penColor[1]=sampleColor->asLongTrueColor()
  sampleColor->setRGB, blue[0], blue[1], blue[2]
  self.penColor[2]=sampleColor->asLongTrueColor()
  sampleColor->setRGB, yellow[0], yellow[1], yellow[2]
  self.penColor[3]=sampleColor->asLongTrueColor()
  sampleColor->setRGB, cyan[0], cyan[1], cyan[2]
  self.penColor[4]=sampleColor->asLongTrueColor()
  sampleColor->setRGB, purple[0], purple[1], purple[2]
  self.penColor[5]=sampleColor->asLongTrueColor()
  obj_destroy, sampleColor
  return , 1
  
end

;****************************************************************************************

pro PlotInfo__Define

  Struct = { PlotInfo , $
    recognizeInfo: obj_new(""), $
    penColor: lonarr(6), $
    title: '', $
    xTitle: '', $
    yTitle: '', $
    xRange: [0.,0.], $
    yRange: [0.,0.] ,$
    xTicks: 0, $
    yTicks: 0, $
    xLabels: ptr_new(), $
    xTickNames: ptr_new(), $
    yTickNames: ptr_new(), $
    yStyle: 0, $
    xStyle: 0 ,$
    xCharSize:0. ,$
    yCharSize:0., $
    charSize:0., $
    Inherits Object $
    }
    
end

;****************************************************************************************
