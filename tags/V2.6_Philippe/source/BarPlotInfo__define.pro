;*****************************
; get/set
;*****************************
FUNCTION BarPlotInfo::hasSymbolValues

  if ptr_valid(self.symbolValues) then return,1 else return,0
  
END

FUNCTION BarPlotInfo::getLegoSequenceNames

  if ptr_valid(self.legoSequenceNames) then return, *self.legoSequenceNames else return, ['']
  
END

PRO BarPlotInfo::setLegoSequenceNames, names

  ;ptr_free, self.legoSequenceNames
  self.legoSequenceNames=ptr_new(names, /NO_COPY)
  
END

FUNCTION BarPlotInfo::getSymbolSequenceNames

  if ptr_valid(self.symbolSequenceNames) then return, *self.symbolSequenceNames else return, ['']
  
END

PRO BarPlotInfo::setSymbolSequenceNames, names

  ;ptr_free, self.symbolSequenceNames
  self.symbolSequenceNames=ptr_new(names, /NO_COPY)
  
END

FUNCTION BarPlotInfo::getLegoNames

  return, *self.legoNames
  
END

PRO BarPlotInfo::setLegoNames, names

  ;ptr_free, self.legoNames
  self.legoNames=ptr_new(names, /NO_COPY)
  
END

FUNCTION BarPlotInfo::getSymbolValues

  return, *self.symbolValues
  
END

PRO BarPlotInfo::setSymbolValues, values

  ;ptr_free, self.symbolValues
  self.symbolValues=ptr_new(values, /NO_COPY)
  
END

FUNCTION BarPlotInfo::getLegoSequenceNumber

  dims=size(self->getLegoValues(), /DIMENSIONS)
  if n_elements(dims) eq 1 then return, 1 else return, dims[0]
  
END

FUNCTION BarPlotInfo::getSymbolSequenceNumber

  if self->hasSymbolValues() then begin
    dims=size(self->getSymbolValues(), /DIMENSIONS)
    if n_elements(dims) eq 1 then return, 1 else return, dims[0]
  endif
  return, 0
  
END

FUNCTION BarPlotInfo::getLegoValues

  return, *self.legoValues
  
END

PRO BarPlotInfo::setLegoValues, values

  ;ptr_free, self.legoValues
  self.legoValues=ptr_new(values, /NO_COPY)
  
END

FUNCTION BarPlotInfo::getLegoNumbers

  return, n_elements(self->getLegoNames())
  
END

FUNCTION BarPlotInfo::getPSymUsed

  return, *self.pSymUsed
  
END

PRO BarPlotInfo::setPSymUsed, values

  ;ptr_free, self.psymUsed
  self.pSymUsed=ptr_new(values, /NO_COPY)
  
END

FUNCTION BarPlotInfo::getValuesRange

  allVals=self->getLegoValues()
  if self->hasSymbolValues() then begin
    symValues=self->getSymbolValues()
    allVals=[reform(allVals, n_elements(allVals)), reform( symValues, n_elements(symValues))]
  endif
  ymax=max(allVals, min=Ymin, /NAN)
  valuesRange=[yMin<0, ymax+ymax*.1]
  if ymin lt 0 and ymax lt 0 then valuesRange=[ymin*1.1,0]
  if ymin lt 0 and ymax gt 0 then valuesRange=[ymin*1.1,ymax+ymax*.1]
  return, valuesRange
  
END

PRO BarPlotInfo::standardSetting

  if self->hasSymbolValues() then begin
    symbolSequenceNo=self->getSymbolSequenceNumber()
    self->setPSymUsed, [2,1,4,5,6,7] ;cut 0,3 and over 7
  endif
  
END

;*****************************
; import/export utility
;*****************************

PRO BarPlotInfo::streamPrint

  print, '***********************'
  print, '**Start of<',OBJ_CLASS(self),'>**'
  print, '**** legoValues:', self->getLegoValues()
  print, '**** symbolValues:', self->getSymbolValues()
  print, '**** legoColors:', self->getLegoColors()
  print, '**** symbolColors:', self->getSymbolColors()
  print, '**** legoSequenceNames:', self->getLegoSequenceNames()
  print, '**** symbolSequenceNames:', self->getSymbolSequenceNames()
  print, '**** psymUsed:', self->getPSymUsed()
  
END

;*****************************
; constructor/destructor
;*****************************

PRO BarPlotInfo::cleanUp

  ptr_free, self.legoValues
  ptr_free, self.legoNames
  ptr_free, self.symbolValues
  ptr_free, self.legoSequenceNames
  ptr_free, self.symbolSequenceNames
  ptr_free, self.pSymUsed
  
  self->Object::cleanup
  
END

FUNCTION BarPlotInfo::init, filename

  if not (self -> Object :: init()) then return, 0
  return, 1
  
END

PRO BarPlotInfo__Define

  Struct = { BarPlotInfo , $
    legoValues: ptr_new(), $
    legoNames : ptr_new(), $
    legoSequenceNames : ptr_new(), $
    symbolValues: ptr_new(), $
    symbolSequenceNames : ptr_new(), $
    psymUsed: ptr_new(), $
    Inherits Object $
    }
    
END