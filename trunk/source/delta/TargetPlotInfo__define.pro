;*****************************
; get/set
;*****************************

FUNCTION TargetPlotInfo::getLegendSymbols

  if ptr_valid(self.legendSymbols) then return, *self.legendSymbols else return, ''
  
END

PRO TargetPlotInfo::setLegendSymbols, legendSymbols

  self.legendSymbols=ptr_new(legendSymbols, /NO_COPY)
  
END

FUNCTION TargetPlotInfo::getLegendNames

  if ptr_valid(self.legendNames) then return, *self.legendNames else return, ''
  
END

PRO TargetPlotInfo::setLegendNames, legendNames

  self.legendNames=ptr_new(legendNames, /NO_COPY)
  
END

FUNCTION TargetPlotInfo::getLegendColors

  if ptr_valid(self.legendColors) then return, *self.legendColors else return, 0
  
END

PRO TargetPlotInfo::setLegendColors, legendColors

  self.legendColors=ptr_new(legendColors, /NO_COPY)
  
END

FUNCTION TargetPlotInfo::getXYs

  if ptr_valid(self.xys) then return, *self.xys else return, [0,0]
  
END

PRO TargetPlotInfo::setXYs, XYs

  self.xys=ptr_new(xys, /NO_COPY)
  
END

FUNCTION TargetPlotInfo::getColors

  if ptr_valid(self.colors) then return, *self.colors else return, [0]
  
END

PRO TargetPlotInfo::setColors, colors

  self.colors=ptr_new(colors, /NO_COPY)
  
END

FUNCTION TargetPlotInfo::getSymbols, symbols

  if ptr_valid(self.symbols) then return, *self.symbols else return, ['']
  
END

PRO TargetPlotInfo::setSymbols, symbols

  self.symbols=ptr_new(symbols, /NO_COPY)
  
END

;*****************************
; import/export utility
;*****************************

PRO TargetPlotInfo::streamPrint

  print, '***********************'
  print, '**Start of<',OBJ_CLASS(self),'>**'
  print, '**** xys:', self->getXYs()
  print, '**** symbols:', self->getSymbols()
  print, '**** colors:', self->getColors()
  print, '**** legendNames:', self->getLegendNames()
  print, '**** legendColors:', self->getLegendColors()
  print, '**** legendSymbols:', self->getLegendSymbols()
  
END

;*****************************
; constructor/destructor
;*****************************

PRO TargetPlotInfo::cleanUp

  ptr_free, self.XYs
  ptr_free, self.colors
  ptr_free, self.symbols
  ptr_free, self.legendColors
  ptr_free, self.legendNames
  ptr_free, self.legendSymbols
  
  self->Object::cleanup
  
END

FUNCTION TargetPlotInfo::init, filename

  if not (self -> Object :: init()) then return, 0
  return, 1
  
END

PRO TargetPlotInfo__Define

  Struct = { TargetPlotInfo , $
    legendSymbols: ptr_new(), $
    legendColors: ptr_new(), $
    legendNames: ptr_new(), $
    colors: ptr_new(), $
    symbols : ptr_new(), $
    xys : ptr_new(), $
    Inherits Object $
    }

END