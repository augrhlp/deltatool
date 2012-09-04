FUNCTION RecognizeInfo::Clone, DEEP=DEEP

  names = *self.names
  values = *self.values
  highLights=*self.highLights
  clone=obj_new("RecognizeInfo", names, values, highLights)
  rEdges = *self.regionEdges
  if ptr_valid(self.regionEdges) then begin
    rEdges=*self.regionEdges
    if keyword_set(DEEP) then begin
      copyREdges=ptrarr(n_elements(rEdges))
      for i=0, n_elements(rEdges)-1 do begin
        if ptr_valid(rEdges[i]) then begin
          tempCopy=*rEdges[i]
          copyREdges[i]=ptr_new(tempCopy, /NO_COPY)
        endif
      endfor
      clone->setRegionEdges, copyREdges
    endif else begin
      clone.regionEdges=self.regionEdges
    endelse
  endif
  
  return, clone
  
END

PRO RecognizeInfo::setRegionEdges, values

  if ptr_valid(self.regionEdges) then ptr_free, self.regionEdges
  self.regionEdges=ptr_new(values, /NO_COPY)
  
END

FUNCTION RecognizeInfo::getRegionEdges

  if ptr_valid(self.regionEdges) then return, *self.regionEdges
  
END

PRO RecognizeInfo::setNames, strings

  if ptr_valid(self.names) then ptr_free, self.names
  self.names=ptr_new(strings, /NO_COPY)
  
END

FUNCTION RecognizeInfo::getNames

  if ptr_valid(self.names) then return, *self.names
  
END

PRO RecognizeInfo::setValues, strings

  if ptr_valid(self.values) then ptr_free, self.values
  self.values=ptr_new(strings, /NO_COPY)
  
END

FUNCTION RecognizeInfo::getValues

  if ptr_valid(self.values) then return, *self.values
  
END

PRO RecognizeInfo::setHighLights, values

  if ptr_valid(self.highLights) then ptr_free, self.highLights
  self.highLights=ptr_new(values, /NO_COPY)
  
END

FUNCTION RecognizeInfo::getHighLights

  if ptr_valid(self.highLights) then return, *self.highLights
  
END


;****************************************************************************************

;****************************************************************************************

FUNCTION RecognizeInfo::cleanUp

  ptr_free, self.regionEdges
  if ptr_valid(self.regionEdges) then begin
    rEdges=*self.regionEdges
    for i=0, n_elements(rEdges)-1 do begin
      ptr_free, rEdges[i]
    endfor
  endif
  ptr_free, self.names
  ptr_free, self.values
  ptr_free, self.highLights
  
END

FUNCTION RecognizeInfo::init, names, values, highLights, rEdges

  if not self -> Object::init() then return , 0
  if n_elements(names) ne 0 then self->setNames, names
  if n_elements(values) ne 0 then self->setValues, values
  if n_elements(highLights) ne 0 then self->sethighLights, highLights
  if n_elements(rEdges) ne 0 then self->setRegionEdges, rEdges
  
  return , 1
  
end

;****************************************************************************************

pro RecognizeInfo__Define

  ; Fair mode 2 - Recognize util
  ; coords (normalize)
  Struct = { RecognizeInfo , $
    regionEdges: ptr_new(), $
    names: ptr_new(), $
    values: ptr_new(), $
    highLights: ptr_new(), $
    Inherits Object $
    }
    
end

;****************************************************************************************
