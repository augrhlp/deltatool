PRO BenchmarkInfo::setDescription, value

  self.description=value
  
END

FUNCTION BenchmarkInfo::getDescription

  return, self.description
  
END

PRO BenchmarkInfo::setOutputFormat, value

  self.outputFormat=value
  
END

FUNCTION BenchmarkInfo::getOutputFormat

  return, self.outputFormat
  
END

PRO BenchmarkInfo::setOutputFormatType, value

  self.outputFormatType=value
  
END

FUNCTION BenchmarkInfo::getOutputFormatType

  return, self.outputFormatType
  
END

PRO BenchmarkInfo::setOutputFileNameDestination, value

  self.outputFileNameDestination=value
  
END

FUNCTION BenchmarkInfo::getOutputFileNameDestination

  return, self.outputFileNameDestination
  
END

PRO BenchmarkInfo::setBatchFileNames, list

  ptr_free, self.batchFileNames
  self.batchFileNames=ptr_new(list, /NO_COPY)
  
END

FUNCTION BenchmarkInfo::getBatchFileNames

  if ptr_valid(self.batchFileNames) then return, *self.batchFileNames
  return, -1
  
END
;
; Save & Restore methods
PRO BenchmarkInfo::saveData, filename

  ; open file for write
  utils=obj_new('FMUtility')
  openw, unit, filename, /GET_LUN
  header1='#BenchMarkInfo - Save File'
  printf, unit, header1
  
  header2='#Never change order of variables!'
  printf, unit, header2
  
  data=strcompress(self->GetBatchFileNames(), /REMOVE)
  record=utils->buildFileDataStream('batchFileNames', data)
  printf, unit, record
  
  data=strcompress(self->getoutputFormat(), /REMOVE)
  record=utils->buildFileDataStream('outputFormat', data)
  printf, unit, record
 
  data=strcompress(self->getoutputFormatType(), /REMOVE)
  if strlowcase(data) eq 'jpg' then data='jpeg'
  record=utils->buildFileDataStream('outputFormatType', data)
  printf, unit, record
 
  data=strcompress(self->getoutputFileNameDestination(), /REMOVE)
  record=utils->buildFileDataStream('outputFileNameDestination', data)
  printf, unit, record
  
  data=self->getDescription()
  record=utils->buildFileDataStream('description', data)
  printf, unit, record
 
  obj_destroy, utils
  close, unit
  free_lun, unit
  
END

PRO BenchmarkInfo::restoreData, filename

  ; open file for read
  utils=obj_new('FMUtility')
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

  readf, unit, bufferString
  readf, unit, bufferString

  readf, unit, bufferString
  utils->convertStreamDataFile, bufferString, dataName, dataList
  ptr_free, self.batchFileNames
  if dataList[0] ne '-1' then self->setBatchFileNames, dataList
  
  readf, unit, bufferString
  utils->convertStreamDataFile, bufferString, dataName, dataList
  self->setOutputFormat, dataList
  
  readf, unit, bufferString
  utils->convertStreamDataFile, bufferString, dataName, dataList
  if strlowcase(dataList) eq 'jpg' then dataList = 'jpeg'
  self->setOutputFormatType, dataList
  
  readf, unit, bufferString
  utils->convertStreamDataFile, bufferString, dataName, dataList
  self->setOutputFileNameDestination, dataList
 
  readf, unit, bufferString
  utils->convertStreamDataFile, bufferString, dataName, dataList
  self->setDescription, dataList
 
  obj_destroy, utils
  close, unit
  free_lun, unit

END

FUNCTION BenchmarkInfo::clone, DEEP=DEEP

  clone=obj_new('BenchmarkInfo')
  if keyword_set(DEEP) then begin
    ; new
    if ptr_valid(self.batchFileNames) then begin
      list=*self.batchFileNames
      clone.batchFileNames=ptr_new(list, /NO_COPY)
    endif
  endif else begin
    ;new
    clone.batchFileNames=self.batchFileNames
  endelse
  clone.outputFormat=self.outputFormat
  clone.outputFormatType=self.outputFormatType
  clone.outputFileNameDestination=self.outputFileNameDestination
  clone.description=self.description
  return, clone
  
END

FUNCTION BenchmarkInfo::init

  if not self -> Object :: init() then return , 0
  ; new
  ptr_free, self.batchFileNames
  return , 1
  
END

PRO BenchmarkInfo::cleanUp

  self -> Object::cleanUp
  ;new
  ptr_free, self.batchFileNames
  
END

;****************************************************************************************

PRO BenchmarkInfo__Define

  Struct = { BenchmarkInfo, $
    batchFileNames: ptr_new(), $
    outputFormat: '', $ ;Direct (like post script) or Image (like bitmap)
    outputFormatType: '', $ ;File Type (.bmp, .jpg, .ps)
    outputFileNameDestination: '', $ ;fileName
    description: '', $ ;fileName
    Inherits Object $
    }
    
END

;****************************************************************************************
