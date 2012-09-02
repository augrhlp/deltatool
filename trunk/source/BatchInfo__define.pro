PRO BatchInfo::setDescription, value

  self.description=value
  
END

FUNCTION BatchInfo::getDescription

  return, self.description
  
END

PRO BatchInfo::setOutputFormat, value

  self.outputFormat=value
  
END

FUNCTION BatchInfo::getOutputFormat

  return, self.outputFormat
  
END

PRO BatchInfo::setOutputFormatType, value

  self.outputFormatType=value
  
END

FUNCTION BatchInfo::getOutputFormatType

  return, self.outputFormatType
  
END

PRO BatchInfo::setOutputFileNameDestination, value

  self.outputFileNameDestination=value
  
END

FUNCTION BatchInfo::getOutputFileNameDestination

  return, self.outputFileNameDestination
  
END

PRO BatchInfo::setPlotLocations, list

  ptr_free, self.plotLocations
  self.plotLocations=ptr_new(list, /NO_COPY)
  
END

FUNCTION BatchInfo::getPlotLocations

  if ptr_valid(self.plotLocations) then return, *self.plotLocations
  return, -1
  
END

FUNCTION BatchInfo::getFileFormatVersion

  return, '2.1'
  
END

;PRO BatchInfo::saveData, filename
;
;  ; open file for write
;  openw, unit, filename, /GET_LUN
;  header1='#BatchInfo - Save File - Never change order of contents'
;  printf, unit, header1
;
;  record=self.utility->buildFileDataStream('Version', self->getFileFormatVersion())
;  printf, unit, record
;
;  data=strcompress(self->GetBatchFileNames(), /REMOVE)
;  record=self.utility->buildFileDataStream('batchFileNames', data)
;  printf, unit, record
;
;  data=strcompress(self->getoutputFormat(), /REMOVE)
;  record=self.utility->buildFileDataStream('outputFormat', data)
;  printf, unit, record
;
;  data=strcompress(self->getoutputFormatType(), /REMOVE)
;  if strlowcase(data) eq 'jpg' then data='jpeg'
;  record=self.utility->buildFileDataStream('outputFormatType', data)
;  printf, unit, record
;
;  data=strcompress(self->getoutputFileNameDestination(), /REMOVE)
;  record=self.utility->buildFileDataStream('outputFileNameDestination', data)
;  printf, unit, record
;
;  data=self->getDescription()
;  record=self.utility->buildFileDataStream('description', data)
;  printf, unit, record
;
;  close, unit
;  free_lun, unit
;
;END

FUNCTION BatchInfo::restoreData, filename, path, resRequests

  if n_elements(path) eq 0 then path=''
  ERROR=0
  catch, error_status
  
  if error_status NE 0 THEN BEGIN
    ERROR=1
    catch, /CANCEL & close, /ALL
    errMsg=dialog_message('problem with file: <'+fileName+'> check format version, existence or read permission.', /ERROR)
    return, 0
  endif
  ; open file for read
  openr, unit, fileName, /GET_LUN
  
  ;1 row of comment
  record=""
  readf, unit, record
  
  readf, unit, record
  self.utility->convertStreamDataFile, record, dataName, datalist
  if dataName eq 'Version' then version=datalist[0] else message, 'File version not compatible'
  if version ne self->getFileFormatVersion() then message, 'File version not compatible'
  
  readf, unit, record
  self.utility->convertStreamDataFile, record, dataName, datalist
  if dataName eq 'NumberOfRequest' then requestNumber=fix(datalist[0]) else message, 'disgraziato'
  
  requests=objarr(requestNumber)
  
  for i=0, requestNumber-1 do begin
    requests[i]=obj_new("Request")
    
    readf, unit, record
    self.utility->convertStreamDataFile, record, dataName, datalist
    if dataName eq 'RequestNumber' then print,  "reading...RequestNumber:", datalist else message, "qualcosa non va"
    
    ;    readf, unit, record
    ;    self.utility->convertStreamDataFile, record, dataName, datalist
    ;    if dataName eq 'EntityFileName' then print,  "reading...EntityFileName:", datalist else message, "qualcosa non va"
    
    ;    requests[i]->setEntityFileName, path+datalist
    
    ;    readf, unit, record
    ;    self.utility->convertStreamDataFile, record, dataName, datalist
    ;    if dataName eq 'ElaborationFileName' then print,  "reading...ElaborationFileName:", datalist else message, "qualcosa non va"
    ;
    ;    requests[i]->setElaborationFileName, path+datalist
    
    ;    readf, unit, record
    ;    self.utility->convertStreamDataFile, record, dataName, datalist
    ;    if dataName eq 'RequestLocation' then print,  "reading...'RequestLocation':", float(datalist) else message, "qualcosa non va"
    
    ;    requests[i]->setLocation, float(datalist)
    
    readf, unit, record
    self.utility->convertStreamDataFile, record, dataName, fileName
    if dataName eq 'RequestFileName' then print,  "reading...RequestFileName:", fileName else message, "RequestFileName in a wrong position"
    
    if requests[i]->restoreData(path+fileName, path) then requests[i]->setFileName, path+fileName else obj_destroy, request[i]
    
    
  endfor
  
  close, unit
  free_lun, unit
  resRequests=requests
  return, 1
  
END

PRO BatchInfo::saveData, filename, path, splitBatchInfo

  if n_elements(path) eq 0 then path=""
  ERROR=0
  catch, error_status
  
  if error_status NE 0 THEN BEGIN
    ERROR=1
    catch, /CANCEL & close, /ALL
    errMsg=dialog_message('problem with file: <'+fileName+'> check format version, existence or read permission.', /ERROR)
    return
  endif
  ; open file for write
  openw, unit, fileName, /GET_LUN
  
  ;1 row of comment
  baseFileName=self.fsm->getBaseFileName(fileName, /PRESERVE_PATH)
  ;baseFileName=strsplit(fileName, '.', /EXTRACT)
  ;baseFileName=baseFileName[0]
  header1='#BatchInfo - Save File - Never change order of contents'
  printf, unit, header1
  
  record=self.utility->buildFileDataStream('Version', self->getFileFormatVersion())
  printf, unit, record
  
  numberOfRequest=n_elements(splitBatchInfo.requests)
  record=self.utility->buildFileDataStream('NumberOfRequest', strcompress(numberOfRequest, /REMOVE))
  printf, unit, record
  
  for i=0, numberOfRequest-1 do begin
    ;requests[i]=obj_new("Request")
  
    prefixFileName=baseFileName+'_'+strcompress(i+1, /REMOVE)
    record=self.utility->buildFileDataStream('RequestNumber', strcompress(i+1, /REMOVE))
    printf, unit, record
    
    ;    readf, unit, record
    ;    self.utility->convertStreamDataFile, record, dataName, datalist
    ;    if dataName eq 'EntityFileName' then print,  "reading...EntityFileName:", datalist else message, "qualcosa non va"
    
    ;    requests[i]->setEntityFileName, path+datalist
    
    ;    readf, unit, record
    ;    self.utility->convertStreamDataFile, record, dataName, datalist
    ;    if dataName eq 'ElaborationFileName' then print,  "reading...ElaborationFileName:", datalist else message, "qualcosa non va"
    ;
    ;    requests[i]->setElaborationFileName, path+datalist
    
    ;    readf, unit, record
    ;    self.utility->convertStreamDataFile, record, dataName, datalist
    ;    if dataName eq 'RequestLocation' then print,  "reading...'RequestLocation':", float(datalist) else message, "qualcosa non va"
    
    ;    requests[i]->setLocation, float(datalist)
    
    ;fsm=obj_new("FMFileSystemManager")
    thisRequest=obj_new("Request")
    
    requestFileName=prefixFileName+self.fsm->getRequestExtension()
    entityDisplayFileName=prefixFileName+self.fsm->getEntityExtension()
    elaborationDisplayFileName=prefixFileName+self.fsm->getElaborationExtension()

    ;MM March 19 2012 Start
    ;shortRequestFileName=strsplit(requestFileName, self.fsm->getSystemDirSeparator(), /EXTRACT)
    ;shortRequestFileName=shortRequestFileName[n_elements(shortRequestFileName)-1]
    shortRequestFileName=self.fsm->getBaseFileName(requestFileName, /PRESERVE_EXT)

    ;shortEntityDisplayFileName=strsplit(entityDisplayFileName, self.fsm->getSystemDirSeparator(), /EXTRACT)
    ;shortEntityDisplayFileName=shortEntityDisplayFileName[n_elements(shortEntityDisplayFileName)-1]
    shortEntityDisplayFileName=self.fsm->getBaseFileName(entityDisplayFileName, /PRESERVE_EXT)

    ;shortElaborationDisplayFileName=strsplit(elaborationDisplayFileName, self.fsm->getSystemDirSeparator(), /EXTRACT)
    ;shortElaborationDisplayFileName=shortElaborationDisplayFileName[n_elements(shortElaborationDisplayFileName)-1]
    shortElaborationDisplayFileName=self.fsm->getBaseFileName(elaborationDisplayFileName, /PRESERVE_EXT)
    ;MM March 19 2012 End
    
    thisRequest->setFileName, shortRequestFileName 
    splitBatchInfo.requests[i].elaboration->saveData, elaborationDisplayFileName
    splitBatchInfo.requests[i].entity->saveData, entityDisplayFileName
    
    thisRequest->setPlotDeviceName, splitBatchInfo.graphicType 
    thisRequest->setPrintOrient, splitBatchInfo.printOrient 
    thisRequest->setElaborationFileName, shortElaborationDisplayFileName 
    thisRequest->setEntityFileName, shortEntityDisplayFileName
    ;Add MM november 2011
    ; 'MP': Multiple page (each graph in a single page), Single Page (more graph inside a page)
    if splitBatchInfo.pageMode eq 'MP' then begin
      if splitBatchInfo.graphicType eq 'IMAGE' then thisRequest->setPageBreak, 'CLOSE' else thisRequest->setPageBreak,  'TRUE'
      thisRequest->setLocation,  splitBatchInfo.fullPageLocation
    endif else begin
      thisRequest->setPageBreak,  'FALSE'
      thisRequest->setLocation,  splitBatchInfo.requests[i].location
    endelse
    if ((numberOfRequest-1) eq i) then thisRequest->setPageBreak, 'CLOSE'
    ;thisRequest->setLocation,  splitBatchInfo.requests[i].location
    thisRequest->saveData, requestFileName

    ;if requests[i]->restoreData(path+fileName, path) then requests[i]->setFileName, path+fileName else obj_destroy, request[i]
    record=self.utility->buildFileDataStream('RequestFileName', shortRequestFileName)
    printf, unit, record
    
  endfor
  
  close, unit
  free_lun, unit
  return
  
END

;PRO BatchInfo::restoreDataBoh, filename
;
;  ; open file for read
;  ERROR=0
;  catch, error_status
;
;  if error_status ne 0 then begin
;    ERROR=1
;    catch, /CANCEL & close, /ALL
;    errMsg=dialog_message('problem with file: <'+fileName+'> check existence, read or file version permission.', /ERROR)
;    return
;  endif
;
;  openr, unit, fileName, /GET_LUN
;
;  bufferString=''
;
;  readf, unit, bufferString
;
;  readf, unit, bufferString
;  self.utility->convertStreamDataFile, bufferString, dataName, datalist
;  if self.utilityame eq 'Version' then version=datalist[0] else message, 'File version not compatible'
;  if version ne self->getFileFormatVersion() then message, 'File version not compatible'
;
;  readf, unit, bufferString
;  self.utility->convertStreamDataFile, bufferString, dataName, dataList
;  ptr_free, self.batchFileNames
;  if dataList[0] ne '-1' then self->setBatchFileNames, dataList
;
;  readf, unit, bufferString
;  self.utility->convertStreamDataFile, bufferString, dataName, dataList
;  self->setOutputFormat, dataList
;
;  readf, unit, bufferString
;  self.utility->convertStreamDataFile, bufferString, dataName, dataList
;  if strlowcase(dataList) eq 'jpg' then dataList = 'jpeg'
;  self->setOutputFormatType, dataList
;
;  readf, unit, bufferString
;  self.utility->convertStreamDataFile, bufferString, dataName, dataList
;  self->setOutputFileNameDestination, dataList
;
;  readf, unit, bufferString
;  self.utility->convertStreamDataFile, bufferString, dataName, dataList
;  self->setDescription, dataList
;
;  close, unit
;  free_lun, unit
;
;END

FUNCTION BatchInfo::clone, DEEP=DEEP

  clone=obj_new('BatchInfo')
  if keyword_set(DEEP) then begin
    if ptr_valid(self.batchFileNames) then begin
      list=*self.batchFileNames
      clone.batchFileNames=ptr_new(list, /NO_COPY)
    endif
  endif else begin
    clone.batchFileNames=self.batchFileNames
  endelse
  clone.outputFormat=self.outputFormat
  clone.outputFormatType=self.outputFormatType
  clone.outputFileNameDestination=self.outputFileNameDestination
  clone.description=self.description
  return, clone
  
END

FUNCTION BatchInfo::init

  if not self -> Object :: init() then return , 0
  self.utility=obj_new('FMUtility')
  self.dtu=obj_new('DateTimeUtility')
  self.fsm=obj_new('FMFileSystemManager')
  return , 1
  
END

PRO BatchInfo::cleanUp

  obj_destroy, self.utility
  obj_destroy, self.dtu
  obj_destroy, self.fsm
  self -> Object::cleanUp
  
END

;****************************************************************************************

PRO BatchInfo__Define

  Struct = { BatchInfo, $
    requestFileNames: ptr_new(), $
    plotLocations: ptr_new(), $
    outputFormat: '', $ ;Direct (like post script) or Image (like bitmap)
    outputFormatType: '', $ ;File Type (.bmp, .jpg, .ps)
    outputFileNameDestination: '', $ ;fileName
    description: '', $ ;fileName
    utility: obj_new(), $
    dtu: obj_new(), $
    fsm: obj_new(), $
    Inherits Object $
    }
    
END

;****************************************************************************************
