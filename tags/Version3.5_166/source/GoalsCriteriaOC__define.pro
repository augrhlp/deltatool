;********************
@structure_definition
;********************
FUNCTION GoalsCriteriaOC::getValuesByKey, parameterCode=parameterCode, scaleName=scaleName, statNickName=statNickName, timeAvgName=timeAvgName, NOVALUES=NOVALUES

  NOVALUES=1
  gcValues=[-1]
  parameterCodes=self->getParameterCodes()
  scaleNames=self->getScaleNames()
  statNickNames=self->getStatNickNames()
  timeAvgNames=self->getTimeAvgNames()
  values=self->getValues()
  
  ;keyIndexes=where(parameterCodes eq parameterCode and scaleNames eq scaleName and statNickNames eq statNickName and timeAvgNames eq timeAvgName, count)
  parameterCodes=strupcase(parameterCodes)
  scaleNames=strupcase(scaleNames)
  statNickNames=strupcase(statNickNames)
  timeAvgNames=strupcase(timeAvgNames)
  
  parameterCode=strupcase(parameterCode)
  scaleName=strupcase(scaleName)
  statNickName=strupcase(statNickName)
  timeAvgName=strupcase(timeAvgName)
  
  keyIndexes=where(parameterCodes eq parameterCode and scaleNames eq scaleName and statNickNames eq statNickName and timeAvgNames eq timeAvgName, count)
  for i=0, count-1 do begin
    gcValues=*(values[keyIndexes[i]])
    NOVALUES=0
  ;print, gcValues
  endfor
  return, gcValues
  
END

FUNCTION GoalsCriteriaOC::getDescriptions

  thisList=*self.list
  return, thisList[*].description
  
END

FUNCTION GoalsCriteriaOC::getValues

  thisList=*self.list
  return, thisList[*].values ; pointer array...
  
END

FUNCTION GoalsCriteriaOC::getTimeAvgNames

  thisList=*self.list
  return, thisList[*].periodName
  
END

FUNCTION GoalsCriteriaOC::getStatNickNames

  thisList=*self.list
  return, thisList[*].statNickName
  
END

FUNCTION GoalsCriteriaOC::getScaleNames

  thisList=*self.list
  return, thisList[*].scaleName
  
END

FUNCTION GoalsCriteriaOC::getCodes

  thisList=*self.list
  return, thisList[*].code
  
END

FUNCTION GoalsCriteriaOC::getParameterCodes

  thisList=*self.list
  return, thisList[*].parameterCode
  
END

PRO GoalsCriteriaOC::streamPrint

  print, '***********************'
  print, '**Start of <',OBJ_CLASS(self),'>**'
  
  print, '**** fileName:<', self.fileName,'>'
  thisList=*self.list
  for i=0, n_elements(thisList)-1 do begin
    print, '**element n.<', strcompress(i, /REMOVE), '>'
    print, '**** code:<', thisList[i].code, '>'
    print, '**** parameterCode:<', thisList[i].parameterCode, '>'
    print, '**** scaleName:<', thisList[i].scaleName, '>'
    print, '**** statNickName:<', thisList[i].statNickName, '>'
    print, '**** periodName:<', thisList[i].periodName, '>'
    if ptr_valid(thisList[i].values) then print, '**** values:<', *thisList[i].values, '>' else print, '**** values:', '<NULL_POINTER>'
    print, '**** description:<', thisList[i].description, '>'
    print, '***********************'
    print, '**End of:',OBJ_CLASS(self),'**'
  endfor
  
END

FUNCTION GoalsCriteriaOC::buildValues, fileField

  values=float(strsplit(fileField, '*', /EXTRACT))
  return, values
  
END

PRO GoalsCriteriaOC::fillDataFromFile, fileName

  self.fileName=fileName
  
  ERROR=0
  catch, error_status
  
  if error_status NE 0 THEN BEGIN
    ERROR=1
    catch, /CANCEL
    errMsg=dialog_message('problem with file: <'+fileName+'> check existence or read permission.', /ERROR)
  endif
  
  openr, unit, fileName, /GET_LUN
  
  bufferString=''
  i=0
  gCOCs=getFMGoalsCriteriaOC()
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
      if n_elements(info) eq 7 then begin
        thisGCOC=getFMGoalsCriteriaOC()
        thisGCOC.code=fix(info[0])
        thisGCOC.parameterCode=info[1]
        thisGCOC.scaleName=info[2]
        thisGCOC.statNickName=info[3]
        thisGCOC.periodName=info[4]
        thisGCOC.values=ptr_new(self->buildValues(info[5]), /NO_COPY)
        thisGCOC.description=info[6]
        gCOCs=[gCOCs, thisGCOC]
      endif else begin
        print, 'Bad conf file at line', i, bufferString
      endelse
    endelse
  endwhile
  close, unit & free_lun, unit
  self.list=ptr_new(gCOCs[1:*], /NO_COPY)
  
END

FUNCTION GoalsCriteriaOC::init

  if not(self -> ConfigurableData::init()) then return, 0
  return, 1
  
END

PRO GoalsCriteriaOC::cleanUp

  self -> ConfigurableData::cleanUp
  
END

PRO GoalsCriteriaOC__Define

  Struct = { GoalsCriteriaOC , $
    Inherits ConfigurableData $
    }
    
END