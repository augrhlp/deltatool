;********************
@structure_definition
;********************
FUNCTION Elaboration::buildMultipleChoiceFlags, dataField

; (parameters, models, scenarios, observations)
; Selection: 1) may be multiple, 2) must be single
  multFlags=strsplit(dataField, '*', /EXTRACT)
  length=n_elements(multFlags)
  flags=bytarr(4)
  ; return, fix(axisCodes)
  
  multipleFlags=bytarr(length, 4)
  for i=0, length-1 do begin
    thisMulti=multFlags[i]
    for j=0, 3 do flags[j]=fix(strmid(thisMulti, j, 1))
    multipleFlags[i, *]=flags
  endfor
  return, multipleFlags
  
END
;for i=0, 3 do flags[i]=fix(strmid(info[8], i, 1))
;thisElab.multipleChoiceFlags=flags
;thisElab.multipleChoiceFlags=self->buildMultipleChoiceFlags(info[8])

PRO Elaboration::fillDataFromFile, fileName

  self.fileName=fileName
  
  ERROR=0
  catch, error_status
  
  if error_status NE 0 THEN BEGIN
    ERROR=1
    catch, /CANCEL
    close, /all
    errMsg=dialog_message('problem with file: <'+fileName+'> check existence or read permission.', /ERROR)
  endif
  
  openr, unit, fileName, /GET_LUN
  
  bufferString=''
  i=0
  elabs=getFMElaboration()
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
    ;print, 'Discard row', i
    ;print, bufferString
    endif else begin
      info=strsplit(bufferString, ';', /EXTRACT)
      if n_elements(info) eq 13 then begin
        thisElab=getFMElaboration()
        thisElab.code=fix(info[0])
        thisElab.idlRoutineCode=fix(info[1])
        thisElab.displayName=info[2]
        thisElab.diagramCode=info[3]
        thisElab.groupByTimeCode=info[4]
        thisElab.groupByStatCode=info[5]
        thisElab.seasonCode=info[6]
        thisElab.dayPeriodCode=info[7]
        flags=bytarr(4)
        ;for i=0, 3 do flags[i]=fix(strmid(info[8], i, 1))
        ;thisElab.multipleChoiceFlags=flags
        thisElab.multipleChoiceFlags=ptr_new(self->buildMultipleChoiceFlags(info[8]), /NO_COPY)
        thisElab.numberRefValue=fix(info[9])
        if strupcase(info[10]) eq 'YES' then thisElab.goalsCriteriaOCFlag=1 else thisElab.goalsCriteriaOCFlag=0
        thisElab.mode=info[11]
        thisElab.description=info[12]
        elabs=[elabs, thisElab]
      endif else begin
        print, 'Bad conf file at line', i, bufferString
      endelse
    endelse
  endwhile
  close, unit & free_lun, unit
  self.list=ptr_new(elabs[1:*], /NO_COPY)
  
END

PRO Elaboration::updateMode

  modeList=self->getModes()
  idxs=self->modeCompatibility(modeList)
  ptr_free, self.modeFilterList
  self.modeFilterList=ptr_new((*self.list)[idxs])
  
END

FUNCTION Elaboration::getModes

  thisList=*self.list
  return, thisList[*].mode
  
END

;FUNCTION Elaboration::getDisplayNameByCode, code
;
; thisList=*self.list
; idx=(where(code eq thisList.code))[0]
; return, thisList[idx].plotCodes
;
;END

FUNCTION Elaboration::getIDLRoutineByCode, code

  thisList=*self.list
  idx=(where(code eq thisList.code))[0]
  return, thisList[idx].IDLRoutineCode
  
END
;*******************************
;
;*******************************
FUNCTION Elaboration::getMultipleChoiceFlagsByCode, code

  thisList=*self.list
  idx=(where(code eq thisList.code))[0]
  return, *thisList[idx].multipleChoiceFlags
  
END

FUNCTION Elaboration::getMultipleChoiceFlags, NOFILTER=NOFILTER

  if keyword_set(NOFILTER) then thisList=*self.list else thisList=*self.modeFilterList
  return, thisList[*].multipleChoiceFlags
  
END

FUNCTION Elaboration::getGroupByTimeCodes, NOFILTER=NOFILTER

  if keyword_set(NOFILTER) then thisList=*self.list else thisList=*self.modeFilterList
  return, thisList[*].groupByTimeCode
  
END

FUNCTION Elaboration::getGroupByStatCodes, NOFILTER=NOFILTER

  if keyword_set(NOFILTER) then thisList=*self.list else thisList=*self.modeFilterList
  return, thisList[*].groupByStatCode
  
END

FUNCTION Elaboration::getDayPeriodCodes, NOFILTER=NOFILTER

  if keyword_set(NOFILTER) then thisList=*self.list else thisList=*self.modeFilterList
  return, thisList[*].dayPeriodCode
  
END

FUNCTION Elaboration::getSeasonCodes, NOFILTER=NOFILTER

  if keyword_set(NOFILTER) then thisList=*self.list else thisList=*self.modeFilterList
  return, thisList[*].seasonCode
  
END

;FUNCTION Elaboration::getParameterCodes
;
; if keyword_set(NOFILTER) then thisList=*self.list else thisList=*self.modeFilterList
; return, thisList[*].parameterCodes
;
;END

FUNCTION Elaboration::getGoalsCriteriaOCFlags, NOFILTER=NOFILTER

  if keyword_set(NOFILTER) then thisList=*self.list else thisList=*self.modeFilterList
  return, thisList[*].goalsCriteriaOCFlag
  
END

FUNCTION Elaboration::getNumberRefValues, NOFILTER=NOFILTER

  if keyword_set(NOFILTER) then thisList=*self.list else thisList=*self.modeFilterList
  return, thisList[*].numberRefValue
  
END

FUNCTION Elaboration::getDiagramCodes, NOFILTER=NOFILTER

  if keyword_set(NOFILTER) then thisList=*self.list else thisList=*self.modeFilterList
  return, thisList[*].diagramCode
  
END

FUNCTION Elaboration::getDisplayNames, NOFILTER=NOFILTER

  if keyword_set(NOFILTER) then thisList=*self.list else thisList=*self.modeFilterList
  return, thisList[*].displayName
  
END

FUNCTION Elaboration::getCodes, NOFILTER=NOFILTER

  if keyword_set(NOFILTER) then thisList=*self.list else thisList=*self.modeFilterList
  return, (thisList[*].code)
  
END

FUNCTION Elaboration::getDescriptions, NOFILTER=NOFILTER

  if keyword_set(NOFILTER) then thisList=*self.list else thisList=*self.modeFilterList
  return, thisList[*].description
  
END
;*******************************
;constructor/destructor
;*******************************
PRO Elaboration::streamPrint

  print, '***********************'
  print, '**Start of <',OBJ_CLASS(self),'>**'
  
  print, '**** fileName:<', self.fileName,'>'
  thisList=*self.list
  for i=0, n_elements(thisList)-1 do begin
    print, '**element n.<', strcompress(i, /REMOVE), '>'
    print, '**** code:<', thisList[i].code, '>'
    print, '**** IDLRoutineCode:<', thisList[i].IDLRoutineCode, '>'
    print, '**** displayName:<', thisList[i].displayName, '>'
    print, '**** diagramCode:<', thisList[i].diagramCode, '>'
    print, '**** groupByTimeCode:<', thisList[i].groupByTimeCode, '>'
    print, '**** groupByStatCode:<', thisList[i].groupByStatCode, '>'
    print, '**** seasonCode:<', thisList[i].seasonCode, '>'
    print, '**** dayPeriodCode:<', thisList[i].dayPeriodCode, '>'
    ;print, '**** multipleChoiceFlags:<', fix(thisList[i].multipleChoiceFlags), '>'
    ;mcList=self->getMultipleChoiceFlags()
    mcList=thisList[i].multipleChoiceFlags
    for j=0, n_elements(mcList)-1 do print, '**** multipleChoiceFlags:<', fix(*(mcList[j])), '>'
    print, '**** goalsCriteriaOCFlag:<', thisList[i].goalsCriteriaOCFlag, '>'
    print, '**** numberRefValue:<', thisList[i].numberRefValue, '>'
    print, '**** StdAdvancedUserFlags:<', thisList[i].mode, '>'
    print, '**** description:<', thisList[i].description, '>'
    print, '**'
  endfor
  
  print, '***********************'
  print, '**End of:',OBJ_CLASS(self),'**'
  
END

PRO Elaboration::cleanUp

  self -> ConfigurableData::cleanUp
  
END

FUNCTION Elaboration::Init

  if not(self -> ConfigurableData::init()) then return, 0
  return, 1
  
END

PRO Elaboration__Define

  Struct = { Elaboration , $
    modeFilterList: ptr_new(), $
    Inherits ConfigurableData $
    }
    
END