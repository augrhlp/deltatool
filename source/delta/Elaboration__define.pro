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
    errMsg=dialog_message('problem with file: <'+fileName+'> check existence, version or read permission.', /ERROR)
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
      ; Modified summer 2012 MM start
      fixedInfoNo=13
      totInfoNo=n_elements(info)
      if totInfoNo ge fixedInfoNo then begin
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
        ;MM summer/fall 2012 start
        ;Advanced*Standard*Benchmark*Observations Groups allowed*Observations Single allowed
        flags=strsplit(info[11], '*', /EXTRACT)
        thisElab.isAdvanced=fix(flags[0])
        thisElab.isStandard=fix(flags[1])
        thisElab.isBenchmark=fix(flags[2])
        thisElab.isSingleObsAllowed=fix(flags[3])
        thisElab.isGroupObsAllowed=fix(flags[4])
        ;MM summer/fall 2012 end
        thisElab.description=info[12]
        if totInfoNo gt fixedInfoNo then thisElab.extraInfos=ptr_new(info[13:*], /NO_COPY)
        elabs=[elabs, thisElab]
      ; Modified summer 2012 MM end
      endif else begin
        print, 'Bad conf file at line', i, bufferString
      endelse
    endelse
  endwhile
  close, unit & free_lun, unit
  self.list=ptr_new(elabs[1:*], /NO_COPY)

END

;MM summer/fall 2012 start
PRO Elaboration::setUserType, userType

  ;filterType eq 0 --> Standard
  ;filterType eq 1 --> Advanced
  ;filterType eq 2 --> Developer/Benchmark

  thisList=*self.list

  if n_elements(userType) eq 0 then userType=9

  if userType eq 0 then begin
    idxs=where(thisList.isStandard)
  endif

  if userType eq 1 then begin
    idxs=indgen(n_elements(thisList))
    ;idxs=where(thisList.isAdvanced)
  endif

  if userType eq 2 then begin
    idxs=indgen(n_elements(thisList))
  endif
  
  if userType gt 2 then message, 'naaaaaa'

;  print, '**userType**'
;  print, userType
;  print, '*************'
  self->setModeUserList, thisList[idxs]

END

PRO Elaboration::setModeUserList, list

  ptr_free, self.modeUserList
  self.modeUserList = ptr_new(list, /NO_COPY)

END

;No more used
;PRO Elaboration::updateMode
;
;  modeList=self->getModes()
;  idxs=self->modeCompatibility(modeList)
;  ptr_free, self.modeFilterList
;  self.modeFilterList=ptr_new((*self.list)[idxs])
;
;END
;
;FUNCTION Elaboration::getModes
;
;  thisList=*self.list
;  return, thisList[*].mode
;
;END
;MM summer/fall 2012 end

;FUNCTION Elaboration::getDisplayNameByCode, code
;
; thisList=*self.list
; idx=(where(code eq thisList.code))[0]
; return, thisList[idx].plotCodes
;
;END

FUNCTION Elaboration::getFilteredList

 filteredList=*self.modeUserList
 return, filteredList

END

FUNCTION Elaboration::getIsSingleObsAllowed, code

  thisList=self->getFilteredList()
  idx=(where(code eq thisList.code))[0]
  return, thisList[idx].isSingleObsAllowed

END

FUNCTION Elaboration::getIsGroupObsAllowed, code

  thisList=self->getFilteredList()
  idx=(where(code eq thisList.code))[0]
  return, thisList[idx].isGroupObsAllowed

END

FUNCTION Elaboration::getGoalsCriteriaOCFlagByCode, code

  thisList=self->getFilteredList()
  idx=(where(code eq thisList.code))[0]
  return, thisList[idx].goalsCriteriaOCFlag

END

; MM summer 2012 Start
FUNCTION Elaboration::getExtraInfosByCode, code, index=index

  thisList=self->getFilteredList()
  idx=(where(code eq thisList.code))[0]

  if ptr_valid(thisList[idx].extraInfos) then infos=*thisList[idx].extraInfos else return, ''
  if n_elements(index) ne 0 then return, infos[index] else return, infos

END
; MM summer 2012 End

FUNCTION Elaboration::getIDLRoutineByCode, code

  thisList=self->getFilteredList()
  idx=(where(code eq thisList.code))[0]
  return, thisList[idx].IDLRoutineCode

END
;*******************************
;
;*******************************
FUNCTION Elaboration::getMultipleChoiceFlagsByCode, code

  thisList=self->getFilteredList()
  idx=(where(code eq thisList.code))[0]
  return, *thisList[idx].multipleChoiceFlags

END

FUNCTION Elaboration::getMultipleChoiceFlags;, NOFILTER=NOFILTER

  ;if keyword_set(NOFILTER) then thisList=*self.list else thisList=*self.modeFilterList
  thisList=self->getFilteredList()
  return, thisList[*].multipleChoiceFlags

END

FUNCTION Elaboration::getGroupByTimeCodes;, NOFILTER=NOFILTER

  ;if keyword_set(NOFILTER) then thisList=*self.list else thisList=*self.modeFilterList
  thisList=self->getFilteredList()
  return, thisList[*].groupByTimeCode

END

FUNCTION Elaboration::getGroupByStatCodes;, NOFILTER=NOFILTER

  ;if keyword_set(NOFILTER) then thisList=*self.list else thisList=*self.modeFilterList
  thisList=self->getFilteredList()
  return, thisList[*].groupByStatCode

END

FUNCTION Elaboration::getDayPeriodCodes;, NOFILTER=NOFILTER

  ;if keyword_set(NOFILTER) then thisList=*self.list else thisList=*self.modeFilterList
  thisList=self->getFilteredList()
  return, thisList[*].dayPeriodCode

END

FUNCTION Elaboration::getSeasonCodes;, NOFILTER=NOFILTER

  ;if keyword_set(NOFILTER) then thisList=*self.list else thisList=*self.modeFilterList
  thisList=self->getFilteredList()
  return, thisList[*].seasonCode

END

;FUNCTION Elaboration::getParameterCodes
;
; if keyword_set(NOFILTER) then thisList=*self.list else thisList=*self.modeFilterList
; return, thisList[*].parameterCodes
;
;END

FUNCTION Elaboration::getGoalsCriteriaOCFlags;, NOFILTER=NOFILTER

  ;if keyword_set(NOFILTER) then thisList=*self.list else thisList=*self.modeFilterList
  thisList=self->getFilteredList()
  return, thisList[*].goalsCriteriaOCFlag

END

FUNCTION Elaboration::getNumberRefValues;, NOFILTER=NOFILTER

  ;if keyword_set(NOFILTER) then thisList=*self.list else thisList=*self.modeFilterList
  thisList=self->getFilteredList()
  return, thisList[*].numberRefValue

END

FUNCTION Elaboration::getDiagramCodes;, NOFILTER=NOFILTER

  ;if keyword_set(NOFILTER) then thisList=*self.list else thisList=*self.modeFilterList
  thisList=self->getFilteredList()
  return, thisList[*].diagramCode

END

FUNCTION Elaboration::getDisplayNames;, NOFILTER=NOFILTER

  ;if keyword_set(NOFILTER) then thisList=*self.list else thisList=*self.modeFilterList
  thisList=self->getFilteredList()
  return, thisList[*].displayName

END

FUNCTION Elaboration::getCodes;, NOFILTER=NOFILTER

  ;if keyword_set(NOFILTER) then thisList=*self.list else thisList=*self.modeFilterList
  thisList=self->getFilteredList()
  return, (thisList[*].code)

END

FUNCTION Elaboration::getDescriptions;, NOFILTER=NOFILTER

  ;if keyword_set(NOFILTER) then thisList=*self.list else thisList=*self.modeFilterList
  thisList=self->getFilteredList()
  return, thisList[*].description

END

FUNCTION Elaboration::getExtraInfos;, NOFILTER=NOFILTER

  ;if keyword_set(NOFILTER) then thisList=*self.list else thisList=*self.modeFilterList
  thisList=self->getFilteredList()
  return, thisList[*].extraInfos

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
    print, '**** extra info:<', thisList[i].extraInfo, '>'
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
    modeUserList: ptr_new(), $
    Inherits ConfigurableData $
    }

END