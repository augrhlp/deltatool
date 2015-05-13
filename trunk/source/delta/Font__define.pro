;********************
@structure_definition
;********************
PRO Font::setAvailableCharSize, list

  ptr_free, self.availableCharSize
  self.availableCharSize=ptr_new(list, /NO_COPY)
  
END

FUNCTION Font::getAvailableCharSize

  list=*self.availableCharSize
  return, list
  
END

PRO Font::setAvailableCharThick, list

  ptr_free, self.availableCharThick
  self.availableCharThick=ptr_new(list, /NO_COPY)
  
END

FUNCTION Font::getAvailableCharThick

  list=*self.availableCharThick
  return, list
  
END

PRO Font::updateSingleLine, newValue, index

  newList=*self.list
  newValue.code=newList[index].code
  newList[index]=newValue
  self->setList, newList
  
END

FUNCTION Font::getAvailableFont, type=type

  device, GET_CURRENT_FONT=currFont
  currFont=!p.font
  !p.font=type
  if type eq 1 then tt_font=1
  device, get_FONTNAMES=fNames, set_font='*', tt_font=tt_font
  !p.font=currFont
  return, fNames
  
END

FUNCTION Font::getFontByCode, code

  allCodes=self->getCodes()
  idx=where(code eq allCodes, count)
  if count eq 1 then begin
    thisList=*self.list
    return, thisList[idx]
  endif
  
END

FUNCTION Font::getFontByName, name

  allNames=self->getDisplayNames()
  idx=where(strupcase(name) eq strupcase(allNames), count)
  if count eq 1 then begin
    thisList=*self.list
    return, thisList[idx]
  endif
  
END

FUNCTION Font::getFontByIndex, index

  thisList=*self.list
  return, thisList[index]
  
END

FUNCTION Font::getAvailableWeight

  return, ['THIN', 'LIGHT', 'BOLD', 'HEAVY']
  
END

FUNCTION Font::getAvailableQuality

  return, ['DRAFT', 'PROOF']
  
END

FUNCTION Font::getAvailableAngle

  return, ['ITALIC']
  
END

FUNCTION Font::getAvailablePitch

  return, ['FIXED', 'VARIABLE']
  
END

FUNCTION Font::getAvailableStrikeOut

  return, ['STRIKEOUT']
  
END

FUNCTION Font::getAvailableUnderline

  return, ['UNDERLINE']
  
END

PRO Font::setAvailableSize, list

  ptr_free, self.availableSize
  self.availableSize=ptr_new(list, /NO_COPY)
  
END

FUNCTION Font::getAvailableSize

  list=*self.availableSize
  return, list
  
END

FUNCTION Font::getAvailableTypeName

  return, ['(-1)Vector', '(0)Hardware', '(1)TrueType']
  
END

FUNCTION Font::getAvailableTypeCode

  return, [-1, 0, 1]
  
END

FUNCTION Font::getDisplayNames

  thisList=*self.list
  return, thisList[*].displayName
  
END

FUNCTION Font::getCodes

  thisList=*self.list
  return, thisList[*].code
  
END

FUNCTION Font::getFontNames

  thisList=*self.list
  return, thisList[*].fontName
  
END

FUNCTION Font::getModifiers

  thisList=*self.list
  return, thisList[*].modifier
  
END

PRO Font::fillDataFromFile, fileName

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
  fonts=getFMFont()
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
      fixedInfoNo=5
      totInfoNo=n_elements(info)
      if totInfoNo ge fixedInfoNo then begin
        thisFont=getFMFont()
        thisFont.code=fix(info[0])
        thisFont.displayName=info[1]
        thisFont.type=fix(info[2])
        thisFont.fontName=info[3]
        thisFont.modifier=info[4]
        thisFont.charSize=float(info[5])
        thisFont.charThick=float(info[6])
        fonts=[fonts, thisFont]
      endif else begin
        print, 'Bad conf file at line', i, bufferString
      endelse
    endelse
  endwhile
  close, unit & free_lun, unit
  self.list=ptr_new(fonts[1:*], /NO_COPY)
  
END

PRO Font::writeFile, fileName

  commentString='[FONT: Code,DisplayName,Type(-1/0/1),fontName;tt modifiers-weight(THIN/LIGHT/BOLD/HEAVY),Quality(DRAFT/PROOF),pitch(FIXED/VARIABLE),STRIKEOUT,UNDERLINE;charsize (vector apply);charthick (vector apply)]'
  if n_elements(fileName) ne 1 then fileName=self.fileName
  
  ERROR=0
  catch, error_status
  
  if error_status NE 0 THEN BEGIN
    ERROR=1
    catch, /CANCEL
    close, /all
    errMsg=dialog_message('problem with file: <'+fileName+'> check existence, version or read permission.', /ERROR)
  endif
  
  fullList=*self.list
  lines=strarr(n_elements(fullList)+1)
  lines[0]=commentString
  for i=0, n_elements(fullList)-1 do begin
    thisFont=fullList[i]
    thisRow=strcompress(thisFont.code, /REMOVE)
    thisRow=thisRow+';'+thisFont.displayName
    thisRow=thisRow+';'+strcompress(thisFont.type,/REMOVE)
    thisRow=thisRow+';'+string(thisFont.fontName)
    thisRow=thisRow+';'+strcompress(thisFont.modifier,/REMOVE)
    thisRow=thisRow+';'+strcompress(thisFont.charSize,/REMOVE)
    thisRow=thisRow+';'+strcompress(thisFont.charThick,/REMOVE)
    lines[i+1]=thisRow
  endfor
  fsm=obj_new('FMFileSystemManager')
  fsm->writePlainTextFile, fileName, lines
  obj_destroy, fsm
  
END

;-1
; Use Hershey vector-drawn fonts—the default
;
;0
; Use device fonts (fonts supplied by the graphics device)
;•  For font weight: THIN, LIGHT, BOLD, HEAVY
; •  For font quality: DRAFT, PROOF
; •  For font pitch: FIXED, VARIABLE
; •  For font angle: ITALIC
; •  For strikeout text: STRIKEOUT
; •  For underlined text: UNDERLINE
; •  For font size: Any number is interpreted as the font height in pixels.
;
;DEVICE, GET_FONTNAMES=fnames, SET_FONT='*'
; /usr/lib/X11/fonts on unix
;
;1
; Use TrueType fonts
; (and set TT_FONT keyword=1)
; to get all TT fonts
;DEVICE, GET_FONTNAMES=fnames, SET_FONT='*', /TT_FONTS
;modifiers are

;*******************************
;constructor/destructor
;*******************************
PRO Font::streamPrint

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

PRO Font::cleanUp

  ptr_free, self.availableSize
  ptr_free, self.availableCharSize
  ptr_free, self.availableCharThick
  self -> ConfigurableData::cleanUp
  
END

FUNCTION Font::Init

  if not(self -> ConfigurableData::init()) then return, 0
  self.availableSize=ptr_new(['2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', $
    '16', '18', '20', '22', '24', '26', '28', '32', '36', '40', '44', '48', '54', '60', '66', '72',  '80','88','96'], /NO_COPY)
  self.availableCharSize=ptr_new(['0.2', '0.4', '0.5', '0.6', '0.8', '1', '1.2', '1.25', '1.3', '1.5', '1.8', '2', '2.2', '2.5', '2.8', '3', '4', '5', '6', '7'], /NO_COPY)
  self.availableCharThick=ptr_new(['0.8','1','1.25', '1.5', '2', '2.3', '3', '4'], /NO_COPY)
  return, 1
  
END

PRO Font__Define

  Struct = { Font , $
    availableSize: ptr_new(), $
    availableCharSize: ptr_new(), $
    availableCharThick: ptr_new(), $
    Inherits ConfigurableData $
    }
    
END