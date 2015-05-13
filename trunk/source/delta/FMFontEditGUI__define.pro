FUNCTION FMFontEditGUI::checkIntegrity

  return, self.mgr->checkIntegrity()
  
END

PRO FMFontEditGUI::modifyBtt

  self.info->updateSingleLine, self->buildFontDesc(), self.userDefIndex
  
END

PRO FMFontEditGUI::okRequest

  if self->checkIntegrity() then begin
    self.info->writeFile
    self->updateToCaller
    obj_destroy, self
  endif else begin
    print, 'Bad ', obj_class(self)
  ;exitMessage=['Something wrong in your selection', 'Check data']
  ;dlgResult=dialog_message(exitMessage, dialog_parent=self->getTopBase(), Title='Warning')
  endelse
  
END

PRO FMFontEditGUI::enable

  self->FMInfoSelectionGUI::enable
  
END

PRO FMFontEditGUI::updateToCaller

  self.mgr->updateToCaller
  
END

; geometry
FUNCTION FMFontEditGUI::getButtonYSize

  return, self.dimensions[0]*.08
  
END

FUNCTION FMFontEditGUI::getLabelGraphXSize

  return, 20
  
END

FUNCTION FMFontEditGUI::getDropListLittleFont

  if strupcase(!VERSION.OS_FAMILY) eq 'WINDOWS' then return, "TIMES ROMAN*8"
  if strupcase(!VERSION.OS_FAMILY) eq 'X' then return, "8X13"
  
END

FUNCTION FMFontEditGUI::getDropListStandardFont

  if strupcase(!VERSION.OS_FAMILY) eq 'WINDOWS' then return, "TIMES ROMAN*10"
  if strupcase(!VERSION.OS_FAMILY) eq 'X' then return, "8X13"
  
END
;FUNCTION FMFontEditGUI::getSplitGraphXSize
;
;  return, self->getYSize()/4
;
;END

FUNCTION FMFontEditGUI::getExampleDrawYSize

  return, self->getYSize()/4
  
END

FUNCTION FMFontEditGUI::getExampleDrawXSize

  return, self->getYSize()/4
  
END

FUNCTION FMFontEditGUI::getXSize

  return, self.dimensions[0]
  
END

FUNCTION FMFontEditGUI::getYSize

  return, self.dimensions[1]
  
END

FUNCTION FMFontEditGUI::getLableYDim

  return, 20
  
END

FUNCTION FMFontEditGUI::getInfoTextXSize

  return, self.dimensions[0]/3-self.dimensions[0]/20
  
END

FUNCTION FMFontEditGUI::getInfoTitleXSize

  if strupcase(!VERSION.OS_FAMILY) eq 'WINDOWS' then return, self.dimensions[0]/3-self.dimensions[0]/20
  if strupcase(!VERSION.OS_FAMILY) eq 'UNIX' then return, self.dimensions[0]/2
  
END

;widget values
PRO FMFontEditGUI::wsetExampleDraw, topBase

  ;splitDraw=widget_info(self->getTopBase(), FIND='SPLITDRAW')
  widget_control, self.exampleFontDraw, get_value=wsetId
  wset, wsetId
  device, /DECOMPOSE
  
END

FUNCTION FMFontEditGUI::getSelectedType, CODE=CODE

  typeText=widget_info(self.fontTypeComboBox, /COMBOBOX_GETTEXT)
  typeCodeList=self.info->getAvailableTypeCode()
  typeNameList=self.info->getAvailableTypeName()
  index=where(typeText eq typeNameList, count)
  typeCode=typeCodeList[index]
  if keyword_set(CODE) then return, typeCode else return, typeText
  
END

FUNCTION FMFontEditGUI::getSelectedFontName

  fontNameText=widget_info(self.fontComboBox, /COMBOBOX_GETTEXT)
  return, fontNameText
  
END

FUNCTION FMFontEditGUI::getSelectedCharSize

  widInfo=self->getWidgetInfo(index=3)
  thisWid=widget_info(self->getTopBase(), FIND_BY_UNAME=widInfo.uname)
  
  charSizeText=widget_info(thisWid, /COMBOBOX_GETTEXT)
  return, charSizeText
  
END

FUNCTION FMFontEditGUI::getSelectedCharThick

  widInfo=self->getWidgetInfo(index=4)
  thisWid=widget_info(self->getTopBase(), FIND_BY_UNAME=widInfo.uname)
  
  charThickText=widget_info(thisWid, /COMBOBOX_GETTEXT)
  return, charThickText
  
END

FUNCTION FMFontEditGUI::getSelectedUserPredefined, CODE=CODE

  predefinedText=widget_info(self.userDefinedFontComboBox, /COMBOBOX_GETTEXT)
  if keyword_set(CODE) then return, typeCode else return, predefinedText
  
END

FUNCTION FMFontEditGUI::getModifierText

  widget_control, self.modifierText, get_value=modifierTextValue
  return, modifierTextValue
  
END

PRO FMFontEditGUI::drawTestFont

  red=obj_new('Color', 200, 0, 0)
  white=obj_new('Color', 200,200, 200)
  fontDesc=self->buildFontDesc()
  
  setUserFont, fontDesc=fontDesc
  self->wsetExampleDraw
  erase, white->asLongTrueColor()
  xyouts, 0.5, 0.5, 'Test', color=red->asLongTrueColor(), /NORM, ALIGN=0.5;, charsize=0.5, charthick=1
  ;xyouts, 0.5, 0.5, 'Test', color=red->asLongTrueColor(), /NORM, ALIGN=0.5, charsize=1, charthick=1
  ;xyouts, 0.5, 0.5, 'Test', color=red->asLongTrueColor(), /NORM, ALIGN=0.5, charsize=1.5, charthick=2
  ;xyouts, 0.5, 0.5, 'Test', color=red->asLongTrueColor(), /NORM, ALIGN=0.5, charsize=2, charthick=2
  setUserFont, /RESET
  obj_destroy, red
  obj_destroy, white
  
END

FUNCTION FMFontEditGUI::buildFontDesc

  fontDesc=getFMFont()
  
  fontDesc.displayName=self->getSelectedUserPredefined()
  fontDesc.type=self->getSelectedType(/CODE)
  fontDesc.fontName=self->getSelectedFontName()
  fontDesc.modifier=self->getModifierText()
  fontDesc.charSize=float(self->getSelectedCharSize())
  fontDesc.charThick=float(self->getSelectedCharThick())
  
  return, fontDesc
  
;  exampleFontDraw: 0l, $
  
END

PRO FMFontEditGUI::refreshModifierSelections

  widNo=self->getWidgetInfo(NUMBER)
  for i=5, widNo-1 do begin
    wid=self->widgetInfo(index=i)
    thisWid=widget_info(self->getTopBase(), FIND_BY_UNAME=widInfo.uname)
    widget_control, thisWid, set_list_top=self.modifierSetting[i-5], SET_COMBOBOX_SELECT=self.modifierSetting[i-5]
  endfor
  
END

PRO FMFontEditGUI::buildModifierTextFromList

  widNo=self->getWidgetInfo(/NUMBER)
  ;exclude userfontname, fonttype, fontname, charsize, charthick
  modifierText=''
  modifierSep='*'
  for j=5, widNo-1 do begin
    widInfo=self->getWidgetInfo(index=j)
    thisWid=widget_info(self->getTopBase(), FIND_BY_UNAME=widInfo.uname)
    selText=widget_info(thisWid, /COMBOBOX_GETTEXT)
    if selText ne '' and strupcase(selText) ne 'NONE' then modifierText=modifierText+modifierSep+selText
  endfor
  if strlen(modifierText) gt 1 then modifierText=strmid(modifierText, 1, strlen(modifierText)-1)
  self->userModifierText, modifierText, /MANUALLY
  
END

PRO FMFontEditGUI::buildModifierSetting, fontDesc

  self.modifierSetting[*]=0
  typeIndex=where(fontDesc.type eq self.info->getAvailableTypeCode())
  fontNameIndex=where(fontDesc.fontName eq self.info->getAvailableFont(TYPE=fontDesc.type), count)
  if count ne 1 then fontNameIndex=0
  
  modifiers=fontDesc.modifier
  
  for i=0, n_elements(fillList)-1 do begin
    toCheck=*fillList[i]
    for j=0, n_elements(toCheck)-1 do begin
      check=strpos(strupcase(modifiers), toCheck[j])
      if check ne -1 then begin
        self.modifierSetting[i]=j
        break
      endif
    endfor
  endfor
  
  byteModifiers=byte(modifiers)
  sizePos=where(byteModifiers ge 48 and byteModifiers le 57, count)
  if count ne 0 then begin
    sizeNum=string(byteModifiers[sizePos])
    widget_control, self.sizeComboBox, COMBOBOX_ADDITEM=sizeNum, COMBOBOX_INDEX=0
    widget_control, self.sizeComboBox, SET_COMBOBOX_SELECT=0
    widget_control, self.sizeComboBox, get_value=newList
    self.info->setAvailableSize, newList
  endif
  
END

PRO FMFontEditGUI::updateFontNames, newList, newSelection

  ;lenghts=strlen(newList)
  ;mapNames1=strmid(newList,0 ,24)
  ;mapNames=strcompress(findgen(20), /REMOVE)
  ;mapNames2=strmid(newList,strlen(newList) > 12, strlen(newList))
  ;before=widget_info(self.fontComboBox, /geometry)
  ;if max(lenghts) gt 25 then widget_control, self.fontComboBox, font=self->getDropListLittleFont() else widget_control, self.fontComboBox, font=self->getDropListStandardFont()
  ;widget_control, self.fontComboBox, set_value=mapNames
  widget_control, self.fontComboBox, set_list_top=0, SET_COMBOBOX_SELECT=0
  widget_control, self.fontComboBox, set_list_top=newSelection, SET_COMBOBOX_SELECT=newSelection
  ;after=widget_info(self.fontComboBox, /geometry)
  ;widget_control, self.fontComboBox, scr_xsize=self->getInfoTextXSize()
  ;widget_control, self.fontComboBox, xsize=self->getInfoTextXSize()
  
END

;events
PRO FMFontEditGUI::userPredefinedFontSelection, value, index

  self.userDefIndex=index
  fontDesc=self.info->getFontByIndex(index)
  
  self->userFontTypeSelection, strcompress(fontDesc.type, /REMOVE), typeIndex, /NODRAW, /USEVALUECODE
  
  self->userFontNameSelection, fontDesc.fontName, nameIndex, /NODRAW
  
  self->userModifierText, fontDesc.modifier, /NODRAW
  
  self->userCharSizeSelection, fontDesc.charSize, /NODRAW
  
  self->userCharThickSelection, fontDesc.charThick, /NODRAW
  
  self->userCharThickSelection, fontDesc.charThick, /NODRAW
  
  self->buildModifierTextFromList

  self->drawTestFont
  
END

PRO FMFontEditGUI::userTTModifierListSelection, value, index

  self->buildModifierTextFromList
;self->drawTestFont
  
END

;PRO FMFontEditGUI::setModifierText, value
;
;  widget_control, self.modifierText, set_value=value
;  self->drawTestFont
;
;END

PRO FMFontEditGUI::userModifierText, value, MANUALLY=MANUALLY, NODRAW=NODRAW

  if n_elements(value) eq 0 then widget_control, self.modifierText, get_value=value else widget_control, self.modifierText, set_value=value
  if not(keyword_set(MANUALLY)) then begin
    widNo=self->getWidgetInfo(/NUMBER)
    ;exclude userfontname, fonttype, fontname... charsize, charthick
    for j=5, widNo-1 do begin
      widInfo=self->getWidgetInfo(index=j)
      fillList=widInfo.list
      self.modifierSetting[j-5]=0
      for i=n_elements(fillList)-1, 0, -1 do begin
        found=strpos(strupcase(value), fillList[i])
        ;print, strupcase(value), fillList[i]
        if fillList[i] ne 'NONE' and found ne -1 then begin
          self.modifierSetting[j-5]=i
          thisWid=widget_info(self->getTopBase(), FIND_BY_UNAME=widInfo.uname)
          widget_control, thisWid, set_list_top=i, SET_COMBOBOX_SELECT=i
          break
        endif
      endfor
    endfor
  endif
  if not(keyword_set(NODRAW)) then self->drawTestFont
;widget_control, self.modifierText, set_value=value
  
END

PRO FMFontEditGUI::userFontNameSelection, value, index, NODRAW=NODRAW

  type=self->getSelectedType(/CODE)
  if n_elements(index) eq 0 then begin
    nameList=self.info->getAvailableFont(TYPE=type)
    index=(where(value eq nameList))[0]
  endif
  if n_elements(value) eq 0 then begin
    nameList=self.info->getAvailableFont(TYPE=type)
    value=nameList[index]
  endif
  widInfo=self->getWidgetInfo(index=2)
  thisWid=widget_info(self->getTopBase(), FIND_BY_UNAME=widInfo.uname)
  widget_control, thisWid, set_list_top=index, SET_COMBOBOX_SELECT=index
  
  if not(keyword_set(NODRAW)) then self->drawTestFont
  
END

PRO FMFontEditGUI::userFontTypeSelection, value, index, NODRAW=NODRAW, USEVALUECODE=USEVALUECODE

  codeList=self.info->getAvailableTypeCode()
  if n_elements(value) eq 0 then begin
    widInfo=self->getWidgetInfo(index=1)
    value=widInfo.list[index]
  endif
  if n_elements(index) eq 0 then begin
    if keyword_set(USEVALUECODE) then list=fix(codeList) else list=self.info->getAvailableNameList()
    index=(where(list eq value))[0]
  endif
  type=fix(codeList[index])
  widget_control, self.fontTypeComboBox, set_list_top=index, SET_COMBOBOX_SELECT=index
  
  newList=self.info->getAvailableFont(TYPE=type)
  self->updateFontNames, newList, 0
  self->updateSensitives, type
  
  self->userFontNameSelection, null, 0, /NODRAW
  
  if not(keyword_set(NODRAW)) then self->drawTestFont
  
END

PRO FMFontEditGUI::userCharSizeSelection, value, index, NODRAW=NODRAW

  if n_elements(value) eq 1 then begin
    listValue=float(value)
    list=self.info->getAvailableCharSize()
    idx=where(list eq listValue, count)
    widInfo=self->getWidgetInfo(index=3)
    thisWid=widget_info(self->getTopBase(), FIND_BY_UNAME=widInfo.uname)
    widget_control, thisWid, set_list_top=idx, SET_COMBOBOX_SELECT=idx
  endif
  if not(keyword_set(NODRAW)) then self->drawTestFont
  
END

PRO FMFontEditGUI::userCharThickSelection, value, index, NODRAW=NODRAW

  if n_elements(value) eq 1 then begin
    listValue=float(value)
    list=self.info->getAvailableCharThick()
    idx=where(list eq listValue, count)
    widInfo=self->getWidgetInfo(index=4)
    thisWid=widget_info(self->getTopBase(), FIND_BY_UNAME=widInfo.uname)
    widget_control, thisWid, set_list_top=idx, SET_COMBOBOX_SELECT=idx
  endif
  if not(keyword_set(NODRAW)) then self->drawTestFont
  
END

PRO FMFontEditGUI::updateSensitives, type

  if type eq -1 then sens=[1,1,0,1,1,0,0,0,0,0,0,0,0]
  if type eq  0 then sens=[1,1,1,0,0,0,0,0,0,0,0,0,0]
  if type eq  1 then sens=[1,1,1,0,0,1,1,1,1,1,1,1,1]
  
  widNo=self->getWidgetInfo(/NUMBER)
  for i=0, widNo-2 do begin
    widInfo=self->getWidgetInfo(index=i)
    thisWid=widget_info(self->getTopBase(), FIND_BY_UNAME=widInfo.uname)
    widget_control, thisWid, sensitive=sens[i]
  endfor
  widget_control, self.modifierText, sensitive=sens[n_elements(sens)-1]
  
END
;build & realize
PRO FMFontEditGUI::buildActionsButton, base

  ;addUserDefFontBtt=widget_button(base, value='Add', UNAME='ADD', $
  ;  event_pro=self.eventprefix+'FontAddRequest', SCR_XSIZE=70, SCR_YSIZE=35, /ALIGN_CENTER)
  exitBtt=widget_button(base, value='Exit', UNAME='EXIT', $
    event_pro=self.eventprefix+'destroyWindow', SCR_XSIZE=70, SCR_YSIZE=35, /ALIGN_CENTER, $
    tooltip='Lost all user defitions unsaved')
  modifyUserDefFontBtt=widget_button(base, value='Modify', UNAME='MODIFY', $
    event_pro=self.eventprefix+'fontgui_'+'modifyBtt', SCR_XSIZE=70, SCR_YSIZE=35, /ALIGN_CENTER, $
    tooltip='Save selected font definition for this Delta Session')
  okBtt=widget_button(base, value='OK', UNAME='OK', $
    event_pro=self.eventprefix+'OKRequest', SCR_XSIZE=70, SCR_YSIZE=35, /ALIGN_CENTER, $
    tooltip='Save all font to a definition file (persist on nexts Delta Sessions)')
    
END

PRO FMFontEditGUI::realize

  titles=strarr(12) & unames=strarr(12) & lists=ptrarr(12) & eventprocs=strarr(12)
  eventprefix=self.eventprefix+'fontgui_'
  
  titles[0]='User Defined Font' & lists[0]=ptr_new(self.info->getDisplayNames(), /NO_COPY) & unames[0]='FONTGUI_USERPREDEF_CB' & eventprocs[0]=eventprefix+'userPredefinedList'
  titles[1]='Type' & lists[1]=ptr_new(self.info->getAvailableTypeName(), /NO_COPY) & unames[1]='FONTGUI_FONTTYPE_CB' & eventprocs[1]=eventprefix+'userFontTypeList'
  titles[2]='Font' & lists[2]=ptr_new(self.info->getAvailableFont(TYPE=0), /NO_COPY) & unames[2]='FONTGUI_FONTNAME_CB' & eventprocs[2]=eventprefix+'userFontNameList'
  titles[3]='Char Size (Vector)' & lists[3]=ptr_new(self.info->getAvailableCharSize(), /NO_COPY) & unames[3]='FONTGUI_CHARSIZE_CB' & eventprocs[3]=eventprefix+'userCharSizeList'
  titles[4]='Char Thick (Vector)' & lists[4]=ptr_new(self.info->getAvailableCharThick(), /NO_COPY) & unames[4]='FONTGUI_CHARTHICK_CB' & eventprocs[4]=eventprefix+'userCharThickList'
  titles[5]='Size (True Type)' & lists[5]=ptr_new(self.info->getAvailableSize(), /NO_COPY) & unames[5]='FONTGUI_SIZE_CB' & eventprocs[5]=eventprefix+'userSizeList'
  titles[6]='Weight (True Type)' & lists[6]=ptr_new(['None', self.info->getAvailableWeight()], /NO_COPY) & unames[6]='FONTGUI_WEIGHT_CB' & eventprocs[6]=eventprefix+'userWeightList'
  titles[7]='Quality (True Type)' & lists[7]=ptr_new(['None', self.info->getAvailableQuality()], /NO_COPY) & unames[7]='FONTGUI_QUALITY_CB' & eventprocs[7]=eventprefix+'userQualityList'
  titles[8]='Pitch (True Type)' & lists[8]=ptr_new(['None', self.info->getAvailablePitch()], /NO_COPY) & unames[8]='FONTGUI_PITCH_CB' & eventprocs[8]=eventprefix+'userPitchList'
  titles[9]='Angle (True Type)' & lists[9]=ptr_new(['None', self.info->getAvailableAngle()], /NO_COPY) & unames[9]='FONTGUI_ANGLE_CB' & eventprocs[9]=eventprefix+'userAngleList'
  titles[10]='Strikeout (True Type)' & lists[10]=ptr_new(['None', self.info->getAvailableStrikeOut()], /NO_COPY) & unames[10]='FONTGUI_STRIKEOUT_CB' & eventprocs[10]=eventprefix+'userStrikeOutList'
  titles[11]='Underline (True Type)' & lists[11]=ptr_new(['None', self.info->getAvailableUnderline()], /NO_COPY) & unames[11]='FONTGUI_UNDERLINE_CB' & eventprocs[11]=eventprefix+'userUnderlineList'
  
  self->setWidgetInfo, titles, lists, unames, eventprocs
  
  self->build
  self->GUI::realize
  self->configure
  
END

PRO FMFontEditGUI::setWidgetInfo, titles, lists, unames, eventProcs

  ptr_free, self.titles & self.titles=ptr_new(titles, /NO_COPY)
  ptr_free, self.lists & self.lists=ptr_new(lists, /NO_COPY)
  ptr_free, self.unames & self.unames=ptr_new(unames, /NO_COPY)
  ptr_free, self.eventProcs & self.eventProcs=ptr_new(eventProcs, /NO_COPY)
  
END

FUNCTION FMFontEditGUI::getWidgetInfo, uname, index=index, ALL=ALL, NUMBER=NUMBER

  if keyword_set(NUMBER) then return, n_elements(*self.titles)
  if n_elements(index) eq 1 then begin
    idx=index
    return, {title:(*self.titles)[idx], list:*(*self.lists)[idx], uname:(*self.unames)[idx], eventPro:(*self.eventProcs)[idx]}
  endif
  if n_elements(uname) eq 1 then begin
    unames=*self.unames
    idx=where(strupcase(uname) eq unames)
    return, {title:*self.titles[idx], list:*self.lists[idx], uname:*self.unames[idx], eventPro:*self.eventProcs[idx]}
  endif
  
END

PRO FMFontEditGUI::build

  ;bestDimensions=self->getBestDimensions()
  mainTitle=self->getTitle()
  
  editBases=lonarr(12)
  
  base = widget_base(/COLUMN, $
    uvalue=self, TLB_FRAME_ATTR=1, $
    title=mainTitle, /TLB_KILL_REQUEST_EVENTS, event_pro=self.eventprefix+'destroyFontEditWindow')
    
  mainBase = widget_base(base,/COLUMN)
  
  infoBase=widget_base(mainbase, xpad=4, ypad=0,space=0,/ROW)
  
  firstColBase=widget_base(infoBase,xpad=0, ypad=3,space=0,/COLUMN)
  driverBase=widget_base(firstColBase,xpad=0, ypad=0,space=0,/COLUMN)
  
  secondColBase=widget_base(infoBase, xpad=4, ypad=0,space=0,/COLUMN)
  vectorTypeBase=widget_base(secondColBase,xpad=0, ypad=0,space=0,/COLUMN, FRAME=1)
  trueTypeBase=widget_base(secondColBase,xpad=0, ypad=0,space=0,/COLUMN, FRAME=1)
  
  lowerBase=widget_base(mainBase,/COLUMN,xpad=0, ypad=2,space=0, /ALIGN_CENTER)
  
  editBases[0]= widget_base(driverBase, xpad=0, ypad=0,space=0,/COLUMN);, /ALIGN_CENTER)
  editBases[1]=widget_base(driverBase, xpad=0, ypad=0,space=0,/COLUMN)
  editBases[2]=widget_base(driverBase, xpad=0, ypad=0,space=0,/COLUMN)
  
  row1_1=widget_base(vectorTypeBase, xpad=0, ypad=0,space=0,/ROW)
  editBases[3]=widget_base(row1_1, xpad=0, ypad=0,space=0,/ROW)
  editBases[4]=widget_base(row1_1, xpad=0, ypad=0,space=0,/ROW)
  
  row2_1=widget_base(trueTypeBase, xpad=0, ypad=0,space=0,/ROW)
  editBases[5]=widget_base(row2_1, xpad=0, ypad=0,space=0,/ROW)
  editBases[6]=widget_base(row2_1, xpad=0, ypad=0,space=0,/ROW)
  
  row2_2=widget_base(trueTypeBase, xpad=0, ypad=0,space=0,/ROW)
  editBases[7]=widget_base(row2_2, xpad=0, ypad=0,space=0,/ROW)
  editBases[8]=widget_base(row2_2, xpad=0, ypad=0,space=0,/ROW)
  
  row2_3=widget_base(trueTypeBase, xpad=0, ypad=0,space=0,/ROW)
  editBases[9]=widget_base(row2_3, xpad=0, ypad=0,space=0,/ROW)
  editBases[10]=widget_base(row2_3, xpad=0, ypad=0,space=0,/ROW)
  
  row2_4=widget_base(trueTypeBase, xpad=0, ypad=0,space=0,/ROW)
  editBases[11]=widget_base(row2_4, xpad=0, ypad=0,space=0,/ROW)
  self.modifierText = widget_text(row2_4, UNAME='MODIFIERTEXT', $
    SCR_XSIZE=self->getInfoTextXSize() ,SCR_YSIZE=self->getValueYDim() ,SENSITIVE=1 , $
    VALUE='Info', font=self.textFont, event_pro=self.eventprefix+'fontgui_'+'modifierText', /EDIT)
    
  exampleBase=widget_base(firstColBase,xpad=0, ypad=5,space=0,/COLUMN, /ALIGN_CENTER)
  
  actionBase= widget_base(base, xpad=0, ypad=2,space=0,/ROW, /ALIGN_RIGHT)
  
  ;  unames[0]='FONTGUI_USERPREDEF_CB'
  ;  unames[1]='FONTGUI_FONTTYPE_CB'
  ;  unames[2]='FONTGUI_FONTNAME_CB'
  ;  unames[3]='FONTGUI_WEIGHT_CB'
  ;  unames[4]='FONTGUI_QUALITY_CB'
  ;  unames[5]='FONTGUI_PITCH_CB'
  ;  unames[6]='FONTGUI_ANGLE_CB'
  ;  unames[7]='FONTGUI_STRIKEOUT_CB'
  ;  unames[8]='FONTGUI_UNDERLINE_CB'
  ;  unames[9]='FONTGUI_SIZE_CB'
  ;  unames[10]='FONTGUI_CHARSIZE_CB'
  ;  unames[11]='FONTGUI_CHARTHICK_CB' & eventprocs[11]=eventprefix+'userCharThickOutList'
  
  index=0
  widStr=self->getWidgetInfo(index=index)
  infoLabel = widget_label(editBases[index], UNAME='LABEL', $
    SCR_XSIZE=self->getInfoTitleXSize() ,SCR_YSIZE=self->getSmallLabelYDim() ,/ALIGN_LEFT, $
    VALUE=widStr.title, font=self.labelLittleFont)
  self.userDefinedFontComboBox=widget_combobox(editBases[index], value=widStr.list, $
    SCR_XSIZE=self->getInfoTitleXSize() ,SCR_YSIZE=self->getSmallLabelYDim() ,/ALIGN_LEFT, $
    event_pro=widStr.eventPro, uname=widStr.uname)
    
  index=1
  widStr=self->getWidgetInfo(index=index)
  infoLabel = widget_label(editBases[index], UNAME='LABEL', $
    SCR_XSIZE=self->getInfoTitleXSize() ,SCR_YSIZE=self->getSmallLabelYDim() ,/ALIGN_LEFT, $
    VALUE=widStr.title, font=self.labelLittleFont)
  self.fontTypeComboBox=widget_combobox(editBases[index], value=widStr.list, $
    SCR_XSIZE=self->getInfoTitleXSize() ,SCR_YSIZE=self->getSmallLabelYDim() ,/ALIGN_LEFT, $
    event_pro=widStr.eventPro, uname=widStr.uName)
    
  index=2
  widStr=self->getWidgetInfo(index=index)
  infoLabel = widget_label(editBases[index], UNAME='LABEL', $
    SCR_XSIZE=self->getInfoTitleXSize() ,SCR_YSIZE=self->getSmallLabelYDim() ,/ALIGN_LEFT, $
    VALUE=widStr.title, font=self.labelLittleFont)
  self.fontComboBox=widget_combobox(editBases[index], value=widStr.list, $
    SCR_XSIZE=self->getInfoTitleXSize() ,SCR_YSIZE=self->getSmallLabelYDim() ,/ALIGN_LEFT, $
    event_pro=widStr.eventPro, uname=widStr.uName)
    
  widNo=self->getWidgetInfo(/NUMBER)
  for i=3, widNo-1 do begin
    widStr=self->getWidgetInfo(index=i)
    self->buildListSection, widStr.title, widStr.list, widStr.uName, widStr.eventPro, editBases[i]
  endfor
  
  self->buildExampleDrawSection, exampleBase
  self->buildActionsButton, actionBase
  
  self->setTopBase, base
  xmanager, 'fairmode', base, /JUST_REG
  
END

PRO FMFontEditGUI::buildListSection, title, valueList, uname, eventPro, refBase

  internalBase=widget_base(refBase, /COLUMN)
  internalList=valueList
  
  infoLabel = widget_label(internalBase, UNAME='LABEL', $
    SCR_XSIZE=self->getInfoTitleXSize() ,SCR_YSIZE=self->getSmallLabelYDim() ,/ALIGN_LEFT, $
    VALUE=title, font=self.labelLittleFont)
  myComboBox=widget_combobox(internalBase, value=internalList, $
    SCR_XSIZE=self->getInfoTitleXSize() ,SCR_YSIZE=self->getSmallLabelYDim() ,/ALIGN_LEFT, $
    event_pro=eventPro, uname=uName)
    
END

PRO FMFontEditGUI::buildExampleDrawSection, exampleBase

  self.exampleFontDraw = widget_draw(exampleBase,RETAIN=2,SCR_XSIZE=self->getExampleDrawXSize(),SCR_YSIZE=self->getExampleDrawYSize(), $
    UNAME='EXAMPLEFONTDRAW')
    
END
; *****************************************************************
; configure, update, display
; *****************************************************************
PRO FMFontEditGUI::configure

  self.userDefIndex=0
  fontDesc=self.info->getFontByIndex(self.userDefIndex)
  self->userPredefinedFontSelection, fontDesc.displayName, self.userDefIndex
  
END

PRO FMFontEditGUI::exitRequest

  self.mgr->exitRequest
  obj_destroy, self.mgr
  obj_destroy, self
  
END

FUNCTION FMFontEditGUI::getTitle

  return, 'Font editing'
  
END
;*************************************************************
; constructor / destructor
;*************************************************************
FUNCTION FMFontEditGUI::init, info, mgr, fonts=fonts

  self.labelLittleFont="TIMES ROMAN*12*BOLD"
  if not self -> FMInfoSelectionGUI :: init(info, mgr, fonts=fonts) then return , 0
  dims=self->getBestDimensions()
  dims[0]=dims[0]*.33
  dims[1]=dims[1]*.66
  self.dimensions=dims
  return , 1
  
END

PRO FMFontEditGUI::cleanUp

  self -> GUI::cleanUp
  
END

;****************************************************************************************

;•  For font weight: THIN, LIGHT, BOLD, HEAVY
; •  For font quality: DRAFT, PROOF
; •  For font pitch: FIXED, VARIABLE
; •  For font angle: ITALIC
; •  For strikeout text: STRIKEOUT
; •  For underlined text: UNDERLINE
; •  For font size: Any number is interpreted as the font height in pixels.

PRO FMFontEditGUI__Define

  Struct = { FMFontEditGUI , $
    userDefIndex: 0, $
    titles: ptr_new(), $
    lists: ptr_new(), $
    unames: ptr_new(), $
    eventProcs: ptr_new(), $
    modifierSetting: intarr(7), $
    modifierText: 0l, $
    labelLittleFont: '', $
    userDefinedFontComboBox: 0l, $
    fontTypeComboBox: 0l, $
    fontComboBox: 0l, $
    exampleFontDraw: 0l, $
    Inherits FMInfoSelectionGUI $
    }
    
END

;****************************************************************************************

