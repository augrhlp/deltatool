FUNCTION FMBenchMarkCreationGUI::getTreeMenuTextName, groupName

  ;widget_control, ev.top, get_uvalue=view
  ;widget_control, ev.id, get_uvalue=widgetText
  widget_control, self.treeMenuNameText, get_value=treeTextName
  ;widget_control, widgetText, get_uvalue=selectedCodes
  return, treeTextName
  
END

PRO FMBenchMarkCreationGUI::userBenchMarkTreeUpdate, treeElemCode

  treeElemWid=widget_info(self.wTreeRoot, FIND_BY_UNAME=strcompress(treeElemCode, /REMOVE))
  widget_control, treeElemWid, /SET_TREE_SELECT
  self.mgr->setBenchMarkTreeElementCode, treeElemCode
  
END

PRO FMBenchMarkCreationGUI::buildBenchMarkTreeSection, base

  bMD=self.mgr->getBenchMarkMenuInfo()
  topLevels=bMD->getSubMenuList(1, 999, NELEM=NELEM)
  eventPro=self.eventPrefix+'benchmarkTreeSelection'
  treeInfoBase = widget_base(base, XOFFSET=0 ,YOFFSET=0, $
    SPACE=0 ,XPAD=0 ,YPAD=0, /COLUMN)
  self.wTreeRoot = WIDGET_TREE(treeInfoBase, UNAME='TREEROOT', SCR_XSIZE=self->getTreeXSize(), $
    SCR_YSIZE=self->getTreeYSize())
    
  for i=0, NELEM-1 do begin
    thisMenuInfo=topLevels[i]
    ;thisMenuBttGF=widget_tree(wTreeRoot, value=thisMenuInfo.displayName, UVALUE=ptr_new(thisMenuInfo), FOLDER=thisMenuInfo.isMenu, event_pro=eventPro, /EXPANDED)
    thisMenuBttGF=widget_tree(self.wTreeRoot, value=thisMenuInfo.displayName, UNAME=strcompress(thisMenuInfo.code, /REMOVE), UVALUE=thisMenuInfo.code, FOLDER=thisMenuInfo.isMenu, event_pro=eventPro, /EXPANDED)
    subLevels=bMD->getSubMenuList(2, thisMenuInfo.code, NELEM=NELEM)
    ;print, thisMenuInfo.code
    for j=0, NELEM-1 do begin
      thisSubMenuInfo=subLevels[j]
      ;      thisMenuBttF=widget_button(thisMenuBttGF, value=thisSubMenuInfo.displayName, UNAME=thisSubMenuInfo.fileName, MENU=thisSubMenuInfo.isMenu, event_pro=eventPro)
      ;thisMenuBttF=widget_tree(thisMenuBttGF, value=thisSubMenuInfo.displayName, UVALUE=ptr_new(thisMenuInfo), event_pro=eventPro)
      thisMenuBttF=widget_tree(thisMenuBttGF, value=thisSubMenuInfo.displayName, UNAME=strcompress(thisSubMenuInfo.code, /REMOVE), UVALUE=thisSubMenuInfo.code, event_pro=eventPro)
    ;print, thisSubMenuInfo.code
    ;      subsubLevels=bMD->getSubMenuList(3, thisSubMenuInfo.code)
    ;      for k=0, n_elements(subsubLevels)-1 do begin
    ;        thisSubSubMenuInfo=subsubLevels[k]
    ;        thisMenuBttS=widget_button(thisMenuBttF, value=thisSubSubMenuInfo.displayName, UNAME=thisSubSubMenuInfo.fileName, event_pro=eventPro)
    ;      endfor
    endfor
  endfor
  treeMenuNameBase = widget_base(treeInfoBase, XOFFSET=0 ,YOFFSET=0, $
    SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
  menuLabel=widget_label(treeMenuNameBase, UNAME='', $
    XOFFSET=0 ,YOFFSET=0, SCR_XSIZE=self->getOptionSelectionXSize() ,SCR_YSIZE= self->getTitleYDim(), /ALIGN_LEFT, $
    VALUE='Menu display name:', font=self.labelFont)
  self.treeMenuNameText = widget_text(treeMenuNameBase, UNAME='TREENAMETXT', XOFFSET=0, $
    SCR_XSIZE=self->getTreeXSize()-self->getOptionSelectionXSize(), SCR_YSIZE=self->getTitleYDim(), SENSITIVE=1, /EDITABLE, $
    font=self.textFont)
    
    
END

FUNCTION  FMBenchMarkCreationGUI::getPageModeButtons

  return, *self.pageModeButtons
  
END

PRO FMBenchMarkCreationGUI::enable

  self->FMInfoSelectionGUI::enable
  
END

PRO FMBenchMarkCreationGUI::updateToCaller

  self.mgr->updateToCaller
  
END

FUNCTION FMBenchMarkCreationGUI::checkIntegrity

  return, self.mgr->checkIntegrity()
  
END

PRO FMBenchMarkCreationGUI::displaySummary

  help, self.mgr
;self.mgr
  
END

PRO FMBenchMarkCreationGUI::okRequest

  if self->checkIntegrity() then begin
    self->updateToCaller
    obj_destroy, self
  endif else begin
    print, 'Bad ', obj_class(self)
  ;exitMessage=['Something wrong in your selection', 'Check data']
  ;dlgResult=dialog_message(exitMessage, dialog_parent=self->getTopBase(), Title='Warning')
  endelse
  
END

PRO FMBenchMarkCreationGUI::exitRequest

  self.mgr->exitRequest
  obj_destroy, self.mgr
  obj_destroy, self
  
END

PRO FMBenchMarkCreationGUI::userEditBatch

  self->disable
  self.mgr->userEditBatch
  
END

PRO FMBenchMarkCreationGUI::userShowRequestBatch

  if self.mgr->selectionIsFilled() then begin
    self->disable
    self.mgr->userShowRequestBatch
  endif else begin
    message=['Fill with a selection to get summary info.']
    dlgResult=dialog_message(message, dialog_parent=self->getTopBase(), Title='Warning')
  endelse
  
END

PRO FMBenchMarkCreationGUI::updateSplitGraph

  white = obj_new("Color", 255, 255, 255)
  erase, white->asLongTrueColor()
  self->turnOffSplit
  self->highLightDrawSplit, self.mgr->getBatchIndex()
  obj_destroy, white
  
END

PRO FMBenchMarkCreationGUI::userUpdatePrintOrientCode, code, widgetId

  widget_control, widgetId, set_button=1
  self.printOrientSelection=code
  print, self.printOrientSelection
  self.mgr->setPrintOrientSelection, code
  
END

PRO FMBenchMarkCreationGUI::userUpdatePageModeCode, code, widgetId

  widget_control, widgetId, set_button=1
  self.pageModeSelection=code
  print, self.pageModeSelection
  self.mgr->setPageModeSelection, code
  
END

PRO FMBenchMarkCreationGUI::userUpdateGraphicTypeCode, code, widgetId

  ;graphicTypeSelection=self->getGraphicTypeMark(code)
  ;button=widget_info(self->getTopBase(), FIND_BY_UNAME=graphicTypeSelection)
  widget_control, widgetId, set_button=1
  self.graphicTypeSelection=code
  pageBtts=self->getPageModeButtons()
  if code ne 'PS' then begin
    pageCodes=self->getAllPageModeCodes()
    for i=0, n_elements(pageBtts)-1 do begin
      widget_control, pageBtts[i], sensitive=0, set_button=0
      ;uname=widget_info(pageBtts[i], /UNAME)
      widget_control, pageBtts[i], get_uvalue=uvalue
      if uvalue eq 'MP' then self->userUpdatePageModeCode, uvalue, pageBtts[i]
    endfor
  endif else begin
    pageCodes=self->getAllPageModeCodes()
    for i=0, n_elements(pageBtts)-1 do begin
      widget_control, pageBtts[i], sensitive=0, set_button=0
      ;uname=widget_info(pageBtts[i], /UNAME)
      widget_control, pageBtts[i], get_uvalue=uvalue
      if uvalue eq 'SP' then self->userUpdatePageModeCode, uvalue, pageBtts[i]
    endfor
  endelse
  self.mgr->setGraphCodeSelection, code
  
END

PRO FMBenchMarkCreationGUI::userUpdateBenchmarkSaveModeCode, code, widgetId

  ;treeRootWid=widget_info(self->getTopBase(), FIND='TREEROT')
  widget_control, widgetId, set_button=1
  if code eq 'MENU' then begin
    widget_control, self.wTreeRoot, sensitive=1
    widget_control, self.treeMenuNameText, sensitive=1
  endif else begin
    widget_control, self.wTreeRoot, sensitive=0
    widget_control, self.treeMenuNameText, sensitive=0
  endelse
  print, code
  self.mgr->setBenchMarkSaveMode, code
  
END

PRO FMBenchMarkCreationGUI::userUpdateIterateOptionCode, code, widgetId

  ;iterateOptionSelection=self->getIterateOptionMark(code)
  ;button=widget_info(self->getTopBase(), FIND_BY_UNAME=graphicTypeSelection)
  widget_control, widgetId, set_button=1
  self.iteractionOptionSelection=code
  
END

PRO FMBenchMarkCreationGUI::updateSplitZoneSelection, index

  print, index
  self.mgr->setBatchIndex, index
  self->updateSplitGraph
  
END

PRO FMBenchMarkCreationGUI::userUpdateSplitStyleCode, code, widgetId

  ;extraValsWid[i]=widget_info(self->getTopBase(), FIND_BY_UNAME=extraValsUNames[i])
  self.mgr->setSplitStyleCodeSelection, code
  widget_control, widgetId, set_button=1
  self.mgr->setBatchIndex, 0
  self.mgr->updateSplitInfo
  self->updateSplitGraph
  self.mgr->setSplitStyleCodeSelection, code
  
END

PRO FMBenchMarkCreationGUI::buildOKButton, base

  okBtt=widget_button(base, value='OK', UNAME='DISPLAYOK', $
    event_pro=self.eventprefix+'BenchmarkOKRequest', SCR_XSIZE=70, SCR_YSIZE=35, /ALIGN_CENTER)
    
END

PRO FMBenchMarkCreationGUI::splitMouseClick, x, y

  destCoord=convert_coord(x, y, /device, /TO_NORMAL)
  self->recognizeSplitSection, destCoord
  
END

PRO FMBenchMarkCreationGUI::recognizeSplitSection, checkCoord

  sInfos=self.mgr->getSplitInfo()
  for i=0, n_elements(sInfos)-1 do begin
    thisRegion=sInfos[i].borderCoords
    if (checkCoord[0] ge thisRegion[0,0]) and $
      (checkCoord[0] le thisRegion[0,2]) and $
      (checkCoord[1] ge thisRegion[1,0]) and $
      (checkCoord[1] le thisRegion[1,2]) then begin
      self->updateSplitZoneSelection, i
      break
    endif
  endfor
  
END

PRO FMBenchMarkCreationGUI::wsetSplitDraw, topBase

  splitDraw=widget_info(self->getTopBase(), FIND='SPLITDRAW')
  widget_control, splitDraw, get_value=wsetId
  wset, wsetId
  device, /DECOMPOSE
  
END

; geometry
FUNCTION FMBenchMarkCreationGUI::getLabelGraphXSize

  return, 20
  
END

FUNCTION FMBenchMarkCreationGUI::getSplitGraphXSize

  return, self->getYSize()/4
  
END

FUNCTION FMBenchMarkCreationGUI::getSplitGraphYSize

  return, self->getYSize()/4
  
END

FUNCTION FMBenchMarkCreationGUI::getXSize

  return, self.dimensions[0]
  
END

FUNCTION FMBenchMarkCreationGUI::getYSize

  return, self.dimensions[1]
  
END

FUNCTION FMBenchMarkCreationGUI::getLableYDim

  return, 20
  
END

FUNCTION FMBenchMarkCreationGUI::getTreeXSize

  return, self->getXSize()/2
  
END

FUNCTION FMBenchMarkCreationGUI::getTreeYSize

  return, self->getYSize()/3
  
END

FUNCTION FMBenchMarkCreationGUI::getOptionSelectionXSize

  return, self->getXSize()/4
  
END

FUNCTION FMBenchMarkCreationGUI::getPrintOrientElementXSize

  return, self->getOptionSelectionXSize()
  
END

FUNCTION FMBenchMarkCreationGUI::getBenchmarkSaveModeElementXSize

  return, self->getOptionSelectionXSize()
  
END

FUNCTION FMBenchMarkCreationGUI::getPageModeElementXSize

  return, self->getOptionSelectionXSize()
  
END

FUNCTION FMBenchMarkCreationGUI::getGraphicTypeElementXSize

  return, self->getOptionSelectionXSize()
  
END

FUNCTION FMBenchMarkCreationGUI::getIterateOptionElementXSize

  return, self->getOptionSelectionXSize()
  
END

FUNCTION FMBenchMarkCreationGUI::getSplitStyleElementXSize

  return, self->getOptionSelectionXSize()/2
  
  
END

FUNCTION FMBenchMarkCreationGUI::getButtonYSize

  return, self.dimensions[0]*.08
  
END

;build & realize
PRO FMBenchMarkCreationGUI::realize

  self->build
  self->GUI::realize
  self->configure
  
END

FUNCTION FMBenchMarkCreationGUI::getIterateOptionMarkByCode, code

  return, 'ITERATEOPTION*'+code
  
END

PRO FMBenchMarkCreationGUI::buildIterateOptionSection, base

  codes=self->getAllIterateOptionCodes()
  names=self->getAllIterateOptionNames()
  nElements=n_elements(names)
  
  iterateOptionBase = widget_base(base, UNAME='', $
    XOFFSET=0 ,YOFFSET=0, $
    SPACE=0 ,XPAD=0 ,YPAD=0, /COLUMN)
    
  iterateOnOptionTitle = widget_label(iterateOptionBase, UNAME='', $
    XOFFSET=0 ,YOFFSET=0, SCR_XSIZE=self->getIterateOptionElementXSize() ,SCR_YSIZE= self->getTitleYDim(), /ALIGN_LEFT, $
    VALUE='Iterate on:', font=self.labelFont)
    
  iterateOptionButtonBase = widget_base(iterateOptionBase, UNAME='', $
    XOFFSET=0 ,YOFFSET=0, /EXCLUSIVE, $
    SPACE=0 ,XPAD=0 ,YPAD=0, /COLUMN)
    
  ;extraValsWid[i]=widget_info(self->getTopBase(), FIND_BY_UNAME=extraValsUNames[i])
  for i=1, nElements do begin
    uname=self->getIterateOptionMarkByCode(codes[i-1])
    uvalue=codes[i-1]
    ;print, "uname:", uname, "uvalue:", uvalue
    option = widget_button(iterateOptionButtonBase, /ALIGN_LEFT, $
      XOFFSET=0 ,YOFFSET=0, VALUE=names[i-1], event_pro=self.eventPrefix+'iterateOptionRadioButton', UNAME=uname, $
      SCR_XSIZE=self->getIterateOptionElementXSize(), SCR_YSIZE=self->getTitleYDim(), /NO_RELEASE, UVALUE=uvalue)
  endfor
  
END

FUNCTION FMBenchMarkCreationGUI::getGraphicTypeMarkByCode, code

  return, 'GRAPHICTYPE*'+code
  
END

PRO FMBenchMarkCreationGUI::buildGraphicTypeSection, base

  codes=self->getAllGraphicTypeCodes()
  names=self->getAllGraphicTypeNames()
  nElements=n_elements(names)
  
  graphicTypeBase = widget_base(base, UNAME='', $
    XOFFSET=0 ,YOFFSET=0, $
    SPACE=0 ,XPAD=0 ,YPAD=0, /COLUMN)
    
  selectionTypeTitle = widget_label(graphicTypeBase, UNAME='STATUSBARMAIN', $
    XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=self->getGraphicTypeElementXSize() ,SCR_YSIZE= self->getTitleYDim(), /ALIGN_LEFT, $
    VALUE='Graphic mode:', font=self.labelFont)
    
  graphicTypeButtonBase = widget_base(graphicTypeBase, UNAME='', $
    XOFFSET=0 ,YOFFSET=0, /EXCLUSIVE, $
    SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
    
  for i=1, nElements do begin
    uname=self->getGraphicTypeMarkByCode(codes[i-1])
    uvalue=codes[i-1]
    ;print, "uname:", uname, "uvalue:", uvalue
    option = widget_button(graphicTypeButtonBase, /ALIGN_LEFT, UNAME=uname, $
      XOFFSET=0 ,YOFFSET=0, VALUE=names[i-1], event_pro=self.eventPrefix+'graphicTypeRadioButton', $
      SCR_XSIZE=self->getGraphicTypeElementXSize() ,SCR_YSIZE=self->getTitleYDim(), /NO_RELEASE, UVALUE=codes[i-1])
  endfor
  
END

FUNCTION FMBenchMarkCreationGUI::getPrintOrientMarkByCode, code

  return, 'PRINTORIENT*'+code
  
END

PRO FMBenchMarkCreationGUI::buildPrintOrientSection, base

  codes=self->getAllPrintOrientCodes()
  names=self->getAllPrintOrientNames()
  nElements=n_elements(names)
  
  printOrientBase = widget_base(base, UNAME='', $
    XOFFSET=0 ,YOFFSET=0, $
    SPACE=0 ,XPAD=0 ,YPAD=0, /COLUMN)
    
  selectionOrientTitle = widget_label(printOrientBase, UNAME='STATUSBARMAIN', $
    XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=self->getPrintOrientElementXSize() ,SCR_YSIZE= self->getTitleYDim(), /ALIGN_LEFT, $
    VALUE='Print orientation:', font=self.labelFont)
    
  printOrientButtonBase = widget_base(printOrientBase, UNAME='', $
    XOFFSET=0 ,YOFFSET=0, /EXCLUSIVE, $
    SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
    
  for i=1, nElements do begin
    uname=self->getPrintOrientMarkByCode(codes[i-1])
    uvalue=codes[i-1]
    ;print, "uname:", uname, "uvalue:", uvalue
    option = widget_button(printOrientButtonBase, /ALIGN_LEFT, UNAME=uname, $
      XOFFSET=0 ,YOFFSET=0, VALUE=names[i-1], event_pro=self.eventPrefix+'printOrientRadioButton', $
      SCR_XSIZE=self->getGraphicTypeElementXSize() ,SCR_YSIZE=self->getTitleYDim(), /NO_RELEASE, UVALUE=codes[i-1])
  endfor
  
END

FUNCTION FMBenchMarkCreationGUI::getSplitStyleMarkByCode, code

  return, 'SPLITSTYLE*'+code
  
END

FUNCTION FMBenchMarkCreationGUI::getBenchMarkSaveModeMarkByCode, code

  return, 'BENCHSAVEMODE*'+code
  
END

FUNCTION FMBenchMarkCreationGUI::getPageModeMarkByCode, code

  return, 'PAGEMODE*'+code
  
END

PRO FMBenchMarkCreationGUI::buildPageModeSection, base

  codes=self->getAllPageModeCodes()
  names=self->getAllPageModeNames()
  nElements=n_elements(names)
  
  pageModeBase = widget_base(base, UNAME='', $
    XOFFSET=0 ,YOFFSET=0, $
    SPACE=0 ,XPAD=0 ,YPAD=0, /COLUMN)
    
  pageModeTitle = widget_label(pageModeBase, UNAME='STATUSBARMAIN', $
    XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=self->getPageModeElementXSize() ,SCR_YSIZE= self->getTitleYDim(), /ALIGN_LEFT, $
    VALUE='Page mode:', font=self.labelFont)
    
  pageModeButtonBase = widget_base(pageModeBase, UNAME='', $
    XOFFSET=0 ,YOFFSET=0, /EXCLUSIVE, $
    SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
    
  pageModeBtts=lonarr(nElements)
  for i=1, nElements do begin
    uname=self->getPageModeMarkByCode(codes[i-1])
    uvalue=codes[i-1]
    ;print, "uname:", uname, "uvalue:", uvalue
    pageModeBtts[i-1] = widget_button(pageModeButtonBase, /ALIGN_LEFT, UNAME=uname, $
      XOFFSET=0 ,YOFFSET=0, VALUE=names[i-1], event_pro=self.eventPrefix+'pageModeRadioButton', $
      SCR_XSIZE=self->getPageModeElementXSize() ,SCR_YSIZE=self->getTitleYDim(), /NO_RELEASE, UVALUE=codes[i-1])
  endfor
  self.pageModeButtons=ptr_new(pageModeBtts, /NO_COPY)
  
END

PRO FMBenchMarkCreationGUI::buildBenchMarkSaveModeSection, base

  codes=self->getAllBenchMarkSaveModeCodes()
  names=self->getAllBenchMarkSaveModeNames()
  nElements=n_elements(names)
  
  benchSaveModeBase = widget_base(base, UNAME='', $
    XOFFSET=0 ,YOFFSET=0, $
    SPACE=0 ,XPAD=0 ,YPAD=0, /COLUMN)
    
  benchSaveModeTitle = widget_label(benchSaveModeBase, UNAME='STATUSBARMAIN', $
    XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=self->getBenchMarkSaveModeElementXSize() ,SCR_YSIZE= self->getTitleYDim(), /ALIGN_LEFT, $
    VALUE='Save mode:', font=self.labelFont)
    
  benchMarkSaveModeButtonBase = widget_base(benchSaveModeBase, UNAME='', $
    XOFFSET=0 ,YOFFSET=0, /EXCLUSIVE, $
    SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
    
  ;pageModeBtts=lonarr(nElements)
  for i=1, nElements do begin
    uname=self->getBenchMarkSaveModeMarkByCode(codes[i-1])
    uvalue=codes[i-1]
    ;print, "uname:", uname, "uvalue:", uvalue
    ;pageModeBtts[i-1] = widget_button(benchMarkSaveModeButtonBase, /ALIGN_LEFT, UNAME=uname, $
    button = widget_button(benchMarkSaveModeButtonBase, /ALIGN_LEFT, UNAME=uname, $
      XOFFSET=0 ,YOFFSET=0, VALUE=names[i-1], event_pro=self.eventPrefix+'benchMarkSaveModeRadioButton', $
      SCR_XSIZE=self->getBenchMarkSaveModeElementXSize() ,SCR_YSIZE=self->getTitleYDim(), /NO_RELEASE, UVALUE=codes[i-1])
  endfor
;self.pageModeButtons=ptr_new(pageModeBtts, /NO_COPY)
  
END

PRO FMBenchMarkCreationGUI::buildSplitStyleSection, base

  internalBase=widget_base(base, xpad=0, ypad=0,space=0,/ROW);, /ALIGN_CENTER)
  
  selectionBase= widget_base(base, xpad=0, ypad=0,space=0,/COLUMN);, /ALIGN_CENTER)
  exampleBase= widget_base(base, xpad=0, ypad=0,space=0,/COLUMN);, /ALIGN_CENTER)
  batchOperationBase= widget_base(base, xpad=0, ypad=0,space=0,/COLUMN);, /ALIGN_CENTER)
  
  splitStyleTitle = widget_label(selectionBase, UNAME='', $
    XOFFSET=0 ,YOFFSET=0, SCR_XSIZE=self->getOptionSelectionXSize() ,SCR_YSIZE= self->getTitleYDim(), /ALIGN_LEFT, $
    VALUE='Split style', font=self.labelFont)
    
  graphicSelectionButtonBase = widget_base(selectionBase, UNAME='', $
    XOFFSET=0 ,YOFFSET=0, /EXCLUSIVE, $
    SPACE=0 ,XPAD=0 ,YPAD=0, COLUMN=2);, ROW=self->getSplitStyleNumber()/2)
    
  codes=self.mgr->getAllSplitStyleCodes()
  names=self.mgr->getAllSplitStyleNames()
  nElements=n_elements(codes)
  
  for i=1, nElements do begin
    uname=self->getSplitStyleMarkByCode(codes[i-1])
    uvalue=codes[i-1]
    ;print, "uname:", uname, "uvalue:", uvalue
    ;if i gt 1 then sensitive=0 else sensitive=1
    sensitive=1
    option = widget_button(graphicSelectionButtonBase, /ALIGN_LEFT, UNAME=uname, $
      XOFFSET=0 ,YOFFSET=0, VALUE=names[i-1], event_pro=self.eventPrefix+'splitStyleRadioButton', $
      SCR_XSIZE=self->getSplitStyleElementXSize(), SCR_YSIZE=self->getButtonYSize(), /NO_RELEASE, UVALUE=uvalue, $
      sensitive=sensitive)
  endfor
  
  splitDraw = widget_draw(exampleBase,RETAIN=2,SCR_XSIZE=self->getSplitGraphXSize(),SCR_YSIZE=self->getSplitGraphYSize(), $
    UNAME='SPLITDRAW', /BUTTON_EVENTS, event_pro=self.eventPrefix+'splitDrawMouse')
    
  editBtt=widget_button(batchOperationBase, value='Edit', UNAME='EDITBATCH', $
    event_pro=self.eventprefix+'editBatch', SCR_XSIZE=55, SCR_YSIZE=55, /ALIGN_CENTER)
  showBtt=widget_button(batchOperationBase, value='Show', UNAME='SHOWBATCH', $
    event_pro=self.eventprefix+'showRequestBatch', SCR_XSIZE=55, SCR_YSIZE=55, /ALIGN_CENTER)
    
END

PRO FMBenchMarkCreationGUI::buildOptionButtonSection, base

END

PRO FMBenchMarkCreationGUI::build

  ;bestDimensions=self->getBestDimensions()
  title=self->getTitle()
  
  base = widget_base(/COLUMN, $
    uvalue=self, TLB_FRAME_ATTR=1, $
    title=title, /TLB_KILL_REQUEST_EVENTS, event_pro=self.eventprefix+'destroyBenchMarkCreationWindow')
    
  mainBase = widget_base(base,/ROW)
  
  mBase=widget_base(mainbase,xpad=0, ypad=0,space=0,/COLUMN)
  
  upperBase=widget_base(mBase, xpad=0, ypad=2,space=0,/COLUMN)
  
  graphicTypeBase= widget_base(upperBase,$
    xpad=0, ypad=0,space=0, FRAME=1, /ROW);, /ALIGN_CENTER)
  ;iterateOnBase= widget_base(upperBase,$
  ;  xpad=1, ypad=1,space=1, FRAME=1, /ROW);, /ALIGN_CENTER)
  pageModeBase= widget_base(upperBase, xpad=0, ypad=0,space=0, FRAME=1,/ROW);, /ALIGN_CENTER)
  printOrientBase= widget_base(upperBase, xpad=0, ypad=0,space=0, FRAME=1,/ROW);, /ALIGN_CENTER)
  
  lowerBase=widget_base(mBase,/COLUMN,$
    xpad=0, ypad=2,space=0, FRAME=1)
  splitStyleBase= widget_base(lowerBase, xpad=0, ypad=0,space=0,/ROW);, /ALIGN_CENTER)
  benchmarkSaveModeBase= widget_base(lowerBase, xpad=0, ypad=0,space=0, FRAME=0,/ROW);, /ALIGN_CENTER)
  benchMarkTreeBase= widget_base(lowerBase, xpad=0, ypad=0,space=0,/ROW);, /ALIGN_CENTER)
  optionBase= widget_base(lowerBase, xpad=0, ypad=0,space=0,/ROW);, /ALIGN_CENTER)
  
  okBase= widget_base(base, xpad=0, ypad=2,space=0,/ROW, /ALIGN_RIGHT)
  
  self->buildGraphicTypeSection, graphicTypeBase
  self->buildPageModeSection, pageModeBase
  self->buildPrintOrientSection, printOrientBase
  
  self->buildSplitStyleSection, splitStyleBase
  ;self->buildIterateOptionSection, iterateOnBase
  self->buildBenchMarkSaveModeSection, benchmarkSaveModeBase
  self->buildBenchMarkTreeSection, benchMarkTreeBase
  ;self->buildOptionSection, optionBase
  self->buildOKButton, okBase
  
  self->setTopBase, base
  xmanager, 'fairmode', base, /JUST_REG
  
END
; contents
FUNCTION FMBenchMarkCreationGUI::getAllPrintOrientNames

  return, self.mgr->getAllPrintOrientNames()
  
END

FUNCTION FMBenchMarkCreationGUI::getAllPrintOrientCodes

  return, self.mgr->getAllPrintOrientCodes()
  
END

;FUNCTION FMBenchMarkCreationGUI::getAllPrintOrientNames
;
;  return, self.mgr->getAllPrintOrientNames()
;  
;END

FUNCTION FMBenchMarkCreationGUI::getAllBenchMarkSaveModeCodes

  return, self.mgr->getAllBenchMarkSaveModeCodes()
  
END

FUNCTION FMBenchMarkCreationGUI::getAllBenchMarkSaveModeNames

  return, self.mgr->getAllBenchMarkSaveModeNames()
  
END

FUNCTION FMBenchMarkCreationGUI::getAllPageModeNames

  return, self.mgr->getAllPageModeNames()
;['Single page', 'Multiple page']
  
END

FUNCTION FMBenchMarkCreationGUI::getAllPageModeCodes

  return, self.mgr->getAllPageModeCodes()
  
END

FUNCTION FMBenchMarkCreationGUI::getAllGraphicTypeNames

  return, self.mgr->getAllGraphicTypeNames()
;['Raster (Image)', 'Vectorial (PostScript)']
  
END

FUNCTION FMBenchMarkCreationGUI::getAllGraphicTypeCodes

  return, self.mgr->getAllGraphicTypeCodes()
;return, ['RST', 'VECT']
  
END
;userUpdateBenchMarkSaveModeCode
;  splitDraw=widget_info(self->getTopBase(), FIND='SPLITDRAW')

FUNCTION FMBenchMarkCreationGUI::getAllIterateOptionCodes

  return, ["NONE", "MOD_ALLSCEN", "SCEN_ALLMOD", "ALLSCEN_ALLMOD"]
  
END

FUNCTION FMBenchMarkCreationGUI::getAllIterateOptionNames

  return, ["None", "Scenario (Single model)", "Model (Single scenario)", "All scenarios/models combinations"]
  
END

;graphics
PRO FMBenchMarkCreationGUI::highLightDrawSplit, index

  red=obj_new('Color', 200, 0, 0)
  self->wsetSplitDraw
  splitInfos=self.mgr->getSplitInfo()
  selected=splitInfos[index]
  self->drawSingleSplit, selected, COLOR=red->asLongTrueColor()
  obj_destroy, red
  
END

PRO FMBenchMarkCreationGUI::drawSplitArea

  self->wsetSplitDraw
  white=obj_new('Color', 200, 200, 200)
  device, DECOMPOSED=1
  erase, white->asLongTrueColor()
  self->turnOffSplit
  obj_destroy, white
  obj_destroy, darkGray
  
END

PRO FMBenchMarkCreationGUI::drawSingleSplit, splitInfo, COLOR=COLOR

  xyouts, splitInfo.labelPosition[0], splitInfo.labelPosition[1], splitInfo.name, COLOR=color, CHARSIZE=self.charThick, /NORM
  ;print, splitInfo.borderCoords
  ;polyfill, splitInfo.borderCoords, THICK=self.lineThick, COLOR=COLOR, /NORM
  plots, splitInfo.borderCoords, THICK=self.lineThick, COLOR=COLOR, /NORM
  
END

PRO FMBenchMarkCreationGUI::turnOffSplit

  darkGray=obj_new('Color', 85, 85, 85)
  self->wsetSplitDraw
  splitInfos=self.mgr->getSplitInfo()
  for i=0, n_elements(splitInfos)-1 do begin
    splitInfo=splitInfos[i]
    self->drawSingleSplit, splitInfo, COLOR=darkGray->asLongTrueColor()
  endfor
  obj_destroy, darkGray
  
END

FUNCTION FMBenchMarkCreationGUI::getSplitStyleIndexSelection

  allSplitStyles=self->getAllSplitStyleCodes()
  index=where(self.splitStyleCodeSelection eq allSplitStyles)
  return, index[0]
  
END
; *****************************************************************
; configure, update, display
; *****************************************************************
PRO FMBenchMarkCreationGUI::configure

  graphicTypeCodes=self->getAllGraphicTypeCodes()
  graphicTypeCode=graphicTypeCodes[0]
  graphicTypeSelection=self->getGraphicTypeMarkByCode(graphicTypeCode)
  gtButton=widget_info(self->getTopBase(), FIND_BY_UNAME=graphicTypeSelection)
  
  ;  iterateOptionCodes=self->getAllIterateOptionCodes()
  ;  iterateOptionCode=iterateOptionCodes[0]
  ;  iterateOptionSelection=self->getIterateOptionMarkByCode(iterateOptionCode)
  ;  ioButton=widget_info(self->getTopBase(), FIND_BY_UNAME=iterateOptionSelection)
  
  splitStyleCodes=self.mgr->getAllSplitStyleCodes()
  splitStyleCode=splitStyleCodes[0]
  splitStyleSelection=self->getSplitStyleMarkByCode(splitStyleCode)
  ssButton=widget_info(self->getTopBase(), FIND_BY_UNAME=splitStyleSelection)
  
  pageModeCodes=self.mgr->getAllPageModeCodes()
  pageModeCode=pageModeCodes[0]
  pageModeSelection=self->getPageModeMarkByCode(pageModeCode)
  pmButton=widget_info(self->getTopBase(), FIND_BY_UNAME=pageModeSelection)
  
  saveModeCodes=self.mgr->getAllBenchmarkSaveModeCodes()
  saveModeCode=saveModeCodes[0]
  saveModeSelection=self->getBenchMarkSaveModeMarkByCode(saveModeCode)
  smButton=widget_info(self->getTopBase(), FIND_BY_UNAME=saveModeSelection)
  
  printOrientCodes=self.mgr->getAllPrintOrientCodes()
  printOrientCode=printOrientCodes[0]
  printOrientSelection=self->getPrintOrientMarkByCode(printOrientCode)
  poButton=widget_info(self->getTopBase(), FIND_BY_UNAME=printOrientSelection)
  
  self->userUpdateSplitStyleCode, splitStyleCode, ssButton
  self->userUpdateGraphicTypeCode, graphicTypeCode, gtButton
  self->userUpdatePageModeCode, pageModeCode, pmButton
  self->userUpdateBenchmarkSaveModeCode, saveModeCode, smButton
  self->userUpdatePrintOrientCode, printOrientCode, poButton
  
  bMD=self.mgr->getBenchMarkMenuInfo()
  topLevels=bMD->getSubMenuList(1, 999)
  self->userBenchMarkTreeUpdate, topLevels[0].code
  
END


FUNCTION FMBenchMarkCreationGUI::getTitle

  return, 'Benchmark building'
  
END
;*************************************************************
; constructor / destructor
;*************************************************************
FUNCTION FMBenchMarkCreationGUI::init, info, mgr, fonts=fonts

  if not self -> FMInfoSelectionGUI :: init(info, mgr, fonts=fonts) then return , 0
  ;eventPro=self.eventPrefix+'benchmarkBtt'
  ;dims=get_screen_size(RESOLUTION=resolution)
  ;dims[0]=dims[0]*.66
  ;dims[1]=dims[1]*.66
  ;self.dimensions=dims
  self.charThick=1.
  self.lineThick=2.
  return , 1
  
END

PRO FMBenchMarkCreationGUI::cleanUp

  ptr_free, self.pageModeButtons
  self -> GUI::cleanUp
  
END

;****************************************************************************************

PRO FMBenchMarkCreationGUI__Define

  Struct = { FMBenchMarkCreationGUI , $
    graphicTypeSelection: '', $
    pageModeSelection: '', $
    printOrientSelection: '', $
    benchMarkSaveModeSelection: '', $
    pageModeButtons: ptr_new(), $
    wTreeRoot: 0l, $
    treeMenuNameText: 0l, $
    charThick: 0., $
    lineThick: 0., $
    Inherits FMInfoSelectionGUI $
    }
    
END

;****************************************************************************************

