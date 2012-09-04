FUNCTION FMModeSelectionGUI::getTitle

 return, 'Mode selection'

END

PRO FMModeSelectionGUI::updateToCaller

 self.mgr->UpdateModeInfo, self.info
 self.mgr->enable

END

FUNCTION FMModeSelectionGUI::getDescriptionAtIndex, index

 descrs=self.info->getDescriptions()
 return, descrs[index]

END

FUNCTION FMModeSelectionGUI::getCurrentDescription

 return, self->getDescriptionAtIndex(self.info->getSelection())

END

FUNCTION FMModeSelectionGUI::getAllDescriptions

 descrs=self.info->getDescriptions()
 return, descrs

END

FUNCTION FMModeSelectionGUI::getNameAtIndex, index

 names=self.info->getNames()
 return, names[index]

END

FUNCTION FMModeSelectionGUI::getCurrentName

 return, self->getNameAtIndex(self.info->getSelection())

END

FUNCTION FMModeSelectionGUI::getAllNames

 names=self.info->getNames()
 return, names

END

FUNCTION FMModeSelectionGUI::getCodeAtIndex, index

 codes=self.info->getCodes()
 return, codes[index]

END

FUNCTION FMModeSelectionGUI::getCurrentCode

 return, self->getCodeAtIndex(self.info->getSelection())

END

FUNCTION FMModeSelectionGUI::getAllCodes

 codes=self.info->getCodes()
 return, codes

END

PRO FMModeSelectionGUI::userSelection, index

 self.info->setSelection, index
 widget_control, self.descriptionLabel, set_value=self->getCurrentDescription()
 widget_control, self.modeComboBox, SET_COMBOBOX_SELECT=self.info->getSelection()

END

PRO FMModeSelectionGUI::fillCombobox

 widget_control, self.modeComboBox, SET_VALUE=self->getAllNames()

END

PRO FMModeSelectionGUI::configure

 ; Realize and MoveToCenter is done by superclass
 ; Here only fill & set internal widget
 ;self.mgr->streamPrintOfMode
 ;self->FillAvailableMode, self.info->getNamesList()
 self->fillCombobox
 self->userSelection, self.info->getSelection()
 ;self.projectionComboBox->prevProjectionInfo.destCode
 ;widget_control, projectionDialogWindowBase, set_uvalue=userValue


END

PRO FMModeSelectionGUI::build

 bestDimensions=self->getBestDimensions()
 title=self->getTitle()

 base = WIDGET_BASE(/COLUMN, $
  uvalue=self, TLB_FRAME_ATTR=1, /TLB_KILL_REQUEST_EVENTS, $
  title=title, event_pro=self.eventPrefix+'modal'+'destroyWindow', /ALIGN_CENTER)

 modeBase=widget_base(base, /COLUMN, YPAD=0, XPAD=7, /ALIGN_CENTER)
 mainButtonBase=widget_base(base, /COLUMN, YPAD=0, XPAD=7, /ALIGN_CENTER)

 titleLabel=widget_label(modeBase, VALUE='Select mode configuration:', font=titleFont, /ALIGN_CENTER)
 self.modeComboBox = widget_combobox(modeBase, $
	value=['a','b'], UNAME='MODECOMBO', /ALIGN_CENTER, $
	SCR_XSIZE=bestDimensions[0]*.33, event_pro=self.eventPrefix+'modeComboSelection')
 self.descriptionLabel = widget_label(modeBase, UNAME='DESCRMODE', $
  XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=bestDimensions[0]*.33 ,SCR_YSIZE=bestDimensions[1]*.25 ,/ALIGN_CENTER, $
  VALUE='---Information about mode selected---', /SUNKEN_FRAME, font=self.labelFont)

 self->buildOKButton, mainButtonBase
 ;okButton = widget_button(mainButtonBase, Value='OK', UNAME='APPLY', $
 ; XSIZE=70, YSIZE=35, event_pro=self.eventPrefix+'okModeBtt', /ALIGN_CENTER)

 self->SetTopBase, base
 xmanager, 'fairmode', base, /JUST_REG

END

FUNCTION FMModeSelectionGUI::checkIntegrity, confInfo

; check=file_Info(confInfo.meteoConfFile)
; if confInfo.meteoConfFile ne '' then $
;  if not(check.read eq 1 and check.regular eq 1) $
;   then return, 0
;
; check=file_Info(confInfo.meteoDataFile)
; if confInfo.meteoDataFile ne '' then $
;  if not(check.read eq 1 and check.regular eq 1) then return, 0
;
; check=file_Info(confInfo.chemConfFile)
; if confInfo.chemConfFile ne '' then $
;  if not(check.read eq 1 and check.regular eq 1) then return, 0
;
; check=file_Info(confInfo.chemDataFile)
; if confInfo.chemDataFile ne '' then $
;  if not(check.read eq 1 and check.regular eq 1) then return, 0
;
; check=file_Info(confInfo.aeroConfFile)
; if confInfo.aeroConfFile ne '' then $
;  if not(check.read eq 1 and check.regular eq 1) then return, 0
;
; check=file_Info(confInfo.aeroDataFile)
; if confInfo.aeroDataFile ne '' then $
;  if not(check.read eq 1 and check.regular eq 1) then return, 0
;
; check=file_Info(confInfo.landuseConfFile)
; if confInfo.landuseConfFile ne '' then $
;  if not(check.read eq 1 and check.regular eq 1) then return, 0
;
; check=file_Info(confInfo.landuseDataFile)
; if confInfo.landuseDataFile ne '' then $
;  if not(check.read eq 1 and check.regular eq 1) then return, 0
;
; check=file_Info(confInfo.obserConfFile    )
; if confInfo.obserConfFile ne '' then $
;  if not(check.read eq 1 and check.regular eq 1) then return, 0
;
;; check=checkDate(confInfo.startDate[0], month=confInfo.startDate[1], $
;;  day=confInfo.startDate[2], hour=confInfo.startDate[3], julNumber=julNumber)
;; if check eq 0 then return, 0
;;
;; startDay=julNumber
;; check=checkDate(confInfo.endDate[0], month=confInfo.endDate[1], $
;;  day=confInfo.endDate[2], hour=confInfo.endDate[3], julNumber=julNumber)
;; if check eq 0 then return, 0
;;
;; if startDay ge julNumber then return, 0
 return, 1

END

;PRO validStartButton, ev
;
; widget_control, ev.top, get_UValue=prevTempStruct
; tempStruct=getConfFileInfoFromWidget(ev.top, prevTempStruct)
; if checkJRCValidIntegrity(tempStruct) then begin
;	modelNameText=widget_info(ev.top, FIND='MODELNAME')
;	widget_Control, modelNameText, GET_VALUE=modelName
;	valid, modelName[0], tempStruct
; endif else begin
;	a=dialog_message('Check file existence and/or date!', title='Warning')
; endelse
;
;END
;
;FUNCTION checkJRCValidIntegrity, confInfo
;
; check=file_Info(confInfo.meteoConfFile)
; if confInfo.meteoConfFile ne '' then $
;  if not(check.read eq 1 and check.regular eq 1) $
;   then return, 0
;
; check=file_Info(confInfo.meteoDataFile)
; if confInfo.meteoDataFile ne '' then $
;  if not(check.read eq 1 and check.regular eq 1) then return, 0
;
; check=file_Info(confInfo.chemConfFile)
; if confInfo.chemConfFile ne '' then $
;  if not(check.read eq 1 and check.regular eq 1) then return, 0
;
; check=file_Info(confInfo.chemDataFile)
; if confInfo.chemDataFile ne '' then $
;  if not(check.read eq 1 and check.regular eq 1) then return, 0
;
; check=file_Info(confInfo.aeroConfFile)
; if confInfo.aeroConfFile ne '' then $
;  if not(check.read eq 1 and check.regular eq 1) then return, 0
;
; check=file_Info(confInfo.aeroDataFile)
; if confInfo.aeroDataFile ne '' then $
;  if not(check.read eq 1 and check.regular eq 1) then return, 0
;
; check=file_Info(confInfo.landuseConfFile)
; if confInfo.landuseConfFile ne '' then $
;  if not(check.read eq 1 and check.regular eq 1) then return, 0
;
; check=file_Info(confInfo.landuseDataFile)
; if confInfo.landuseDataFile ne '' then $
;  if not(check.read eq 1 and check.regular eq 1) then return, 0
;
; check=file_Info(confInfo.obserConfFile    )
; if confInfo.obserConfFile ne '' then $
;  if not(check.read eq 1 and check.regular eq 1) then return, 0
;
;; check=checkDate(confInfo.startDate[0], month=confInfo.startDate[1], $
;;  day=confInfo.startDate[2], hour=confInfo.startDate[3], julNumber=julNumber)
;; if check eq 0 then return, 0
;;
;; startDay=julNumber
;; check=checkDate(confInfo.endDate[0], month=confInfo.endDate[1], $
;;  day=confInfo.endDate[2], hour=confInfo.endDate[3], julNumber=julNumber)
;; if check eq 0 then return, 0
;;
;; if startDay ge julNumber then return, 0
; return, 1
;
;END
;
;FUNCTION buildEntitySelectionGUI, mainApp
;
; util=obj_new('FMUtility')
; titleFont=util->getTitleTextFont()
; labelFont=util->getStandardLabelFont()
; textFont=util->getStandardTextFont()
; bestDimensions=GetBestDimensions()
; title='Choose your entity related'
; ;proPrefix='showAll_'
;
; base = WIDGET_BASE(/COLUMN,scr_XSIZE=bestDimensions[0]*1.05, $
;  scr_ysize=bestDimensions[1]*1.10+30, $
;  uvalue=mainApp, MBAR=saMenuBar, $
;  /SCROLL, KILL_NOTIFY='destroyWindow', $
;  title=title)
;
; mainBase = WIDGET_BASE(base, /ROW)
;
; modelMenu=widget_button(saMenuBar, value='Model', UNAME='MODELMENU', /MENU)
; observedMenu=widget_button(saMenuBar, value='Observed', UNAME='OBSMENU', /MENU)
; modeMenu=widget_button(saMenuBar, value='Mode', UNAME='MODEMENU', /MENU)
; displayMenu=widget_button(saMenuBar, value='Display', UNAME='DISPLAYMENU', /MENU)
; utilityMenu=widget_button(saMenuBar, value='Utility', UNAME='UTILITYMENU', /MENU)
; helpMenu=Widget_Button(saMenuBar, VALUE='Help', UNAME='HELPMENU', UVALUE='HELPMENU', /MENU)
;
; modelParMenu=widget_button(modelMenu, value='Parameter', UNAME='MODELPARMENU', /MENU)
; modelSliceMenu=widget_button(modelMenu, value='Slice', UNAME='MODELSLICEMENU', /MENU)
;
; buildParameterMenu, modelParMenu, meteoNames, chemNames, aeroNames, lUseNames, proPrefix, OUTfirstParWId
; buildObservedMenu, observedMenu, observedData, proPrefix, OUTfirstObsWId
;
; slicePlanXYButton=widget_button(modelSliceMenu, value='XY Plane', UNAME='PLANEXY', /CHECKED_MENU, event_pro=proPrefix+'planeTypeMenuSelection')
; slicePlanXZButton=widget_button(modelSliceMenu, value='XZ Plane', UNAME='PLANEXZ', /CHECKED_MENU, event_pro=proPrefix+'planeTypeMenuSelection')
; slicePlanYZButton=widget_button(modelSliceMenu, value='YZ Plane', UNAME='PLANEYZ', /CHECKED_MENU, event_pro=proPrefix+'planeTypeMenuSelection')
; ;slicePlanUserXYSliceButton=widget_button(modelSliceMenu, value='User slice', UNAME='PLANEUSER', /CHECKED_MENU, event_pro=proPrefix+'planeTypeMenuSelection', SENSITIVE=0)
; exitButton=widget_button(modelMenu, value='Exit', UNAME='EXIT', event_pro='destroyWindow')
;
; modeRecognizeModelMenu=widget_button(modeMenu, UVALUE=0, value='Recognize Model', UNAME='MODERECMODEL', /CHECKED_MENU, event_pro=proPrefix+'interactionModeMenuSelection')
; ;modeRecognizeObservedMenu=widget_button(modeMenu, UVALUE=1, value='Recognize ground station', UNAME='MODERECGND', /CHECKED_MENU, event_pro=proPrefix+'interactionModeMenuSelection')
; modeZoomMenu=widget_button(modeMenu, UVALUE=2, value='Zoom', UNAME='MODEZOOM', /CHECKED_MENU, event_pro=proPrefix+'interactionModeMenuSelection')
; modeModelXYFreeProfileMenu=widget_button(modeMenu, UVALUE=7, value='SZ User section', UNAME='XYSPROFILEMODEL', /CHECKED_MENU, event_pro=proPrefix+'interactionModeMenuSelection')
;
;; modeModelGraphsMenu=widget_button(modeMenu, value='Model Graphs', UNAME='MODEMODGRAPHSMENU', /MENU)
;; modeObservedGraphsMenu=widget_button(modeMenu, value='Observed Graphs', UNAME='MODEOBSGRAPHSMENU', /MENU)
;
;; modeModelTimeEvolutionMenu=widget_button(modeModelGraphsMenu, UVALUE=3, value='Time evolution', UNAME='TEVOLUTIONMODEL', /CHECKED_MENU, event_pro=proPrefix+'interactionModeMenuSelection')
;; modeModelZProfileMenu=widget_button(modeModelGraphsMenu, UVALUE=4, value='Z profile', UNAME='ZPROFILEMODEL', /CHECKED_MENU, event_pro=proPrefix+'interactionModeMenuSelection')
;; modeModelXProfileMenu=widget_button(modeModelGraphsMenu, UVALUE=5, value='X profile', UNAME='XPROFILEMODEL', /CHECKED_MENU, event_pro=proPrefix+'interactionModeMenuSelection')
;; modeModelYProfileMenu=widget_button(modeModelGraphsMenu, UVALUE=6, value='Y profile', UNAME='YPROFILEMODEL', /CHECKED_MENU, event_pro=proPrefix+'interactionModeMenuSelection')
; modeModelTimeEvolutionMenu=widget_button(modeMenu, UVALUE=3, value='Time evolution', UNAME='TEVOLUTIONMODEL', /CHECKED_MENU, event_pro=proPrefix+'interactionModeMenuSelection')
; modeModelZProfileMenu=widget_button(modeMenu, UVALUE=4, value='Z profile', UNAME='ZPROFILEMODEL', /CHECKED_MENU, event_pro=proPrefix+'interactionModeMenuSelection')
; modeModelXProfileMenu=widget_button(modeMenu, UVALUE=5, value='X profile', UNAME='XPROFILEMODEL', /CHECKED_MENU, event_pro=proPrefix+'interactionModeMenuSelection')
; modeModelYProfileMenu=widget_button(modeMenu, UVALUE=6, value='Y profile', UNAME='YPROFILEMODEL', /CHECKED_MENU, event_pro=proPrefix+'interactionModeMenuSelection')
;
;;modeObsTimeEvolutionMenu=widget_button(modeObservedGraphsMenu, value='Time evolution', UNAME='TEVOLUTIONOBS')
;;modeObsZProfileMenu=widget_button(modeObservedGraphsMenu, value='Z profile', UNAME='ZPROFILEOBS')
;;modeObsOtherMenu=widget_button(modeObservedGraphsMenu, value='Recognize Observed', UNAME='OTHEROBS')
;
;
; displayWindBtt=widget_button(displayMenu, value='Show wind field', UNAME='DISPLAYWIND', /CHECKED_MENU, event_pro=proPrefix+'displaySelection')
; displayGridBtt=widget_button(displayMenu, value='Show data as grid', UNAME='DISPLAYGRID', /CHECKED_MENU, event_pro=proPrefix+'displaySelection')
; displayValuesBtt=widget_button(displayMenu, value='Show contour Values', UNAME='DISPLAYVALUES', /CHECKED_MENU, event_pro=proPrefix+'displaySelection')
; displayTopoBtt=widget_button(displayMenu, value='Show topography', UNAME='DISPLAYTOPO', /CHECKED_MENU, event_pro=proPrefix+'displaySelection')
; displayGndStationBtt=widget_button(displayMenu, value='Show ground station', UNAME='DISPLAYGNDSTATION', /CHECKED_MENU, event_pro=proPrefix+'displaySelection')
; displayGraphLegendBtt=widget_button(displayMenu, value='Show graph legend', UNAME='DISPLAYGRAPHLEGEND', /CHECKED_MENU, event_pro=proPrefix+'displaySelection')
;
; utilityResetZoomOptionButton=widget_button(utilityMenu, value='Reset zoom', UNAME='UTILITYRESETZOOM', event_pro=proPrefix+'utilityResetZoomSelection')
; utilityAnimationButton=widget_button(utilityMenu, value='Animation', UNAME='UTILITYANIMATION', event_pro=proPrefix+'utilityAnimationSelection')
; utilityLegendOptionButton=widget_button(utilityMenu, value='Open legend manager...', UNAME='UTILITYLEGEND', event_pro=proPrefix+'utilityOpenColorScaleSelection')
; utilityGraphRangeButton=widget_button(utilityMenu, value='Set graph range...', UNAME='UTILITYGRAPHRANGE', event_pro=proPrefix+'utilityGraphRangeSelection')
; utilitySaveAsButton=widget_button(utilityMenu, value='Save as...', UNAME='UTILITYSAVEAS', event_pro=proPrefix+'utilitySaveAsSelection')
; utilityObsLevelButton=widget_button(utilityMenu, value='Level recognize...', UNAME='UTILITYOBSLEVEL', event_pro=proPrefix+'utilityLevelRecognizeSelection')
; utilityProjectionButton=widget_button(utilityMenu, value='Projection selection...', UNAME='UTILITYPROJECTION', event_pro=proPrefix+'utilityProjectionSelection', sens=0)
;
; helpDescBtt=widget_Button(helpMenu, VALUE='Help file', UNAME='Help file...', UVALUE='Help file', event_pro=proPrefix+'helpMenuSelection')
; wwwPageBtt=widget_Button(helpMenu, VALUE='EuroDelta WWW', UNAME='Download site...', UVALUE='OPENWWW', event_pro=proPrefix+'downloadMenuSelection')
; aboutBtt=widget_Button(helpMenu, VALUE='About...', UNAME='Display map', UVALUE='DISPMAP', event_pro='aboutMenuSelection')
;
; subBase1 = WIDGET_BASE(mainbase,xpad=0, ypad=2,space=0,/ROW)
; subBase11 = WIDGET_BASE(subBase1,xpad=0, ypad=0,space=0,/COLUMN)
; subBase12 = WIDGET_BASE(subBase1,xpad=0, ypad=0,space=0,/COLUMN)
; subBase121 = WIDGET_BASE(subBase12,xpad=0, ypad=0,space=0,/COLUMN)
; subBase122 = WIDGET_BASE(subBase12,xpad=0, ypad=0,space=0,/COLUMN)
;
; buildUserInteractionSection, subBase11, [bestDimensions[0]*.35,bestDimensions[1]*1.], proPrefix
; buildMainDrawSection, subBase121, [bestDimensions[0]*.65,bestDimensions[1]*.7], proPrefix
; buildGraphSectionSection, subBase122, [bestDimensions[0]*.65,bestDimensions[1]*.3], proPrefix
;
; statusBarMain = Widget_Label(base, UNAME='STATUSBARMAIN', $
;  XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=bestDimensions[0]+5 ,SCR_YSIZE=20 ,/ALIGN_LEFT, $
;  VALUE='---Information about application---', /SUNKEN_FRAME, font=labelFont)
;
; statusBarMap=widget_info(base, FIND='STATUSBARMAP')
; statusBarGraph=widget_info(base, FIND='STATUSBARGRAPH')
; OUTfirstParWId=OUTfirstParWId
; OUTfirstObsWId=OUTfirstObsWId
; OUTslicePlanXYButton=slicePlanXYButton
; OUTmodeRecognizeModelMenu=modeRecognizeModelMenu
; OUTdisplayOptionBtts=[displayWindBtt, displayGridBtt, displayValuesBtt, $
;  displayTopoBtt, displayGndStationBtt, displayGraphLegendBtt]
;
; return, base
;
;END
;
;PRO configureMainStatusBar, topBase, statusMessage
;
; statusLabel=widget_info(topBase, FIND='STATUSBARMAIN')
; widget_control, statusLabel, set_value=statusMessage
;
;END
;
;PRO configureButtonForSliceOption, topBase, ON=ON, OFF=OFF, newPlaneSection=newPlaneSection, FROMSLICE=FROMSLICE, LANDUSE=LANDUSE
;
; widget_control, topBase, get_uvalue=mainRefStruct
; controlWindow=*(mainRefStruct.controlWindow)
; sliceInfo=*(mainRefStruct.sliceInfo)
; parameter=*(mainRefStruct.parameterData)
;
; modelParMenu=widget_info(topBase, FIND='MODELPARMENU')
; modeMenu=widget_info(topBase, FIND='MODEMENU')
; observedMenu=widget_info(topBase, FIND='OBSMENU')
; displayMenu=widget_info(topBase, FIND='DISPLAYMENU')
; sliderT=widget_info(topBase, FIND='SLIDERTIME')
; sliderX=widget_info(topBase, FIND='SLIDERX')
; sliderY=widget_info(topBase, FIND='SLIDERY')
; sliderZ=widget_info(topBase, FIND='SLIDERZ')
;
; resetZoomButton=widget_info(topBase, FIND='UTILITYRESETZOOM')
; animationButton=widget_info(topBase, FIND='UTILITYANIMATION')
; legendButton=widget_info(topBase, FIND='UTILITYLEGEND')
; graphRangeButton=widget_info(topBase, FIND='UTILITYGRAPHRANGE')
; obsLevelButton=widget_info(topBase, FIND='UTILITYOBSLEVEL')
; projectionButton=widget_info(topBase, FIND='UTILITYPROJECTION')
; slicePlanXYButton=widget_info(topBase, FIND='PLANEXY')
; slicePlanXZButton=widget_info(topBase, FIND='PLANEXZ')
; slicePlanYZButton=widget_info(topBase, FIND='PLANEYZ')
; ;slicePlanUserXYSliceButton=widget_info(topBase, FIND='PLANEUSER')
;
; if keyword_set(ON) then begin
;	sens=0
;	widget_control, slicePlanXYButton, set_button=0
;	widget_control, slicePlanXZButton, set_button=0
;	widget_control, slicePlanYZButton, set_button=0
;	widget_control, sliderT, sens=0
;	widget_control, sliderX, sens=0
;	widget_control, sliderY, sens=0
;	widget_control, sliderZ, sens=0
;	;widget_control, slicePlanUserXYSliceButton, set_button=1
;	configureMapTitle, topBase, parameter, sliceInfo, /USERSECTION
;	newPlaneSection='USER'
; endif else begin
;	sens=1
;	;widget_control, slicePlanUserXYSliceButton, set_button=0
;	if widget_info(controlWindow.prevSliceBtt, /VALID_ID) and not(keyword_set(FROMSLICE))then begin
;		widget_control, controlWindow.prevSliceBtt, set_button=1
;		uname=widget_info(controlWindow.prevSliceBtt, /UNAME)
;		check=strpos(uname, 'PLANE', /REVERSE_S)
;		if check[0] ne -1 then newPlaneSection=strmid(uname, 5, 2)
;	endif
; endelse
;
; widget_control, modeMenu, sensitive=sens
; widget_control, modelParMenu, sensitive=sens
; widget_control, observedMenu, sensitive=sens
; widget_control, displayMenu, sensitive=sens
; widget_control, resetZoomButton, sensitive=sens
; widget_control, animationButton, sensitive=sens
; widget_control, legendButton, sensitive=sens
; widget_control, graphRangeButton, sensitive=sens
; widget_control, obsLevelButton, sensitive=sens
; ;widget_control, projectionButton, sensitive=sens
; widget_control, topBase, UPDATE=1
;
; if keyword_set(LANDUSE) then begin
;	widget_control, slicePlanXYButton, sens=0
;	widget_control, slicePlanXZButton, sens=0
;	widget_control, slicePlanYZButton, sens=0
; endif else begin
;	widget_control, slicePlanXYButton, sens=1
;	widget_control, slicePlanXZButton, sens=1
;	widget_control, slicePlanYZButton, sens=1
; endelse
;
;END
;
;PRO configureSAObserved, topBase, obsInfo, value, validity
;
; if n_elements(value) eq 0 then begin
;	highLightStation, [obsInfo.xGeoLocation, obsInfo.yGeoLocation]
;	value='' & validity=''
; endif else begin
;	highLightObservation, [obsInfo.xMapLoc, obsInfo.yMapLoc], obsInfo.polyCode
; endelse
;
; textX=widget_info(topBase, FIND='OBSXPOS')
; textY=widget_info(topBase, FIND='OBSYPOS')
; textZ=widget_info(topBase, FIND='OBSZPOS')
; ;textHeight=widget_info(topBase, FIND='OBSHEIGHT')
; ;textValidity=widget_info(topBase, FIND='OBSVALIDITY')
; textValue=widget_info(topBase, FIND='OBSVALUE')
; textType=widget_info(topBase, FIND='OBSTYPE')
; textCategory=widget_info(topBase, FIND='OBSCATEGORY')
; textName=widget_info(topBase, FIND='OBSNAME')
;
; dataPresent=(where(tag_names(obsInfo) eq strupcase('zGeoLocation')))[0]
; widget_control, textX, set_value=strtrim(obsInfo.xGeoLocation, 2)
; widget_control, textY, set_value=strtrim(obsInfo.yGeoLocation, 2)
; if dataPresent ne -1 then widget_control, textZ, set_value=strtrim(obsInfo.zGeoLocation, 2)
; ;widget_control, textHeight, set_value=strtrim(obsInfo.height, 2)
; widget_control, textName, set_value=obsInfo.name
; widget_control, textType, set_value=obsInfo.type
; widget_control, textCategory, set_value=obsInfo.category
; widget_control, textValue, set_value=value
; ;widget_control, textValidity, set_value=validity
;
;END
;
;PRO configureMapTitle, topBase, parameter, sliceInfo, USERSECTION=USERSECTION
;
; title=parameter.type+' '+parameter.name+'['+parameter.measureUnit+'] - '
; title=title+' '+'Section: '
; if not (keyword_set(USERSECTION)) then title=title+sliceInfo.planeSection else title=title+'user xy'
; title=title+'  at: '
; title=title+'Time +'+strtrim(sliceInfo.timeIndex, 2)
; if sliceInfo.planeSection eq 'XY' then title=title+' GridZ :'+strtrim(sliceInfo.zIndex, 2)
; if sliceInfo.planeSection eq 'YZ' then title=title+' GridX :'+strtrim(sliceInfo.xIndex, 2)
; if sliceInfo.planeSection eq 'XZ' then title=title+' GridY :'+strtrim(sliceInfo.yIndex, 2)
;
; if sliceInfo.planeSection eq 'XYS' then title=title+'XYS'+strtrim(sliceInfo.yIndex, 2)
;
; statusBarMap=widget_info(topBase, FIND='STATUSBARMAP')
; widget_control, statusBarMap, set_value=title
;
;END
;
;PRO configureGraphTitle, topBase, title
;
;; title=parameter.type+' '+parameter.name+'['+parameter.measureUnit+'] - '
;; title=title+' '+'Section: '+sliceInfo.planeSection+'  at: '
;; title=title+'Time +'+strtrim(sliceInfo.timeIndex, 2)
;; if sliceInfo.planeSection eq 'XY' then title=title+' GridZ +'+strtrim(sliceInfo.zIndex, 2)
;; if sliceInfo.planeSection eq 'YZ' then title=title+' GridX +'+strtrim(sliceInfo.xIndex, 2)
;; if sliceInfo.planeSection eq 'XZ' then title=title+' GridY +'+strtrim(sliceInfo.yIndex, 2)
;
; statusBarMap=widget_info(topBase, FIND='STATUSBARGRAPH')
; widget_control, statusBarMap, set_value=title
;
;END
;
;PRO wsetGeoDataDraw, topBase
;
; widget_control, topBase, get_uvalue=mainRefStruct
; geoDraw=widget_info(topBase, FIND='MAINDRAW')
; widget_control, geoDraw, get_value=wsetId
; wset, wsetId
; if ptr_valid(mainRefStruct.controlWindow) then begin
; 	controlWindow=*(mainRefStruct.controlWindow)
;	!P=controlWindow.systemP[0] & !X=controlWindow.systemX[0]
;	!Y=controlWindow.systemY[0] & !Z=controlWindow.systemZ[0]
; endif
;
;END
;
;PRO wsetGraphDataDraw, topBase
;
; widget_control, topBase, get_uvalue=mainRefStruct
; graphDraw=widget_info(topBase, FIND='GRAPHDRAW')
; widget_control, graphDraw, get_value=wsetId
; wset, wsetId
; if ptr_valid(mainRefStruct.controlWindow) then begin
; 	controlWindow=*(mainRefStruct.controlWindow)
;	!P=controlWindow.systemP[1] & !X=controlWindow.systemX[1]
;	!Y=controlWindow.systemY[1] & !Z=controlWindow.systemZ[1]
; endif
;
;END
;
;PRO configureSAGeo, base, geoPosition
;
; textGeoXPos=widget_info(base, FIND='GEOXPOS')
; textGeoYPos=widget_info(base, FIND='GEOYPOS')
; textGeoZPos=widget_info(base, FIND='GEOZPOS')
;
; widget_control, textGeoXPos, SET_VALUE=strtrim(geoPosition[0], 2)
; widget_control, textGeoYPos, SET_VALUE=strtrim(geoPosition[1], 2)
; widget_control, textGeoZPos, SET_VALUE=strtrim(geoPosition[2], 2)
;
;END
;
;PRO configureSAGrid, base, gridPosition, gridvalue
;
; textGridXPos=widget_info(base, FIND='GRIDXPOS')
; textGridYPos=widget_info(base, FIND='GRIDYPOS')
; textGridZPos=widget_info(base, FIND='GRIDZPOS')
; textGridValue=widget_info(base, FIND='GRIDVALUE')
;
; widget_control, textGridXPos, SET_VALUE=strtrim(gridPosition[0], 2)
; widget_control, textGridYPos, SET_VALUE=strtrim(gridPosition[1], 2)
; widget_control, textGridZPos, SET_VALUE=strtrim(gridPosition[2], 2)
; widget_control, textGridValue, SET_VALUE=strtrim(gridValue, 2)
;
;END
;
;PRO configureSAMenuBar, base, controlWindow
;
; widget_control, controlWindow.prevParBttId, set_button=1
; widget_control, controlWindow.prevSliceBtt, set_button=1
; widget_control, controlWindow.prevObsBtt, set_button=1
; widget_control, controlWindow.prevModeBtt, set_button=1
;
;END
;
;PRO configureSAStats, base, sliceInfo, rawData
;
; textMinSlice=widget_info(base, FIND='MINSLICETEXT')
; textMinSliceAllTime=widget_info(base, FIND='MINSLICEALLTIMETEXT')
; textMinData=widget_info(base, FIND='MINDATATEXT')
; textMaxSlice=widget_info(base, FIND='MAXSLICETEXT')
; textMaxSliceAllTime=widget_info(base, FIND='MAXSLICEALLTIMETEXT')
; textMaxData=widget_info(base, FIND='MAXDATATEXT')
;
; dim1StartIndex=sliceInfo.zoomIndexStart[0];0
; dim1EndIndex=sliceInfo.zoomIndexEnd[0];parameter.xdimension-1
; dim2StartIndex=sliceInfo.zoomIndexStart[1];0
; dim2EndIndex=sliceInfo.zoomIndexEnd[1];parameter.ydimension-1
; minOverAll=min(rawData, max=maxOverAll, /NAN)
;
; case sliceInfo.planeSection of
;; 	'XY':testData=reform(rawData(*,*,sliceInfo.zindex,sliceInfo.timeindex))
;; 	'XZ':testData=reform(rawData(*,sliceInfo.yindex,*,sliceInfo.timeindex))
;; 	'YZ':testData=reform(rawData(sliceInfo.yindex,*,*,sliceInfo.timeindex))
;; 	'XYS':testData=reform(rawData(*,*,sliceInfo.zindex,sliceInfo.timeindex))
; 	'XY':testData=reform(rawData(dim1StartIndex:dim1EndIndex,dim2StartIndex:dim2EndIndex,sliceInfo.zindex,*))
; 	'XZ':testData=reform(rawData(dim1StartIndex:dim1EndIndex,sliceInfo.yindex,dim2StartIndex:dim2EndIndex,*))
; 	'YZ':testData=reform(rawData(sliceInfo.yindex,dim1StartIndex:dim1EndIndex,dim2StartIndex:dim2EndIndex,*))
; 	'XYS':testData=reform(rawData(dim1StartIndex:dim1EndIndex,dim2StartIndex:dim2EndIndex,sliceInfo.zindex,*))
; endcase
; prevExcept=!EXCEPT
; sliceMin=min(testData[*,*,sliceInfo.timeindex], max=sliceMax, /NAN)
; sliceAllTimeMin=min(testData, max=sliceAllTimeMax, /NAN)
;; print, 'Math error occurred in critical section.'
;; print, 'sliceMin, sliceMax, sliceAverage'
;; print, sliceMin, sliceMax, sliceAverage
;; print, 'overallMin, overallMax, overallAverage'
;; print, overallMin, overallMax, overallAverage
;
; widget_control,textMinSlice, SET_VALUE=strcompress(sliceMin, /REMOVE_ALL)
; widget_control,textMinSliceAllTime, SET_VALUE=strcompress(sliceAllTimeMin, /REMOVE_ALL)
; widget_control,textMinData, SET_VALUE=strcompress(minOverAll, /REMOVE_ALL)
; widget_control,textMaxSlice, SET_VALUE=strcompress(sliceMax, /REMOVE_ALL)
; widget_control,textMaxSliceAllTime, SET_VALUE=strcompress(sliceAllTimeMax, /REMOVE_ALL)
; widget_control,textMaxData, SET_VALUE=strcompress(maxOverAll, /REMOVE_ALL)
;
;END
;
;PRO configureSASlider, base, parameter, sliceInfo, LANDUSE=LANDUSE
;
; sliderT=widget_info(base, FIND='SLIDERTIME')
; sliderX=widget_info(base, FIND='SLIDERX')
; sliderY=widget_info(base, FIND='SLIDERY')
; sliderZ=widget_info(base, FIND='SLIDERZ')
; labelT=widget_info(base, FIND='TSLICE')
; labelX=widget_info(base, FIND='XSLICE')
; labelY=widget_info(base, FIND='YSLICE')
; labelZ=widget_info(base, FIND='ZSLICE')
;; xPos=strtrim((*parameter.xGeoLoc)[sliceInfo.xIndex,sliceInfo.yIndex,sliceInfo.yIndex], 2)
;; yPos=strtrim((*parameter.yGeoLoc)[sliceInfo.xIndex,sliceInfo.yIndex,sliceInfo.yIndex], 2)
;; zPos=strtrim((*parameter.zGeoLoc)[sliceInfo.xIndex,sliceInfo.yIndex,sliceInfo.yIndex], 2)
; if keyword_set(LANDUSE) or parameter.type eq 'landuse' then begin
;	;widget_control, labelT, sens=0
;	;widget_control, labelX, sens=0
;	;widget_control, labelY, sens=0
;	;widget_control, labelZ, SET_VALUE=zPos;, scr_xsize=40
;	widget_control, sliderT, sens=0
;	widget_control, sliderX, sens=0
;	widget_control, sliderY, sens=0
;	widget_control, sliderZ, sens=0
; endif else begin
;	date=getDate(parameter.startDate, parameter.sampleFreq, sliceInfo.timeIndex)
;	xPos=strtrim(string((*parameter.xGeoLoc)[sliceInfo.xIndex,sliceInfo.yIndex,sliceInfo.zIndex], format='(f7.2)'), 1)
;	yPos=strtrim(string((*parameter.yGeoLoc)[sliceInfo.xIndex,sliceInfo.yIndex,sliceInfo.zIndex], format='(f7.2)'), 1)
;	zPos=strtrim(string((*parameter.zGeoLoc)[sliceInfo.xIndex,sliceInfo.yIndex,sliceInfo.zIndex], format='(f7.2)'), 1)
;	;widget_control, GET_VALUE=value, SET_SLIDER_MAX=value, SET_SLIDER_MIN=value, SET_VALUE=value
;	widget_control, labelT, SET_VALUE=date;, scr_xsize=75, scr_ysize=40
;	widget_control, labelX, SET_VALUE=xPos;, scr_xsize=40
;	widget_control, labelY, SET_VALUE=yPos;, scr_xsize=40
;	;widget_control, labelZ, SET_VALUE=zPos;, scr_xsize=40
;	widget_control, sliderT, SET_SLIDER_MAX=sliceInfo.timeDimension-1, SET_SLIDER_MIN=0, SET_VALUE=sliceInfo.timeIndex, SENSITIVE=sliceInfo.sliderSensitive[0]
;	widget_control, sliderX, SET_SLIDER_MAX=sliceInfo.xDimension-1, SET_SLIDER_MIN=0, SET_VALUE=sliceInfo.xIndex, SENSITIVE=sliceInfo.sliderSensitive[1]
;	widget_control, sliderY, SET_SLIDER_MAX=sliceInfo.yDimension-1, SET_SLIDER_MIN=0, SET_VALUE=sliceInfo.yIndex, SENSITIVE=sliceInfo.sliderSensitive[2]
;	widget_control, sliderZ, SET_SLIDER_MAX=sliceInfo.zDimension-1, SET_SLIDER_MIN=0, SET_VALUE=sliceInfo.zIndex, SENSITIVE=sliceInfo.sliderSensitive[3]
; endelse
;
;END
;
;PRO buildUserInteractionSection, base, dims, proPrefix
;
; titleFont=getTitleTextFont()
; labelFont=getStandardLabelFont()
; textFont=getStandardTextFont()
; labelX=dims[0]-5
; labelY=20
; smalltextX=(dims[0]-10)/6
; smalltextY=20
; bigTextX=(dims[0]-10)/4
; bigTextY=20
; veryBigTextX=(dims[0]-10)/3
; veryBigTextY=20
;
; interactionBase = Widget_Base(base, UNAME='interactionBase', $
;  XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=dims[0] ,SCR_YSIZE=dims[1], $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=1, /COLUMN, /ALIGN_LEFT)
;
;
;; geoInfoBase = Widget_Base(interactionBase, UNAME='geoInfoBase', $
;;  FRAME=1 ,XOFFSET=0 ,YOFFSET=0, /ALIGN_LEFT, $
;;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=1, /COLUMN ,SCR_XSIZE=dims[0])
;
; modelInfoBase = Widget_Base(interactionBase, UNAME='modelInfoBase', $
;  FRAME=1 ,XOFFSET=0 ,YOFFSET=0, /ALIGN_LEFT, $
;  TITLE='IDL' ,SPACE=0 ,XPAD=2 ,YPAD=1, /COLUMN,SCR_XSIZE=dims[0])
;
; observedInfoBase = Widget_Base(interactionBase, $
; UNAME='observedInfoBase' ,FRAME=1 ,XOFFSET=0 ,YOFFSET=0, $
;  SCR_XSIZE=dims[0] ,TITLE='IDL' ,SPACE=0 ,XPAD=2, $
;  YPAD=1, /COLUMN)
;
; sliceInfoBase = Widget_Base(interactionBase, UNAME='sliceInfoBase', $
;  FRAME=1 ,XOFFSET=0 ,SCR_XSIZE=dims[0], $
;  TITLE='IDL' ,SPACE=0 ,XPAD=2 ,YPAD=0, /ALIGN_LEFT, /COLUMN)
;
;
; statsBase = Widget_Base(interactionBase, $
; UNAME='statsBase' ,FRAME=1 ,XOFFSET=0 ,YOFFSET=0, $
;  SCR_XSIZE=dims[0] ,TITLE='IDL' ,SPACE=0 ,XPAD=2, $
;  YPAD=0, /COLUMN, /ALIGN_LEFT)
;
; helpBase = Widget_Base(interactionBase, $
; UNAME='helpBase' ,FRAME=1 ,XOFFSET=0 ,YOFFSET=0, $
;  SCR_XSIZE=dims[0] ,TITLE='IDL' ,SPACE=0 ,XPAD=2, $
;  YPAD=0, /COLUMN, /ALIGN_LEFT)
;
;; titleBase = Widget_Base(geoInfoBase, UNAME='WID_BASE_1' , $
;;  XOFFSET=0, YOFFSET=0, /ALIGN_CENTER, $
;;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
;;
;;
;; captionTitle = Widget_Label(titleBase, UNAME='WID_LABEL_0', $
;;  SCR_XSIZE=labelX ,SCR_YSIZE=labelY ,/ALIGN_CENTER, $
;;  VALUE='Geo Info', FONT=titleFont)
;;
;;
;; baseFor2 = Widget_Base(geoInfoBase, UNAME='WID_BASE_2', $
;;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
;;
;;
;; base1of2 = Widget_Base(baseFor2, UNAME='WID_BASE_3' ,XOFFSET=7, $
;;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
;;
;;
;; WID_LABEL_1 = Widget_Label(base1of2, UNAME='WID_LABEL_1', $
;;  SCR_XSIZE=bigTextX ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;;  VALUE='x:', font=labelFont)
;;
;;
;; WID_TEXT_0 = Widget_Text(base1of2, UNAME='GEOXPOS' ,XOFFSET=29, $
;;  SCR_XSIZE=bigTextX ,SCR_YSIZE=smallTextY ,SENSITIVE=1,$
;;  /ALL_EV, font=textFont)
;;
;;
;; base2of2 = Widget_Base(baseFor2, UNAME='WID_BASE_4', $
;;  XOFFSET=0 ,YOFFSET=0, $
;;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
;;
;;
;;
;; WID_LABEL_2 = Widget_Label(base2of2, UNAME='', $
;;  XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=bigTextX ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;;  VALUE='y:', font=labelFont)
;;
;;
;; WID_TEXT_1 = Widget_Text(base2of2, UNAME='GEOYPOS' ,XOFFSET=29, $
;;  YOFFSET=0, SCR_XSIZE=bigTextX ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV, $
;;  XSIZE=20 ,YSIZE=1, font=textFont)
;
;
; titleBase = Widget_Base(modelInfoBase, UNAME='WID_BASE_1' , $
;  XOFFSET=0, YOFFSET=0, /ALIGN_CENTER, $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
;
;
; captionTitle = Widget_Label(titleBase, UNAME='WID_LABEL_0', $
;  SCR_XSIZE=labelX ,SCR_YSIZE=labelY ,/ALIGN_CENTER, $
;  VALUE='Model Info', FONT=titleFont)
;
;
; baseFor3 = Widget_Base(modelInfoBase, UNAME='WID_BASE_2', $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=1, /ROW)
;
; base1of3 = Widget_Base(baseFor3, UNAME='WID_BASE_3' ,XOFFSET=7, $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
;
;
; WID_LABEL_1 = Widget_Label(base1of3, UNAME='', $
;  SCR_XSIZE=smallTextX ,SCR_YSIZE=smallTextY ,/ALIGN_RIGHT, $
;  VALUE='x:', font=labelFont)
;
;
; WID_TEXT_0 = Widget_Text(base1of3, UNAME='GEOXPOS' ,XOFFSET=29, $
;  SCR_XSIZE=smallTextX-3 ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
;  , font=textFont)
;
; base2of3 = Widget_Base(baseFor3, UNAME='WID_BASE_3' ,XOFFSET=7, $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
;
;
; WID_LABEL_1 = Widget_Label(base2of3, UNAME='WID_LABEL_1', $
;  SCR_XSIZE=smallTextX ,SCR_YSIZE=smallTextY ,/ALIGN_RIGHT, $
;  VALUE='y:', font=labelFont)
;
;
; WID_TEXT_0 = Widget_Text(base2of3, UNAME='GEOYPOS' ,XOFFSET=29, $
;  SCR_XSIZE=smallTextX-3 ,SCR_YSIZE=smallTextY  ,SENSITIVE=1 ,/ALL_EV $
;  , font=textFont)
;
;
; base3of3 = Widget_Base(baseFor3, UNAME='WID_BASE_4', $
;  XOFFSET=0 ,YOFFSET=0, $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
;
; WID_LABEL_2 = Widget_Label(base3of3, UNAME='WID_LABEL_2', $
;  XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=smallTextX ,SCR_YSIZE=smallTextY ,/ALIGN_RIGHT, $
;  VALUE='z-asl:', font=labelFont)
;
; WID_TEXT_1 = Widget_Text(base3of3, UNAME='GEOZPOS' ,XOFFSET=29, $
;  YOFFSET=0, SCR_XSIZE=smallTextX-3 ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
;  , font=textFont)
;
;;;
;
; baseFor3 = Widget_Base(modelInfoBase, UNAME='WID_BASE_2', $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=1, /ROW)
;
; base1of3 = Widget_Base(baseFor3, UNAME='WID_BASE_3' ,XOFFSET=7, $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
;
;
; WID_LABEL_1 = Widget_Label(base1of3, UNAME='', $
;  SCR_XSIZE=smallTextX+5 ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;  VALUE='grid x:', font=labelFont)
;
;
; WID_TEXT_0 = Widget_Text(base1of3, UNAME='GRIDXPOS' ,XOFFSET=29, $
;  SCR_XSIZE=smallTextX-8 ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
;  , font=textFont)
;
; base2of3 = Widget_Base(baseFor3, UNAME='WID_BASE_3' ,XOFFSET=7, $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
;
;
; WID_LABEL_1 = Widget_Label(base2of3, UNAME='WID_LABEL_1', $
;  SCR_XSIZE=smallTextX+5 ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;  VALUE='grid y:', font=labelFont)
;
;
; WID_TEXT_0 = Widget_Text(base2of3, UNAME='GRIDYPOS' ,XOFFSET=29, $
;  SCR_XSIZE=smallTextX-8 ,SCR_YSIZE=smallTextY  ,SENSITIVE=1 ,/ALL_EV $
;  , font=textFont)
;
;
; base3of3 = Widget_Base(baseFor3, UNAME='WID_BASE_4', $
;  XOFFSET=0 ,YOFFSET=0, $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
;
;
; WID_LABEL_2 = Widget_Label(base3of3, UNAME='WID_LABEL_2', $
;  XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=smallTextX+5 ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;  VALUE='grid z:', font=labelFont)
;
;
; WID_TEXT_1 = Widget_Text(base3of3, UNAME='GRIDZPOS' ,XOFFSET=29, $
;  YOFFSET=0, SCR_XSIZE=smallTextX-8 ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
;  , font=textFont)
;
; baseFor1 = Widget_Base(modelInfoBase, UNAME='WID_BASE_2', $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=1, /ROW)
;
;
; base1Of1 = Widget_Base(baseFor1, UNAME='WID_BASE_3' ,XOFFSET=7, $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
;
;
; WID_LABEL_1 = Widget_Label(base1Of1, UNAME='WID_LABEL_1', $
;  SCR_XSIZE=veryBigTextX ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;  VALUE='value:', font=labelFont)
;
;
; WID_TEXT_0 = Widget_Text(base1Of1, UNAME='GRIDVALUE' ,XOFFSET=29, $
;  SCR_XSIZE=veryBigTextX ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
;  , font=textFont)
;
; WID_LABEL_1 = Widget_Label(base1Of1, UNAME='GRIDMU', $
;  SCR_XSIZE=veryBigTextX ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;  VALUE='measure unit', font=labelFont)
;
;
; titleBase = Widget_Base(observedInfoBase, UNAME='WID_BASE_1' , $
;  XOFFSET=0, YOFFSET=0, /ALIGN_CENTER, $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
;
; captionTitle = Widget_Label(titleBase, UNAME='WID_LABEL_0', $
;  SCR_XSIZE=labelX ,SCR_YSIZE=labelY ,/ALIGN_CENTER, $
;  VALUE='Observation Info', FONT=titleFont)
;
; baseFor1 = Widget_Base(observedInfoBase, UNAME='WID_BASE_2', $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
;
; base1Of1 = Widget_Base(baseFor1, UNAME='WID_BASE_3' ,XOFFSET=7, $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
;
;
; WID_LABEL_1 = Widget_Label(base1Of1, UNAME='WID_LABEL_1', $
;  SCR_XSIZE=bigTextX ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;  VALUE='name:', font=labelFont)
;
;
; WID_TEXT_0 = Widget_Text(base1Of1, UNAME='OBSNAME' ,XOFFSET=29, $
;  SCR_XSIZE=veryBigTextX ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
;  , font=textFont)
;
; baseFor2 = Widget_Base(observedInfoBase, UNAME='WID_BASE_2', $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=1, /ROW)
;
;
; base1of2 = Widget_Base(baseFor2, UNAME='WID_BASE_3' ,XOFFSET=7, $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
;
;
; WID_LABEL_1 = Widget_Label(base1of2, UNAME='WID_LABEL_1', $
;  SCR_XSIZE=bigTextX ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;  VALUE='type:', font=labelFont)
;
;
; WID_TEXT_0 = Widget_Text(base1of2, UNAME='OBSTYPE' ,XOFFSET=29, $
;  SCR_XSIZE=bigTextX ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
;  , font=textFont)
;
;
; base2of2 = Widget_Base(baseFor2, UNAME='WID_BASE_4', $
;  XOFFSET=0 ,YOFFSET=0, $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
;
; WID_LABEL_2 = Widget_Label(base2of2, UNAME='WID_LABEL_2', $
;  XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=bigTextX ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;  VALUE='category:', font=labelFont)
;
; WID_TEXT_1 = Widget_Text(base2of2, UNAME='OBSCATEGORY' ,XOFFSET=29, $
;  YOFFSET=0, SCR_XSIZE=bigTextX ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
;  , font=textFont)
;
; baseFor2 = Widget_Base(observedInfoBase, UNAME='WID_BASE_2', $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=1, /ROW)
;
;
; base1of2 = Widget_Base(baseFor2, UNAME='WID_BASE_3' ,XOFFSET=7, $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
;
;
; WID_LABEL_1 = Widget_Label(base1of2, UNAME='WID_LABEL_1', $
;  SCR_XSIZE=bigTextX ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;  VALUE='value:', font=labelFont)
;
;
; WID_TEXT_0 = Widget_Text(base1of2, UNAME='OBSVALUE' ,XOFFSET=29, $
;  SCR_XSIZE=bigTextX ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
;  , font=textFont)
;
;
;; base2of2 = Widget_Base(baseFor2, UNAME='WID_BASE_3' ,XOFFSET=7, $
;;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
;;
;; WID_LABEL_2 = Widget_Label(base2of2, UNAME='WID_LABEL_2', $
;;  XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=bigTextX ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;;  VALUE='height:', font=labelFont)
;;
;;
;; WID_TEXT_1 = Widget_Text(base2of2, UNAME='OBSHEIGHT' ,XOFFSET=29, $
;;  YOFFSET=0, SCR_XSIZE=bigTextX ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
;;  , font=textFont)
;;
;; base2of2 = Widget_Base(baseFor2, UNAME='WID_BASE_4', $
;;  XOFFSET=0 ,YOFFSET=0, $
;;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
;
;; WID_LABEL_2 = Widget_Label(base2of2, UNAME='WID_LABEL_2', $
;;  XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=bigTextX ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;;  VALUE='validity:', font=labelFont)
;;
;;
;; WID_TEXT_1 = Widget_Text(base2of2, UNAME='OBSVALIDITY' ,XOFFSET=29, $
;;  YOFFSET=0, SCR_XSIZE=bigTextX ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
;;  , font=textFont)
;;
;; baseFor3 = Widget_Base(observedInfoBase, UNAME='WID_BASE_2', $
;;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=1, /ROW)
;;
;; base1of3 = Widget_Base(baseFor3, UNAME='WID_BASE_3' ,XOFFSET=7, $
;;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
;;
;;
;; WID_LABEL_1 = Widget_Label(base1of3, UNAME='', $
;;  SCR_XSIZE=smallTextX+5 ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;;  VALUE='obs x:', font=labelFont)
;;
;;
;; WID_TEXT_0 = Widget_Text(base1of3, UNAME='OBSX' ,XOFFSET=29, $
;;  SCR_XSIZE=smallTextX-5 ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
;;  , font=textFont)
;;
;; base2of3 = Widget_Base(baseFor3, UNAME='WID_BASE_3' ,XOFFSET=7, $
;;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
;;
;;
;; WID_LABEL_1 = Widget_Label(base2of3, UNAME='WID_LABEL_1', $
;;  SCR_XSIZE=smallTextX+5 ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;;  VALUE='obs y:', font=labelFont)
;
;
;; WID_TEXT_0 = Widget_Text(base2of3, UNAME='OBSY' ,XOFFSET=29, $
;;  SCR_XSIZE=smallTextX-5 ,SCR_YSIZE=smallTextY  ,SENSITIVE=1 ,/ALL_EV $
;;  , font=textFont)
;
;
;; base3of3 = Widget_Base(baseFor3, UNAME='WID_BASE_4', $
;;  XOFFSET=0 ,YOFFSET=0, $
;;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
;;
;;
;;
;; WID_LABEL_2 = Widget_Label(base3of3, UNAME='WID_LABEL_2', $
;;  XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=smallTextX+5 ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;;  VALUE='obs height:', font=labelFont)
;;
;;
;; WID_TEXT_1 = Widget_Text(base3of3, UNAME='OBSHEIGHT' ,XOFFSET=29, $
;;  YOFFSET=0, SCR_XSIZE=smallTextX-5 ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
;;  , font=textFont)
;
; baseFor3 = Widget_Base(observedInfoBase, UNAME='WID_BASE_2', $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=1, /ROW)
;
; base1of3 = Widget_Base(baseFor3, UNAME='WID_BASE_3' ,XOFFSET=7, $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
;
;
; WID_LABEL_1 = Widget_Label(base1of3, UNAME='', $
;  SCR_XSIZE=smallTextX+5 ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;  VALUE='obs x:', font=labelFont)
;
;
; WID_TEXT_0 = Widget_Text(base1of3, UNAME='OBSXPOS' ,XOFFSET=29, $
;  SCR_XSIZE=smallTextX-8 ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
;  , font=textFont)
;
; base2of3 = Widget_Base(baseFor3, UNAME='WID_BASE_3' ,XOFFSET=7, $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
;
;
; WID_LABEL_1 = Widget_Label(base2of3, UNAME='WID_LABEL_1', $
;  SCR_XSIZE=smallTextX+5 ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;  VALUE='obs y:', font=labelFont)
;
;
; WID_TEXT_0 = Widget_Text(base2of3, UNAME='OBSYPOS' ,XOFFSET=29, $
;  SCR_XSIZE=smallTextX-8 ,SCR_YSIZE=smallTextY  ,SENSITIVE=1 ,/ALL_EV $
;  , font=textFont)
;
;
; base3of3 = Widget_Base(baseFor3, UNAME='WID_BASE_4', $
;  XOFFSET=0 ,YOFFSET=0, $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
;
;
; WID_LABEL_2 = Widget_Label(base3of3, UNAME='WID_LABEL_2', $
;  XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=smallTextX+5 ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;  VALUE='obs z:', font=labelFont)
;
;
; WID_TEXT_1 = Widget_Text(base3of3, UNAME='OBSZPOS' ,XOFFSET=29, $
;  YOFFSET=0, SCR_XSIZE=smallTextX-8 ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
;  , font=textFont)
;
; titleBase = Widget_Base(sliceInfoBase, UNAME='WID_BASE_1' , $
;  XOFFSET=0, YOFFSET=0, /ALIGN_CENTER, $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
;
; captionTitle = Widget_Label(titleBase, UNAME='WID_LABEL_0', $
;  SCR_XSIZE=labelX ,SCR_YSIZE=labelY ,/ALIGN_CENTER, $
;  VALUE='Slice Selection', FONT=titleFont)
;
; sliderBase = Widget_Base(sliceInfoBase, UNAME='WID_BASE_1' , $
;  XOFFSET=0, YOFFSET=0, /ALIGN_CENTER, $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /COLUMN)
;
; infoTimeBase=widget_base(sliderBase, UNAME='WID_BASE_1' , $
;  XOFFSET=0, YOFFSET=0, /ALIGN_CENTER, $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
;
; sliderX=100
; sliderY=50
;
; slideTime = widget_slider(infoTimeBase,UNAME='SLIDERTIME',minimum=0,maximum=10,title='Time',value=0, /ALIGN_CENTER, $
;  scr_xsize=sliderX-25, scr_ySize=sliderY, event_pro=proPrefix+'sliderSliceAction')
; labelTime=Widget_Label(infoTimeBase, UNAME='TSLICE', $
;  SCR_XSIZE=sliderX+25 ,SCR_YSIZE=20 ,/ALIGN_CENTER, $
;  VALUE='T', font=labelFont)
;
; threeDBase=widget_base(sliderBase, /ROW, xpad=0, ypad=0, space=0, xoffset=0, yoffset=0, /ROW)
; xyBase=widget_base(threeDBase, /COLUMN, xpad=0, ypad=0, space=0, xoffset=0, yoffset=0)
; infoXBase=widget_base(xyBase, /ROW, xpad=0, ypad=0, space=0, xoffset=0, yoffset=0, /ALIGN_CENTER)
; slideX = widget_slider(infoXBase,UNAME='SLIDERX',minimum=0,maximum=10,title='X slice pos',value=0, $
;  scr_xsize=sliderX-25, scr_ySize=sliderY, event_pro=proPrefix+'sliderSliceAction')
; labelX=Widget_Label(infoXBase, UNAME='XSLICE', $
;  SCR_XSIZE=sliderX ,SCR_YSIZE=20 ,/ALIGN_CENTER, $
;  VALUE='X', font=labelFont)
; infoYBase=widget_base(xyBase, /ROW, xpad=0, ypad=0, space=0, xoffset=0, yoffset=0, /ALIGN_CENTER)
; slideY = widget_slider(infoYBase,UNAME='SLIDERY',minimum=0,maximum=10,title='Y slice pos',value=0, $
;  scr_xsize=sliderX-25, scr_ySize=sliderY, event_pro=proPrefix+'sliderSliceAction')
; labelY=Widget_Label(infoYBase, UNAME='YSLICE', $
;  SCR_XSIZE=sliderX ,SCR_YSIZE=20 ,/ALIGN_CENTER, $
;  VALUE='Y', font=labelFont)
; infoZBase=widget_base(threeDBase, /COLUMN, xpad=0, ypad=0, space=0, xoffset=0, yoffset=0, /ALIGN_RIGHT)
; slideZ = widget_slider(infoZBase,UNAME='SLIDERZ',minimum=0,maximum=10,title='Z',value=0, $
;  scr_xsize=sliderY, scr_ySize=sliderX-12, event_pro=proPrefix+'sliderSliceAction', /VERTICAL, /ALIGN_CENTER)
;; labelZ=Widget_Label(infoZBase, UNAME='ZSLICE', $
;;  SCR_XSIZE=sliderX ,SCR_YSIZE=20 ,/ALIGN_CENTER, $
;;  VALUE='Z', font=labelFont)
;
; titleBase = Widget_Base(statsBase, UNAME='WID_BASE_1' , $
;  XOFFSET=0, YOFFSET=0, /ALIGN_CENTER, $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /COLUMN, SCR_YSIZE=30)
;
; captionTitle = Widget_Label(titleBase, UNAME='WID_LABEL_0', $
;  SCR_XSIZE=labelX ,SCR_YSIZE=labelY ,/ALIGN_CENTER, $
;  VALUE='Stats', FONT=titleFont)
;
;; baseFor2 = Widget_Base(statsBase, UNAME='WID_BASE_2', $
;;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=1, /ROW, SCR_YSIZE=30)
;;
;; WID_LABEL_1 = Widget_Label(baseFor2, UNAME='WID_LABEL_1', $
;;  SCR_XSIZE=bigTextX*2, SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;;  VALUE='min (slice):', font=labelFont)
;;
;;
;; WID_TEXT_0 = Widget_Text(baseFor2, UNAME='MINSLICETEXT' ,XOFFSET=0, $
;;  SCR_XSIZE=bigTextX ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
;;  , font=textFont)
;;
;;
;; baseFor2 = Widget_Base(statsBase, UNAME='WID_BASE_2', $
;;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=1, /ROW)
;;
;;
;; WID_LABEL_2 = Widget_Label(baseFor2, UNAME='WID_LABEL_2', $
;;  XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=bigTextX*2 ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;;  VALUE='min overall:', font=labelFont)
;;
;;
;; WID_TEXT_1 = Widget_Text(baseFor2, UNAME='MINOVERALLTEXT' ,XOFFSET=29, $
;;  YOFFSET=0, SCR_XSIZE=bigTextX ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
;;  , font=textFont)
;
;; baseFor2 = Widget_Base(statsBase, UNAME='WID_BASE_2', $
;;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=1, /ROW)
;;
;;
;; WID_LABEL_1 = Widget_Label(baseFor2, UNAME='WID_LABEL_1', $
;;  SCR_XSIZE=bigTextX*2 ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;;  VALUE='mean (slice):', font=labelFont)
;;
;;
;; WID_TEXT_0 = Widget_Text(baseFor2, UNAME='MEANSLICETEXT' ,XOFFSET=0, $
;;  SCR_XSIZE=bigTextX ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
;;  , font=textFont)
;
;
;; baseFor2 = Widget_Base(statsBase, UNAME='WID_BASE_2', $
;;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=1, /ROW)
;;
;; WID_LABEL_2 = Widget_Label(baseFor2, UNAME='WID_LABEL_2', $
;;  XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=bigTextX*2 ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;;  VALUE='mean overall:', font=labelFont)
;;
;; WID_TEXT_1 = Widget_Text(baseFor2, UNAME='MEANOVERALLTEXT' ,XOFFSET=29, $
;;  YOFFSET=0, SCR_XSIZE=bigTextX ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
;;  , font=textFont)
;
; baseFor5 = Widget_Base(statsBase, UNAME='WID_BASE_2', $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=1, /ROW)
;
; WID_LABEL_1 = Widget_Label(baseFor5, UNAME='WID_LABEL_1', $
;  SCR_XSIZE=veryBigTextX+18 ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;  VALUE='This slice', font=labelFont)
;
; WID_LABEL_1 = Widget_Label(baseFor5, UNAME='WID_LABEL_1', $
;  SCR_XSIZE=smallTextX-16 ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;  VALUE='min:', font=labelFont)
;
; WID_TEXT_0 = Widget_Text(baseFor5, UNAME='MAXSLICETEXT' ,XOFFSET=0, $
;  SCR_XSIZE=smallTextX+7 ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
;  , font=textFont)
;
; WID_LABEL_1 = Widget_Label(baseFor5, UNAME='WID_LABEL_1', $
;  SCR_XSIZE=smallTextX-16 ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;  VALUE='max:', font=labelFont)
;
; WID_TEXT_0 = Widget_Text(baseFor5, UNAME='MINSLICETEXT' ,XOFFSET=0, $
;  SCR_XSIZE=smallTextX+7 ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
;  , font=textFont)
;
;
; baseFor5 = Widget_Base(statsBase, UNAME='WID_BASE_2', $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=1, /ROW)
;
; WID_LABEL_1 = Widget_Label(baseFor5, UNAME='WID_LABEL_1', $
;  SCR_XSIZE=veryBigTextX+18 ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;  VALUE='All time slice', font=labelFont)
;
; WID_LABEL_1 = Widget_Label(baseFor5, UNAME='WID_LABEL_1', $
;  SCR_XSIZE=smallTextX-16 ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;  VALUE='min:', font=labelFont)
;
; WID_TEXT_0 = Widget_Text(baseFor5, UNAME='MINSLICEALLTIMETEXT' ,XOFFSET=0, $
;  SCR_XSIZE=smallTextX+7 ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
;  , font=textFont)
;
; WID_LABEL_1 = Widget_Label(baseFor5, UNAME='WID_LABEL_1', $
;  SCR_XSIZE=smallTextX-16 ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;  VALUE='max:', font=labelFont)
;
; WID_TEXT_0 = Widget_Text(baseFor5, UNAME='MAXSLICEALLTIMETEXT' ,XOFFSET=0, $
;  SCR_XSIZE=smallTextX+7 ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
;  , font=textFont)
;
; baseFor5 = Widget_Base(statsBase, UNAME='WID_BASE_2', $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=1, /ROW)
;
; WID_LABEL_1 = Widget_Label(baseFor5, UNAME='WID_LABEL_1', $
;  SCR_XSIZE=veryBigTextX+18 ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;  VALUE='All data', font=labelFont)
;
; WID_LABEL_1 = Widget_Label(baseFor5, UNAME='WID_LABEL_1', $
;  SCR_XSIZE=smallTextX-16 ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;  VALUE='min:', font=labelFont)
;
; WID_TEXT_0 = Widget_Text(baseFor5, UNAME='MINDATATEXT' ,XOFFSET=0, $
;  SCR_XSIZE=smallTextX+7 ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
;  , font=textFont)
;
; WID_LABEL_1 = Widget_Label(baseFor5, UNAME='WID_LABEL_1', $
;  SCR_XSIZE=smallTextX-16 ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;  VALUE='max:', font=labelFont)
;
; WID_TEXT_0 = Widget_Text(baseFor5, UNAME='MAXDATATEXT' ,XOFFSET=0, $
;  SCR_XSIZE=smallTextX+7 ,SCR_YSIZE=smallTextY ,SENSITIVE=1 ,/ALL_EV $
;  , font=textFont)
;
; aboutButton=widget_button(helpBase, value=getBmpLogoFileName(), event_pro='aboutMenuSelection', $
;  SCR_XSIZE=50, SCR_YSIZE=34, /BITMAP)
;
;END
;
;PRO buildMainDrawSection, base, dims, proPrefix
;
; titleFont=getTitleTextFont()
; labelFont=getStandardLabelFont()
; textFont=getStandardTextFont()
; labelX=dims[0]-20
; labelY=20
; smalltextX=dims[0]/6
; smalltextY=20
; bigTextX=dims[0]/4
; bigTextY=20
; veryBigTextX=dims[0]/3
; veryBigTextY=20
;
; mainDrawBase = Widget_Base(base, UNAME='mainDrawBase', $
;  XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=dims[0] ,SCR_YSIZE=dims[1], $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=2, /COLUMN, /ALIGN_CENTER)
;
; titleLabel = Widget_Label(mainDrawBase, UNAME='STATUSBARMAP', $
;  XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=dims[0] ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;  VALUE='---Information about plot data---', /SUNKEN_FRAME, font=labelFont)
;
; mainDraw = WIDGET_DRAW(mainDrawBase,RETAIN=2,SCR_XSIZE=dims[0],SCR_YSIZE=dims[1]-smallTextY, $
;   /MOTION_EVENTS, /BUTTON_EVENTS, event_pro=proPrefix+'mapDrawMouse', UNAME='MAINDRAW')
;
;END
;
;PRO buildGraphSectionSection, base, dims, proPrefix
;
; titleFont=getTitleTextFont()
; labelFont=getStandardLabelFont()
; textFont=getStandardTextFont()
; labelX=dims[0]-20
; labelY=20
; smalltextX=dims[0]/6
; smalltextY=20
; bigTextX=dims[0]/4
; bigTextY=20
; veryBigTextX=dims[0]/3
; veryBigTextY=20
;
; graphDrawBase = Widget_Base(base, UNAME='mainDrawBase', $
;  XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=dims[0] ,SCR_YSIZE=dims[1], $
;  TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=2, /COLUMN, /ALIGN_CENTER)
;
; titleLabel = Widget_Label(graphDrawBase, UNAME='STATUSBARGRAPH', $
;  XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=dims[0] ,SCR_YSIZE=smallTextY ,/ALIGN_LEFT, $
;  VALUE='---Information about plot graph data---', /SUNKEN_FRAME, font=labelFont)
;
; graphDraw = WIDGET_DRAW(graphDrawBase,RETAIN=2,SCR_XSIZE=dims[0],SCR_YSIZE=dims[1]-smallTextY, $
;   UNAME='GRAPHDRAW')
;
;END
;
;PRO buildObservedMenu, menuTop, obsList, proPrefix, OUTfirstObsWId, ALL=ALL
;
; prevType=''
; prevCategory=''
; lastTypeRoot=menuTop
; for i=0, n_elements(obsList)-1 do begin
;	thisType=obsList[i].type
;	thisCategory=obsList[i].category
;	thisName=obsList[i].name
;	thisId=strmid(obsList[i].id, 2)
; 	if thisType ne prevType then begin
; 		; create new "main type" root with name=type
; 		if lastTypeRoot eq menuTop then begin
; 			noneButton=widget_button(lastTypeRoot, value='None', uname='OBSNONE*', /CHECKED_MENU, event_pro=proPrefix+'observedMenuSelection')
; 			mainAllButton=widget_button(lastTypeRoot, value='All', uname='OBSALL*', /CHECKED_MENU, event_pro=proPrefix+'observedMenuSelection')
; 		endif
;		lastTypeRoot=widget_button(menuTop, value=thisType, UNAME=thisType, /MENU)
; 		allButton=widget_button(lastTypeRoot, value='All', uname='OBSTYPE*'+thisType, /CHECKED_MENU, event_pro=proPrefix+'observedMenuSelection')
; 		prevType=thisType
; 	endif
; 	if thisCategory ne prevCategory then begin
; 		; create new "secondary category" root with name=category
; 		lastCategoryRoot=widget_button(lastTypeRoot, value=thisCategory, UNAME=thisCategory, /MENU)
; 		allButton=widget_button(lastCategoryRoot, value='All', uname='OBSCAT*'+thisCategory+'*'+thisType, /CHECKED_MENU, event_pro=proPrefix+'observedMenuSelection')
; 		prevCategory=thisCategory
; 	endif
;	thisButton=widget_button(lastCategoryRoot, value=thisName, uname='OBS*'+thisId+'*'+thisCategory+'*'+thisType, /CHECKED_MENU, event_pro=proPrefix+'observedMenuSelection')
; endfor
;
; if keyword_set(ALL) then OUTfirstObsWId=mainAllButton else OUTfirstObsWId=noneButton
;
;END
;
;PRO buildParameterMenu, menuTop, meteoList, chemicalList, aerosolList, lUseList, proPrefix, OUTfirstParWId
;
; ;parameter=loadModelParameter()
; parMeteoMenu=widget_button(menuTop, value='Meteo', UNAME='METEOMENU', /MENU)
; parChemMenu=widget_button(menuTop, value='Chemical', UNAME='CHEMICALMENU', /MENU)
; parAeroMenu=widget_button(menuTop, value='Aerosol', UNAME='AEROSOLMENU', /MENU)
; parLUseMenu=widget_button(menuTop, value='LandUse', UNAME='LANDUSEMENU', /MENU)
;
; for i=0, n_elements(meteoList)-1 do meteoButton=widget_button(parMeteoMenu, value=meteoList[i], uname=strtrim(i,1)+'*'+meteoList[i], event_pro=proPrefix+'meteoMenuSelection', /CHECKED_MENU)
; for i=0, n_elements(chemicalList)-1 do chemButton=widget_button(parChemMenu, value=chemicalList[i], uname=strtrim(i,1)+'*'+chemicalList[i], event_pro=proPrefix+'chemicalMenuSelection', /CHECKED_MENU)
; for i=0, n_elements(aerosolList)-1 do aeroButton=widget_button(parAeroMenu, value=aerosolList[i], uname=strtrim(i,1)+'*'+aerosolList[i], event_pro=proPrefix+'aerosolMenuSelection', /CHECKED_MENU)
; for i=0, n_elements(lUseList)-1 do lUseButton=widget_button(parLUseMenu, value=lUseList[i], uname=strtrim(i,1)+'*'+lUseList[i], event_pro=proPrefix+'landUseMenuSelection', /CHECKED_MENU)
;
; if size(lUseList, /N_DIMENSIONS) eq 0 then begin
;	widget_control, parLUseMenu, sensitive=0
;	widget_control, parLUseMenu, /DESTROY
; endif else begin
;	OUTfirstParWId=widget_info(menuTop, find='0*'+lUseList[0])
; endelse
;
; if size(aerosolList, /N_DIMENSIONS) eq 0 then begin
;	widget_control, parAeroMenu, sensitive=0
;	widget_control, parAeroMenu, /DESTROY
; endif else begin
;	OUTfirstParWId=widget_info(menuTop, find='0*'+aerosolList[0])
; endelse
;
; if size(chemicalList, /N_DIMENSIONS) eq 0 then begin
;	widget_control, parChemMenu, sensitive=0
;	widget_control, parChemMenu, /DESTROY
; endif else begin
;	OUTfirstParWId=widget_info(menuTop, find='0*'+chemicalList[0])
; endelse
;
; if size(meteoList, /N_DIMENSIONS) eq 0 then begin
;	widget_control, parMeteoMenu, sensitive=0
;	widget_control, parMeteoMenu, /DESTROY
; endif else begin
;	OUTfirstParWId=widget_info(menuTop, find='0*'+meteoList[0])
; endelse
;
;END

FUNCTION FMModeSelectionGUI::init, info, mgr, fonts=fonts

 if not self -> FMInfoSelectionGUI :: init(info, mgr, fonts=fonts) then return , 0
 return , 1

END

PRO FMModeSelectionGUI::cleanUp

 self -> FMInfoSelectionGUI::cleanUp

END

;****************************************************************************************

PRO FMModeSelectionGUI__Define

Struct = { FMModeSelectionGUI , $
		descriptionLabel: 0l, $
		modeComboBox: 0l, $
		Inherits FMInfoSelectionGUI $
		 }

END

;****************************************************************************************

