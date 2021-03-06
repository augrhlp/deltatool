;PRO fairmode_destroyBenchMarkCreationWindow, ev
;
;  widget_control, ev, get_uvalue=view
;
;  ;print, 'fairmode_destroyBenchMarkCreationWindow'
;  view->exitRequest
;  ;view->setObsGroupStatIndex, index
;
;END

PRO fairmode_doNothing, ev

  print, 'Nothing to do'
  
END

PRO fairmode_fontgui_modifyBtt, ev

  widget_control, ev.top, get_uvalue=view
  view->modifyBtt
  
END

PRO fairmode_fontgui_userCharSizeList, ev

  widget_control, ev.top, get_uvalue=view
  widget_control, ev.id, get_uvalue=index
  userName=widget_info(ev.id, /UNAME)
  view->userCharSizeSelection, ev.str, ev.index
  
END

PRO fairmode_fontgui_userCharThickList, ev

  widget_control, ev.top, get_uvalue=view
  widget_control, ev.id, get_uvalue=index
  userName=widget_info(ev.id, /UNAME)
  view->userCharThickSelection, ev.str, ev.index
  
END

PRO fairmode_fontgui_modifierText, ev

  widget_control, ev.top, get_uvalue=view
  view->userModifierText
  
END

PRO fairmode_fontgui_userFontTypeList, ev

  widget_control, ev.top, get_uvalue=view
  widget_control, ev.id, get_uvalue=index
  userName=widget_info(ev.id, /UNAME)
  view->userFontTypeSelection, ev.str, ev.index
  
END

PRO fairmode_fontgui_fontModifierComboBoxSelection, ev

  widget_control, ev.top, get_uvalue=view
  widget_control, ev.id, get_uvalue=index
  userName=widget_info(ev.id, /UNAME)
  
  view->updateUserModifierSelection, ev.str, ev.index
  
END

PRO fairmode_fontgui_userFontNameList, ev

  widget_control, ev.top, get_uvalue=view
  widget_control, ev.id, get_uvalue=index
  view->userFontNameSelection, ev.str, ev.index
    
END

PRO fairmode_fontgui_userPredefinedList, ev

  widget_control, ev.top, get_uvalue=view
  view->userPredefinedFontSelection, ev.str, ev.index
  
END

PRO fairmode_fontgui_userSizeList, ev

  widget_control, ev.top, get_uvalue=view
  view->userTTModifierListSelection, ev.str, ev.index
  
END

PRO fairmode_fontgui_userWeightList, ev

  widget_control, ev.top, get_uvalue=view
  view->userTTModifierListSelection, ev.str, ev.index
  
END

PRO fairmode_fontgui_userQualityList, ev

  widget_control, ev.top, get_uvalue=view
  view->userTTModifierListSelection, ev.str, ev.index
  
END

PRO fairmode_fontgui_userPitchList, ev

  widget_control, ev.top, get_uvalue=view
  view->userTTModifierListSelection, ev.str, ev.index
  
END

PRO fairmode_fontgui_userAngleList, ev

  widget_control, ev.top, get_uvalue=view
  view->userTTModifierListSelection, ev.str, ev.index
  
END

PRO fairmode_fontgui_userStrikeOutList, ev

  widget_control, ev.top, get_uvalue=view
  view->userTTModifierListSelection, ev.str, ev.index
  
END

PRO fairmode_fontgui_userUnderlineList, ev

  widget_control, ev.top, get_uvalue=view
  view->userTTModifierListSelection, ev.str, ev.index
  
END

PRO fairmode_benchmarkOKRequest, ev

  widget_control, ev.top, get_uvalue=view
  widget_control, ev.id, get_uvalue=index
  
  ;print, 'fairmode_benchmarkOKRequest'
  view->okRequest
;view->setObsGroupStatIndex, index
  
END

PRO fairmode_benchmarkTreeSelection, ev

  ;benchMarkTreeElemInfo=widget_info(ev.id, /UNAME)
  ;widget_control, ev.id, get_uvalue=benchMarkTreeElemInfo
  widget_control, ev.id, get_uvalue=treeElemCode
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_obsModMainBtt'
  ;view->userBenchMarkTreeUpdate, benchMarkTreeElemInfo
  view->userBenchMarkTreeUpdate, treeElemCode
  
END

PRO fairmode_groupStatSelection, ev

  widget_control, ev.top, get_uvalue=view
  widget_control, ev.id, get_uvalue=index
  
  view->setObsGroupStatIndex, index
  
END

PRO fairmode_groupNameOKRequest, ev

  ;help, ev, /str
  widget_control, ev.top, get_uvalue=view
  widget_control, ev.id, get_uvalue=widgetText
  widget_control, widgetText, get_value=groupName
  widget_control, widgetText, get_uvalue=selectedCodes
  
  ;print, 'fairmode_groupNameOKRequest'
  view->groupNameOKRequest, ev.top, groupName, selectedCodes
  
END

PRO fairmode_editBatch, ev

  ;help, ev, /str
  widget_control, ev.id, get_uvalue=type
  widgetId=ev.id
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_graphicTypeRadioButton'
  view->userEditBatch
  
END

PRO fairmode_showRequestBatch, ev

  ;help, ev, /str
  widget_control, ev.id, get_uvalue=type
  widgetId=ev.id
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_graphicTypeRadioButton'
  view->userShowRequestBatch
  
END

PRO fairmode_printOrientRadioButton, ev

  widget_control, ev.id, get_uvalue=type
  widgetId=ev.id
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_pageModeRadioButton'
  view->userUpdatePrintOrientCode, type, widgetId
  
END

PRO fairmode_benchMarkSaveModeRadioButton, ev

  widget_control, ev.id, get_uvalue=type
  widgetId=ev.id
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_pageModeRadioButton'
  view->userUpdateBenchMarkSaveModeCode, type, widgetId
  
END

PRO fairmode_pageModeRadioButton, ev

  ;help, ev, /str
  widget_control, ev.id, get_uvalue=type
  widgetId=ev.id
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_pageModeRadioButton'
  view->userUpdatePageModeCode, type, widgetId
  
END

PRO fairmode_graphicTypeRadioButton, ev

  ;help, ev, /str
  widget_control, ev.id, get_uvalue=type
  widgetId=ev.id
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_graphicTypeRadioButton'
  view->userUpdateGraphicTypeCode, type, widgetId
  
END

;PRO fairmode_iterateOptionRadioButton, ev
;
;  ;help, ev, /str
;  widget_control, ev.id, get_uvalue=type
;  widgetId=ev.id
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;print, 'fairmode_iterateOptionRadioButton'
;  view->userUpdateIterateOptionCode, type, widgetId
;
;END

PRO fairmode_splitStyleRadioButton, ev

  ;help, ev, /str
  widget_control, ev.id, get_uvalue=type
  widgetId=ev.id
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_splitStyleRadioButton'
  view->userUpdateSplitStyleCode, type, widgetId
  
END

PRO fairmode_splitDrawMouse, ev

  ;  print, "clicking...", ev.press
  if (ev.press eq 1) then begin
    ;    print, "left split click"
    xCoord = ev.x
    yCoord = ev.y
    if size(ev, /TYPE) eq 8 then ev=ev.top
    widget_control, ev, get_uvalue=view
    view->splitMouseClick, xCoord, yCoord
  endif
;print, 'fairmode_mapDrawMouse'
  
END

PRO fairmode_benchmarkOperationButton, ev

  ;help, ev, /str
  select=ev.select
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  print, 'fairmode_benchmarkOperationButton'
;view->userAllStationsButton, select
  
END

PRO fairmode_checkDataIntegrityMenuSelection, ev

  widget_control, ev.top, get_uvalue=view
  view->checkDataIntegrity
  
END

;KeesC 08FEB2016
PRO fairmode_editDumpFile, ev
  fullString='Notepad'
  ERROR=0
  catch, error_status

  if error_status NE 0 THEN BEGIN
    ERROR=1
    catch, /CANCEL
    errMsg=dialog_message([['Please check path to Notepad location'], fullString, [' Check existence or read permission'], [' and modify -HOME/resource/init.ini- accordingly.']], /ERROR)
    return
  endif
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  print, 'fairmode_editDumpFile'
  mgr=view->getMgr()
  command=Mgr->getNotepadLocation()
  fs=mgr->getFileSystemMgr()
  dumpDir=fs->getDumpDir(/with)
  fullString=dumpDir+'DumpFile.txt'
  spawn,[command,fullString],/noshell,/nowait
END

;KeesC 08FEB2016
PRO fairmode_editSummaryReport, ev
  fullString='Excel'
  ERROR=0
  catch, error_status

  if error_status NE 0 THEN BEGIN
    ERROR=1
    catch, /CANCEL
    errMsg=dialog_message([['Please check path to Notepad location'], fullString, [' Check existence or read permission'], [' and modify -HOME/resource/init.ini- accordingly.']], /ERROR)
    return
  endif
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  print, 'fairmode_editSummaryReport'
  mgr=view->getMgr()
  command=Mgr->getWorkSheetLocation()
  fs=mgr->getFileSystemMgr()
  dumpDir=fs->getDumpDir(/with)
  fullString=dumpDir+'Summary_Report.csv'
  spawn,[command,fullString],/noshell,/nowait
END

PRO fairmode_helpMenuSelection, ev

  ERROR=0
  catch, error_status
  
  if error_status NE 0 THEN BEGIN
    ERROR=1
    catch, /CANCEL
    errMsg=dialog_message([['Please check path to pdf reader location'], fullString, [' Check existence or read permission'], [' and modify -HOME/resource/init.ini- accordingly.']], /ERROR)
    return
  endif
  
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  print, 'fairmode_helpMenuSelection'
  mgr=view->getMgr()
  print, 'Doc reader:', mgr->getDocReaderLocation()
  print, 'Pdf reader:',mgr->getPdfReaderLocation()
  print, 'Browser launcher:',mgr->getBrowserLocation()
  fs=mgr->getFileSystemMgr()
  helpFile=fs->getHelpFileName(/FULLPATH)
  fullString=[mgr->getpdfReaderLocation(),helpFile]
  spawn,fullString,/noshell,/nowait
;helpfolder=fs->getHelpDir(withseparator=withseparator)+'\'
;spawn,[mgr->getpdfReaderLocation(),helpfolder+'DELTA_UserGuide_V4_0.pdf'],/noshell,/nowait
END

PRO fairmode_downloadMenuSelection, ev

  ERROR=0
  catch, error_status
  
  if error_status NE 0 THEN BEGIN
    ERROR=1
    catch, /CANCEL
    errMsg=dialog_message([['Please check path to browser location'], fullString, [' Check existence or read permission'], [' and modify -HOME/resource/init.ini- accordingly.']], /ERROR)
    return
  endif
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  print, 'fairmode_downloadMenuSelection'
  mgr=view->getMgr()
  print, 'Doc reader:', mgr->getDocReaderLocation()
  print, 'Pdf reader:',mgr->getPdfReaderLocation()
  print, 'Browser launcher:',mgr->getBrowserLocation()
  fullString=[mgr->getBrowserLocation(),'aqm.jrc.ec.europa.eu/DELTA/']
  spawn,fullString,/noshell,/nowait
END

PRO fairmode_flagAllObservationsButton, ev

  ;help, ev, /str
  select=ev.select
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_singleObsRadioButton'
  view->userAllObservationsButton, select
  
END

PRO fairmode_flagAllModelsButton, ev

  ;help, ev, /str
  select=ev.select
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_singleObsRadioButton'
  view->userAllModelsButton, select
  
END

PRO fairmode_flagAllScenariosButton, ev

  ;help, ev, /str
  select=ev.select
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_singleObsRadioButton'
  view->userAllScenariosButton, select
  
END

PRO fairmode_flagAllParametersButton, ev

  ;help, ev, /str
  select=ev.select
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_singleObsRadioButton'
  view->userAllParametersButton, select
  
END

PRO fairmode_recognizeSwitchBtt, ev

  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  view->SwitchRecognize
  
END

PRO fairmode_aboutOKBTT, ev

  ;;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  print, 'fairmode_aboutOKBTT'
  view->exitRequest
  
END

PRO fairmode_aboutSplash, ev

  ;;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_aboutSplash'
  view->showAboutSplash
  
END

PRO fairmode_disclaimer, ev

  ;;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_disclaimer'
  view->showDisclaimer
  
END

PRO fairmode_configureExecutable, ev

  ;;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_disclaimer'
  view->configureExecutable
  
END

PRO fairmode_license, ev

  ERROR=0
  catch, error_status
  
  if error_status NE 0 THEN BEGIN
    ERROR=1
    catch, /CANCEL
    errMsg=dialog_message([['Please check path to rtf reader location'], fullString, [' Check existence or read permission'], [' and modify -HOME/resource/init.ini- accordingly.']], /ERROR)
    return
  endif
  
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  print, 'fairmode_license'
  mgr=view->getMgr()
  rtfReaderFile=mgr->getDocReaderLocation()
  print, 'Doc reader:', rtfReaderFile
  ;print, 'Pdf reader:',mgr->getPdfReaderLocation()
  ;print, 'Browser launcher:',mgr->getBrowserLocation()
  fs=mgr->getFileSystemMgr()
  licenseFile=fs->getLicenseFileName(/FULLPATH)
  fullString=[rtfReaderFile,licenseFile]
  spawn,fullString,/noshell,/nowait
;helpfolder=fs->getHelpDir(withseparator=withseparator)+'\'
;spawn,[mgr->getpdfReaderLocation(),helpfolder+'DELTA_UserGuide_V4_0.pdf'],/noshell,/nowait
  
END

PRO fairmode_destroyObsTable, ev

  ;;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_destroyObsTable'
  view->unlockObsTable
  
END

PRO fairmode_userViewObsDetails, ev

  ;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_userViewObsDetails'
  view->showObsDetails
  
END

PRO fairmode_userUpdateStartDate, ev

  ;help, ev, /str
  index=ev.index
  widget_control, ev.id, get_uvalue=type
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_userUpdateStartDate'
  view->changeStartDate, type, index
  
END

PRO fairmode_userUpdateEndDate, ev

  ;help, ev, /str
  index=ev.index
  widget_control, ev.id, get_uvalue=type
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_userUpdateEndDate'
  view->changeEndDate, type, index
  
END

PRO fairmode_benchmarkBtt, ev

  benchMarkFileName=widget_info(ev.id, /UNAME)
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_obsModMainBtt'
  view->startBenchMark, benchMarkFileName
  
END

PRO fairmode_runInteractiveMenuSelection, ev

  ;testBenchMarkFileNames=widget_info(ev.id, /UNAME)
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_obsModMainBtt'
  view->dataFormatConversionUtility

END

;PRO fairmode_runBatchMenuSelection, ev
;
;  testBenchMarkFileNames=widget_info(ev.id, /UNAME)
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;print, 'fairmode_obsModMainBtt'
;  view->convertObsFromCSVtoCDF
;
;END

PRO fairmode_testQualityMenuSelection, ev

  ;testBenchMarkFileNames=widget_info(ev.id, /UNAME)
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_obsModMainBtt'
  view->testPlotQuality
  
END

PRO fairmode_fontEdit, ev

  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  view->displayFontEditGUI
  
END

PRO fairmode_allAvailableScenarioButton, ev

  ;help, ev, /str
  select=ev.select
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_obsModMainBtt'
  view->userAllAvailableScenarioSelection, select
  
END

PRO fairmode_obsModMainBtt, ev

  ;help, ev, /str
  select=ev.select
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_obsModMainBtt'
  view->userObsModSelection, select
  
END

PRO fairmode_execRequest, ev

  ;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_execRequest'
  view->execRequest
  
END

PRO fairmode_startHourSelection, ev

  ;help, ev, /str
  index=ev.index
  ;print, index
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_startHourSelection'
  view->userStartHourSelection, index
  
END

PRO fairmode_startDaySelection, ev

  ;help, ev, /str
  index=ev.index
  ;print, index
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_startDaySelection'
  view->userStartDaySelection, index
  
END

PRO fairmode_startMonthSelection, ev

  ;help, ev, /str
  index=ev.index
  ;print, index
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_startMonthSelection'
  view->userStartMonthSelection, index
  
END

PRO fairmode_startYearSelection, ev

  ;help, ev, /str
  index=ev.index
  ;print, index
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
;print, 'fairmode_startYearSelection'
  
END

PRO fairmode_endHourSelection, ev

  ;help, ev, /str
  index=ev.index
  ;print, index
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_endHourSelection'
  view->userEndHourSelection, index
  
END

PRO fairmode_endDaySelection, ev

  ;help, ev, /str
  index=ev.index
  ;print, index
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_endDaySelection'
  view->userEndDaySelection, index
  
END

PRO fairmode_endMonthSelection, ev

  ;help, ev, /str
  index=ev.index
  ;print, index
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_endMonthSelection'
  view->userEndMonthSelection, index
  
END

PRO fairmode_endYearSelection, ev

  ;help, ev, /str
  index=ev.index
  ;print, index
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
;print, 'fairmode_endYearSelection'
;view->userGroupByTimeSelection, index
  
END

PRO fairmode_groupByTimeSelection, ev

  ;help, ev, /str
  widget_control, ev.id, get_uvalue=index
  ;print, index
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_groupByTimeSelection'
  view->userGroupByTimeSelection, index
  
END

PRO fairmode_groupByStatUserSelection, ev

  ;help, ev, /str
  widget_control, ev.id, get_uvalue=index
  ;print, index
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_GroupByStatUserSelection'
  view->userGroupByStatSelection, index
  
END

PRO fairmode_periodSeasonSelection, ev

  ;help, ev, /str
  widget_control, ev.id, get_uvalue=index
  ;print, index
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_PeriodSeasonSelection'
  view->userSeasonSelection, index
  
END

PRO fairmode_periodDaySelection, ev

  ;help, ev, /str
  widget_control, ev.id, get_uvalue=index
  ;print, index
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_PeriodDaySelection'
  view->userDayPeriodSelection, index
  
END

PRO fairmode_OKRequest, ev

  ;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_OKRequest'
  view->OKRequest
  
END

PRO fairmode_useObsModButton, ev

  ;help, ev, /str
  select=ev.select
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_singleObsRadioButton'
  view->userUseObsModButton, select
  
END

PRO fairmode_singleObsRadioButton, ev

  ;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_singleObsRadioButton'
  view->singleObsRadioButton
  
END

PRO fairmode_groupObsRadioButton, ev

  ;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_groupObsRadioButton'
  view->groupObsRadioButton
  
END

PRO fairmode_addObsButton, ev

  ;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_addObsButton'
  view->addObsButton
  
END

PRO fairmode_removeObsButton, ev

  ;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_removeObsButton'
  view->removeObsButton
  
END

PRO fairmode_obsStoredListSelection, ev

  ;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_obsStoredListSelection'
  view->obsStoredListSelection
  
END

PRO fairmode_loadObsButton, ev

  ;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_loadObsButton'
  view->loadObsButton
  
END

PRO fairmode_saveObsButton, ev

  ;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_saveObsButton'
  view->saveObsButton
  
END

PRO fairmode_obsQueryListSelection, ev

  ;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_obsQueryListSelection'
  view->obsQueryListSelection
  
END

PRO fairmode_categoryValuesExclusiveSelection, ev

  ;help, ev, /str
  wid=ev.id
  widget_control, wid, get_uvalue=uvalue
  index=uvalue.thisidx & catIndex=uvalue.catIdx
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_categoryValuesExclusiveSelection'
  view->categoryValuesExclusiveSelection, wid, catIndex, index
  
END

PRO fairmode_categoryValuesListSelection, ev

  ;help, ev, /str
  wid=ev.id
  widget_control, wid, get_uvalue=uvalue
  index=ev.index & catIndex=uvalue.catIdx
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_categoryValuesListSelection'
  ;  view->categoryValuesListSelection, wid, catIndex, index-1
  view->categoryValuesExclusiveSelection, wid, catIndex, index-1
  
END

PRO fairmode_useCriteriaButton, ev

  ;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
;print, 'fairmode_useCriteriaButton'
;view->useCriteriaButton
  
END

PRO fairmode_useThresholdsButton, ev

  ;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_useThresholdsButton'
  view->useThresholdsButton
  
END

PRO fairmode_parameterListSelection, ev

  ;help, ev, /str
  indexes=widget_info(ev.id, /LIST_SELECT)
  widget_control, ev.id, get_uvalue=codes
  selCodes=codes[indexes]
  ;  print, selCodes
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_parameterListSelection'
  ;view->userParameterSelections, indexes
  view->userParameterCodeSelections, selCodes
  
END

PRO fairmode_parameterTypeListSelection, ev

  ;help, ev, /str
  indexes=widget_info(ev.id, /LIST_SELECT)
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_parameterTypeListSelection'
  view->userParameterTypeSelections, indexes
  
END

PRO fairmode_elabNameListSelection, ev

  ;help, ev, /str
  index=ev.index
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_elabNameListSelection'
  view->userElabSelection, index
  
END

PRO fairmode_elabDiagramListSelection, ev

  ;help, ev, /str
  ;wid=ev.id
  index=ev.index
  ;widget_control, wid, get_uvalue=uvalue
  ;index=uvalue.thisidx & catIndex=uvalue.catIdx
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_elabDiagramListSelection'
  view->userDiagramSelection, index
  
END

;PRO fairmode_elabAxisListSelection, ev
;
; ;help, ev, /str
; index=ev.index
; if size(ev, /TYPE) eq 8 then ev=ev.top
; widget_control, ev, get_uvalue=view
; ;print, 'fairmode_elabAxisListSelection'
; view->userAxisSelection, index
;
;END

PRO fairmode_scenarioListSelection, ev

  indexes=widget_info(ev.id, /LIST_SELECT)
  ;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_scenarioListSelection'
  view->userScenarioSelections, indexes
  
END

PRO fairmode_modelListSelection, ev

  indexes=widget_info(ev.id, /LIST_SELECT)
  ;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_modelListSelection'
  view->userModelSelections, indexes
  
END

PRO fairmode_runListSelection, ev

  index=widget_info(ev.id, /LIST_SELECT)
  ;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_runListSelection'
  view->userRunSelection, index
  
END

;Kees 06FEB2016
;pro fairMode_editDumpFile,ev
;stop
;util=obj_new('FMUtility')
;dumpDir=self.fileSysMgr->getDumpDir(/WITH)
;end

PRO fairmode_saveImage, ev

  ;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_saveImage'
  view->saveImage
  
END

PRO fairmode_saveImageBlack, ev

  ;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_saveImageBlack'
  view->saveImageBlack
  
END

PRO fairmode_saveImageWhite, ev

  ;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_saveImageWhite'
  view->saveImageWhite
  
END

PRO fairmode_saveBatch, ev

  ;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_saveBatch'
  a=view->saveBatch()
  
END

PRO fairmode_restoreBatch, ev

  ;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_restoreBatch'
  view->restoreBatch
;view->startBatchMode
  
END


PRO fairmode_dateComboSelection, ev

  ;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_dateComboSelection'
  view->dateComboSelection
  
END

PRO fairmode_modeComboSelection, ev

  index=ev.index
  ;;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_modeComboSelection'
  view->userSelection, index
  
END

PRO fairmode_modeMenuSelection, ev

  ;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_modeMenuSelection'
  view->displayModeSelectionGUI
  
END

PRO fairmode_batchComposition, ev

  ;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_benchmarkBuildMenuSelection'
  view->displayCompositeBatchGUI
  
END

PRO fairmode_displayEntity, ev

  ;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_entityMenuSelection'
  view->displayEntitySelectionGUI
  
END

PRO fairmode_saveEntity, ev

  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_entityMenuSave'
  view->saveEntity
  
END

PRO fairmode_restoreEntity, ev

  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_entityMenuLoad'
  view->restoreEntity
  
END

PRO fairmode_displayElaboration, ev

  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_elaborationMenuSelection'
  view->displayElaborationSelectionGUI
  
END

PRO fairmode_magicEnt, ev

  print, 'allCombination-Entity'
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_magicElab'
  view->buildAllAvailableCombination
  
END

PRO fairmode_magicElab, ev

  print, 'allCombination-Elab'
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_magicElab'
  view->buildAllAvailableCombination
  
END

PRO fairmode_saveElaboration, ev

  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_elaborationMenuSave'
  view->saveElaboration
  
END

PRO fairmode_restoreElaboration, ev

  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_elaborationMenuLoad'
  view->restoreElaboration
  
END

PRO fairmode_dateMenuSelection, ev

  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_dateMenuSelection'
  view->displayDateSelectionGUI
  
END

PRO fairmode_event, event

  widget_control, event.top, get_uvalue=view
  ;print, 'fairmode_event'
  if tag_names(event, /STRUCTURE_NAME) EQ $
    'WIDGET_KILL_REQUEST' then view->exitRequest
    
    
END

PRO fairmode_destroyWindow, ev

  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_destroyWindow'
  view->exitRequest
  
END

PRO fairmode_modaldestroyWindow, ev

  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;print, 'fairmode_modaldestroyWindow'
  obj_destroy, view
  
END

PRO fairmode_mapDrawMouse, ev

  ;  print, "clicking...", ev.press
  if (ev.press eq 1) then begin
    ;    print, "left"
    xCoord = ev.x
    yCoord = ev.y
    if size(ev, /TYPE) eq 8 then ev=ev.top
    widget_control, ev, get_uvalue=view
    view->recognize, xCoord, yCoord
  endif
;print, 'fairmode_mapDrawMouse'
  
END