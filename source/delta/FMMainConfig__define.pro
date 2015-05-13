@../common/structure_definition
;********************************
;
;********************************
FUNCTION FMMainConfig::getUserType

  return, self.userType
  
END

FUNCTION FMMainConfig::getBenchmarkManagingEnabled

  return, self.benchmarkManagingEnabled
;if self.userType ge 2
  
END

PRO FMMainConfig::setBenchmarkManagingEnabled, benchmarkManagingEnabled

  if n_elements(benchmarkManagingEnabled) ne 0 then self.benchmarkManagingEnabled=benchmarkManagingEnabled
  
END

PRO FMMainConfig::setUserType, userType

  self.userType=userType
  self.elaborationList->setUserType, userType
  
END

PRO FMMainConfig::fillFlexyData, confDir, fs, frequencyType, parameterName=parameterName, parameterValue=parameterValue

  ;self.fileName = fileName
  ;readInitFile, fileName, parameterName=parameterName, parameterValue=parameterValue
  for i=0, n_elements(parameterName)-1 do begin
    fileName=confDir+parameterValue[i]
    case strupCase(parameterName[i]) of
      'OBSERVEDCATEGORY_FILE' : self.observedCategoryList->FillDataFromFile, fileName
      'RUN_FILE' : self.runList->FillDataFromFile, fileName
      'RUNPARAMETER_FILE' : self.runParameterList->FillDataFromFile, fileName
      'FONT_FILE' : self->configFontFile, fileName;self.fontList->FillDataFromFile, fileName
      ;'PARAMETERFILE' : self.parameterList->FillDataFromFile, fileName
      'OBSERVEDPARAMETER_FILE' : self.observedParameterList->FillDataFromFile, fileName
      ;'OBSERVEDFILE' : self.observedList->FillDataFromFile, fileName
      'ELABORATION_FILE' : self->configElaborationFile, fs, confDir, parameterValue[i], frequencyType
      'GOALSCRITERIAOC_FILE' : self.goalsCriteriaOCList->FillDataFromFile, fileName
    else : print, 'Extra parameter on init file: <', parameterName[i], '=', parameterValue[i], '>'
  endcase
endfor

END

PRO FMMainConfig::configElaborationFile, fs, dir, fileName, frequencyType

  fs->modelFrequencyRename, dir, fileName, frequencyType
  self.elaborationList->FillDataFromFile, dir+fileName
  
END

PRO FMMainConfig::configFontFile, fullFileName

  ;fs->modelFrequencyRename, dir, fileName, frequencyType
  self.fontList->FillDataFromFile, fullFileName
  ; setting for RESET option of setUserFont
  setCurrentFont, self.fontList
  
END

;********************************
;get/set
;********************************
PRO FMMainConfig::setRunList, list

  obj_destroy, self.runList
  self.runList=list
  
END

FUNCTION FMMainConfig::getRunList

  return, self.runList
  
END

PRO FMMainConfig::setRunParameterList, list

  obj_destroy, self.runParameterList
  self.runParameterList=list
  
END

FUNCTION FMMainConfig::getRunParameterList

  return, self.runParameterList
  
END

PRO FMMainConfig::setObservedParameterList, list

  obj_destroy, self.observedParameterList
  self.observedParameterList=list
  
END

FUNCTION FMMainConfig::getObservedParameterList

  return, self.observedParameterList
  
END
PRO FMMainConfig::setElaborationList, list

  obj_destroy, self.elaborationList
  self.elaborationList=list
  
END

FUNCTION FMMainConfig::getElaborationList

  return, self.elaborationList
  
END

PRO FMMainConfig::setgoalsCriteriaOCList, list

  obj_destroy, self.goalsCriteriaOCList
  self.goalsCriteriaOCList=list
  
END

FUNCTION FMMainConfig::getGoalsCriteriaOCList

  return, self.goalsCriteriaOCList
  
END

PRO FMMainConfig::setFontList, list

  obj_destroy, self.fontList
  self.fontList=list
  
END

FUNCTION FMMainConfig::getFontList

  return, self.fontList
  
END

PRO FMMainConfig::setObservedCategoryList, list

  obj_destroy, self.observedCategoryList
  self.observedCategoryList=list
  
END

FUNCTION FMMainConfig::getObservedCategoryList

  return, self.observedCategoryList
  
END

PRO FMMainConfig::streamPrint

  print, '***********************'
  print, '**Start of',OBJ_CLASS(self),'**'
  
  print, '**** fileName:<', self.fileName,'>'
  if obj_valid(self.elaborationList) then self.elaborationList->streamPrint
  if obj_valid(self.runList) then self.runList->streamPrint
  if obj_valid(self.runParameterList) then self.runParameterList->streamPrint
  if obj_valid(self.observedCategoryList) then self.observedCategoryList->streamPrint
  if obj_valid(self.observedParameterList) then self.observedParameterList->streamPrint
  if obj_valid(self.observedCategory) then self.observedCategory->streamPrint
  if obj_valid(self.goalsCriteriaOCList) then self.goalsCriteriaOCList->streamPrint
  
  print, '***********************'
  print, '**End of:',OBJ_CLASS(self),'**'
  
END
;********************************
;constructor/desctructor
;********************************
PRO FMMainConfig::CleanUp

  self -> Object :: cleanUp
  obj_destroy, self.runList
  obj_destroy, self.runParameterList
  obj_destroy, self.observedParameterList
  obj_destroy, self.elaborationList
  obj_destroy, self.goalsCriteriaOCList
  obj_destroy, self.observedCategoryList
  obj_destroy, self.fontList
  
END

FUNCTION FMMainConfig::init

  if not self -> Object :: init() then return , 0
  self.goalsCriteriaOCList=obj_new('GoalsCriteriaOC')
  self.elaborationList=obj_new('Elaboration')
  self.fontList=obj_new('Font')
  self.runList=obj_new('Run')
  self.runParameterList=obj_new('RunParameter')
  self.observedCategoryList=obj_new('ObservedCategory')
  self.observedParameterList=obj_new('ObservedParameter')
  return, 1
  
END

PRO FMMainConfig__Define

  Struct = { FMMainConfig , $
    userType: 0, $
    benchmarkManagingEnabled: 0, $
    elaborationList: obj_new(''), $
    runList: obj_new(''), $
    runParameterList: obj_new(''), $
    observedCategoryList: obj_new(''), $
    observedParameterList: obj_new(''), $
    observedCategory : obj_new(), $
    goalsCriteriaOCList : obj_new(), $
    fontList : obj_new(), $
    Inherits ConfigurableData $
    }
    
END

;****************************************************************************************
