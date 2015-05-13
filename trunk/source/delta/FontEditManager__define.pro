FUNCTION FontEditManager::dialogMessage, textMessage, title=title, INFORMATION=INFORMATION, error=error, question=question, WARNING=WARNING, CENTER=CENTER

 return, self.mainMgr->dialogMessage(textMessage, title=title, INFORMATION=INFORMATION, error=error, question=question, WARNING=WARNING, CENTER=CENTER)

END

FUNCTION FontEditManager::getMainMgr

  return, self.mainMgr
  
END

PRO FontEditManager::enable

  self.refView->enable
  
END

FUNCTION FontEditManager::checkIntegrity

  return, 1
  
END

PRO FontEditManager::updateToCaller

  infoText=['User Definition font file updated']
  infoTitle=['Configuration File updated']
  a=dialogMsg(infoText, title=infoTitle)
  setUserFont, /RESET
  self.mainMgr->enable
  self.mainMgr->setBlockWindowControl, /OFF
  
END

PRO FontEditManager::exitRequest

  self.mainMgr->enable
  
END

PRO FontEditManager::realize

  self.refView=obj_new('FMFontEditGUI', self.fontInfo, self)
  self.refView->realize
  
END

FUNCTION FontEditManager::abort

  self.refView->enable
  obj_destroy, self
  
END

PRO FontEditManager::configure

  ; MM October 2001: saving memory, better objects & pointers management
  if obj_valid(self.batchEntityView) then obj_destroy, self.batchEntityView
  if obj_valid(self.batchElaborationView) then obj_destroy, self.batchElaborationView
  ;end
  
END


FUNCTION FontEditManager::getTitle

  return, 'User Def Font editor'
  
END
;*************************************************************
; constructor / destructor
;*************************************************************
FUNCTION FontEditManager::init, mainMgr, fontInfo

  self.mainMgr=mainMgr
  self.fontInfo=fontInfo
  
  return , 1
  
END

PRO FontEditManager::cleanUp

  ;obj_destroy, self.fontInfo
  ;for i=0, 3 do if obj_valid(self.batchElaborationInfos[i]) then obj_destroy, self.batchElaborationInfos[i]
  self.refView=obj_new('')
  self.mainMgr=obj_new('')
; end
  
END

;****************************************************************************************

PRO FontEditManager__Define

  Struct = { FontEditManager , $
    fontInfo: obj_new(), $
    mainMgr: obj_new(), $
    refView: obj_new(), $
    modified: fltarr(4), $
    Inherits Object $
    }
    
END

;****************************************************************************************

