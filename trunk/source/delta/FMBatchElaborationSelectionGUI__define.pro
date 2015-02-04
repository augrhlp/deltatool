FUNCTION FMBatchElaborationSelectionGUI::getMainMgr

 return, self.mgr->getMainMgr()

END

PRO FMBatchElaborationSelectionGUI::updateToCaller

  ; change the back calling
  self.mgr->elaborationOK, self.info
  ;self.mgr->enable
  
END

; events
PRO FMBatchElaborationSelectionGUI::OKRequest

 ;self.info->setExclusives, self.radioSelections
 if self->checkIntegrity() then begin
  ;self.mgr->setBlockWindowControl, /OFF
  self->updateToCaller
  obj_destroy, self
 endif else begin
  print, 'Bad ', obj_class(self)
  ;exitMessage=['Something wrong in your selection', 'Check data']
  ;dlgResult=dialog_message(exitMessage, dialog_parent=self->getTopBase(), Title='Warning')
 endelse
  
END

; check changing of main gui data
; gui building
PRO FMBatchElaborationSelectionGUI::build

  title=self->getTitle()
  
  base = widget_base(/COLUMN, $
    uvalue=self, TLB_FRAME_ATTR=1, $
    title=title, /TLB_KILL_REQUEST_EVENTS, event_pro=self.eventprefix+'destroyWindow')
    
  mainBase = widget_base(base, /COLUMN)
  
  subBase11 = widget_base(mainbase,xpad=self.xPad, ypad=self.yPad,space=0,/ROW)
  subBase12 = widget_base(mainbase,xpad=self.xPad, ypad=self.yPad,space=0,/ROW, /ALIGN_CENTER)
  subBase111 = widget_base(subBase11,xpad=1, ypad=1,space=1,/COLUMN, FRAME=1)
  subBase112 = widget_base(subBase11,xpad=1, ypad=1,space=1,/COLUMN, FRAME=1)
  
  self->buildElaborationSection, subBase111
  self->buildGroupAndDateSection, subBase112
  self->buildOKButton, subBase12
  
  self->SetTopBase, base
  xmanager, 'fairmode', base, /JUST_REG, /CATCH
  
END


; set/get
FUNCTION FMBatchElaborationSelectionGUI::getTitle

  return, 'Batch Analysis'
  
END

; constructor/destructor
FUNCTION FMBatchElaborationSelectionGUI::init, info, mgr, fonts=fonts

  if not self -> FMElaborationSelectionGUI :: init(info, mgr, fonts=fonts) then return , 0
  return , 1
  
  
END

PRO FMBatchElaborationSelectionGUI::cleanUp

  self -> FMElaborationSelectionGUI::cleanUp
  
END

;****************************************************************************************

PRO FMBatchElaborationSelectionGUI__Define

  Struct = { FMBatchElaborationSelectionGUI , $
    Inherits FMElaborationSelectionGUI $
    }
    
END

;****************************************************************************************

