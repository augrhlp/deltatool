FUNCTION FMInfoSelectionGUI::getInfo

 return, self.info

END

PRO FMInfoSelectionGUI::setInfo, info

 self.info=info

END

FUNCTION FMInfoSelectionGUI::getVisibleXSize

 return, self.dimensions[0]-self.xPad*2

END

FUNCTION  FMInfoSelectionGUI::getVisibleYSize

 return, self.dimensions[1]-self.yPad*2

END

PRO FMInfoSelectionGUI::configure

 message, 'You must implement this one on your child class!!!'

END

FUNCTION FMInfoSelectionGUI::getTitle

 return, 'Generic selection'

END

PRO FMInfoSelectionGUI::build

 message, 'You must implement this one on your child class!!!'

END

PRO FMInfoSelectionGUI::realize

 self->build
 self->GUI::realize
 self->configure

END

PRO FMInfoSelectionGUI::exitRequest

 ;print, 'Destroy Info gui'
 obj_destroy, self.info
 obj_destroy, self

END

PRO FMInfoSelectionGUI::buildOKButton, base

 subBase=base
 okBtt=widget_button(subBase, value='OK', UNAME='DISPLAYOK', $
	event_pro=self.eventprefix+'OKRequest', SCR_XSIZE=70, SCR_YSIZE=35, /ALIGN_CENTER)
 ;okButton = widget_button(mainButtonBase, Value='OK', UNAME='APPLY', $
 ; XSIZE=70, YSIZE=35, event_pro=self.eventPrefix+'okModeBtt', /ALIGN_CENTER)

END

PRO FMInfoSelectionGUI::OKRequest

 if self->checkIntegrity() then begin
	self.mgr->setBlockWindowControl, /OFF
	self->updateToCaller
	obj_destroy, self
 endif else begin
  print, 'Bad ', obj_class(self)
	;exitMessage=['Something wrong in your selection', 'Check data']
	;dlgResult=dialog_message(exitMessage, dialog_parent=self->getTopBase(), Title='Warning')
 endelse

END

FUNCTION FMInfoSelectionGUI::checkIntegrity

 message, 'You must implement this one on your child class!!!'
 return, 0

END
FUNCTION FMInfoSelectionGUI::getSmallLabelXDim

 return, self.dimensions[0]-20

END

FUNCTION FMInfoSelectionGUI::getSmallLabelYDim

 return, 20

END

FUNCTION FMInfoSelectionGUI::getSubTitleYDim

 return, 20

END

FUNCTION FMInfoSelectionGUI::getValueXDim

 return, (self->xSummarySize()-12)*.68

END

FUNCTION FMInfoSelectionGUI::getValueYDim

 return, 20

END

FUNCTION FMInfoSelectionGUI::getSubTitleXDim

 return, (self->xSummarySize()-12)-self->getValueXDim()

END

FUNCTION FMInfoSelectionGUI::getTitleYDim

 return, 20

END

FUNCTION FMInfoSelectionGUI::getTitleXDim

 return, self.dimensions[0]/3

END

FUNCTION FMInfoSelectionGUI::xSummarySize


 return, self.dimensions[0]-self->xGraphSize()

END

FUNCTION FMInfoSelectionGUI::ySummarySize

 return, self.dimensions[1]*1.

END

FUNCTION FMInfoSelectionGUI::xGraphSize

 return, self.dimensions[0]*.7

END

FUNCTION FMInfoSelectionGUI::yGraphSize

 return, self.dimensions[1]*.7

END

FUNCTION FMInfoSelectionGUI::xInfoGraphSize

 return, self->xGraphSize()

END

FUNCTION FMInfoSelectionGUI::yInfoGraphSize

 return, self.dimensions[1]-self->yGraphSize()

END

FUNCTION FMInfoSelectionGUI::getBestDimensions

 return, get_screen_size(RESOLUTION=resolution)

END

PRO FMInfoSelectionGUI::updateToCaller, info

 message, 'You must implement this one on your child class!!!'
 ;self.info->Clone(/DEEP)
 ; this must be implemented one level up
 ;self.mgr->UpdateXXX, self.info

END

FUNCTION FMInfoSelectionGUI::init, info, mgr, fonts=fonts

 if not self -> GUI :: init(mgr, fonts=fonts) then return , 0
 ;self.mgr->disable
 dims=self->getBestDimensions()
 dims[0]=dims[0]*.65
 dims[1]=dims[1]*.50
 self.dimensions=dims
 self.yPad=2
 self.xPad=2
 self.info=info
 return , 1

END

PRO FMInfoSelectionGUI::cleanUp

 self.mgr->enable
 obj_destroy, self.info
 self -> GUI::cleanUp

END

;****************************************************************************************

PRO FMInfoSelectionGUI__Define

Struct = { FMInfoSelectionGUI , $
		info: obj_new(), $
		dimensions: lonarr(2), $
		yPad: 0, $
		xPad: 0, $
		Inherits GUI $
		 }

END

;****************************************************************************************

