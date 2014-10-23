FUNCTION GUI::getMgr

 return, self.mgr

END

FUNCTION GUI::getScreenSize

 return, self.screenSize

END

FUNCTION GUI::getTitle

 message, 'You must implement this one on your child class!!!'
 return, ''

END

FUNCTION GUI::getBestDimensions, RESOLUTION=RESOLUTION

 return, get_screen_size(RESOLUTION=resolution)*0.85

END

FUNCTION GUI::dialogMessage, textMessage, title=title, INFORMATION=INFORMATION, error=error, question=question, WARNING=WARNING

 return, dialog_message(textMessage, dialog_parent=self.topBase, title=title, INFORMATION=INFORMATION, ERROR=ERROR, QUESTION=QUESTION)

END

PRO GUI::moveToCenterScreen

 screendimension = get_screen_size()
 geom = widget_info(self.topBase, /GEOMETRY)
 xoffset = (screendimension[0] - geom.scr_xsize)/2
 yoffset = (screendimension[1] - geom.scr_ysize)/2
 self.screenSize=screendimension
 widget_control, self.topBase, XOFFSET=xoffset ,YOFFSET=yoffset

END

PRO GUI::moveToUpperCenterScreen

 screendimension = get_screen_size()
 geom = widget_info(self.topBase, /GEOMETRY)
 xoffset = (screendimension[0] - geom.scr_xsize)/2
 yoffset = (screendimension[1] - geom.scr_ysize)/5
 widget_control, self.topBase, XOFFSET=xoffset ,YOFFSET=yoffset

END

PRO GUI::moveToLowerCenterScreen

 screendimension = get_screen_size()
 geom = widget_info(self.topBase, /GEOMETRY)
 xoffset = (screendimension[0] - geom.scr_xsize)/1.5
 yoffset = (screendimension[1] - geom.scr_ysize)*(12./13)
 widget_control, self.topBase, XOFFSET=xoffset ,YOFFSET=yoffset

END

PRO GUI::exitRequest

 self.mgr->exitRequest

END

PRO GUI::destroy

 widget_control, self.topBase, /DESTROY

END

PRO GUI::hide

 widget_control, self.topBase, MAP=0

END

PRO GUI::show

 widget_control, self.topBase, /MAP

END

PRO GUI::disable

 widget_control, self.topBase, SENSITIVE=0

END

PRO GUI::enable

 widget_control, self.topBase, /SENSITIVE

END

PRO GUI::realize

 self->moveToCenterScreen
 if widget_info(self.topBase, /VALID_ID) then widget_control, self.topBase, /REALIZE
 ;xmanager

END

PRO GUI::setTopBase, id

 self.topBase=id

END

FUNCTION GUI::getTopBase

 return, self.topBase

END


FUNCTION GUI::init, mgr, fonts=fonts

 if not self -> Object :: init() then return , 0

 if obj_valid(mgr) then self.mgr=mgr
 if n_elements(fonts) ne 3 then begin
	util=obj_new('FMUtility')
	self.titleFont=util->getTitleTextFont()
	self.labelFont=util->getStandardLabelFont()
	self.textFont=util->getStandardTextFont()
	obj_destroy, util
 endif else begin
	self.titleFont=fonts[0]
	self.labelFont=fonts[1]
	self.textFont=fonts[2]
 endelse
 self.eventPrefix='fairMode_'

 return , 1

END

PRO GUI::cleanUp

 self.mgr=obj_new()
 widget_control, self.topBase, /DESTROY
 self -> Object::cleanUp

END

;****************************************************************************************

PRO GUI__Define

Struct = { GUI , $
		mgr: obj_new(), $
		topBase: 0l, $
		titleFont: '', $
		labelFont: '', $
		textFont: '', $
		eventPrefix: '', $
		screenSize: lonarr(2), $
		Inherits Object $
		 }

END

;****************************************************************************************

