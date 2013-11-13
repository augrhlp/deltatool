PRO FMRecognizeGUI::SwitchRecognize

 self.mgr->SwitchRecognize

END

PRO FMRecognizeGUI::exitRequest

 obj_destroy, self

END

PRO FMRecognizeGUI::update, dataName, dataValue

  widget_control, self.valueTxt, set_value=dataValue
  widget_control, self.nameTxt, set_value=dataName
  
END

PRO FMRecognizeGUI::realize

  self->build
  self->GUI::realize
  
END

PRO FMRecognizeGUI::build

  title=self->getTitle()
  base = widget_base(/COLUMN, scr_XSIZE=self.dimensions[0], TLB_FRAME_ATTR =9, $
    scr_ysize=self.dimensions[1]*2+self.dimensions[1]/10, uvalue=self, title=title, $
    /TLB_KILL_REQUEST_EVENTS, event_pro=self.eventprefix+'event')
    
  mainBase = widget_base(base, /COLUMN)
  
  ;  fileMenu=widget_button(fmMenuBar, value='File', UNAME='FILEMENU', /MENU)
  
  subBase1 = widget_base(mainbase,xpad=0, ypad=2,space=0,/COLUMN)
  ;subBase2 = widget_base(mainBase,xpad=0, ypad=0,space=0,/COLUMN)
  
  ;self->buildLogoSection, subBase1
  self->buildInfoSection, subBase1
  
  self->setTopBase, base
  xmanager, 'fairmode', base, /JUST_REG
  
END

PRO FMRecognizeGUI::buildInfoSection, base

  dataNameBase=widget_base(base, xpad=0, ypad=0, space=0, /ROW)
  dataValueBase=widget_base(base, xpad=0, ypad=0, space=0, /ROW)
  bttBase=widget_base(base, xpad=0, ypad=5, space=0, /ROW, /ALIGN_CENTER)
  
  nameLabel = widget_label(dataNameBase, $
    SCR_XSIZE=self.dimensions[0]/3*1,SCR_YSIZE=self.dimensions[1]/3,SENSITIVE=1,$
    font=self.labelFont, VALUE=self.nameTitle)

  self.nameTxt = widget_text(dataNameBase, $
    SCR_XSIZE=self.dimensions[0]/3*2-15 ,SCR_YSIZE=self.dimensions[1]/3,SENSITIVE=1,$
    /ALL_EV, font=self.textFont, VALUE="")
;    /ALL_EV, font=self.textFont, /SCROLL, /WRAP, VALUE="")
    
  valueLabel = widget_label(dataValueBase, $
    SCR_XSIZE=self.dimensions[0]/3*1,SCR_YSIZE=self.dimensions[1]/3,SENSITIVE=1,$
    font=self.labelFont, VALUE=self.valueTitle)

  self.valueTxt = widget_text(dataValueBase, $
    SCR_XSIZE=self.dimensions[0]/3*2-15 ,SCR_YSIZE=self.dimensions[1]/3,SENSITIVE=1,$
    /ALL_EV, font=self.textFont, VALUE="")
;    /ALL_EV, font=self.textFont, /SCROLL, /WRAP, VALUE="")

  hideBtt=widget_button(bttBase, UNAME='RECOGNIZEOK_BTT', SCR_YSIZE=self.dimensions[1]/3, $
  SCR_XSIZE=self.dimensions[0]/3, VALUE='HIDE', event_pro=self.eventprefix+'recognizeSWITCHBTT')
  
END

PRO FMRecognizeGUI::buildLogoSection, base

  logoDrawBase = widget_Base(base, UNAME='mainDrawBase', $
    XOFFSET=0 ,YOFFSET=0, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=2, /COLUMN, /ALIGN_CENTER)
    
  logoDraw = widget_draw(logoDrawBase,RETAIN=2,SCR_XSIZE=self.dimensions[0] ,SCR_YSIZE=self.dimensions[1], $
    UNAME='LOGODRAW')
    
END

; *****************************************************************
; configure, update, display
; *****************************************************************
PRO FMRecognizeGUI::configureMainStatusBar, topBase, statusMessage

  statusLabel=widget_info(topBase, FIND='STATUSBARMAIN')
  widget_control, statusLabel, set_value=statusMessage
  
END

FUNCTION FMRecognizeGUI::getTitle

  return, 'Recognize'
  
END
;*************************************************************
; constructor / destructor
;*************************************************************
FUNCTION FMRecognizeGUI::init, mgr, nameTitle, valueTitle, dims, fonts=fonts

  if n_elements(dims) eq 2 then begin
    xSize=dims[0]
    ySize=dims[1]
  endif else begin
    xSize=100
    xSize=70
  endelse
  self.nameTitle=nameTitle
  self.valueTitle=valueTitle
  self.dimensions=[xSize, ySize]
  if not self -> GUI :: init(mgr, fonts=fonts) then return , 0
  ;mgr->setMainView, self
  return , 1
  
END

PRO FMRecognizeGUI::cleanUp

  self -> GUI::cleanUp
  
END

;****************************************************************************************

PRO FMRecognizeGUI__Define

  Struct = { FMRecognizeGUI , $
    dimensions: lonarr(2), $
    nameTxt: 0l, $
    valueTxt: 0l, $
    nameTitle: "", $
    valueTitle: "", $
    hideBtt: 0l, $
    Inherits GUI $
    }
    
END

;****************************************************************************************

