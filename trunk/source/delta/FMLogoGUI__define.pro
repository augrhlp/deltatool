PRO FMLogoGUI::exitRequest

 obj_destroy, self

END

FUNCTION FMLogoGUI::getLogo

  return, *self.logo
  
END

PRO FMLogoGUI::showAboutSplash

  logo=self->getLogo()
  self->wsetLogoDraw
  tv, logo, true=self.true+1
  xyouts, 410, 25, self.versionDate, /device, CHARSIZE=1.5, CHARTHICK=1.5
  xyouts, 410, 85, self.versionCode, /device,  CHARSIZE=1.5, CHARTHICK=1.5
  
END

PRO FMLogoGUI::wsetLogoDraw, topBase

  logoDraw=widget_info(self->getTopBase(), FIND='LOGODRAW')
  widget_control, logoDraw, get_value=wsetId
  wset, wsetId
  device,/DECOMPOSE
  
END

PRO FMLogoGUI::realize

  self->build
  self->GUI::realize
  
END

PRO FMLogoGUI::build

  title=self->getTitle()
  base = widget_base(/COLUMN, scr_XSIZE=self.dimensions[0], TLB_FRAME_ATTR =17, $
    scr_ysize=self.dimensions[1]*2+self.dimensions[1]/10, uvalue=self, title=title, $
    /TLB_KILL_REQUEST_EVENTS, event_pro=self.eventprefix+'event')
    
  mainBase = widget_base(base, /COLUMN)
  
  ;  fileMenu=widget_button(fmMenuBar, value='File', UNAME='FILEMENU', /MENU)
  
  subBase1 = widget_base(mainbase,xpad=0, ypad=2,space=0,/COLUMN)
  subBase2 = widget_base(mainBase,xpad=0, ypad=0,space=0,/COLUMN)
  
  self->buildLogoSection, subBase1
  self->buildInfoSection, subBase2
  
  self->SetTopBase, base
  xmanager, 'fairmode', base, /JUST_REG
  
END

PRO FMLogoGUI::buildInfoSection, base

  infoRow=strarr(3)
  infoRow[0]='**FairMode** - A JRC Tool (C) 2011'
  infoRow[1]='Version: '+self.versionCode
  infoRow[2]='Released: '+self.versionDate
  infoBase=widget_base(base, xpad=0, ypad=0, space=0, /ROW, /ALIGN_CENTER)
  descBase=widget_base(infoBase, xpad=0, ypad=0, space=0, /ROW)
  ;bttBase=widget_base(infoBase, xpad=0, ypad=0, space=0, /ROW, /ALIGN_CENTER)
  
  notesText = widget_Text(descBase, $
    SCR_XSIZE=self.dimensions[0]/3*2-10 ,SCR_YSIZE=self.dimensions[1]/2,SENSITIVE=1,$
    /ALL_EV, font=self.textFont, /SCROLL, /WRAP, VALUE=infoRow)
    
  OKBtt=widget_button(descBase, UNAME='ABOUTOK_BTT', SCR_YSIZE=self.dimensions[1]/2, $
  SCR_XSIZE=self.dimensions[0]/3-10, VALUE='EXIT', event_pro=self.eventprefix+'aboutOKBTT')
  
END

PRO FMLogoGUI::buildLogoSection, base

  logoDrawBase = widget_Base(base, UNAME='mainDrawBase', $
    XOFFSET=0 ,YOFFSET=0, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=2, /COLUMN, /ALIGN_CENTER)
    
  logoDraw = widget_draw(logoDrawBase,RETAIN=2,SCR_XSIZE=self.dimensions[0] ,SCR_YSIZE=self.dimensions[1], $
    UNAME='LOGODRAW')
    
END

; *****************************************************************
; configure, update, display
; *****************************************************************
PRO FMLogoGUI::configureMainStatusBar, topBase, statusMessage

  statusLabel=widget_info(topBase, FIND='STATUSBARMAIN')
  widget_control, statusLabel, set_value=statusMessage
  
END

FUNCTION FMLogoGUI::getTitle

  return, 'About'
  
END
;*************************************************************
; constructor / destructor
;*************************************************************
FUNCTION FMLogoGUI::init, mgr, logoImage, versionCode, versionDate, fonts=fonts

;  image=self->getLogo()
;  stop
;  ;check dims!!
  dims=size(logoImage, /DIM)
  trueIndex=where(dims eq 3, count, complement=imageDim)
  if count eq 1 then self.true=trueIndex
  self.dimensions=[dims[imageDim[0]], dims[imageDim[1]]]
  self.logo=ptr_new(logoImage, /NO_COPY)
  self.versionCode=versionCode
  self.versionDate=versionDate
  if not self -> GUI :: init(mgr, fonts=fonts) then return , 0
  ;mgr->setMainView, self
  return , 1
  
END

PRO FMLogoGUI::cleanUp

  ptr_free, self.logo
  self -> GUI::cleanUp
  
END

;****************************************************************************************

PRO FMLogoGUI__Define

  Struct = { FMLogoGUI , $
    logo: ptr_new(), $
    true: 0, $
    versionCode: '', $
    versionDate: '', $
    dimensions: lonarr(2), $
    Inherits GUI $
    }
    
END

;****************************************************************************************

