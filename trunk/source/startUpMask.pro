;************************************************************************************************************************
function disclaimer_startup

  day_d=31
  month_d=12
  year_d=2016
  dline = JULDAY(month_d,day_d,year_d,0,0,0)
  return, ['JOINT RESEARCH CENTRE', 'EUROPEAN COMMISSION', $
    'INSTITUTE FOR ENVIRONMENT AND SUSTAINABILITY', 'JRC --- exJRC --- UnivStrb', 'Copyright 2013-2016 European Commission - JRC-IES', $
    'Version 1.0', 'Expiration:'+$
    strtrim(day_d,2)+'/'+strtrim(month_d,2)+'/'+strtrim(year_d,2)]
    
end

Pro startUpMask_Event, event
  Widget_Control, event.top, GET_UVALUE=pState
  deltaMgr=(*pState).deltaMgr
  ; Chiusura con pulsante di sistema
  if Tag_Names(event, /STRUCTURE_NAME) eq 'WIDGET_KILL_REQUEST' $
    then event.id = (*pState).WID_BUTTON_CANCEL
  Widget_Control, event.top, GET_UVALUE=pState
  case event.id of
    (*pState).WID_BTN_DELTA  : begin
      Widget_Control, event.top, /DESTROY
      ;DlgStartUp
      deltaMgr->display
      Ptr_Free, pState
    end
    (*pState).WID_BTN_IO_CHECK  : begin
      Widget_Control, event.top, /DESTROY
      ;DlgStartUp
      DeltaCheck_IO, pstate, deltaMgr, /NOVIEW
      Ptr_Free, pState
    end
    (*pState).WID_BTN_ET1  : begin
      Widget_Control, event.top, /DESTROY
      ;DlgStartUp
      nonlinear, pState, deltaMgr
      Ptr_Free, pState
    end
    (*pState).WID_BTN_ET2  : begin
      Widget_Control, event.top, /DESTROY
      ;DlgStartUp
      ht_plane, pState, deltaMgr
      Ptr_Free, pState
    end
    (*pState).WID_BTN_ET3  : begin
      Widget_Control, event.top, /DESTROY
      ;DlgStartUp
      conversion, pstate, deltaMgr
      Ptr_Free, pState
    end
;    (*pState).WID_BTN_ET4  : begin
;      Widget_Control, event.top, /DESTROY
;      ;DlgStartUp
;      obsCSV2CDF, pstate, deltaMgr
;      Ptr_Free, pState
;    end
    (*pState).WID_BUTTON_CANCEL : begin
      Widget_Control, event.top, /DESTROY
      device, DECOMPOSED=(*pState).gd
      TvLct,(*pState).orig_r,(*pState).orig_g,(*pState).orig_b
      Ptr_Free, pState
    end
    else:
  endcase
End

PRO fillLogoImages, mainWin, deltaMgr

  widget_control, mainWin, get_uvalue=state
  state=*state
  littleLogoDim=deltaMgr->getLittleLogoSize()
  mediumLogoDim=deltaMgr->getMediumLogoSize()
  DIMX_JRC = littleLogoDim[0]
  DIMY_JRC = littleLogoDim[1]
  DIMX_CD = mediumLogoDim[0]
  DIMY_CD = mediumLogoDim[1]
  DIMX_IES = DIMX_JRC
  DIMY_IES = DIMY_JRC
  fm=deltaMgr->getFileSystemMgr()
  jrcLogo=fm->getJrcLogoFileName()
  iesLogo=fm->getIesLogoFileName()
  fmLogo=fm->getFairModeLogoFileName()
  jrcLogo = Read_Image(jrcLogo)
  jrcLogo = Congrid(jrcLogo, 3, DIMX_JRC, DIMX_JRC)
  iesLogo = Read_Image(iesLogo)
  iesLogo = Congrid(iesLogo, 3, DIMX_IES, DIMX_IES)
  fmLogo  = Read_Image(fmLogo)
  fmLogo  = Congrid(fmLogo, 3, DIMX_CD, DIMX_CD)
  ; Lavoro in modalita' decomposed per la visualizzazione delle immagini.
  device, DECOMPOSED=1
  Widget_Control, state.WID_DRAW_JRC, GET_VALUE=windex
  WSet, windex
  Tv, jrcLogo, /TRUE
  Widget_Control, state.WID_DRAW_CITY_DELTA, GET_VALUE=windex
  WSet, windex
  Tv, iesLogo, /TRUE
  Widget_Control, state.WID_DRAW_IES, GET_VALUE=windex
  WSet, windex
  Tv, fmLogo, /TRUE
  ; Ritorno alla modalita' index color
  device, DECOMPOSED=0
  
END

Pro startUpMask, deltaMgr, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

  close, /all
  xmanager, /catch
  if obj_valid(deltaMgr) then begin
    littleLogoDim=deltaMgr->getLittleLogoSize()
    mediumLogoDim=deltaMgr->getMediumLogoSize()
    DIMX_JRC = littleLogoDim[0]
    DIMY_JRC = littleLogoDim[1]
    DIMX_CD = mediumLogoDim[0]
    DIMY_CD = mediumLogoDim[1]
    DIMX_IES = DIMX_JRC
    DIMY_IES = DIMY_JRC
    fm=deltaMgr->getFileSystemMgr()
    homeDir=fm->getHomeDir()
  endif
  ; Salvo la modalita corrente di rappresentazione dei colori.
  device, GET_DECOMPOSED=gd
  ; Setto la modalita' index color per la rappresentazione dei colori
  device, DECOMPOSED=0
  TvLct, orig_r, orig_g, orig_b, /GET
  LoadCt, 39, /SILENT
  current_julday = systime(/julian)
  ; Definizione costanti
  X_PAD = 5
  X_DRAW_OFFSET = 210
  WID_BASE_STARTUP = Widget_Base( GROUP_LEADER=wGroup,  $
    UNAME='WID_BASE_STARTUP',  $
    TITLE='FairMode Planning Tool' ,$
    SPACE=3 ,XPAD=3, YPAD=3,TLB_FRAME_ATTR=1, /ALIGN_CENTER, /COLUMN)
  draw_base1=widget_base(WID_BASE_STARTUP, /ROW, /ALIGN_CENTER)
  double_base=widget_base(WID_BASE_STARTUP, /ROW, /ALIGN_CENTER)
  main_btt_base = Widget_Base(double_base,  $
    UNAME='WID_BASE_BUTTONS' ,FRAME=0,  $
    TITLE='IDL' ,SPACE=3 ,XPAD=3,  $
    YPAD=3, /COLUMN, /ALIGN_CENTER)
  draw_base2=widget_base(double_base, /ROW, /ALIGN_CENTER)
  opt_btt_base=widget_base(WID_BASE_STARTUP, /COLUMN)
  disclaimerText=disclaimer_startup()
  WID_TEXT_COPYRIGHT = Widget_text(opt_btt_base, EDIT=0, /ALL_EV, /WRAP, VALUE=disclaimer_startup(), $
    UNAME='WID_TEXT_COPYRIGHT', /ALIGN_CENTER, XSIZE=max(strlen(disclaimerText)), YSIZE=n_elements(disclaimerText))
  WID_BTN_DELTA = Widget_Button(main_btt_base, UNAME='WID_BTN_DELTA',  $
    SCR_XSIZE=200 ,SCR_YSIZE=30,$
    /ALIGN_CENTER ,VALUE='DELTA')
  WID_BTN_IO_CHECK = Widget_Button(main_btt_base, UNAME='WID_BTN_IO_CHECK',  $
    SCR_XSIZE=200 ,SCR_YSIZE=30,$
    /ALIGN_CENTER ,VALUE='IO_CHECK')
  WID_BTN_ET1 = Widget_Button(main_btt_base, UNAME='WID_BTN_ET1',  $
    SCR_XSIZE=200 ,SCR_YSIZE=30,$
    /ALIGN_CENTER ,VALUE='NON LINEAR')
  WID_BTN_ET2 = Widget_Button(main_btt_base, UNAME='WID_BTN_ET2',  $
    SCR_XSIZE=200 ,SCR_YSIZE=30,$
    /ALIGN_CENTER ,VALUE='PLANNING')
  WID_BTN_ET3 = Widget_Button(main_btt_base, UNAME='WID_BTN_ET3',  $
    SCR_XSIZE=200 ,SCR_YSIZE=30,$
    /ALIGN_CENTER ,VALUE='DATA FORMAT')
  ;WID_BTN_ET4 = Widget_Button(main_btt_base, UNAME='WID_BTN_ET4',  $
  ;  SCR_XSIZE=200 ,SCR_YSIZE=30,$
  ;  /ALIGN_CENTER ,VALUE='OBS_CONVERSION')
  WID_DRAW_JRC = Widget_Draw(draw_base1, UNAME='WID_DRAW_JRC',  $
    SCR_XSIZE=DIMX_JRC ,SCR_YSIZE=DIMY_JRC, $
    /FRAME)
  WID_DRAW_CITY_DELTA = Widget_Draw(draw_base2,  $
    UNAME='WID_DRAW_CITY_DELTA' ,/FRAME ,$
    SCR_YSIZE=DIMY_CD)
  WID_DRAW_IES = Widget_Draw(draw_base1, UNAME='WID_DRAW_IES'  $
    , /FRAME $
    ,SCR_XSIZE=DIMX_JRC ,SCR_YSIZE=DIMY_JRC)
  WID_BUTTON_CANCEL = Widget_Button(opt_btt_base,  $
    UNAME='WID_BUTTON_CANCEL'  $
    ,SCR_XSIZE=100 ,SCR_YSIZE=30 ,/ALIGN_CENTER ,VALUE='EXIT')
  CenterWindow,WID_BASE_STARTUP
  state = {                     $
    gd:gd,                       $
    orig_r:orig_r,                  $
    orig_g:orig_g,                  $
    orig_b:orig_b,                $
    deltaMgr : deltaMgr, $
    WID_BTN_DELTA  : WID_BTN_DELTA,        $
    WID_BTN_IO_CHECK  : WID_BTN_IO_CHECK,        $
    WID_BTN_ET1  : WID_BTN_ET1,        $
    WID_BTN_ET2  : WID_BTN_ET2,        $
    WID_BTN_ET3 : WID_BTN_ET3, $
    ;WID_BTN_ET4 : WID_BTN_ET4, $
    WID_BUTTON_CANCEL : WID_BUTTON_CANCEL,   $
    WID_DRAW_JRC : WID_DRAW_JRC,   $
    WID_DRAW_CITY_DELTA: WID_DRAW_CITY_DELTA,   $
    WID_DRAW_IES : WID_DRAW_IES $
    }
  Widget_Control, WID_BASE_STARTUP, SET_UVALUE=Ptr_New(state, /NO_COPY)
  ; Lettura dei loghi
  Widget_Control, /REALIZE, WID_BASE_STARTUP
  fillLogoImages, WID_BASE_STARTUP, deltaMgr
  XManager, 'startUpMask', WID_BASE_STARTUP, /NO_BLOCK
  
end
;************************************************************************************************************************