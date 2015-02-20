PRO conversionConfig, ev

  Widget_Control, ev, GET_UVALUE=pState
  pState=*pState
  prefixWid=WIDGET_INFO( ev, find_by_uname='INPUT_PREFIX')
  deltaMgr=pState.deltaMgr
  
  pState=conversionObsModelSetting(ev, pState, /MODEL)
  Widget_Control, ev, SET_UVALUE=Ptr_New(pstate, /NO_COPY)
  
END



