FUNCTION conversionObsModelSetting, evTop, pState, MODEL=MODEL, OBS=OBS

  deltaMgr=pState.deltaMgr
  prefixWid=WIDGET_INFO( evTop, find_by_uname='INPUT_PREFIX')
  
  if keyword_set(OBS) then begin
    widget_control, prefixWid, sensitive=0
    pState.observedFlag=1
    pState.iread=0
    pstate.dir_obs=deltaMgr->cleanPath(pstate.dir_obs)
    pstate.dirOut=pstate.dir_obs
    pstate.dirIn=pstate.dir_obs
    pstate.prefixId=''
    fillConversionWids, pstate
    widget_control,pstate.butgo2,sensitive=1
  endif else begin
    widget_control, prefixWid, sensitive=1
    pState.observedFlag=0
    pState.iread=0
    pstate.dir_mod_csv=deltaMgr->cleanPath(pstate.dir_mod_csv)
    pState.dirOut=pstate.dir_mod_cdf
    pState.dirIn=pstate.dir_mod_csv
    pstate.prefixId=''
    ;pstate.prefixId=pstate.year
    fillConversionWids, pstate
    widget_control,pstate.butgo2,sensitive=1
  endelse
  return, pState
  
END



