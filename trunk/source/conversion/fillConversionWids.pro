PRO fillConversionWids, pstate, ev	  textvalues=[pstate.startUpFile, $            pstate.initRun, pstate.endRun, pstate.dirIn,$            pstate.prefixId, pstate.dirOut, pstate.year, $            pstate.modelName, pstate.postFix]  wids=pstate.infoWids  deltaMgr=pstate.deltaMgr    ;dir_in=deltaMgr->cleanPath(dir_in)  ;dir_out=deltaMgr->cleanPath(dir_out)    for i=0, n_elements(wids)-1 do widget_control, wids[i], set_value=textvalues[i]  widget_control, pstate.modelBtt, SET_BUTTON=1-pstate.observedFlag  ;widget_control, pstate.observedBtt, SET_BUTTON=pstate.observedFlag  ;widget_control,pState.modelButton, SET_VALUE=1-pstate.observedFlag  ;widget_control,pState.observedButton, SET_VALUE=pstate.observedFlag  if keyword_set(pstate.observedFlag) then begin   ;widget_control, wids[6], sensitive=0   widget_control, wids[7], sensitive=0  endif else begin   ;widget_control, wids[6], sensitive=1   widget_control, wids[7], sensitive=1  endelse  widget_control,pstate.labWids[0],set_value=buildInputTemplateFileName(pstate.dirIn, pstate.prefixId)  widget_control,pstate.labWids[1],set_value=buildOutputFileName(pstate.dirOut, pstate.year, pstate.modelName, pstate.postFix, MODELTYPE=1-pstate.observedFlag)END