FUNCTION buildStoredValues, pstate, filename

; return, [ pstate.startupFile, pstate.initRun, pstate.endRun, $
;    pstate.dirIn, pstate.prefixId, pstate.dirOut, pstate.modelName]
 return, [ pstate.startupFile, pstate.initRun, pstate.endRun, $
    pstate.dirIn, pstate.prefixId, pstate.dirOut, pstate.year, pstate.modelName, $
    pstate.postfix, '0']

END