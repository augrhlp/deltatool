FUNCTION buildStordeValues, pstate

 return, [ pstate.starup, pstate.initRun, pstate.endRun, $
    pstate.dirIn, pstate.prefixId, pstate.dirOut, pstate.modelId]

END