function csv2cdf, startUpFile, $
  startHour, endHour, inputDir, outputDir, $
  prefixId, modelName, fulloutFileName, stringStartHour, stringEndHour, $
  ; KeesC 17FEB2015
  logWin=logWin, PROGRESSBAR=PROGRESSBAR,progWIN=progWIN

  ; change this value where you're sure that everything went good and a right cdf was created.
  ERROR=0
  catch, error_status

  if error_status ne 0 THEN BEGIN
    ERROR=1
    catch, /CANCEL
    ; KeesC 27JUN2016: error output text changed
    a=dialog_message([[fulloutFileName], ['Problem in csv2cdf routine']], title='Error')
    return, 0
  endif

  processOK=0
  checkInputDir=strpos(inputDir, path_sep(), /REVERSE_SEARCH)
  checkOutputDir=strpos(outputDir, path_sep(), /REVERSE_SEARCH)
  if strlen(inputDir)-1 ne checkInputDir then inputDir=inputDir+path_sep()
  if strlen(outputDir)-1 ne checkOutputDir then outputDir=outputDir+path_sep()
  ;  startupini=startup   ;'d:\DeltaTool\resource\startup30km.ini'
  ;  modDir=dir_in   ;'d:\DeltaTool\data\modeling\'   ; change into modeling dir
  ;  outFile=dir_out+model   ; d:\DeltaTool\data\cdf_out\2009_MODEL_TIME.cdf'
  ;  startHour=hour0
  ;  endHour=hour1

  if keyword_set(PROGRESS_BAR) then a=dialog_message(title='Csv to Cdf', 'Now start cdf conversion of observation...')
  atxt=' '
  iyear=0 ; normal year
  year=fix(strmid(stringStartHour,0,4))
  day_sum=[0,31,60,91,121,152,182,213,244,274,305,335,366]

  species=ReadSpecStats(startUpFile, logWin=logWin, statnames=statnames, spec_stations=spec_stations, fileyear=fileyear)
  if year ne fileyear then begin
    txt=['STOP','Year from Input Window <> Year from Startup.ini']
    addLogText, logWin, txt
    ierror=1
    return, 0
  endif

  if 4*(year/4) eq year then iyear=1
  hour0=fix(startHour)
  hour1=fix(endHour)
  nspec=n_elements(species) & nstat=n_elements(statnames)
  ;Kees 11 Sept 2015
  polls=fltarr(nspec)

  ;tempFullOutFileName='temp_'+fulloutFileName
  fNamePos=strpos(fulloutFileName, path_sep(), /REVERSE_SEARCH)
  fName=strmid(fulloutFileName, fNamePos+1, strlen(fulloutFileName)-fNamePos)

  folder=strmid(fulloutFileName, 0, fNamePos+1)
  ; KeesC 27JUN2016: write directly to FullOutFileName, temp* is not used anymore; problem in file_remove
  ;  tempFullOutFileName=folder+'temp_'+fName
  ;  idout=ncdf_create(tempFullOutFileName,/clobber)
  idout=ncdf_create(FullOutFileName,/clobber)
  dim_nv=ncdf_dimdef(idout,'V',nspec)
  dim_nt=ncdf_dimdef(idout,'T',hour1-hour0+1)
  vardim=[dim_nv,dim_nt]
  for i=0,nstat-1 do begin
    name_var=statnames(i)
    print,'Define variable '+string(fix(i+1))+'   in OUTPUT file'
    Idvar=ncdf_vardef(idout,name_var,vardim)
  endfor
  pp=''
  for ispec=0,nspec-1 do pp=pp+species(ispec)+' '
  ncdf_attput,idout,'Parameters',byte(pp),/global
  ncdf_attput,idout,'Year',year,/global
  ncdf_attput,idout,'StartHour',hour0,/global
  ncdf_attput,idout,'EndHour',hour1,/global
  ncdf_control,idout,/endef

  ;Kees 11 sept 2015
  polls=fltarr(nspec)
  for is=0,nstat-1 do begin
    storeData=fltarr(nspec,8785) & storeData(*,*)=-999  ;8760
    if strupcase(spec_stations(is)) ne 'NOOBS' and strupcase(spec_stations(is)) ne 'NOVAL' then begin
      hlp1=strtrim(is+1,2)
      hlp2=strtrim(nstat,2)
      ; KeesC 17FEB2015 2 lines
      ;      addLogText, progWin, hlp1+' / '+hlp2+'  [ = nstat ]   ... '+statnames(is)
      widget_control,progWIN,set_value=' '+hlp1+' / '+hlp2+'  [=nstat ]    '+statnames(is)
      wait,.0005
      if prefixId eq '' then begin
        filename=inputDir+statnames(is)+'.csv'
      endif else begin
        fileName=inputDir+prefixId+statnames(is)+'.csv'
      endelse
      res=file_test(fileName)
      if res ne 1 then begin
        fName=strsplit(modelName, '.', /EXTRACT, count=count, /PRESERVE_NULL)
        modelName=modelName+'.csv'
        res=file_test(modelName)
        if res ne 1 then begin
          txt=fileName+' NOT found'
          addLogText, logWin, txt
          ;txtall=['STOP',txt,txtall]
          ;widget_control,labcom_txt,set_value=txtall

          ierror=1
          break
          return, 0
        endif
      endif
      openr, unit, fileName, /GET_LUN
      bufferString=' '
      header=' '
      firstRow=1
      ;read at least one line
      readNext='H'
      while not(eof(unit)) do begin
        readf, unit, bufferString
        checkFirst=strmid(bufferString, 0,1)
        check1=(strpos(checkFirst, '[')+1) > 0
        check2=(strpos(checkFirst, ';')+1) > 0
        check3=(strpos(checkFirst, '#')+1) > 0
        null=strlen(strcompress(bufferString, /REMOVE_all)) eq 0
        if (check1+check2+check3) eq 0 and ~null then begin
          info=strsplit(bufferString, ';', /EXTRACT, count=count, /PRESERVE_NULL)
          info=strcompress(info,/remove_all)
          if firstRow eq 1 then begin
            if strupcase(info[0]) eq 'YEARLYAVG' then begin
              specStat=info(2:n_elements(info)-1)
              fileTime='Y'
              ; read the next -valid- single line
            endif else begin
              specStat=info(4:n_elements(info)-1)
              fileTime='H'
            endelse
            firstRow=0
          endif else begin
            if fileTime eq 'H' then begin
              k1=day_sum(fix(info(1))-1)*24
              k2=(fix(info(2))-1)*24
              k3=fix(info(3))
              k0=k1+k2+k3
              polls(*)=-999
            endif
            for isp=0,nspec-1 do begin
              cc=where(specStat eq species(isp),nc)
              if fileTime eq 'H' then begin
                if nc eq 1 then polls(isp)=float(info(4+cc[0]))
              endif else begin
                if nc eq 1 then storeData[cc[0],*]=float(info(cc[0]))
              endelse
            endfor
            if fileTime eq 'H' then storeData[*,k0]=polls
          endelse
        endif
      endwhile
      if iyear eq 0 then begin   ;normal year: shift 24 hours back
        storeHlp=storeData(*,60*24:366*24-1)  ;01/03/year 0hr - 31/dec/year 23hr
        storeData(*,59*24:365*24-1)=storeHlp
      endif
      close, unit & free_lun, unit
      name_var=statnames(is)
      Idvar=ncdf_varid(idout,name_var)
      ncdf_varput,idout,Idvar,storeData(*,hour0:hour1)
    endif else begin
      name_var=statnames(is)
      Idvar=ncdf_varid(idout,name_var)
      ncdf_varput,idout,Idvar,storeData(*,hour0:hour1)
    endelse
    processOK=1
  endfor
  ncdf_close,idout
  ; only if everything goes fine rename...
  ;KeesC 27JUN2016: changes in next lines, writing directly to FullOutFileName
  ;  if processOK then begin
  ;close, /all
  ;    file_move, tempFullOutFileName, fulloutFileName, /OVERWRITE
  ;  endif else begin
  if processOK eq 0 then begin
    a=dialog_message(title='Wrong conversion', ['Delta can''t proceed, check your conversion settings.'], /ERROR)
    ;    file_delete, tempFullOutFileName
    return, 0
  endelse
  ;widget_control,labpr_txt,set_value=' '
  txt='End CSV_to_CDF'
  addLogText, logWin, txt
  print,'End CSV_to_CDF'
  if keyword_set(PROGRESS_BAR) then a=dialog_message(title='Csv to Cdf', 'Done')
  return, 1

end



