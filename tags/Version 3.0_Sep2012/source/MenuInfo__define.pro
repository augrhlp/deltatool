;********************
@structure_definition
;********************
FUNCTION MenuInfo::getLevelByCode, code, NOTFOUND=NOTFOUND

  allElems=self->getList()
  idx=where(allElems.code eq code, count)
  NOTFOUND=1
  if count ne 0 then begin
    NOTFOUND=0
    return, allElems[idx].level
  endif

END

PRO MenuInfo::addElement, filename, displayName, parentCode, confDir

  allElems=self->getList()
  level=self->getLevelByCode(parentCode)
  newCode=max(allElems.code)+1
  newMenuElement=getFMMenuInfo()
  newMenuElement.code=newCode
  newMenuElement.displayName=displayName
  newMenuElement.fileName=filename
  newMenuElement.isMenu=0
  newMenuElement.level=level+1
  newMenuElement.fatherCode=parentCode
  allElems=[allElems, newMenuElement]
  self->setList, allElems 
  self->writeDataToFile, PATH=confDir
  
END

FUNCTION MenuInfo::findParent, code, ISFOLDER=ISFOLDER, NOTFOUND=NOTFOUND

  allElems=self->getList()
  idx=where(allElems.code eq code, count)
  NOTFOUND=1
  if count ne 0 then begin
    NOTFOUND=0
    if allElems[idx].isMenu then begin
      ISFOLDER=1
      return, code
    endif
    return, allElems[idx].fatherCode
  endif
  return, -999
  
END

FUNCTION MenuInfo::findChilds, code, ISFOLDER=ISFOLDER, NOTFOUND=NOTFOUND

  allElems=self->getList()
  idx=where(allElems.fatherCode eq code, count)
  NOTFOUND=1
  if count ne 0 then begin
    NOTFOUND=0
    ;if allElems[idx].isFather then begin
    ;  ISFOLDER=1
    return, allElems[idx].code
  ;endif
  ;return, allElems[idx.father]
  endif
  return, -999
  
END

FUNCTION MenuInfo::getNamesByCodes, codes

  allElems=self->getList()
  nToFind=n_elements(codes)
  findIndexes=intarr(nToFind)
  for i=0, nToFind-1 do findIndexes[i]=(where(codes[i] eq allElems.code))[0]
  return, allElems[findIndexes].displayName
  
END

FUNCTION MenuInfo::getListAtLevel, level

  list=self->getList()
  
  idxs=where(list.level eq level, count)
  if count ne 0 then return, list[idxs] else return, [-1]
  
END

FUNCTION MenuInfo::getFileNameByCode, code

  list=self->getList()
  
  idxs=where(list.code eq code, count)
  if count ne 0 then return, list[idxs] else return, [-1]
  
END

FUNCTION MenuInfo::getSubMenuList, level, fatherCode, NELEM=NELEM

  list=self->getList()
  
  idxs=where(list.fatherCode eq fatherCode and list.level eq level, NELEM)
  if NELEM ne 0 then return, list[idxs] else return, [-1]
  
END

PRO MenuInfo::setList, list

  ptr_free, self.list 
  self.list=ptr_new(list, /NO_COPY)
  
END

FUNCTION MenuInfo::getList

  if ptr_valid(self.list) then return, *self.list else return, ptr_new()
  
END

FUNCTION MenuInfo::getListNumber

  if ptr_valid(self.list) then if not(obj_valid(*self.list[0])) then return, 0
  return, n_elements(*self.list)
  
END

PRO MenuInfo::streamPrint

  print, '***********************'
  print, '**Start of <',OBJ_CLASS(self),'>**'
  
  print, '**** fileName:<', self.fileName,'>'
  thisList=*self.list
  for i=0, n_elements(thisList)-1 do begin
    print, '**element n.<', strcompress(i, /REMOVE), '>'
    print, '**** code:<', thisList[i].code, '>'
    print, '**** displayName:<', thisList[i].displayName, '>'
    print, '**** description:<', thisList[i].description, '>'
    print, '**'
  endfor
  
  print, '***********************'
  print, '**End of:',OBJ_CLASS(self),'**'
  
END

PRO MenuInfo::writeDataToFile, fileName, PATH=PATH

  if n_elements(fileName) eq 0 then fileName=self.fileName
  ;if n_elements(PATH) ne 0 then fileName= PATH+fileName
  
  ERROR=0
  catch, error_status
  
  if error_status NE 0 THEN BEGIN
    ERROR=1
    catch, /CANCEL
    errMsg=dialog_message('problem with file: <'+fileName+'> check existence or read permission.', /ERROR)
    return
  endif
  
  openw, unit, fileName, /GET_LUN
  
  bufferString=''
  i=0
  header='[BENCHMARK: Code,DisplayName,FileName,Level,FatherCode]'
  printf, unit, header
  
  allElems=self->getList()
  for i=0, n_elements(allElems)-1 do begin
    code=strcompress(allElems[i].code, /REMOVE)
    displayName=allElems[i].displayName
    if allElems[i].isMenu eq 1 then fileName='NONE' else fileName=allElems[i].fileName
    level=strcompress(allElems[i].level, /REMOVE)
    fatherCode=strcompress(allElems[i].fatherCode, /REMOVE)
    printf, unit, code+';'+displayName+';'+fileName+';'+level+';'+fatherCode
  endfor
  close, unit & free_lun, unit
  
END

PRO MenuInfo::FillDataFromFile, fileName

  self.fileName=fileName
  
  ERROR=0
  catch, error_status
  
  if error_status NE 0 THEN BEGIN
    ERROR=1
    catch, /CANCEL
    errMsg=dialog_message('problem with file: <'+fileName+'> check existence or read permission.', /ERROR)
    return
  endif
  
  openr, unit, fileName, /GET_LUN
  
  bufferString=''
  i=0
  menus=getFMMenuInfo()
  while not(eof(unit)) do begin
    readf, unit, bufferString
    i++
    checkFirst=strmid(bufferString, 0,1)
    check1=(strpos(checkFirst, '[')+1) > 0
    check2=(strpos(checkFirst, ';')+1) > 0
    check3=(strpos(checkFirst, '#')+1) > 0
    null=strlen(strcompress(bufferString, /REMOVE)) eq 0
    ; #, ; is a comment
    ; void string string is discarded
    ; [ header is discarded
    if (check1+check2+check3) gt 0 or null then begin
    ;print, 'Discard row', i
    ;print, bufferString
    endif else begin
      info=strsplit(bufferString, ';', /EXTRACT)
      if n_elements(info) eq 5 then begin
        thisMenu=getFMMenuInfo()
        thisMenu.code=fix(info[0])
        thisMenu.displayName=info[1]
        thisMenu.fileName=info[2]
        if strupcase(info[2]) eq 'NONE' then thisMenu.isMenu=1 else thisMenu.isMenu=0
        thisMenu.level=fix(info[3])
        thisMenu.fatherCode=fix(info[4])
        menus=[menus, thisMenu]
      endif else begin
        print, 'Bad conf file at line', i, bufferString
      endelse
    endelse
  endwhile
  close, unit & free_lun, unit
  self.list=ptr_new(menus[1:*], /NO_COPY)
  
END

FUNCTION MenuInfo::Init, fileName

  if not(self -> ConfigurableData::init(filename)) then return, 0
  return, 1
  
END

PRO MenuInfo::CleanUp

  self -> ConfigurableData::cleanUp
  
END

PRO MenuInfo__Define

  Struct = { MenuInfo , $
    Inherits ConfigurableData $
    }
    
END