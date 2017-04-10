;+
; NAME:
;    FSC_BASE_FILENAME
;
; PURPOSE:
;
;    The purpose of this is to extract from a long file path, the
;    base file name. That is, the name of the actual file without
;    the preceeding directory information or the final file extension.
;    The directory information and file extension can be obtained via
;    keywords. The file is named so as not to interfere with FILE_BASENAME,
;    which was introduced in IDL 6.0 and performs a similar function.
;
; AUTHOR:
;
;   FANNING SOFTWARE CONSULTING
;   David Fanning, Ph.D.
;   1645 Sheely Drive
;   Fort Collins, CO 80526 USA
;   Phone: 970-221-0438
;   E-mail: david@idlcoyote.com
;   Coyote's Guide to IDL Programming: http://www.idlcoyote.com/
;
; CATEGORY:
;
;    Utility.
;
; CALLING SEQUENCE:
;
;    baseFilename = FSC_Base_Filename(thePath)
;
; INPUTS:
;
;    thePath:      This is the file path you wish to extract a base file name from.
;                  It is a string variable of the sort returned from Dialog_Pickfile.
;
; KEYWORDS:
;
;    DIRECTORY:      The directory information obtained from the input file path.
;                    The directory always ends in a directory separator character.
;
;    EXTENSION:      The file extension associated with the input file path.
;
;    PATH_SEPARATOR: The string to use as a path separator. If undefined, the output
;                    of PATH_SEP() will be used.
;
; RETURN_VALUE:
;
;    baseFilename:   The base filename, stripped of directory and file extension information.
;
; RESTRICTIONS:
;
;    This is a quick and dirty program. It has been tested on Windows machines and *lightly*
;    tested on UNIX machines. Please contact me at the e-mail address above if you discover
;    problems.
;
; EXAMPLE:
;
;    IDL> thePath = "C:\rsi\idl7.8\lib\jester.pro"
;    IDL> Print, FSC_Base_Filename(thePath, Directory=theDirectory, Extension=theExtension)
;         jester
;    IDL> Print, theDirectory
;         C:\rsi\idl7.8\lib\
;    IDL> Print, theExtension
;         pro
;
;
; MODIFICATION HISTORY:
;
;    Written by: David W. Fanning, 31 July 2003.
;    Modified by KaRo, 13 Feb. 2005 to allow dots in the filename.
;    Added PATH_SEPARATOR keyword. 25 July 2005. DWF.
;    Added ability to recongnize directory by path separator in last character. 19 Sept 2005. DWF.
;    If directory is blank (because a relative filename was passed), set to current directory. 6 Aug 2009. DWF.
;    There were a couple of instances where the directory did NOT end in a path separator. Fixed. 24 Feb 2012. DWF.
;-
;******************************************************************************************;
;  Copyright (c) 2008-2012, by Fanning Software Consulting, Inc.                           ;
;  All rights reserved.                                                                    ;
;                                                                                          ;
;  Redistribution and use in source and binary forms, with or without                      ;
;  modification, are permitted provided that the following conditions are met:             ;
;                                                                                          ;
;      * Redistributions of source code must retain the above copyright                    ;
;        notice, this list of conditions and the following disclaimer.                     ;
;      * Redistributions in binary form must reproduce the above copyright                 ;
;        notice, this list of conditions and the following disclaimer in the               ;
;        documentation and/or other materials provided with the distribution.              ;
;      * Neither the name of Fanning Software Consulting, Inc. nor the names of its        ;
;        contributors may be used to endorse or promote products derived from this         ;
;        software without specific prior written permission.                               ;
;                                                                                          ;
;  THIS SOFTWARE IS PROVIDED BY FANNING SOFTWARE CONSULTING, INC. ''AS IS'' AND ANY        ;
;  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES    ;
;  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT     ;
;  SHALL FANNING SOFTWARE CONSULTING, INC. BE LIABLE FOR ANY DIRECT, INDIRECT,             ;
;  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED    ;
;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;         ;
;  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             ;
;  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT              ;
;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS           ;
;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                            ;
;******************************************************************************************;
FUNCTION FSC_Base_Filename, filename, $
    Directory=directory, $
    Extension=extension, $
    Path_Separator=pathsep
    
  On_Error, 2
  
  ; Default values.
  directory = ""
  extension = ""
  file = ""
  
  ; If there is no filename, return NULL.
  IF (N_Elements(filename) EQ 0) OR (filename EQ "") THEN RETURN, file
  
  ; Is a path separator specified?
  IF N_Elements(pathsep) EQ 0 THEN pathsep = Path_Sep()
  
  ; If the last element of filename is a path separator, then separation is easy.
  IF StrMid(filename, StrLen(filename)-1, 1) EQ pathsep THEN BEGIN
    directory = filename
    RETURN, file
  ENDIF
  
  ; Split the file by the path separator and extract into parts.
  parts = StrSplit(filename, pathsep, /Extract)
  IF StrMid(filename, 0, 1) EQ pathsep AND N_Elements(parts) GT 1 THEN parts[0] = pathsep + parts[0]
  numParts = N_Elements(parts)
  
  ; Put the parts back together after identifying them.
  CASE numParts OF
    1: BEGIN
      subparts = StrSplit(filename, ".", /Extract)
      numsubParts = N_Elements(subparts)
      CASE numsubParts OF
        1: file = subparts[0]
        2: BEGIN
          file = subparts[0]
          extension = subparts[1]
        END
        ELSE: BEGIN
          file = StrJoin(subparts[0:numsubParts-2],'.')
          extension = subparts[numsubParts-1]
        END
      ENDCASE
    END
    
    2: BEGIN
      file = parts[1]
      directory = parts[0] + pathsep
      subparts = StrSplit(file, ".", /Extract)
      numsubParts = N_Elements(subparts)
      CASE numsubParts OF
        1: file = subparts[0]
        2: BEGIN
          file = subparts[0]
          extension = subparts[1]
        END
        ELSE: BEGIN
          file = StrJoin(subparts[0:numsubParts-2],'.')
          extension = subparts[numsubParts-1]
        END
      ENDCASE
    END
    
    ELSE: BEGIN
    
      file = parts[numParts-1]
      subparts = StrSplit(file, ".", /Extract)
      numsubParts = N_Elements(subparts)
      CASE numsubParts OF
        1: file = subparts[0]
        2: BEGIN
          file = subparts[0]
          extension = subparts[1]
        END
        ELSE: BEGIN
          file = StrJoin(subparts[0:numsubParts-2],'.')
          extension = subparts[numsubParts-1]
        END
      ENDCASE
      directory = parts[0]
      FOR j=1,numParts-2 DO BEGIN
        directory = directory + pathsep + parts[j]
      ENDFOR
      directory = directory + pathsep
    END
    
  ENDCASE
  
  ; If the directory is a null string. Make it the current directory.
  IF directory EQ "" THEN CD, CURRENT=directory
  
  ; Does the directory need a final path separator.
  IF (directory NE "") THEN BEGIN
    lastChar = StrMid(directory, 0, 1, /REVERSE_OFFSET)
    IF lastChar NE pathsep THEN directory = directory + pathsep
  ENDIF
  
  RETURN, file
  
END

;+
; NAME:
;       FIXPS
;
; PURPOSE:
;
;       Modifies an IDL-produced PostScript landscape mode file so that the output
;       is right side up rather than upside down.
;
; AUTHOR:
;       FANNING SOFTWARE CONSULTING:
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; CATEGORY:
;
;       Graphics
;
; CALLING SEQUENCE:
;
;       FixPS, inputFile, outputFile
;
; Auguments:
;
;      inputFile:    A IDL-produced PostScript file in Landscape mode.
;
;      outputFile:   The name of the fixed output PostScript file. If not provided, the input
;                    file is overwritten. Assumes proper read/write permission in TEMP directory
;                    and in the directory where the input file is located.
;
;  KEYWORDS:
;
;      A4:           Set this keyword if the PostScript file is using a A4 Europeran sized page.
;      LEDGER:       Set this keyword if the PostScript file is using a US ledger size (11 x 17 inch) page.
;      LEGAL:        Set this keyword if the PostScript file is using a US legal size (8.5 x 14 inch) page.
;      LETTER:       Set this keyword if the PostScript file is using a US letter size (8.5 x 11 inch) page.
;      PAGETYPE:     A generic way to set the page size. A string of "LETTER", "LEDGER", "LEGAL", or "A4".
;                    By default, set to "LETTER".
;      QUIET:        Set this keyword to suppress error messages from the program.
;      SUCCESS:      If this keyword is set to a named variable, then on output the variable will
;                    return a 1 if the operation was successful, and a 0 otherwise. Using this
;                    keyword also supresses the program's ability to "throw" an error. Informational
;                    messages are issued about program developments, but this program will allow the
;                    program caller to decide what to do with unsuccessful program completion.
;
; SIDE EFFECTS and RESTRICTIONS:
;
;       Files that are not currently in Landscape mode will be ignored. Tested with single and
;       multiple page PostScript output from IDL 7.0.1 and 7.1.
;
; MODIFICATION HISTORY:
;
;       Written by: David W. Fanning, 6 August 2009.
;       Change to overwrite input file if output filename is not provided. 6 August 2009. DWF.
;       Incorporated checks for non-landscape mode files and files that have already been fixed. 6 August 2009. DWF.
;       Modified to fix multiple-page PostScript files and to work seamlessly with PS_START output. 8 August 2009. DWF.
;       Ran into a problem in which the PostScript file is stored in the directory pointed
;          to by the IDL_TMPDIR environment variable. Now check to see if the input filename
;          is the same as the output filename and make a change, if necessary. 22 July 2010. DWF.
;        Retreated to standard error handling with ERROR_MESSAGE as there are inevitable errors. 2 August 2010. DWF.
;        Output file was created, even if not used. Now deleting file and issuing messages to
;           explain why output file was not created. 1 November 2010. DWF.
;        Added SUCCESS and QUIET keywords. 15 Novemember 2010. DWF.
;        PostScript file structure changed in IDL 8. Made adjustment to find the
;            PageBoundingBox line. 19 Dec 2010. DWF.
;-
;
;******************************************************************************************;
;  Copyright (c) 2009, by Fanning Software Consulting, Inc.                                ;
;  All rights reserved.                                                                    ;
;                                                                                          ;
;  Redistribution and use in source and binary forms, with or without                      ;
;  modification, are permitted provided that the following conditions are met:             ;
;                                                                                          ;
;      * Redistributions of source code must retain the above copyright                    ;
;        notice, this list of conditions and the following disclaimer.                     ;
;      * Redistributions in binary form must reproduce the above copyright                 ;
;        notice, this list of conditions and the following disclaimer in the               ;
;        documentation and/or other materials provided with the distribution.              ;
;      * Neither the name of Fanning Software Consulting, Inc. nor the names of its        ;
;        contributors may be used to endorse or promote products derived from this         ;
;        software without specific prior written permission.                               ;
;                                                                                          ;
;  THIS SOFTWARE IS PROVIDED BY FANNING SOFTWARE CONSULTING, INC. ''AS IS'' AND ANY        ;
;  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES    ;
;  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT     ;
;  SHALL FANNING SOFTWARE CONSULTING, INC. BE LIABLE FOR ANY DIRECT, INDIRECT,             ;
;  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED    ;
;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;         ;
;  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             ;
;  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT              ;
;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS           ;
;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                            ;
;******************************************************************************************;

PRO FIXPS, in_filename, out_filename, $
    A4=A4, $
    LEDGER=ledger, $
    LEGAL=legal, $
    LETTER=letter, $
    PAGETYPE=pagetype, $
    QUIET=quiet, $
    SUCCESS=success
    
  Compile_Opt idl2
  
  ; Error handling.
  IF Arg_Present(success) THEN BEGIN
    Catch, theError
    IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      success = 0
      IF N_Elements(out_lun) NE 0 THEN Free_Lun, out_lun
      IF N_Elements(in_lun) NE 0 THEN Free_Lun, in_lun
      IF ~Keyword_Set(quiet) THEN Print, !Error_State.MSG
      RETURN
    ENDIF
  ENDIF ELSE BEGIN
    Catch, theError
    IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      IF ~Keyword_Set(quiet) THEN ok = Error_Message()
      success = 0
      IF N_Elements(out_lun) NE 0 THEN Free_Lun, out_lun
      IF N_Elements(in_lun) NE 0 THEN Free_Lun, in_lun
      RETURN
    ENDIF
  ENDELSE
  
  ; Assume success
  success = 1
  
  ; Is there an input filename?
  IF N_Elements(in_filename) EQ 0 THEN BEGIN
    in_filename = Dialog_Pickfile(FILTER='*.ps', TITLE='Select PostScript file to repair.')
    IF in_filename EQ "" THEN RETURN
  ENDIF
  
  ; Is there an output filename?
  IF N_Elements(out_filename) EQ 0 THEN BEGIN
    ;root_name = FSC_Base_Filename(in_filename, EXTENSION=ext)
    ;root_name = FSC_Base_Filename(in_filename, EXTENSION=ext)
    fsm=obj_new('FMFileSystemManager')
    root_name=fsm->getFileName(in_filename)
    ext='ps'
    obj_destroy, fsm
    out_filename = Filepath(ROOT_DIR=GetEnv('IDL_TMPDIR'), root_name + '_tmp.' + ext)
    print_out = 1
    no_output_filename = 1
  ENDIF ELSE no_output_filename = 0
  
  ; The out_filename can be the same as the in_filename in some cases.
  IF out_filename EQ in_filename THEN BEGIN
    rootname = FSC_Base_Filename(out_filename, DIRECTORY=outDir, EXTENSION=ext)
    out_filename = FilePath(ROOT_DIR=outDir, rootname + '_tmp.' + ext)
  ENDIF
  
  ; Open the output filename.
  OpenW, out_lun, out_filename, /GET_LUN
  
  ; What kind of page is this?
  IF N_Elements(pagetype) EQ 0 THEN BEGIN
    IF Keyword_Set(A4) THEN pageType = 'A4'
    IF Keyword_Set(legal) THEN pageType = 'LEGAL'
    IF Keyword_Set(ledger) THEN pageType = 'LEDGER'
    IF Keyword_Set(letter) THEN pageType = 'LETTER'
    IF N_Elements(pageType) EQ 0 THEN pageType = 'LETTER'
  ENDIF ELSE pageType = StrUpCase(pageType)
  
  ; Set the rotate/translate command appropriately.
  CASE pageType OF
    'LETTER': rtcmd = '180 rotate -612 -792 translate'
    'A4':     rtcmd = '180 rotate -595.28 -841.89 translate'
    'LEGAL':  rtcmd = '180 rotate -612 -1008 translate'
    'LEDGER': rtcmd = '180 rotate -792 -1224 translate'
    ELSE: Message, 'Unknown PageType: ' + pageType
  ENDCASE
  
  ; Move along in the file until the end of the Prolog.
  line = ""
  count = 0
  target = "void"
  buffer = StrArr(100)
  
  OpenR, in_lun, in_filename, /GET_LUN
  WHILE target NE '%%EndProlog' DO BEGIN
    ReadF, in_lun, line
    buffer[count] = line
    target = StrMid(line, 0, 11)
    count = count + 1
    IF count MOD 100 EQ 0 THEN buffer = [buffer, StrArr(100)]
  ENDWHILE
  
  ; Read the next 10 lines all at once. If you have already processed
  ; this file, exit. But if you haven't write the buffer to the output file.
  in_lines = StrArr(10)
  ReadF, in_lun, in_lines
  
  ; Is this a landscape file? If not, out of here.
  index = Where(in_lines EQ '%%PageOrientation: Landscape', landscape_cnt)
  IF landscape_cnt EQ 0 THEN BEGIN
    Free_Lun, in_lun
    Free_Lun, out_lun
    File_Delete, out_filename
    Message, 'File not a landscape file. Exiting...', /Informational
    RETURN
  ENDIF
  
  ; Has this file already been mucked with? If so, out of here. If not, write
  ; the buffer to the output file.
  IF StrMid(in_lines[1], 0, 10) EQ '180 rotate' THEN BEGIN
    Free_Lun, in_lun
    Free_Lun, out_lun
    File_Delete, out_filename
    Message, 'File is in the proper rotation. Exiting...', /Informational
    RETURN
  ENDIF ELSE BEGIN
    FOR j=0,count-1 DO PrintF, out_lun, buffer[j]
  ENDELSE
  
  count = count + 10
  
  ; We are going to add an extra line in the output file.
  out_lines = StrArr(11)
  out_lines[0] = in_lines[0]
  out_lines[2:10] = in_lines[1:9]
  out_lines[1] = rtcmd
  
  ; Calculate the new bounding box boundaries.
  bbox_line_num = Where(StrMid(in_lines, 0, 17) EQ '%%PageBoundingBox', bbox_count)
  IF bbox_count EQ 0 THEN Message, 'Cannot find PageBoundingBox line in file.'
  bbox = StrMid(in_lines[bbox_line_num[0]], 18)
  x0 = 0
  x1 = 0
  y0 = 0
  y1 = 0
  ReadS, bbox, x0, y0, x1, y1
  CASE pageType OF
    'LETTER': BEGIN
      lx = String(612 - x1, FORMAT='(I5)')
      ly = String(792 - y1, FORMAT='(I5)')
      ux = String(612 - lx, FORMAT='(I5)')
      uy = String(792 - ly, FORMAT='(I5)')
    END
    'A4': BEGIN
      lx = String(595.28 - x1, FORMAT='(I5)')
      ly = String(841.89 - y1, FORMAT='(I5)')
      ux = String(595.28 - lx, FORMAT='(I5)')
      uy = String(841.89 - ly, FORMAT='(I5)')
    END
    'LEGAL': BEGIN
      lx = String(612 - x1, FORMAT='(I5)')
      ly = String(1008 - y1, FORMAT='(I5)')
      ux = String(612 - lx, FORMAT='(I5)')
      uy = String(1008 - ly, FORMAT='(I5)')
    END
    'LEDGER': BEGIN
      lx = String(792 - x1, FORMAT='(I5)')
      ly = String(1224 - y1, FORMAT='(I5)')
      ux = String(792 - lx, FORMAT='(I5)')
      uy = String(1224 - ly, FORMAT='(I5)')
    END
  ENDCASE
  
  ; Output the new boundaries.
  out_lines[5] = '%%PageBoundingBox: ' + lx + ly + ux + uy
  FOR j=0,10 DO PrintF, out_lun, out_lines[j]
  
  ; Output the rest of the file, looking for another "%%Page:" marker.
  WHILE ~EOF(in_lun) DO BEGIN
    ReadF, in_lun, line
    PrintF, out_lun, line
    IF StrMid(line, 0, 7) EQ '%%Page:' THEN BEGIN
      IF Keyword_Set(A4) THEN BEGIN
        PrintF, out_lun, rtcmd
      ENDIF ELSE BEGIN
        PrintF, out_lun, rtcmd
      ENDELSE
    ENDIF
  ENDWHILE
  
  ; Clean up.
  close, in_lun;/all
  close, out_lun;/all
  Free_lun, in_lun
  Free_lun, out_lun
  
  ; If there was no output filename given, then we are going
  ; to replace the input file with the temporary output file.
  IF no_output_filename THEN BEGIN
    inputDir = File_Dirname(in_filename)
    root_name = File_BaseName(in_filename)
    
    ; Can you write into the input directory?
    IF File_Test(in_filename, /WRITE) EQ 0 THEN Message, 'Cannot write TEMPORARY file into input file directory.'
    
    ; Replace the input file with the temporary output file.
    close, out_lun;/all
    File_Delete, in_filename
    File_Move, out_filename, in_filename
  ENDIF
  
END

FUNCTION Plotter::getSilentMode

  return, self.silent
  
END

PRO Plotter::setSilentMode, value

  self.silent=value
  
END

FUNCTION Plotter::getPSCharSizeFactor

  if self->currentDeviceIsPostscript() then return, self.mainView->getPSCharSizeFactor() else return, 1.
  
END

PRO Plotter::postScriptFixing

  fName=self->getLastPostScriptFileName()
  fInfo=file_info(fName)
  if fInfo.exists and fInfo.size gt 0 then FIXPS, fName, /A4 else print, fName, ' :no ps to fix...', 'Check plot/elaboration routines'
  
  
END

PRO Plotter::setLastPostScriptFileName, fullFileName

  self.lastPostScriptFileName=fullFileName
  
END

FUNCTION Plotter::getLastPostScriptFileName

  return, self.lastPostScriptFileName
  
END

PRO Plotter::erase, color

  if ~self->currentDeviceIsPostscript() then erase, color
  
END

FUNCTION Plotter::sizeCorrection, recArrays, X=X, Y=Y, SYMBOL=SYMBOL

  ;print, '*****'
  for i=0, n_elements(recArrays)-1 do begin
    if ptr_valid(recArrays[i]) then begin
      thisRec=recArrays[i]
      help, *thisRec
      ;print, *recArrays
      ;print, '*****'
      ;print, 'width', (*thisRec)[3,0]-(*thisRec)[0,0]
      ;print, '*****'
      xDiff=abs((*thisRec)[3,0]-(*thisRec)[0,0])
      yDiff=abs((*thisRec)[2,1]-(*thisRec)[0,1])
      minDiff=0.01
      maxDiff=0.05
      if xDiff lt minDiff and keyword_set(X) then begin
        xCenter=mean((*thisRec)[*,0])
        (*thisRec)[0:1,0]=xCenter-minDiff/2
        (*thisRec)[2:3,0]=xCenter+minDiff/2
      endif
      if yDiff lt minDiff and keyword_set(Y) then begin
        yCenter=mean((*thisRec)[*,1])
        (*thisRec)[0,1]=yCenter-minDiff/2
        (*thisRec)[3,1]=yCenter-minDiff/2
        (*thisRec)[1,1]=yCenter+minDiff/2
        (*thisRec)[2,1]=yCenter+minDiff/2
      endif
      if yDiff gt maxDiff and keyword_set(SYMBOL) then begin
        yCenter=mean((*thisRec)[*,1])
        (*thisRec)[0,1]=yCenter-maxDiff/2
        (*thisRec)[3,1]=yCenter-maxDiff/2
        (*thisRec)[1,1]=yCenter+maxDiff/2
        (*thisRec)[2,1]=yCenter+maxDiff/2
      endif
      if xDiff gt maxDiff and keyword_set(SYMBOL) then begin
        xCenter=mean((*thisRec)[*,0])
        (*thisRec)[0:1,0]=xCenter-maxDiff/2
        (*thisRec)[2:3,0]=xCenter+maxDiff/2
      endif
    endif else begin
      polygon=fltarr(4,2)
      polygon[*,0]=[0.,0.,0.,0.]
      polygon[*,1]=[0.,0.,0.,0.]
      recArrays[i]=ptr_new(polygon, /NO_COPY)
    endelse
  endfor
  return, recArrays
  
END

FUNCTION Plotter::getImageType

  return, self.imageType
  
END

FUNCTION Plotter::setImageType, value

  self.imageType=value
  
END

PRO Plotter::setCurrentdevice, deviceName

  devName=deviceName
  starPos=strpos(deviceName, '+')
  if starPos ne -1 then begin
    imageType=strmid(deviceName, starPos+1, strlen(deviceName))
    devName=strmid(deviceName, 0, starPos)
    self.imageType=imageType
  endif else begin
    devName=deviceName
  end
  self.currentDeviceName=devName
  if devName ne 'PS' then set_plot, self.mainDeviceName else set_plot, self.currentDeviceName
  
END

FUNCTION Plotter::currentDeviceIsPostscript

  return, self.currentDeviceName eq 'PS'
  
END

FUNCTION Plotter::getOverplotKeyword, originalValue

  if self->currentDeviceIsPostscript() then return, 1
  return, originalValue
  
END

FUNCTION Plotter::deviceIsOpen

  return, self.deviceIsOpen
  
END

FUNCTION Plotter::getPosition

  ;print, !P.POSITION
  if  self->currentDeviceIsPostscript() then begin
    if self.orientation eq 'LANDSCAPE' then begin
      position=self.position
      position[1]=self.legendSpaceYNorm*(position[3]-position[1])+position[1]
    endif else begin
      ;'PORTRAIT'
      position=self.position
      position[0]=position[0]+0.1
      ;position[2]=position[2]+position[2]/10
      position[1]=self.legendSpaceYNorm*(position[3]-position[1])+position[1]
    ;position[0]=
    ;print, position
    endelse
  ;print, 'position', position
  endif
  return, position
  
END

FUNCTION Plotter::legendNormalize, coords

  if self->currentDeviceIsPostscript() then begin
    normalizeY=self.legendSpaceYNorm*(self.position[3]-self.position[1])
    ;normalizeY=(self.legendSpaceYNorm*.75)*(self.position[3]-self.position[1])
    offSetY=self.position[1]
    
    normalizeX=self.position[2]-self.position[0]
    offSetX=self.position[0]
    
    ;if size(coords, /N_DIMENSIONS) eq 0 then coords=(coords-correction*correction)*correction else coords[1,*]=(coords[1,*]-correction*correction)*correction
    if size(coords, /N_DIMENSIONS) eq 0 then begin
      coords[0]=coords[0]*normalizeX+offSetX
      coords[1]=coords[1]*normalizeY+offSetY
    endif else begin
      coords[0,*]=coords[0,*]*normalizeX+offSetX
      coords[1,*]=coords[1,*]*normalizeY+offSetY
    endelse
    print, coords
    
  endif else begin
    ; no correction
    correction=0
  endelse
  return, coords
  
END

FUNCTION Plotter::plotNormalize, coords

  if self->currentDeviceIsPostscript() then begin
    ;normalizeY=(self.legendSpaceYNorm*.75)*(self.position[3]-self.position[1])
    normalizeY=(self.position[3]-self.position[1])*(1.-self.legendSpaceYNorm)
    legendEnd=self->legendNormalize([0.,1.])
    offSetY=legendEnd[1]
    
    normalizeX=self.position[2]-self.position[0]
    offSetX=self.position[0]
    
    ;if size(coords, /N_DIMENSIONS) eq 0 then coords=(coords-correction*correction)*correction else coords[1,*]=(coords[1,*]-correction*correction)*correction
    if size(coords, /N_DIMENSIONS) eq 0 then begin
      coords[0]=coords[0]*normalizeX+offSetX
      coords[1]=coords[1]*normalizeY+offSetY
    endif else begin
      coords[0,*]=coords[0,*]*normalizeX+offSetX
      coords[1,*]=coords[1,*]*normalizeY+offSetY
    endelse
    
  endif else begin
    ; no correction
    correction=0
  endelse
  return, coords
  
END

PRO Plotter::opendevice, deviceName, fileName, orientation, pageBreak, location, WORKINGDIR=WORKINGDIR, BATCH=BATCH

  self.position=location
  if deviceName eq 'PS' then begin
    ;self.previousPMulti=!P.MULTI[0:2]
    ;!P.MULTI=[0,1,2]
    ;self.currentPMulti=!P.MULTI[0:2]
  
    if ~self.deviceIsOpen then begin
      self.previousDeviceName=!D.NAME
      ;set_plot, deviceName
      self->setCurrentdevice, deviceName
      self.deviceIsOpen=1
      self.orientation=orientation
      fsm=obj_new('FMFileSystemManager')
      psExtension=fsm->getPSExtension()
      if n_elements(WORKINGDIR) eq 0 then saveDir=fsm->getSaveDir(/WITH) else saveDir=WORKINGDIR
      if orientation eq 'LANDSCAPE' then begin
        device, /LANDSCAPE
      endif else begin
        device, /PORTRAIT
        ;check right definition of A4 size...
        device,/INCHES,XSIZE=8.3,SCALE_FACTOR=1.
        device,/INCHES,YSIZE=11.7,SCALE_FACTOR=1.
      endelse
      if fileName ne '' then fileName=fsm->setExtension(fileName, psExtension)
      if not(keyword_set(BATCH)) then begin
        repeat begin
          filter=['*'+psExtension]
          fix_filter='*'+psExtension
          fullFileName=dialog_pickfile(DEFAULT_EXTENSION=psExtension, $
            DIALOG_PARENT=self.mainView->getTopBase(), $
            FILTER=filter, FIX_FILTER=fix_filter, $
            GET_PATH=path, PATH=saveDir, $
            file=fileName, $
            TITLE='Postscript selection', /OVERWRITE_PROMPT, /WRITE)
        endrep until fullFileName ne ''
        fullFileName=fsm->setExtension(fullFileName, psExtension)
      endif else begin
        fullFileName=saveDir+fileName
      endelse
      ;fullFileName=dir+fileName
      self->setLastPostScriptFileName, fullFileName
      device, /ENCAPSULATED, BITS_PER_PIXEL=24, file=fullFileName, /COLOR
    endif
    ;print, '!D.NAME', 'self.currentDeviceName', 'self.previousDeviceName'
    ;print, !D.NAME, self.currentDeviceName, self.previousDeviceName
    return
  endif
  
  ; IMAGE and standards (X, WIN)
  if deviceName ne 'PS' then begin
    self.deviceIsOpen=1
    self.previousDeviceName=!D.NAME
    self->setCurrentdevice, deviceName
    ;print, '!D.NAME', 'self.currentDeviceName', 'self.previousDeviceName'
    ;print, !D.NAME, self.currentDeviceName, self.previousDeviceName
    return
  endif
  
END

PRO Plotter::mosaicImages, fileName, multipleDrawMainTitle

  width=0
  height=0
  shiftLocW=0.
  shiftLocX=0.
  for i=0, n_elements(self.storedImage)-1 do begin
    if ptr_valid(self.storedImage[i]) then begin
      location=*(self.pageLocation[i])
      image=*(self.storedImage[i])
      dims=size(image, /DIM)
      width=max(dims[1],width)
      height=max(dims[2],height)
      shiftLocW=max(location[0], shiftLocW)
      shiftLocH=max(location[1], shiftLocH)
    ;tv, image, true=1
    ;print, location
    endif
  endfor
  
  half=where(shiftLocW ge 0.4 and shiftLocW le 0.6, count)
  if count eq 1 then wMagn=2 else wMagn=1
  half=where(shiftLocH ge 0.4 and shiftLocH le 0.6, count)
  if count eq 1 then hMagn=2 else hMagn=1
  destImg=bytarr(3, wMagn*width+5, hMagn*height+5)
  for i=0, n_elements(self.storedImage)-1 do begin
    if ptr_valid(self.storedImage[i]) then begin
      location=*self.pageLocation[i]
      sourceImg=*self.storedImage[i]
      zero=where(location ge 0 and location lt 0.1, count)
      if count gt 0 then location[zero]=0.
      half=where(location ge 0.4 and location le 0.6, count)
      if count gt 0 then location[half]=0.5
      whole=where(location ge 0.85 and location le 1, count)
      if count gt 0 then location[whole]=1
      location[[0,2]]=location[[0,2]]*wMagn*width
      location[[1,3]]=location[[1,3]]*hMagn*height
      ; shifting a little bit to avoid collate effect
      if location[0] gt 0 then location[[0,2]]=location[[0,2]]+2
      if location[1] gt 0 then location[[1,3]]=location[[1,3]]+2
      destImg[*, location[0]:location[2]-1, location[1]:location[3]-1]=sourceImg[*,*,*]
      continue
    endif
    break
  endfor
  print, self.imageType
  rasterFormat=getenv('RASTER_FORMAT')
  if rasterFormat eq '' then rasterFormat='bmp'
  fsm=obj_new('FMFileSystemManager')
  findExt=strpos(fileName, '_', /REVERSE_SEARCH)
  if findExt gt 1 then fileName=strmid(fileName, 0, findExt)
  newFileName=fsm->setExtension(fileName, '.'+strlowcase(rasterFormat))
  print, fileName
  obj_destroy, fsm
  dims=size(destImg, /DIM)
  destImg=self->mapTitle(destImg, multipleDrawMainTitle)
  write_image, newFileName, rasterFormat, destImg
  msg=self.mainView->dialogMessage(['Image saved in:', '<'+newFileName+'> file.', 'format = '+rasterFormat], title=['Image'], /INFORMATION)
;dialog... confirm
  
END

FUNCTION Plotter::mapTitle, destImg, multipleDrawMainTitle

  white=obj_new('Color', 200, 200, 200)
  black=obj_new('Color', 0, 0, 0)
  dims=size(destImg, /DIM)
  font=self.mainView->getFontMgr()
  fontDesc=font->getFontByIndex(0)
  setUserFont, 'MainTitleFont'
  titleHeight=50
  shiftPos=get_screen_size()
  window, 1, xsize=dims[1], xpos=shiftPos[0]+1, ysize=titleHeight, ypos=shiftPos[1]+1, /PIXMAP
  erase, white->AsLongTrueColor()
  xyouts, 0.5, 0.5, multipleDrawMainTitle, ALIGN=0.5, color=black->asLongTrueColor(), /NORM
  titleImage=tvrd(/TRUE)
  newImg=bytarr(3, dims[1], dims[2]+titleHeight)
  newImg[*,*,0:dims[2]-1]=destImg[*,*,*]
  newImg[*,*,dims[2]:dims[2]+titleHeight-1]=titleImage[0:2,0:dims[1]-1,0:titleHeight-1]
  setUserFont, /RESET
  obj_destroy, black
  obj_destroy, white
  return, newImg

END


PRO Plotter::addMultiPageRaster, image, location

  for i=0, n_elements(self.pageLocation)-1 do begin
    if not(ptr_valid(self.pageLocation[i])) then begin
      self.pageLocation[i]=ptr_new(location, /NO_COPY)
      self.storedImage[i]=ptr_new(image, /NO_COPY)
      break
    endif
  endfor
  
END

PRO Plotter::cleanMultiPageRaster

  for i=0, n_elements(self.pageLocation)-1 do begin
    ptr_free, self.pageLocation[i]
    ptr_free, self.storedImage[i]
  endfor
  
END

FUNCTION Plotter::isMultiPageRaster

  if ptr_valid(self.pageLocation[0]) then return, 1 else return, 0
  
END

PRO Plotter::closedevice, pageBreak, fileName, printOrientation, multipleDrawMainTitle, WORKINGDIR=WORKINGDIR

  ;!p.position=[0.,0.,0.,0.]
  if self->currentDeviceIsPostscript() then begin
    if pageBreak eq 'TRUE' then erase ;newPage
    ;if pageBreak eq 'FALSE' then ;do nothing
    if pageBreak eq 'CLOSE' then begin
      device, /CLOSE_FILE
      self.deviceIsOpen=0
      ;set_plot, self.previousDeviceName
      self->setCurrentdevice, self.previousDeviceName
      ;print, 'printOrientation: ', printOrientation
      self->postScriptFixing
    ;if printOrientation eq 'LANDSCAPE' then self->postScriptFixing
    endif
    print, '!D.NAME', 'self.currentDeviceName', 'self.previousDeviceName'
    print, !D.NAME, self.currentDeviceName, self.previousDeviceName
    return
  endif
  
  if self.currentDeviceName eq 'IMAGE' then begin
  
    if pageBreak eq 'CLOSE' or pageBreak eq 'TRUE' then begin
      if self->isMultiPageRaster() then begin
        self->addMultiPageRaster, self.mainView->getImageToSave(), self.position
        self->mosaicImages, fileName, multipleDrawMainTitle
      endif else begin
        self.mainView->saveImage, fileName, self.imageType, /NO_CONFIRM, WORKINGDIR=WORKINGDIR
      endelse
      self.deviceIsOpen=0
      self->cleanMultiPageRaster
      ;set_plot, self.previousDeviceName
      self->setCurrentdevice, self.previousDeviceName
      return
    endif
    self->addMultiPageRaster, self.mainView->getImageToSave(), self.position
    print, 'pageBreak:', pageBreak
    ;print, '!D.NAME', 'self.currentDeviceName', 'self.previousDeviceName'
    ;print, !D.NAME, self.currentDeviceName, self.previousDeviceName
    return
  endif
  
  if ~self->currentDeviceIsPostscript() then begin
    ;set_plot, self.previousDeviceName
    self->setCurrentdevice, self.previousDeviceName
    self.deviceIsOpen=0
    ;print, '!D.NAME', 'self.currentDeviceName', 'self.previousDeviceName'
    ;print, !D.NAME, self.currentDeviceName, self.previousDeviceName
    return
  endif
  
END

PRO Plotter::saveImage

  self.mainView->saveImage, self.imageType
  
END

PRO Plotter::wsetInfoDataDraw

  if ~self->currentDeviceIsPostscript() then self.mainView->wsetInfoDataDraw
  
END

PRO Plotter::wsetMainDataDraw, request, result, PROGRESS=PROGRESS

  if ~self->currentDeviceIsPostscript() then begin
    self.mainView->wsetMainDataDraw
    if keyword_set(PROGRESS) then begin
      ; only for test
      erase, 10
      xyouts, 0.5, 0.0, 'Plot in progress...', ALIGN=0.5, /NORM, CHARSIZE=3., CHARTHICK=3., FONT=0
      if n_elements(request) eq 1 then begin
        infoToShow=strarr(9)
        infoToShow[0]='ElaborationCode: '+request->getElaborationCode()
        ;infoToShow[1]='is Single Obs Present:'+request->isSingleObsPresent()
        ;infoToShow[2]='isGroupObsPresent: '+request->isGroupObsPresent()
        ;infoToShow[4]='ParameterCodes: '+request->getParameterCodes()
        infoToShow[1]='ElaborationName: '+request->getElaborationName()
        ;infoToShow[2]='ElaborationOCTimeAvgName: '+request->getElaborationOCTimeAvgName()
        ;infoToShow[3]='ElaborationOCStat: '+request->getElaborationOCStat()
        infoToShow[2]='Diagram Code: '+request->getDiagramCode()
        infoToShow[3]='Diagram Name: '+request->getDiagramName()
        multiple=request->getMultipleChoiceUserSelectionFlags()
        mult=''
        for j=0, 3 do mult=mult+strcompress(fix(multiple[j]), /REMOVE)
        infoToShow[4]='getMultipleChoiceUserSelectionFlags: '+mult
        infoToShow[5]='No single obs'
        sobsnames=request->getSingleObsNames()
        mult=''
        for j=0, n_elements(sobsnames)-1 do mult=mult+strcompress(sobsnames[j], /REMOVE)+';'
        infoToShow[5]='Obs: '+mult
        infoToShow[6]='No groups'
        if request->isGroupObsPresent() then begin
          groupTitles=request->getGroupTitles()
          ngroups=n_elements(groupTitles)
          mult=''
          for j=0, ngroups-1 do mult=mult+strcompress(groupTitles[j], /REMOVE)+';'
          infoToShow[6]='Group Obs: '+mult
        endif
        ;infoToShow[6]='UseObservedModel: '+request->getUseObservedModel()  ; 0=0ld case; 1=no obs
        ;KeesC 12MAR2017: Pb next line 'request->getModelCodes()' is string array and can not be assigned to infotoshow[7]
        hlp=request->getModelCodes()
        hlp0=hlp[0]
        if n_elements(hlp) ge 2 then begin
          for ih=1,n_elements(hlp)-1 do hlp0=hlp0+';'+hlp[ih]
        endif
        infoToShow[7]='ModelCodes: '+hlp0     ;'ModelCodes: '+request->getModelCodes()
        hlp=request->getScenarioCodes()
        hlp0=hlp[0]
        if n_elements(hlp) ge 2 then begin
          for ih=1,n_elements(hlp)-1 do hlp0=hlp0+';'+hlp[ih]
        endif
        infoToShow[8]='ScenarioCodes: '+hlp0  ;'ScenarioCodes: '+request->getScenarioCodes()
        for i=0, n_elements(infoToShow)-1 do begin
          if i eq 5 and strlen(infoToShow[5]) gt 150 then infoToShow[5]=strmid(infoToShow[i],0,150) 
 ;         xyouts, 0.2, 0.2+float(i)/n_elements(infoToShow), infoToShow[i], ALIGN=0.5, /NORM, CHARSIZE=3., CHARTHICK=3., FONT=0
          xyouts, 0.05, 0.1+0.75*float(i)/n_elements(infoToShow), infoToShow[i], /NORM, CHARSIZE=3., CHARTHICK=3., FONT=0
        endfor
      endif
    ;      DEVICE, GET_FONTNAMES=fnames, SET_FONT='*'
    ;      for i=0, n_elements(fnames)-1 do begin
    ;        device, set_font=fnames[i]+'*15*LIGHT*ITALIC'
    ;        erase, 10
    ;        xyouts, 0.5, 0.5, 'Plot in progress', ALIGN=0.5, /NORM, CHARSIZE=3., CHARTHICK=3., FONT=0
    ;        wait, 0.1
    ;      endfor
    ;      DEVICE, GET_FONTNAMES=fnames, SET_FONT='*', /TT_FONT
    ;      for i=0, n_elements(fnames)-1 do begin
    ;        device, set_font=fnames[i]+'*15*LIGHT*ITALIC'
    ;        erase, 10
    ;        xyouts, 0.5, 0.5, 'Plot in progress', ALIGN=0.5, /NORM, CHARSIZE=3., CHARTHICK=3., FONT=1
    ;        wait, 0.1
    ;      endfor
    endif
  endif
  device, /DECOMPOSE
  
END

FUNCTION Plotter::buildXRange

  return, self->getStartIndex()+indgen(self->getEndIndex()-self->getStartIndex())
  
END

PRO Plotter::setStartIndex, value

  self.startIndex=value
  
END

FUNCTION Plotter::getStartIndex

  return, self.startIndex
  
END

PRO Plotter::setEndIndex, value

  self.endIndex=value
  
END

FUNCTION Plotter::getEndIndex

  return, self.endIndex
  
END

PRO Plotter::plotAll, request, result

  ;ToDo: call procedures from "diagram.dat" config file
  ; plot procedures may be found in "plot_routines.pro" file
  call_procedure, request.plotRoutine, self, request, result
  ;self->closePage
  call_procedure, request.plotRoutine+'Legend', self, request, result
;self->closePage
;Previous version (using internal objects)
;  call_method, request.plotRoutine, self, request, result
;  call_method, request.plotRoutine+'Legend', self, request, result
  
END

;PRO Plotter::plotStandardLegend, request, result
;
;  self.mainView->wsetInfoDataDraw
;
;END

PRO Plotter::setInfoDataDrawArea

  if ~self->currentDeviceIsPostscript() then self.mainView->wsetInfoDataDraw
  device,/DECOMPOSE
  
END

PRO Plotter::setMainDataDrawArea

  if ~self->currentDeviceIsPostscript() then self.mainView->wsetMainDataDraw
  device,/DECOMPOSE
  
END

PRO Plotter::plotBarsLegend, request, result

  self->setInfoDataDrawArea
  white=obj_new('Color', 200, 200, 200)
  black=obj_new('Color', 0, 0, 0)
  whiteL=white->AsLongTrueColor()
  blackL=black->AsLongTrueColor()
  obj_destroy, white
  obj_destroy, black
  device, DECOMPOSED=1
  self->erase, whiteL
  ;  if ~self->currentDeviceIsPostscript() then begin
  ;    erase, whiteL
  ;    offset=0.
  ;  ;self.yoffset=0
  ;  endif else begin
  ;    offset=0.
  ;  ;self.yoffset=self.postscriptYoffset
  ;  endelse
  bpInfo=Result->getBarPlotInfo()
  plotInfo=result->getPlotInfo()
  legoSequenceNo=bpInfo->getLegoSequenceNumber()
  legoSequenceNames=bpInfo->getLegoSequenceNames()
  legoNames=bpInfo->getLegoNames()
  
  psyms=[2,1,4,5,6,7]
  legoWidth=.05
  legoHeight=.05
  startX=.01
  maxWidth=0
  
  for i=0, legoSequenceNo-1 do begin
    startY=1.-((i+1)*legoHeight*2)
    lego=[[startX,startY], [startX,startY+legoHeight], [startX+legoWidth,startY+legoHeight], [startX+legoWidth,startY]]
    if result->HaveObs() then begin
      legoCoords=self->legendNormalize(lego)
      polyfill, legoCoords, color=self->getLegoColorAtIndex(i), /NORM
      xyOutsCoord=self->legendNormalize([startX+legoWidth+.01, startY+.002])
      xyouts, xyOutsCoord[0], xyOutsCoord[1], 'Observations', COLOR=blackl, /NORM, charsize=.8, charthick=.8,  WIDTH=textWidth
      maxWidth=max([textWidth, maxWidth])
    endif
  ;xyouts, startX+legoWidth+.01, startY+.002, legoSequenceNames[i], COLOR=blackl, /NORM, charsize=.8, charthick=.8,  WIDTH=textWidth
  endfor
  
  ;  if criteria gt 0 then plots,[
  
  symbolSequenceNo=bpInfo->getSymbolSequenceNumber()
  if symbolSequenceNo gt 0 then begin
    symbolSequenceNames=bpInfo->getSymbolSequenceNames()
    symbolPSym=bpInfo->getPSymUsed()
    symbolPSymNo=n_elements(symbolPSym)
  endif
  
  for i=0, symbolSequenceNo-1 do begin
    startY=1.-((i+1)*legoHeight*2)
    thisStartX=startX+maxWidth+legoWidth+.02
    lego=[[thisStartX,startY], [thisStartX,startY+legoHeight], [thisStartX+legoWidth,startY+legoHeight], [thisStartX+legoWidth,startY]]
    legoCoords=self->legendNormalize(lego)
    plots, legoCoords, color=self->getSymbolColorAtIndex(i), psym=symbolPSym[i mod symbolPSymNo], /NORM
    xyOutsCoords=self->legendNormalize([thisStartX+legoWidth+.01, startY+.002])
    xyouts, xyOutsCoords[0], xyOutsCoords[1], symbolSequenceNames[i], COLOR=blackl, /NORM, charsize=.8, charthick=.8,  WIDTH=textWidth
  endfor
  
END

PRO Plotter::plotLegoAndSymbols, outLegoCoords, outSymbolCoords, legoValues, barName, symbolValues=symbolValues, $
    title=title, valuesRange=valuesRange, legonames=legonames, $
    legoColors=legoColors, symbolColors=symbolColors, baselines=baselines,$
    xtitle=xtitle,ytitle=ytitle,baserange=baserange, outTickPos=outTickPos, $
    barwidth=barwidth,barspace=barspaceIn,baroffset=baroffset, $
    outline=outline,overplot=overplot,background=background, $
    rotate=rotate, usedPsym=usedPsym, FIRST=FIRST, DRAWAXIS=DRAWAXIS, tickvs=tickvs
    
  nbars=n_elements(legoValues)    ; Determine number of bars
  allVals=legoValues
  legoNans=finite(legovalues)
  nSymbols=0
  if keyword_set(symbolValues) then begin
    nSymbols=n_elements(symbolValues)
    symbolNans=finite(symbolValues)
    ; Default sym colors spaced evenly in current color table
    ;    if not(keyword_set(symbolColors)) then $
    ;      symbolColors=256-fix(((!d.n_colors < 256)/float(nColors+1))*(Lindgen(nColors+1)+0.5))
    allVals=[allVals, reform(symbolValues, n_elements(symbolValues))]
    outSymbolCoords=ptrarr(nSymbols)
  endif
  nColors=1+nSymbols
  ; Baselines (bars extend from baselines through values); default=0
  if not(keyword_set(baselines)) then baselines=intarr(nbars)
  ; Default colors spaced evenly in current color table
  ;  if not(keyword_set(legoColors)) then $
  ;    legoColors=fix(((!d.n_colors < 256)/float(nColors+1))*(Lindgen(nColors+1)+0.5))
  ; Labels for the individual bars; none by default
  ;barnames = (N_Elements(barnamesIn) gt 0) ? barnamesIn : strarr(nbars)+' '
  ; Main title
  if not(keyword_set(title)) then title=''
  ; Centered title under X-axis
  if not(keyword_set(xtitle)) then xtitle=''
  ; Title for Y-axis
  if not(keyword_set(ytitle)) then ytitle=''
  ; Fraction (0-1) of full X range to use
  if not(keyword_set(baserange)) then baserange=1.0
  ; Space betw. bars, taken from nominal bar widths; default is none
  barspace = (N_Elements(barspaceIn) gt 0) ? Float(barspaceIn) : 0.2
  ; Bar width scaling factor, relative to nominal
  if not(keyword_set(barwidth)) then barwidth=1.0 - barspace - barspace / nbars
  ; Initial X offset, in scaled bar widths; default is none
  if not(keyword_set(baroffset)) then baroffset=barspace/barwidth
  ; Outline of bars; default is none
  outline = keyword_set(outline)
  ; Overplot (do not erase the existing display); default is to create new plot
  overplot = keyword_set(overplot)
  ; Background color index; defaults to 0 (usually black) if not specified
  if not(keyword_set(background)) then background=0
  ; Rotate (make horizontal bars); default is vertical bars
  rotate = keyword_set(rotate)
  xRange=[0.,1.]
  mnB = MIN(baselines, MAX=mxB, /NAN)
  mnV = MIN(allVals, MAX=mxV, /NAN)
  
  range=[mnB < mnV, $    ;Minimum of bases & values
    mxB > mxV]      ;Maximum of bases & values
    
  if (rotate) then begin           ;Horizontal bars
    if (!x.range[0] eq 0) and (!x.range[1] eq 0) $  ;Determine range for X-axis
      then xrange=valuesRange $
    else xrange=valuesRange;!x.range         ;Or, use range specified
    if (!y.range[0] eq 0) and (!y.range[1] eq 0) $  ;Plot will calculate
      then $                                       ; defaults for X, but not
      yrange = [0., n_elements(values)] $         ; for Ys, so fill in here.
    else $
      yrange=!y.range          ;Axis perpend. to bars
    yticks=1            ;Suppress ticks in plot
    ytickname=strarr(2)+' '
    xticks=0
    xtickname=strarr(1)+''
  endif else begin           ;Vertical bars
    if (!y.range[0] eq 0) and (!y.range[1] eq 0) $  ;Determine range for Y-axis
      then yrange=valuesRange $
    else yrange=valuesRange;!y.range                 ;Or, use range specified
    xrange=!x.range           ;Axis perpend. to bars
    xticks=1            ;Suppress ticks in plot
    xtickname=strarr(2)+' '
    yticks=0
    ytickname=strarr(1)+''
  endelse
  if (keyword_set(FIRST)) then begin        ;Create new plot, no data
  
    plot,valuesRange,/nodata,title=title,xtitle=xtitle,ytitle=ytitle, $
      ;noerase=overplot,xrange=xrange,yrange=yrange,xticks=xticks, $
      noerase=1,xrange=xrange,yrange=yrange,xticks=xticks, $
      xtickname=xtickname,yticks=yticks,ytickname=ytickname, $
      xstyle=1,ystyle=1,/data,background=background,color=0, position=self->getPosition()
  endif
  if (rotate) then begin           ;Horizontal bars
    base_win=!y.window          ;Window range in Y
    scal_fact=!x.s          ;Scaling factors
    tick_scal_fact=!y.s           ;Tick scaling factors
  endif else begin           ;Vertical bars
    base_win=!x.window          ;Window range in X
    scal_fact=!y.s          ;Scaling factors
    tick_scal_fact=!x.s           ;Tick scaling factors
  endelse
  winrange=baserange*(base_win[1]-base_win[0])     ;Normal. window range
  barsize=barwidth*winrange/nbars        ;Normal. bar width
  winoffset=base_win[0]+(baroffset*barsize)    ;Normal. first offset
  bases=scal_fact[0]+(scal_fact[1]*baselines)    ;Baselines, in normal coor.
  normal=scal_fact[0]+(scal_fact[1]*legoValues)    ;Values, in normal coor.
  if keyword_set(symbolValues) then begin
    symNormal=scal_fact[0]+(scal_fact[1]*symbolValues)    ;SymValues, in normal coor.
    usedPsym=[2,1,4,5,6,7]
    symBarPos=nbars/2
  endif
  barstart=Lindgen(nbars)*(barsize+barspace*(winrange/nbars)) ;Coor. at left edges
  tickv=winoffset+barstart+(0.5*barsize)       ;Tick coor. (centered)
  ;print, '-->', barsize
  if not(keyword_set(FIRST)) then begin
    for i=0,nbars-1 do begin         ;Draw the bars
      width=winoffset+[barstart[i],barstart[i], $     ;Compute bar width
        (barstart[i]+barsize),(barstart[i]+barsize)]
      length=[bases[i],normal[i],normal[i],bases[i]]  ;Compute bar length
      idxs=where(width[*] gt 1., count)
      if count gt 0 then width[idxs]=.95
      if keyword_set(symbolValues) then begin
        widthMiddle=winoffset+(barstart[i]+barsize)/2
        lengthMiddle=symNormal[*]
      endif
      if (rotate) then begin        ;Horizontal bars
        x=length             ;X-axis is "length" axis
        y=width            ;Y-axis is "width" axis
        if legonans eq 1 then begin
          outLegoCoordsV=[[x],[y]]
          polyfill,x,y,color=colors[i],/normal       ;Polyfill with color
        endif
        outTickPos=(x[0]+x[1])/2
        if keywod_set(symbolValues) then begin
          xSym=lengthMiddle             ;X-axis is "length" axis
          ySym=(width[0]+width[2])/2            ;Y-axis is "width" axis
          if i eq symBarPos then begin
            ;print, 'nSymbols-->', nSymbols
            for j=0, nSymbols-1 do begin
              if symbolNans[j] eq 1 then begin
                outSymbolCoords=ptr_new([xSym[j], ySym], /NO_COPY)
                plots, xSym[j], ySym,color=symbolColors[j], /normal, psym=usedPsym[j mod 6], SYMSIZE=1.
              endif
            endfor
          endif
        endif
      endif else begin          ;Vertical bars
        x=width            ;X-axis is "width" axis
        y=length            ;Y-axis is "length" axis
        outLegoCoordsV=[[x],[y]]
        legoDims=(x[3]-x[1])/2
        if legoNans eq 1 then begin
          polyfill,x,y,color=legoColors[i],/normal       ;Polyfill with color
        endif
        ;test text
        plots, [(x[0]+x[2])/2, (x[0]+x[2])/2], [y[0], y[0]-.02], /NORMAL, color=0
        xyouts, x[0], y[0]-.04, barname, /NORMAL, ALIGN=0., COLOR=0
        
        outTickPos=(x[0]+x[1])/2
        if keyword_set(symbolValues) then begin
          xSym=(width[0]+width[2])/2            ;X-axis is "width" axis
          ySym=lengthMiddle             ;Y-axis is "length" axis
          if i eq symBarPos then begin
            ;print, 'nSymbols-->', nSymbols
            for j=0, nSymbols-1 do begin
              if symbolNans[j] eq 1 then begin
                symX=outLegoCoordsV[*,0]
                ;symX=outTickPos
                outSymbolCoords[j]=ptr_new([[symX],[ySym[j]-legoDims, ySym[j]+legoDims, ySym[j]+legoDims, ySym[j]-legoDims]], /NO_COPY)
                plots, xSym, ySym[j],color=symbolColors[j], /normal, psym=usedPsym[j mod 6], thick=3,SYMSIZE=1.5
              endif
            endfor
          endif
        endif
      endelse
      if (outline) and (legoNans eq 1) then begin
        plots,x,y,/normal       ;Outline using !p.color
      endif
    endfor
  endif
  outLegoCoords=ptr_new(outLegoCoordsV, /NO_COPY)
  
;  if (keyword_set(DRAWAXIS)) then begin
;    if (rotate) then begin           ;Label the bars (Y-axis)
;      axis,yaxis=0,ystyle=1,yticks=(nbars-1),ytickv=tickvs,ytickname=legonames, $
;        yticklen=0.0, /NORMAL
;    endif else begin             ;Label the bars (X-axis)
;      legonumbers=n_elements(legonames)
;      tickvs[legonumbers-1]=outTickPos
;      axis,xaxis=0,xstyle=1,xticks=legonumbers-1,xtickv=tickvs,xtickname=legonames, $
;        xticklen=0.0, /NORMAL
;    endelse
;  endif
  
END

PRO Plotter::plotBars, request, result

  self->setMainDataDrawArea
  self->setStartIndex, request->getStartPlotIndex()
  self->setEndIndex, request->getEndPlotIndex()
  plotNo=result->getRawElementNumber()
  plotInfo=result->getPlotInfo()
  ;  singleRawData=result->getSingleRawData()
  ;  singlerDMatrix=result->getSingleRawDataCheckMatrix(monitIndexes, runIndexes)
  
  elabcode=request->getElaborationCode()
  if elabcode eq 2 then statistics='R'
  if elabcode eq 7 then statistics='IOA'
  if elabcode eq 8 then statistics='RDE'
  if elabcode eq 23 then statistics='NMB'
  if elabcode eq 28 then statistics='Target'
  if elabcode eq 30 then statistics='RPE'
  if elabcode eq 54 then statistics='MFS'
  if total(where(elabCode eq [2,7,8,23,28,30,54])) ge 0 then begin
    ; KeesC 30MAR2012: undefined obsTemp
    ;    CheckCriteria, request, result, statistics, criteria, obsTemp, 0,alpha,criteriaOrig,LV,nobsAv
    ; changed into dummy
    adummy=fltarr(10) & adummy(*)=1.
    CheckCriteria, request, result, statistics, criteria, adummy,alpha,criteriaOrig,LV
  endif else begin
    criteria=0
  endelse
  
  title=plotInfo->getTitle()
  xTitle=plotInfo->getXTitle()
  yTitle=plotInfo->getYTitle()
  xRange=plotInfo->getXRange()
  yRange=plotInfo->getYRange()
  xTicks=plotInfo->getxTicks()
  xTickNumber=plotInfo->getXTickNumber()
  xTickNames=plotInfo->getXTickNames(self->getStartIndex(), self->getEndIndex(), /SUBSET)
  yTicks=plotInfo->getyTicks()
  yTickNames=plotInfo->getyTickNames()
  xStyle=plotInfo->getxStyle()
  yStyle=plotInfo->getyStyle()
  ;position=plotInfo->getyStyle()
  xCharSize=plotInfo->getXCharSize()
  yCharSize=plotInfo->getYCharSize()
  charsize=plotInfo->getcharSize()
  
  bpInfo=Result->getBarPlotInfo()
  symbolSequencesNo=0
  if bpInfo->hasSymbolValues() then begin
    symbolSequencesNo=bpInfo->getSymbolSequenceNumber()
    symbolColors=self->buildSymbolColors(symbolSequencesNo)
    symbolValues=bpInfo->getSymbolValues()
  endif
  legoSequenceNo=bpInfo->getLegoSequenceNumber()
  legoNumbers=bpInfo->getLegoNumbers()
  legoColors=self->buildLegoColors(legoSequenceNo)
  valuesRange=bpInfo->getValuesRange()
  legoValues=bpInfo->getLegoValues()
  legoNames=bpInfo->getLegoNames()
  
  white=obj_new('Color', 255, 255, 255)
  whiteL=white->AsLongTrueColor()
  obj_destroy, white
  ;self.mainView->wsetMainDataDraw
  device, DECOMPOSED=1
  ;    device,DECOMPOSE=0
  LOADCT,39
  ; use "tek" color table...
  ;    mytek_color;, 0, 32
  ;Make axes black:
  ;!P.COLOR=0
  self->erase, whiteL
  ;if ~self->currentDeviceIsPostscript() then erase, whiteL
  
  ;BAR_PLOT_EXTRA, legoValues[0:legoSeries-1,0], symbolValues[0:symbolSeries-1,0], COLORS=legoColors, BACKGROUND=255, $
  ;BARWIDTH=0.73, BARSPACE=0.05, BAROFFSET=0*(1.4*legoNumbers), barName=legoNames[0], $
  ;BASERANGE=1./legoNumbers, valuesRange=valuesRange, symColors=symbolColors
  ;FOR I = 1, legoNumbers-1 DO BEGIN
  ;BAR_PLOT_EXTRA, legoValues[0:legoSeries-1,i], symbolValues[0:symbolSeries-1,i], COLORS=extraColors, BACKGROUND=255, $
  ;BARWIDTH=0.73, BARSPACE=0.05, BAROFFSET=i*(1.4*legoSeries), barName=legoNames[i], $
  ;OVER=(I GT 0), BASERANGE=1./legoNumbers, valuesRange=valuesRange, symColors=extraSymColors
  ;ENDFOR
  i=0
  ; FM V2 To be filled with right information...
  recognizeHighLight=bytarr((symbolSequencesNo+1)*legonumbers)
  recognizeRegionEdges=ptrarr((symbolSequencesNo+1)*legonumbers) ; coords (normalized standard)
  recognizeNames=strarr((symbolSequencesNo+1)*legonumbers)
  recognizeValues=strarr((symbolSequencesNo+1)*legonumbers)
  ;recognizeHighLight: ptr_new(), $
  ;
  
  if bpInfo->hasSymbolValues() then thisSymbolValues=symbolValues[0:symbolSequencesNo-1,i]
  ;recognizeValues[0]=strcompress(legoValues[0], /REMOVE)
  ;recognizeNames[0]=legoNames[i]
  self->plotLegoAndSymbols, outLegoCoords, outSymbolCoords, legoValues[0:legoSequenceNo-1, i], legoNames[i], symbolValues=thisSymbolValues, $
    title=title, yTitle=yTitle, valuesRange=valuesRange, legoColors=legoColors, symbolColors=symbolColors, $
    BAROFFSET=i*(1.*legoNumbers), BASERANGE=1./legoNumbers, BARWIDTH=1./legoNumbers, BARSPACE=0.05, $
    BACKGROUND=whiteL, /FIRST
    
  if criteria gt 0 then begin
    symbolColorsTest=self->buildSymbolColors(10)
    if elabcode eq 2 or elabcode eq 7 then polyfill,[0,0,1,1,0],[criteria,min([1,valuesRange(1)]),min([1,valuesRange(1)]),criteria,criteria],/data,color=symbolColorsTest(1)
    if elabcode eq 8 or elabcode eq 30 or elabcode eq 28 then polyfill,[0,0,1,1,0],[0,min([criteria,abs(valuesRange(1))]),min([criteria,abs(valuesRange(1))]),0,0],/data,color=symbolColorsTest(1)
    if elabcode eq 23 or elabcode eq 54 then polyfill,[0,0,1,1,0],[-min([criteria,abs(valuesRange(0))]),$
      min([criteria,abs(valuesRange(1))]),min([criteria,abs(valuesRange(1))]),-min([criteria,abs(valuesRange(0))]),$
      -min([criteria,abs(valuesRange(0))])],/data,color=symbolColorsTest(1)
  endif
  
  ;recognizeRegionEdges[0]=ptr_new(outLegoCoords, /NO_COPY)
  saveOutTick=fltarr(legonumbers)
  symbolNames=bpInfo->getSymbolSequenceNames()
  ;lastIndex=0
  for i=0, legoNumbers-1 do begin
    if bpInfo->hasSymbolValues() then begin
      symIndex=(n_elements(thisSymbolValues)+1)*i
      nameIndex=n_elements(thisSymbolValues)*i
      thisSymbolValues=symbolValues[0:symbolSequencesNo-1,i]
      ;thisSymbolNames=symbolNames[0:symbolSequencesNo-1,i]
      ;thisSymbolNames=symbolNames[0, symIndex:symIndex+symbolSequencesNo-1]
      thisSymbolNames=symbolNames[0, nameIndex:nameIndex+symbolSequencesNo-1]
      recognizeValues[symIndex:symIndex+n_elements(thisSymbolValues)]=strcompress(([reform(legoValues[i]), thisSymbolValues]), /REMOVE)
      recognizeNames[symIndex:symIndex+n_elements(thisSymbolValues)]=reform([["OBS - "+legoNames[i]], [legoNames[i]+" - "+strcompress(thisSymbolNames, /REMOVE_ALL)]])
    endif else begin
      recognizeValues[i]=strcompress(reform(legoValues[i]), /REMOVE)
      recognizeNames[i]=["OBS - "+reform(legoNames[i])]
    endelse
    self->plotLegoAndSymbols, outLegoCoords, outSymbolCoords, legoValues[0:legoSequenceNo-1,i], legoNames[i], symbolValues=thisSymbolValues, $
      title=title, yTitle=yTitle, valuesRange=valuesRange, legoColors=legoColors, symbolColors=symbolColors, $
      BAROFFSET=i*(1.*legoNumbers), BASERANGE=1./legoNumbers, BARWIDTH=1./legoNumbers, BARSPACE=0.05, $
      BACKGROUND=whiteL, OVERPLOT=self->getOverplotKeyword(1), DRAWAXIS=(i eq legoNumbers-1), legonames=legonames, outTickPos=outTickPos, tickvs=saveOutTick
    if bpInfo->hasSymbolValues() then begin
      ;help, [outLegoCoords, outSymbolCoords]
      ;help, *outLegoCoords
      if n_elements(outSymbolCoords) eq 0 then begin
        myCoords=*outLegoCoords
        outSymbolCoords=ptr_new(myCoords, /NO_COPY)
      endif
      outLegoCoords=self->sizeCorrection(outLegoCoords, /X)
      outSymbolCoords=self->sizeCorrection(outSymbolCoords, /X, /Y, /SYMBOL)
      recognizeRegionEdges[symIndex:symIndex+n_elements(thisSymbolValues)]=[outLegoCoords, outSymbolCoords]
    endif else begin
      ;help, outLegoCoords
      ;print, *outLegoCoords
      outLegoCoords=self->sizeCorrection(outLegoCoords, /X)
      recognizeRegionEdges[i]=outLegoCoords
    endelse
    saveOutTick[i]=outTickPos
  endfor
  ;criteria set for RDE
  ;  plots,[0,1],[50,50],/data,color=150,thick=2
  ;  print, "recognizeValues"
  ;  help, recognizeValues
  ;  print, recognizeValues
  ;  print, "***************"
  ;  print, "recognizeNames"
  ;  help, recognizeNames
  ;  print, recognizeNames
  ;  print, "***************"
  ;  print, "recognizeRegionEdges"
  ;  help, recognizeRegionEdges
  ;  print, recognizeRegionEdges
  ;  print, "***************"
  
  rInfo = obj_new("RecognizeInfo", recognizeNames, recognizeValues, recognizeHighLight, recognizeRegionEdges)
  ;plotInfo->setRecognizeHighLight,
  ;plotInfo->setRecognizeValues,
  ;plotInfo->setRecognizeNames,
  ;plotInfo->setRecognizeRegionEdges,
  plotInfo->setRecognizeInfo, rInfo
  
  print, '...draw done...'
  
END

PRO Plotter::plotTimeSeriesLegend, request, result

  ;self.mainView->wsetInfoDataDraw
  self->wsetInfoDataDraw
  white=obj_new('Color', 200, 200, 200)
  black=obj_new('Color', 0, 0, 0)
  whiteL=white->AsLongTrueColor()
  blackL=black->AsLongTrueColor()
  obj_destroy, white
  obj_destroy, black
  ;self.mainView->wsetMainDataDraw
  device, DECOMPOSED=1
  ;Make axes black:
  ;!P.COLOR=0
  self->erase, whiteL
  plotNo=result->rawElementNumber()
  plotInfo=result->getPlotInfo()
  title=plotInfo->getTitle()
  ;  rawData=result->getRawData()
  ;  rDMatrix=result->getRawDataCheckMatrix(monitIndexes, runIndexes)
  singleRawData=result->getSingleRawData()
  singleRDMatrix=result->getSingleRawDataCheckMatrix(sMonitIndexes, sRunIndexes)
  groupRawData=result->getGroupRawData()
  groupRDMatrix=result->getGroupRawDataCheckMatrix(gMonitIndexes, gRunIndexes)
  psyms=[2,1,4,5,6,7]
  legoWidth=.05
  legoHeight=.05
  startX=.01
  maxWidth=-1
  
  if monitIndexes[0] ne -1 then begin
    i=0
    obsNames=rawData.observedCode
    toPlotNames=obsNames[uniq(obsNames, sort(obsNames))]
    mElems=n_elements(toPlotNames)
    toPlotIndexes=intarr(mElems)
    title=title+rawData[[monitIndexes[i]]].parameterCode
    plotInfo->setTitle, title
    for j=0, mElems-1 do toPlotIndexes[j]=(where(toPlotNames[j] eq obsNames))[0]
    name=toPlotNames[i];rawData[monitIndexes[i]].observedCode
    penColor=self->getLegoColorAtIndex(i)
    BACKGROUND=whiteL
    psym=psyms[0]
    startY=1.-((i+1)*legoHeight*2)
    lego=[[startX,startY], [startX,startY+legoHeight], [startX+legoWidth,startY+legoHeight], [startX+legoWidth,startY]]
    plots, lego, color=penColor, psym=psym, /NORM, SYMSIZE=1.5
    xyouts, startX+legoWidth+.01, startY+.002, name, COLOR=blackl, /NORM, charsize=.8, charthick=.8,  WIDTH=textWidth
    maxWidth=max([textWidth, maxWidth])
    for i=1, mElems-1 do begin
      penColor=self->getLegoColorAtIndex(i)
      name=toPlotNames[i]
      psym=psyms[i mod 6]
      startY=1.-((i+1)*legoHeight*2)
      lego=[[startX,startY], [startX,startY+legoHeight], [startX+legoWidth,startY+legoHeight], [startX+legoWidth,startY]]
      plots, lego, color=penColor, psym=psym, /NORM, SYMSIZE=1.5
      xyouts, startX+legoWidth+.01, startY+.002, name, COLOR=blackl, /NORM, charsize=.8, charthick=.8,  WIDTH=textWidth
      maxWidth=max([textWidth, maxWidth])
    endfor
  endif
  
  if runIndexes[0] ne -1 then begin
    rElems=n_elements(runIndexes)
    for i=0, rElems-1 do begin
      penColor=self->getSymbolColorAtIndex(i)
      name=rawData[runIndexes[i]].scenarioCode+' '+rawData[runIndexes[i]].modelCode+' '+rawData[runIndexes[i]].observedCode
      psym=psyms[i mod 6]
      startY=1.-((i+1)*legoHeight*2)
      thisStartX=startX+maxWidth+legoWidth+.02
      lego=[[thisStartX,startY], [thisStartX,startY+legoHeight], [thisStartX+legoWidth,startY+legoHeight], [thisStartX+legoWidth,startY]]
      plots, lego, color=penColor, psym=psym, /NORM, SYMSIZE=1.5
      xyouts, thisStartX+legoWidth+.01, startY+.002, name, COLOR=blackl, /NORM, charsize=.8, charthick=.8,  WIDTH=textWidth
    endfor
  endif
  
END

PRO Plotter::plotTimeSeries, request, result

  ;self.mainView->wsetMainDataDraw
  self->wsetMainDataDraw
  device, decomposed=1
  white=obj_new('Color', 200, 200, 200)
  black=obj_new('Color', 0, 0, 0)
  whiteL=white->AsLongTrueColor()
  blackL=black->AsLongTrueColor()
  obj_destroy, white
  obj_destroy, black
  
  self->setStartIndex, request->getStartPlotIndex()
  self->setEndIndex, request->getEndPlotIndex()
  
  isSingleSelection=request->isSingleObsPresent()
  isGroupSelection=request->isGroupObsPresent()
  
  plotNo=result->getrawElementNumber()
  plotInfo=result->getPlotInfo()
  
  if isSingleSelection then begin
    singleRawData=result->getSingleRawData()
    singleRDMatrix=result->getSingleRawDataCheckMatrix(sMonitIndexes, sRunIndexes)
  endif
  if isGroupSelection then begin
    groupRawData=result->getGroupRawData()
    groupRDMatrix=result->getGroupRawDataCheckMatrix(gMonitIndexes, gRunIndexes)
  endif
  
  psym=[2,1,4,5,6,7]
  title=plotInfo->getTitle()
  xTitle=plotInfo->getXTitle()
  yTitle=plotInfo->getYTitle()
  xRange=plotInfo->getXRange()
  yRange=plotInfo->getYRange()
  xTicks=plotInfo->getxTicks()
  xTickNumber=plotInfo->getXTickNumber()
  xTickNames=plotInfo->getXTickNames(self->getStartIndex(), self->getEndIndex(), /SUBSET)
  yTicks=plotInfo->getyTicks()
  yTickNames=plotInfo->getyTickNames()
  xStyle=plotInfo->getxStyle()
  yStyle=plotInfo->getyStyle()
  position=plotInfo->getyStyle()
  xCharSize=plotInfo->getXCharSize()
  yCharSize=plotInfo->getYCharSize()
  charsize=plotInfo->getcharSize()
  
  if smonitIndexes[0] ne -1 then begin
    i=0
    obsNames=singleRawData.observedCode
    toPlotIndexes=uniq(obsNames[sort(obsNames)])
    mElems=n_elements(toPlotIndexes)
    values=*singleRawData[toPlotIndexes[0]].observedData
    penColor=self->getLegoColorAtIndex(i)
    overplot=0 & noerase=0
    plotInfo->setTitle, title
    plot, values[self->getStartIndex():self->getEndIndex()], color=penColor, title=title, xtitle=xtitle, ytitle=ytitle, yrange=yrange, $
      xstyle=1, xtickname=xTickNames, xTicks=xTickNumber, $;, xrange=[self->getStartIndex(),self->getEndIndex()], $
      xcharsize=.7, BACKGROUND=whiteL, psym=-psym[i];, ymargin=[.25,.75], xmargin=[.25,.75]
    ;noerase=(overplot or noerase),xrange=xrange,yrange=yrange,xticks=xticks, $
    ;xtickname=xtickname,yticks=yticks,ytickname=ytickname, $
    ;xstyle=1,ystyle=1,background=background, position=position, $
    ;XCHARSIZE=XCHARSIZE, YCHARSIZE=YCHARSIZE, charsize=charsize, color=penColor ; ,/data
    for i=1, mElems-1 do begin
      penColor=self->getLegoColorAtIndex(i)
      values=*rawData[toPlotIndexes[i]].observedData
      oplot, values[self->getStartIndex():self->getEndIndex()], color=penColor, psym =-psym[i mod 6]
    endfor
  endif
  
  if srunIndexes[0] ne -1 then begin
    rElems=n_elements(runIndexes)
    for i=0, rElems-1 do begin
      penColor=self->getSymbolColorAtIndex(i)
      values=*rawData[runIndexes[i]].runData
      oplot, values[self->getStartIndex():self->getEndIndex()], color=penColor, psym =-psym[i mod 6]
    endfor
  endif
  print, '...draw done...'
  
END

PRO Plotter::plotScatterLegend, request, result

  self->plotStandardLegend, request, result
  ;self.mainView->wsetInfoDataDraw
  self->wsetInfoDataDraw
  
END

PRO Plotter::plotScatter, request, result

  ;To be implemented
  print, '...draw done...'
  
END

PRO Plotter::plotTaylorLegend, request, result

  ;To be implemented
  print, '...draw done...'
  
END

PRO Plotter::plotTaylor, request, result

  ;To be implemented
  print, '...draw done...'
  
END

PRO Plotter::plot2DLegend, request, result

  ;To be implemented
  print, '...draw done...'
  
END

PRO Plotter::plot2D, request, result

  ;To be implemented
  print, '...draw done...'
  
END

PRO Plotter::plotUndefLegend, request, result

  ;To be implemented
  print, '...draw done...'
  
END

PRO Plotter::plotUndef, request, result

  ;To be implemented
  print, '...draw done...'
  
END

FUNCTION Plotter::getCodes

  thisList=*self.list
  return, thisList[*].code
  
END

FUNCTION Plotter::getNames

  thisList=*self.list
  return, thisList[*].name
  
END

FUNCTION Plotter::getIdlRoutineCodes

  thisList=*self.list
  return, thisList[*].idlRoutineCode
  
END

FUNCTION Plotter::getMultipleChoices

  thisList=*self.list
  return, thisList[*].multipleChoice
  
END

FUNCTION Plotter::getDescriptions

  thisList=*self.list
  return, thisList[*].description
  
END

PRO Plotter::streamPrint

  print, '***********************'
  print, '**Start of<',OBJ_CLASS(self),'>**'
  
  print, '**** fileName:<', self.fileName,'>'
  thisList=*self.list
  for i=0, n_elements(thisList)-1 do begin
    print, '**element n.<', strcompress(i, /REMOVE),'>'
    print, '**** code:<', thisList[i].code,'>'
    print, '**** IDLRoutineCode:<', thisList[i].idlRoutineCode,'>'
    print, '**** multipleChoice:<', thisList[i].multipleChoice,'>'
    print, '**** description:<', thisList[i].description,'>'
    print, '**** name:<', thisList[i].name,'>'
    print, '**'
  endfor
  
  print, '***********************'
  print, '**End of:<',OBJ_CLASS(self),'>**'
  
END

FUNCTION  Plotter::getLegoColorAtIndex, index

  return, self.legoColors[index mod n_elements(self.legoColors)]
  
END

FUNCTION  Plotter::getCurrentLegoColor

  return, self->getLegoColorAtIndex(self.legoColorIndex)
  
END

FUNCTION  Plotter::getNextLegoColor

  self.legoColorIndex++
  return, self->getCurrentLegoColor()
  
END

FUNCTION  Plotter::getSymbolColorAtIndex, index

  return, self.symbolColors[index mod n_elements(self.symbolColors)]
  
END

FUNCTION  Plotter::getCurrentSymbolColor

  return, self->getSymbolColorAtIndex(self.symbolColorIndex)
  
END

FUNCTION  Plotter::getNextSymbolColor

  self.symbolColorIndex++
  return, self->getCurrentSymbolColor()
  
END

FUNCTION Plotter::buildLegoColors, howMany

  return, self.legoColors[indgen(howMany)]
  
END

FUNCTION Plotter::buildSymbolColors, howMany

  return, self.symbolColors[indgen(howMany)]
  
END

PRO Plotter::configureColor

  legoColors=lonarr(15)
  symbolColors=lonarr(15)
  testColor=obj_new('Color', 255, 0, 0)
  legoColors[0]=testColor->AsLongTrueColor();red
  testColor->setRGB, 0, 255, 0 & legoColors[1]=testColor->AsLongTrueColor();green
  testColor->setRGB, 0, 0, 255 & legoColors[2]=testColor->AsLongTrueColor();blue
  testColor->setRGB, 255, 255, 0 & legoColors[3]=testColor->AsLongTrueColor();comp1
  testColor->setRGB, 255, 0, 255 & legoColors[4]=testColor->AsLongTrueColor();comp2
  testColor->setRGB, 0, 255, 255 & legoColors[5]=testColor->AsLongTrueColor();comp3
  testColor->setRGB, 128, 128, 0 & legoColors[6]=testColor->AsLongTrueColor();comp4
  testColor->setRGB, 128, 0, 128 & legoColors[7]=testColor->AsLongTrueColor();comp5
  testColor->setRGB, 0, 128, 128 & legoColors[8]=testColor->AsLongTrueColor();comp6
  testColor->setRGB, 128, 128, 255 & legoColors[9]=testColor->AsLongTrueColor();comp4
  testColor->setRGB, 128, 255, 128 & legoColors[10]=testColor->AsLongTrueColor();comp5
  testColor->setRGB, 255, 128, 128 & legoColors[11]=testColor->AsLongTrueColor();comp6
  testColor->setRGB, 128, 128, 128 & legoColors[12]=testColor->AsLongTrueColor();comp7
  testColor->setRGB, 0, 255, 0 & legoColors[13]=testColor->AsLongTrueColor();comp6
  testColor->setRGB, 0, 0, 255 & legoColors[14]=testColor->AsLongTrueColor();comp7
  self.legoColors=legoColors
  self.symbolColors=reverse(legoColors)
  self.symbolColorsIndex=0
  self.legoColorsIndex=0
  obj_destroy, testColor
  
END

PRO Plotter::fillDataFromFile, fileName

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
  ;plotTypes=getFMPlotType()
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
    ;      print, 'Discard row', i
    ;      print, bufferString
    endif else begin
      info=strsplit(bufferString, ';', /EXTRACT)
      if n_elements(info) eq 5 then begin
        ;thisPlotType=getFMPlotType()
        thisPlotType.code=fix(info[0])
        thisPlotType.name=info[1]
        thisPlotType.idlRoutineCode=fix(info[2])
        thisPlotType.multipleChoice=info[3]
        thisPlotType.description=info[4]
        plotTypes=[plotTypes, thisPlotType]
      endif else begin
        print, 'Bad conf file at line', i, bufferString
      endelse
    endelse
  endwhile
  close, unit & free_lun, unit
  self.list=ptr_new(plotTypes[1:*], /NO_COPY)
  
END

PRO Plotter::cleanUp

  self -> Object :: cleanUp
  
END

FUNCTION Plotter::init, mainView

  if not (self -> Object :: init()) then return , 0
  self.mainView=mainView
  self.legendSpaceYNorm=0.25
  self->configureColor
  
  os=strupcase(!version.os_family)
  ;if os eq 'X' then
  self.mainDeviceName='X'
  if os eq 'WINDOWS' then self.mainDeviceName='WIN'
  return , 1
  
END

PRO Plotter__Define

  Struct = { Plotter , $
    mainView: obj_new(), $
    mainDeviceName: '', $
    silent: 0, $
    lastPostScriptFileName: '', $
    startIndex: 0l, $
    endIndex: 0l, $
    deviceIsOpen: 0, $
    orientation: '', $
    position: fltarr(4), $
    legendSpaceYNorm: 0., $
    previousDeviceName: '', $
    currentDeviceName: '', $
    symbolColors: lonarr(15), $
    legoColors: lonarr(15), $
    symbolColorsIndex: 0, $
    legoColorsIndex: 0, $
    imageType: '', $
    storedImage: ptrarr(4), $
    pageLocation: ptrarr(4), $
    Inherits Object $
    }
    
END