;standard date template is: strarr(6)
; Second, Minute, Hour, Day,  Month, Year

FUNCTION DateTimeUtility::getDateTimeStruct

  struct = { dateTime, $
    second: 0, $
    minute: 0, $
    hour: 0, $
    day: 0, $
    month: 0, $
    year: 0 $
    }
  return, struct
  
END

FUNCTION DateTimeUtility::getTimeStampStruct

  struct = { timeStamp, $
    value: '', $
    template: '' $
    }
  return, struct
  
END

FUNCTION DateTimeUtility::addHeaderZeroes, number, outdigit

  ;for index=index0, index1 do begin
  strNumber=strtrim(number, 1)
  digits=strlen(strNumber)
  zeros=''
  for j=digits, outdigit-1 do zeros=zeros+'0'
  return, zeros+strNumber
;endfor
  
END

FUNCTION DateTimeUtility::containHour, template

  if strpos(template, self.validCode[2]) ne -1 then return, 1
  if strpos(template, self.validCode[3]) ne -1 then return, 1
  return, 0
  
END

FUNCTION DateTimeUtility::calcYearsDaysFromMonth, monthNumber

  if monthNumber gt 1 then days=total(self.monthsDays[0:monthNumber-2]) else days=0
  return, days
  
END

FUNCTION DateTimeUtility::getAbsoluteHours, dates, template=template, struct=struct

  ; self.conversionFactorToHour
  ; self.validCode
  if keyword_set(struct) then internalTemplate='hhddmm' else internalTemplate=template
  datesNumber=n_elements(dates)
  totHours=fltarr(datesNumber)
  for j=0, datesNumber-1 do begin
    for i=0, n_elements(self.validCode)-1 do begin
      ;search valid template substring
      if keyword_set(struct) then thisdate=self->addHeaderZeroes(dates[j].hour, 2)+$
        self->addHeaderZeroes(dates[j].day, 2)+self->addHeaderZeroes(dates[j].month, 2) $
      else thisDate=dates[j]
      casePos=strpos(internalTemplate, self.validCode[i])
      if casePos ne -1 then begin
        value=strmid(thisdate, casePos, strlen(self.validCode[i]))
        if i eq 6 then begin
          ; (-1) 'cause entire previous monhths is added
          days=self->calcYearsDaysFromMonth(value)
          hours=days*24
        endif else begin
          ;from day must be subtract 1
          hours=float(value)*self.conversionFactorToHour[i]-self.offsetToHour[i]
        endelse
        totHours[j]=totHours[j]+hours
      endif
    endfor
  endfor
  
  return, totHours
  
END

FUNCTION DateTimeUtility::calcDayOfYear, date, outHour=outHour

  ;month, day, year, hour, minute, seconds
  if date.year eq 0 then year=2001 else year=date.year
  jDay = julday(date.month, date.day, year, date.hour, date.minute, date.second)
  jDayFirst = julday(1, 1, year, 0, 0, 0)
  ;jDayFirst = julday(1, 1, date[0], 0)
  dayDiff=fix(jDay-jDayFirst)
  outHour=fix((dayDiff-fix(jDay-jDayFirst))*24)
  return, dayDiff
  
END

FUNCTION DateTimeUtility::getDayNumberOfYear, year

  month=1 & emonth=12
  day=1 & eday=31
  hour=0 & ehour=23
  minute=0 & eminute=59
  second=0 & esecond=59
  
  jDay = julday(month, day, year, hour, minute, second)
  
  jDaye = julday(emonth, eday, year, ehour, eminute, esecond)
  return, round(jDaye-jDay)
  
END

FUNCTION DateTimeUtility::checkDate, year, month=month, day=day, hour=hour, minute=minute, second=second, julNumber=julNumber

  if n_elements(month) eq 0 then month=1
  if n_elements(day) eq 0 then day=1
  if n_elements(hour) eq 0 then hour=0
  if n_elements(minute) eq 0 then minute=0
  if n_elements(second) eq 0 then second=0
  
  jDay = julday(month, day, year, hour, minute, second)
  julNumber=jDay
  caldat, jDay, resMonth, resDay, resYear, resHour, resMinute, resSecond
  resSecond=round(resSecond)
  
  if resYear eq year and resMonth eq month and $
    resDay eq day and resHour eq hour and $
    resMinute eq minute and resSecond eq second then return, 1
    
  return, 0
  
END

FUNCTION DateTimeUtility::getDate, startDate, frequency, frameIndex

  ;start=julday(Month, Day, Year, Hour, Minute, Second)
  startCalc=julday(startDate[1], startDate[2], startDate[0], startDate[3], startDate[4], startDate[5])
  dayStep=1.
  hourStep=1./24
  minuteStep=1./(24*60)
  secondStep=1./(24*60*60)
  
  case frequency of
    '1H':begin
    caldat, startCalc+frameIndex*hourStep, month , day, year, hour, minute, second
    ;print, month , day, year, hour, minute, second
    ;resDate=strtrim(hour, 1)+':'+strtrim(minute, 1)+'.'+strtrim(floor(second), 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
    resDate=strtrim(year, 1)+':'+strtrim(month, 1)+':'+strtrim(day, 1)+':'+strtrim(hour, 1)
  ;resDate[i]=strtrim(hour, 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
  end
  '1D':begin
  caldat, startCalc+frameIndex*dayStep, month , day, year, hour, minute, second
  resDate=strtrim(hour, 1)+':'+strtrim(minute, 1)+'.'+strtrim(floor(second), 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
end
'1M':begin
caldat, startCalc+gframeIndex*minuteStep, month , day, year, hour, minute, second
resDate=strtrim(hour, 1)+':'+strtrim(minute, 1)+'.'+strtrim(floor(second), 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
end
'1S':begin
caldat, startCalc+frameIndex*secondStep, month , day, year, hour, minute, second
resDate=strtrim(hour, 1)+':'+strtrim(minute, 1)+'.'+strtrim(floor(second), 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
end
endcase
return, resDate

END

FUNCTION DateTimeUtility::formatDate, date, template=template

  if n_elements(template) eq 0 then template=self.template
  ;date var of type intarr(6) [0]: year [5]: seconds
  case template of
    'dd':begin
    stringDate=strtrim(date[2], 1)
  end
  'hh:dd':begin
  stringDate=strtrim(date[3], 1)+':'+strtrim(date[2], 1)
end
'dd:mm':begin
stringDate=strtrim(date[2], 1)+':'+strtrim(date[1], 1)
end
'mm:yyyy':begin
stringDate=strtrim(date[1], 1)+':'+strtrim(date[0], 1)
end
'hh:dd:mm:yyyy':begin
stringDate=strtrim(date[3], 1)+':'+strtrim(date[2], 1)+':'+strtrim(date[1], 1)+':'+strtrim(date[0], 1)
end
else:begin
stringDate=strtrim(date[3], 1)+':'+strtrim(date[2], 1)+':'+strtrim(date[1], 1)+':'+strtrim(date[0], 1)
end
endcase

return, stringDate

END

FUNCTION DateTimeUtility::buildDateAxisTickMark, startDate, frequency, frameNumber

  ;start=julday(Month, Day, Year, Hour, Minute, Second)
  startCalc=julday(startDate[1], startDate[2], startDate[0], startDate[3], startDate[4], startDate[5])
  resDate=strarr(frameNumber)
  dayStep=1.
  hourStep=1./24
  minuteStep=1./(24*60)
  secondStep=1./(24*60*60)
  
  case frequency of
    'DD-MM':begin
    for i=0, frameNumber-1 do begin
      caldat, startCalc+i*hourStep, month , day, year, hour, minute, second
      ;print, month , day, year, hour, minute, second
      ;resDate[i]=strtrim(hour, 1)+':'+strtrim(minute, 1)+'.'+strtrim(second, 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
      resDate[i]=strtrim(day, 1)+'/'+strtrim(month, 1)
    endfor
  end
  '1H':begin
  for i=0, frameNumber-1 do begin
    caldat, startCalc+i*hourStep, month , day, year, hour, minute, second
    ;print, month , day, year, hour, minute, second
    ;resDate[i]=strtrim(hour, 1)+':'+strtrim(minute, 1)+'.'+strtrim(second, 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
    resDate[i]=strtrim(hour, 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
  endfor
end
'1D':begin
for i=0, frameNumber-1 do begin
  caldat, startCalc+i*dayStep, month , day, year, hour, minute, second
  resDate[i]=strtrim(hour, 1)+':'+strtrim(minute, 1)+'.'+strtrim(second, 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
endfor
end
'1M':begin
for i=0, frameNumber-1 do begin
  caldat, startCalc+i*minuteStep, month , day, year, hour, minute, second
  resDate[i]=strtrim(hour, 1)+':'+strtrim(minute, 1)+'.'+strtrim(second, 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
endfor
end
'1S':begin
for i=0, frameNumber-1 do begin
  caldat, startCalc+i*secondStep, month , day, year, hour, minute, second
  resDate[i]=strtrim(hour, 1)+':'+strtrim(minute, 1)+'.'+strtrim(second, 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
endfor
end
endcase
return, resDate

END

FUNCTION DateTimeUtility::buildBestDateAxisTickMark, startDate, frequency, frameNumber

  ;start=julday(Month, Day, Year, Hour, Minute, Second)
  startCalc=julday(startDate[1], startDate[2], startDate[0], startDate[3], startDate[4], startDate[5])
  resDate=strarr(frameNumber)
  dayStep=1.
  hourStep=1./24
  minuteStep=1./(24*60)
  secondStep=1./(24*60*60)
  
  if frequency eq '1H' then begin
    stepValue=hourStep
    if frameNumber le 3*24 then template='hh:dd'
    if frameNumber gt 3*24 and frameNumber le 60*24 then template='dd:mm'
    if frameNumber gt 60*24 and frameNumber le 150*24 then template='dd:mm'
    if frameNumber gt 150*24 then template='mm:yyyy'
  endif
  if frequency eq '1D' then begin
    stepValue=dayStep
    if frameNumber le 3 then template='dd'
    if frameNumber gt 3 and frameNumber le 60 then template='dd:mm'
    if frameNumber gt 60 and frameNumber le 150 then template='dd:mm'
    if frameNumber gt 150 then template='mm:yyyy'
  endif
  
  case template of
    'dd':begin
    for i=0, frameNumber-1 do begin
      caldat, startCalc+i*stepValue, month , day, year, hour, minute, second
      ;print, month , day, year, hour, minute, second
      ;resDate[i]=strtrim(hour, 1)+':'+strtrim(minute, 1)+'.'+strtrim(second, 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
      resDate[i]=strtrim(day, 1)
    endfor
  end
  'hh:dd':begin
  for i=0, frameNumber-1 do begin
    caldat, startCalc+i*stepValue, month , day, year, hour, minute, second
    ;print, month , day, year, hour, minute, second
    ;resDate[i]=strtrim(hour, 1)+':'+strtrim(minute, 1)+'.'+strtrim(second, 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
    resDate[i]=strtrim(hour, 1)+':'+strtrim(day, 1)
  endfor
end
'dd:mm':begin
for i=0, frameNumber-1 do begin
  caldat, startCalc+i*stepValue, month , day, year, hour, minute, second
  ;print, month , day, year, hour, minute, second
  ;resDate[i]=strtrim(hour, 1)+':'+strtrim(minute, 1)+'.'+strtrim(second, 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
  resDate[i]=strtrim(day, 1)+':'+strtrim(month, 1)
endfor
end
'mm:yyyy':begin
for i=0, frameNumber-1 do begin
  caldat, startCalc+i*stepValue, month , day, year, hour, minute, second
  resDate[i]=strtrim(month, 1)+':'+strtrim(year, 1)
endfor
end
else:begin
for i=0, frameNumber-1 do begin
  caldat, startCalc+i*stepValue, month , day, year, hour, minute, second
  resDate[i]=strtrim(day, 1)+':'+strtrim(month, 1)+':'+strtrim(year, 1)
endfor
endcase
; '1M':begin
;   for i=0, frameNumber-1 do begin
;     caldat, startCalc+i*minuteStep, month , day, year, hour, minute, second
;     resDate[i]=strtrim(hour, 1)+':'+strtrim(minute, 1)+'.'+strtrim(second, 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
;   endfor
; end
; '1S':begin
;   for i=0, frameNumber-1 do begin
;     caldat, startCalc+i*secondStep, month , day, year, hour, minute, second
;     resDate[i]=strtrim(hour, 1)+':'+strtrim(minute, 1)+'.'+strtrim(second, 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
;   endfor
; end
endcase
return, resDate

END

FUNCTION DateTimeUtility::getSystemDateTime

  date=strcompress(systime(/UTC), /REMOVE)
  yyyy=strmid(date, 16, 4)
  mo=strmid(date, 3, 3)
  dd=strmid(date, 6, 2)
  hh=strmid(date, 8, 2)
  mi=strmid(date, 11, 2)
  ss=strmid(date, 11, 2)
  return, yyyy+mo+dd+hh+mi+ss
  
END

;*****************************
; constructor/destructor
;*****************************

PRO DateTimeUtility::CleanUp

  self.template=''
  self->Object::cleanup
  
END

FUNCTION DateTimeUtility::init, template

  if not (self -> Object :: init()) then return, 0
  
  if n_elements( template ) eq 1 then self.template = template else self.template='hh:dd:mm:yyyy'
  ; ss:seconds (0:59), mi: minute(0:59), hh24: hours (00:23), hh: hours [01:12] [am/pm], dd: days [00,31], mm: months [01, 12], month: months [Jan...Dec], yy: years [00:99], yyyy: years [4 digit], dow: [Mon...Sun]
  self.validCode=['ss', 'mi', 'hh24', 'hh', 'dd',  'dow', 'mm', 'month', 'yy', 'yyyy']
  self.conversionFactorToHour=[1./3600, 1./60, 1., 1., 24.,  7.*24, 31, 31, 365., 365.]
  self.offsetToHour=[0, 0, 0, 0, 24,  0, 0, 0, 0, 0]
;KeesC leapyear
;  self.monthsDays=[31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  self.monthsDays=[31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  return, 1
  
END

PRO DateTimeUtility__Define

  Struct = { DateTimeUtility , $
    template : '', $
    validCode : strarr(10), $
    conversionFactorToHour : fltarr(10), $
    offsetToHour : fltarr(10), $
    monthsDays: intarr(12), $
    Inherits Object $
    }
    
END
