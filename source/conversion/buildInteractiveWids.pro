  FUNCTION buildInteractiveWids, refBase, labWids=labWids, bttWids=bttWids
  
  firstColWidth=30
  secondColWidth=70

  firstColFont='times Roman*14*bold'
  secondColFont='times Roman*14*bold'

  interactiveBase=widget_base(refBase, /COLUMN)
  
  fileTypeSelectionBase = Widget_Base(interactiveBase, $
    XOFFSET=0 ,YOFFSET=0, $
    SPACE=0 ,XPAD=0 ,YPAD=0, /ROW)
  
  label=widget_label(fileTypeSelectionBase, VALUE='Select file type')
  buttonRadioSelectionBase= Widget_Base(fileTypeSelectionBase, $
    XOFFSET=0 ,YOFFSET=0, /EXCLUSIVE, $
    TITLE='IDL' ,SPACE=0 ,XPAD=0 ,YPAD=0, /COLUMN)

  modelButton=widget_button(buttonRadioSelectionBase, UVALUE='MODEL', VALUE='Model', /NO_RELEASE, UNAME='MODEL')
  ;observedButton=widget_button(buttonRadioSelectionBase, UVALUE='OBSERVED', VALUE='Observed', /NO_RELEASE, UNAME='OBSERVED')
  widget_control, modelButton, set_button=1 
  ;widget_control, observedButton, /hide
   
 
  textSelectionBase=widget_base(interactiveBase, /ROW)

  col1Base=widget_base(textSelectionBase, /COLUMN)
  col2Base=widget_base(textSelectionBase, /COLUMN)

  title1=WIDGET_text(col1Base,value='STARTUP_FILE', XSIZE=firstColWidth, font=firstColFont, EDITABLE=0, ALL_EVENTS=0, SENSITIVE=0, /NO_NEWLINE)
  info1=WIDGET_text(col2Base,XSIZE=secondColWidth,uvalue='startup',value=' ', /editable, font=secondColFont, /NO_NEWLINE)

  title2=widget_text(col1Base,value='INIT_RUN', XSIZE=firstColWidth, font=firstColFont, EDITABLE=0, ALL_EVENTS=0, SENSITIVE=0, /NO_NEWLINE)
  info2=WIDGET_text(col2Base,uvalue='initrun',value=' ', /editable, font=secondColFont, /NO_NEWLINE)
    
  title3=widget_text(col1Base,value='END_RUN', XSIZE=firstColWidth, font=firstColFont, EDITABLE=0, ALL_EVENTS=0, SENSITIVE=0, /NO_NEWLINE)
  info3=WIDGET_text(col2Base,uvalue='endrun',value=' ', /editable, font=secondColFont, /NO_NEWLINE)
    
  title4=widget_text(col1Base,value='INPUT_DIR', XSIZE=firstColWidth, font=firstColFont, EDITABLE=0, ALL_EVENTS=0, SENSITIVE=0, /NO_NEWLINE)
  info4=WIDGET_text(col2Base,uvalue='inputdir', /editable, font=secondColFont, /NO_NEWLINE)
    
  title5=widget_text(col1Base,value='INPUT_PREFIX', XSIZE=firstColWidth, font=firstColFont, EDITABLE=0, ALL_EVENTS=0, SENSITIVE=0, /NO_NEWLINE)
  info5=WIDGET_text(col2Base,uvalue='prefixid', /editable, font=secondColFont, /NO_NEWLINE, UNAME='INPUT_PREFIX')
  

  title6=WIDGET_text(col1Base,value='INPUT_FILE_TEMPLATE', XSIZE=firstColWidth, font=firstColFont, EDITABLE=0, ALL_EVENTS=0, SENSITIVE=0, /NO_NEWLINE)
  info6=WIDGET_text(col2Base,uvalue='', font=secondColFont, /NO_NEWLINE)

  title7=widget_text(col1Base,value='OUTPUT_DIR', XSIZE=firstColWidth, font=firstColFont, EDITABLE=0, ALL_EVENTS=0, SENSITIVE=0, /NO_NEWLINE)
  info7=WIDGET_text(col2Base,uvalue='outputdir', /editable, font=secondColFont, /NO_NEWLINE)

  title8=widget_text(col1Base,value='YEAR', XSIZE=firstColWidth, font=firstColFont, EDITABLE=0, ALL_EVENTS=0, SENSITIVE=0, /NO_NEWLINE)
  info8=WIDGET_text(col2Base,uvalue='year', /editable, font=secondColFont, /NO_NEWLINE)

  title9=widget_text(col1Base,value='MODEL_NAME', XSIZE=firstColWidth, font=firstColFont, EDITABLE=0, ALL_EVENTS=0, SENSITIVE=0, /NO_NEWLINE)
  info9=WIDGET_text(col2Base,uvalue='modelname', /editable, font=secondColFont, /NO_NEWLINE)

  title10=widget_text(col1Base,value='POSTFIX', XSIZE=firstColWidth, font=firstColFont, EDITABLE=0, ALL_EVENTS=0, SENSITIVE=0, /NO_NEWLINE)
  info10=WIDGET_text(col2Base,uvalue='postfix', /editable, font=secondColFont, /NO_NEWLINE)

  title11=widget_text(col1Base,value='FULL_OUTPUT_FILE', XSIZE=firstColWidth, font=firstColFont, EDITABLE=0, ALL_EVENTS=0, SENSITIVE=0, /NO_NEWLINE)
  info11=WIDGET_text(col2Base,uvalue='', sensitive=0, font=secondColFont, /NO_NEWLINE)

  labWids=[info6,info11]
  ;bttWids=[modelButton, observedButton]
  bttWids=modelButton

  return, [info1, info2, info3, info4, info5, info7, info8, info9, info10]  

  end