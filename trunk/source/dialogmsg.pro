function dialogMsg, textMessage, dialog_parent=dialog_parent, title=title, INFORMATION=INFORMATION, error=error, question=question, WARNING=WARNING, CENTER=CENTER, FORCELOG=FORCELOG 

  if not(keyword_set(FORCELOG)) then return, dialog_message(textMessage, dialog_parent=dialog_parent, title=title, INFORMATION=INFORMATION, ERROR=ERROR, QUESTION=QUESTION, CENTER=CENTER)
  print, textMessage
  answer='YES'
  return, answer
 
end