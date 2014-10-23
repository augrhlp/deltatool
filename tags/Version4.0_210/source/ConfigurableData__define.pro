PRO ConfigurableData::setList, list

 ptr_free, self.list
 self.list=ptr_new(list, /NO_COPY)

END

PRO ConfigurableData::FillDataFromFile, fileName

 message, 'You must implement this one on your child class!!!'

END

FUNCTION ConfigurableData::modeCompatibility, compList

 idxs=where(strmid(compList, self.mode, 1) eq '1')
 return, idxs

END

FUNCTION ConfigurableData::getMode

 return, self.mode

END

PRO ConfigurableData::setMode, value

 self.mode=value
 self->updateMode 

END

PRO ConfigurableData::CleanUp

 ptr_free, self.list

END

FUNCTION ConfigurableData::init, filename, mode=mode

 if not (self -> Object :: init()) then return, 0

 if n_elements( fileName ) eq 1 then self.filename = filename
 if n_elements( mode) ne 0 then self.mode=mode
 return, 1

END

PRO ConfigurableData__Define

Struct = { ConfigurableData , $
		fileName : '', $
		list: ptr_new(), $
		mode: 0, $
		Inherits Object $
		 }

END