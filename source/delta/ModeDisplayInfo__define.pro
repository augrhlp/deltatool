;****************************************************************************************

PRO ModeDisplayInfo::setNames, list

 ptr_free, self.names
 self.names=ptr_new(list, /NO_COPY)

END

FUNCTION ModeDisplayInfo::getNames

 if ptr_valid(self.names) then return, *self.names
 return, -1

END

PRO ModeDisplayInfo::setCodes, list

 ptr_free, self.codes
 self.codes=ptr_new(list, /NO_COPY)

END

FUNCTION ModeDisplayInfo::getCodes

 if ptr_valid(self.codesList) then return, *self.codes
 return, -1

END

PRO ModeDisplayInfo::setDescriptions, list

 ptr_free, self.descriptions
 self.descriptions=ptr_new(list, /NO_COPY)

END

FUNCTION ModeDisplayInfo::getDescriptions

 if ptr_valid(self.descriptions) then return, *self.descriptions
 return, -1

END

PRO ModeDisplayInfo::setSelection, index

 self.selection=index

END

FUNCTION ModeDisplayInfo::getSelection

 return,  self.selection

END

FUNCTION ModeDisplayInfo::clone, DEEP=DEEP

 clone=obj_new('ModeDisplayInfo')
 if keyword_set(DEEP) then begin
	if ptr_valid(self.names) then begin
		list=*self.names
		clone.names=ptr_new(list, /NO_COPY)
	endif
	if ptr_valid(self.codes) then begin
		list=*self.codes
		clone.codes=ptr_new(list, /NO_COPY)
	endif
	if ptr_valid(self.descriptions) then begin
		list=*self.descriptions
		clone.descriptions=ptr_new(list, /NO_COPY)
	endif
 endif else begin
 	clone.names=self.names
 	clone.codes=self.codes
 	clone.descriptions=self.descriptions
 endelse
 clone.selection=self.selection
 return, clone

END

FUNCTION ModeDisplayInfo::init

 if not self -> Object :: init() then return , 0
 ptr_free, self.names
 ptr_free, self.codes
 ptr_free, self.descriptions
 return , 1

END

PRO ModeDisplayInfo::cleanUp

 self -> Object::cleanUp
 ptr_free, self.names
 ptr_free, self.codes
 ptr_free, self.descriptions

END

;****************************************************************************************

PRO ModeDisplayInfo__Define

Struct = { ModeDisplayInfo , $
		names: ptr_new(), $
		codes: ptr_new(), $
		descriptions: ptr_new(), $
		selection: 0, $
		Inherits Object $
		 }

END

;****************************************************************************************
