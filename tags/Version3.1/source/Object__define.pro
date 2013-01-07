;********************
@structure_definition
;********************
;****************************************************************************************

function Object :: clone

return , obj_new()

end

;****************************************************************************************

function Object :: getClass , superclass = superclass , count = count

return , obj_class( self , superclass = superclass , count = count )

end

;****************************************************************************************

function Object :: isA , class

if ( size( class ) )[ 0 ] eq 0 and ( size( class ) )[ ( size( class ) )[ 0 ] + 1 ] eq 7 then return , obj_isa( self , class ) else return , 0

end

;****************************************************************************************

pro Object :: cleanUp

self = 0b

end

;****************************************************************************************

function Object :: init

return , 1

end

;****************************************************************************************

pro Object__Define

Struct = { Object , $
		   idObject : 0ll $
		 }

end

;****************************************************************************************
