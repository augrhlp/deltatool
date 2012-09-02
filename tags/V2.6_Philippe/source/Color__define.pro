;****************************************************************************************

function Color :: deserialize , stream

  obj = obj_new( 'Color' )
  
  catch , ioerror
  
  if ioerror ne 0 then begin
  
    obj_destroy , obj
    
    return , obj_new()
    
  endif
  
  in = 0b
  obj.r = stream -> fRead( in )
  obj.g = stream -> fRead( in )
  obj.b = stream -> fRead( in )
  
  return , obj
  
end

;****************************************************************************************

function Color :: serialize , stream

  if not stream -> fWrite( self.r ) then return , 0
  if not stream -> fWrite( self.g ) then return , 0
  if not stream -> fWrite( self.b ) then return , 0
  
  return , 1
  
end

;****************************************************************************************

function Color :: getComplementary

  return , obj_new( 'Color' , 255b - self.r , 255b - self.g , 255b - self.b )
  
end

;****************************************************************************************

function Color :: equals, obj

  if not obj_valid(obj) then return, 0
  if not obj -> isA('Color') then return, 0
  
  if obj.r ne self.r then return, 0
  if obj.g ne self.g then return, 0
  if obj.b ne self.b then return, 0
  
  return, 1
  
end

;****************************************************************************************

function Color :: clone

  clone = obj_new( 'Color' )
  clone.r = self.r
  clone.g = self.g
  clone.b = self.b
  
  return , clone
  
end

;****************************************************************************************

function Color :: asLongTrueColor

  return , self.r + 256l * ( self.g + 256l * self.b )
  
end

;****************************************************************************************

pro Color :: setRed , r

  self.r = r
  
end

;****************************************************************************************

pro Color :: setGreen , g

  self.g = g
  
end

;****************************************************************************************

pro Color :: setBlue , b

  self.b = b
  
end

;****************************************************************************************

function Color :: getRed

  return , self.r
  
end

;****************************************************************************************

function Color :: getGreen

  return , self.g
  
end

;****************************************************************************************

function Color :: getBlue

  return , self.b
  
end

;****************************************************************************************

pro Color :: setRGB , r , g , b

  self.r = r
  self.g = g
  self.b = b
  
end

;****************************************************************************************

pro Color :: getRGB , r , g , b

  r = self.r
  g = self.g
  b = self.b
  
end

;****************************************************************************************

pro Color :: setFromTrueColor , v

  self.b = ( v / 256l / 256l ) mod 256
  self.g = ( v / 256 ) mod 256
  self.r = v mod 256
  
end

;****************************************************************************************

function Color :: init , r , g , b , fake = fake , longValue = longValue

  if not self -> Object :: init() then return , 0
  
  if keyword_set( longValue ) then begin
  
    self -> setFromTrueColor , longValue
    
    return , 1
    
  endif
  
  if keyword_set( fake ) then begin
  
    self.r = byte( randomn( s ) * 255b )
    self.g = byte( randomn( s ) * 255b )
    self.b = byte( randomn( s ) * 255b )
    
    return , 1
    
  endif
  
  if n_elements( r ) ne 0 then self.r = r
  if n_elements( g ) ne 0 then self.g = g
  if n_elements( b ) ne 0 then self.b = b
  
  return , 1
  
end

;****************************************************************************************

pro Color__Define

  Struct = { Color , $
    r : 0b , $
    g : 0b , $
    b : 0b , $
    Inherits Object $
    }
    
end

;****************************************************************************************
