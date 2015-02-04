pro mypsym,number,size_symb

  CASE number OF
  
    0: begin ; diamond empty
      X = [-size_symb, 0, size_symb, 0, -size_symb]
      Y = [0, size_symb, 0, -size_symb, 0]
      USERSYM, X, Y,thick=2
    end
    
    1: begin ;square empty
      X = [-size_symb, -size_symb, size_symb,  size_symb, -size_symb]*0.75
      Y = [-size_symb,  size_symb, size_symb, -size_symb, -size_symb]*0.75
      USERSYM, X, Y,thick=2
    end
    
    2: begin ;triangle up empty
      X = [-size_symb,  0,  size_symb, -size_symb]*0.9
      Y = [-size_symb,  size_symb, -size_symb, -size_symb]*0.9
      USERSYM, X, Y,thick=2
    end
    
    3: begin ; triangle down empty
      X = [-size_symb,  0,           size_symb, -size_symb]*.9
      Y = [ size_symb,  -size_symb,  size_symb,  size_symb]*.9
      USERSYM, X, Y,thick=2
    end
    
    4: begin ; diamond full
      X = [-size_symb, 0, size_symb, 0, -size_symb]
      Y = [0, size_symb, 0, -size_symb, 0]
      USERSYM, X, Y,/fill
    end
    
    5: begin ; square full
      X = [-size_symb, -size_symb, size_symb,  size_symb, -size_symb]*.75
      Y = [-size_symb,  size_symb, size_symb, -size_symb, -size_symb]*.75
      USERSYM, X, Y,/fill
    end
    
    6: begin ; triangle up full
      X = [-size_symb,  0,  size_symb, -size_symb]*.9
      Y = [-size_symb,  size_symb, -size_symb, -size_symb]*.9
      USERSYM, X, Y,/fill
    end
    
    7: begin ; triangle down full
      X = [-size_symb,  0,           size_symb, -size_symb]*.9
      Y = [ size_symb,  -size_symb,  size_symb,  size_symb]*.9
      USERSYM, X, Y,/fill
    end
    
    8: begin ; + sign
      X = [0, 0, 0, size_symb, -size_symb]
      Y = [size_symb, -size_symb,0, 0,  0]
      USERSYM, X, Y,thick=2
    end
    
    9: begin ; circle full
      A = FINDGEN(17) * (!PI*2/16.)
      USERSYM, size_symb*COS(A), size_symb*SIN(A), /FILL
    end
    
    10: begin ; * sign
      X = [-size_symb, size_symb, 0, -size_symb,  size_symb,0,0, 0, 0, size_symb, -size_symb]
      Y = [-size_symb, size_symb, 0,  size_symb, -size_symb,0,size_symb, -size_symb,0, 0,  0]
      USERSYM, X, Y,thick=2
    end
    
    11: begin ; X sign
      X = [-size_symb, size_symb, 0, -size_symb,  size_symb]
      Y = [-size_symb, size_symb, 0,  size_symb, -size_symb]
      USERSYM, X, Y,thick=2
    end
    
    12: begin ; - sign
      X = [-size_symb   ,size_symb    ,size_symb     ,-size_symb    ,-size_symb]
      Y = [size_symb*0.2,size_symb*0.2,-size_symb*0.2,-size_symb*0.2,-size_symb*0.2]
      USERSYM, X, Y,thick=2
    end
    
    13:begin ; circle empty
    A = FINDGEN(17) * (!PI*2/16.)
    USERSYM, size_symb*1.7*COS(A), 1.7*SIN(A),thick=2
  end
  
  ; KeesC 20JUN2012
  14: begin ; square line
    X = [-size_symb, -size_symb, size_symb,  size_symb, -size_symb]*.75
    Y = [-size_symb,  size_symb, size_symb, -size_symb, -size_symb]*.75
    USERSYM, X, Y, color=0
  end
  else:  begin ; square line
    print, 'psym not present'
    X = [-size_symb, -size_symb, size_symb,  size_symb, -size_symb]*.75
    Y = [-size_symb,  size_symb, size_symb, -size_symb, -size_symb]*.75
    USERSYM, X, Y, color=0
  end
endcase

end