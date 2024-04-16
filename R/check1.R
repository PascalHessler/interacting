



#Check if a variable is as it should be
 

#1) Auxiliary - Integer?    
    is.integer2 = function(x) all(floor(x)==x)
  

#2) Check 1 function
      check1 = function(var,varname, length.check, type.check)
      {
        
        #Check legnth
          if (length(var)!=length.check) {
              exit("interprobe() says the variable '",varname,"' must be of length '",length.check,"' but it is of length '",length(var),"'")  
          }
            
      
        #Check type integer
          if (type.check=='integer')
            {
            if (is.integer2(var)==FALSE) exit("interprobe() says the variable '",varname,"' must be an integer, but '",var, "' is not an integer.")  
          }
        
        #Check type character
          if (type.check=='character')
            {
            if (is.character(var)==FALSE) exit("interprobe() says the variable '",varname,"' must be a character variable but '",var, "' is not a character variable")  
          }
        
        #Check type numeric
          if (type.check=='numeric')
            {
            if (is.numeric(var)==FALSE) exit("interprobe() says the variable '",varname,"' must be a numeric, but '",var, "' is not numeric")  
          }
        
        
        }