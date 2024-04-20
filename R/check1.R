



#Check if a variable is as it should be
 

#1) Auxiliary - Integer?    
    is.integer2 = function(x) all(floor(x)==x)
  

#2) check1(): function that evaluates type and length of a given argument in the function
      check1 = function(var,varname, length.check, type.check)
      {
        
        #Check length
          if (length(var)!=length.check) {
              exit("interprobe() says the argument '",varname,"' must be of length '",length.check,"'\nbut it is of length '",length(var),"'")  
          }
            
      
        #Check type integer
          if (type.check=='integer')
            {
            if (is.integer2(var)==FALSE) {
              exit("interprobe() says the argument '",varname,"' must be an integer, but '",var, "' isn't.")  
            }
              
              }
        
        #Check type character
          if (type.check=='character')
            {
            if (is.character(var)==FALSE) exit("interprobe() says the argument '",varname,"' must be a character variable but '",var, "' isn't.")  
          }
        
        #Check type numeric
          if (type.check=='numeric')
            {
            if (is.numeric(var)==FALSE) exit("interprobe() says the argument '",varname,"' must be a numeric, but '",var, "' isn't.")  
          }
        
        
        }