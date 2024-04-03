
#1 Auxiliary functions
#2 Validate function


#-----------------------------------------------


#1 Auxiliary functions
  #1.1 Integer?    
    is.integer2 = function(x) all(floor(x)==x)
  
    
  #1.2 Check if a number is an integer of length k
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

#----------------------------------------------

#2 Validate function
  

  validate.arguments=function(x, z ,y , 
                              model, data,
                              k,
                              spotlights,spotlight.labels,
                              histogram, 
                              max.unique,n.bin.continuous, n.max ,
                              xlab,ylab1,ylab2,main1,main2,
                              cols,
                              draw,
                              legend.round,
                              xlim,
                              file)

  {

    #0 Model may not be character, dataframe
    if (class(model) %in% c('logical','integer','numeric','data.frame','factor')) {
      exit("interprobe() says: There is an error in the arguments provided.\n",
           "If you are providing a model as input, make sure to reference \n",
           "it explicitly, for example:\n    lm1=lm(y~x*z)\n    interprobe(model=lm1,x=x,z=z) ")
            }
    
   #1 if x and z are specified they must be of the same length
          if (!is.null(x) && !is.null(z)) {
          
        # Check if they are of the same type 
          if (typeof(x) != typeof(z))       exit("interprobe says(): x and z must be of the same type")
    
        # Check if they have the same length
          if (length(x) != length(z))      exit("interprobe says(): x and z must have the same length")
            }
        
  
    
  #2 If data is specified, check that x,z,y are in it
      if (!is.null(data))
      {
        n1=names(data)
        if (!x %in% n1) exit("interprobe() says the focal variable x ('",x,    "') is not in the dataset")
        if (!z %in% n1) exit("interprobe() says the moderator variable z ('",z,"') is not in the dataset")
        if (!y %in% n1) exit("interprobe() says the dependent variable y ('",y,"') is not in the dataset")
        }
   
    
  #3 If model is specified, check that x,z are in it
 
   if (!is.null(model))
      {
        n2=names(model$model)
        if (!x %in% n2) exit("interprobe() says the focal variable x ('",x    ,"') is not in the model")
        if (!z %in% n2) exit("interprobe() says the moderator variable z ('",z,"') is not in the model")
        }
    
    
  #4 k must have lenght 1 and be an integer
    if (!is.null(k))     check1(k,'k',1,'integer')
      
      

   #5 spotlights must be of length 3
     if (!is.null(spotlights) && length(spotlights) !=3) {
          exit("interprobe() says the argument 'spotlights' must be of length 3")
     }
    
     if (!is.null(spotlight.labels) & length(spotlight.labels) !=3) {
          exit("interprobe() says the argument 'spotlight.labels' must be  of length 3")
     }
    
  #6 Histogram
      if(! is.logical(histogram) && length(histogram) == 1) {
        exit(("interprobe() says, The argument 'histogram' must be TRUE or FALSE and of length 1"))
      }
    

    
  #7  max.unique,n.bin.continuous, n.max ,
        check1 (max.unique, "max.unique", 1, 'integer')
        check1 (n.bin.continuous, "n.bin.continuous", 1, 'integer')
        check1 (n.max, "n.max", 1, 'integer')

  #8 graph axes
      check1 (xlab, "xlab", 1, 'character')
      check1 (ylab1, "ylab1", 1, 'character')
      check1 (ylab2, "ylab2", 1, 'character')
      check1 (main1, "main1", 1, 'character')
      check1 (main2, "main2", 1, 'character')
      check1 (legend.round, "legend.round", 2, 'integer')

      
  #9 Colors
      if (length(cols)!=3)    exit(("interprobe() says the argument 'cols' must be of length 3"))
      
  #10 Draw
      if (!is.logical(draw)) exit(("interprobe() says the argument 'draw' must be either TRUE or FALSE"))
 
  #11 xlim
      if (!is.null(xlim)) check1 (xlim, "xlim", 2, 'numeric')
      
  #12 file
      if (!is.null(file)) {
              
          #Get extension of file name
              extension= tools::file_ext(file)
                  
          #Type of figure file
              if (!extension %in% c('svg','png')) exit("interprobe() says 'file' must be either a png or svg format.")
      
              } #End of file check
      
      
  }#End of functiono
