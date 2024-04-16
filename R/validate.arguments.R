
# Validate function
  

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
                              file,
							  xvar,zvar,yvar)

  {
    
    
     #Case 1: data
        if (!is.null(data)) {
          
          
          #1 Create local variable with name of the dataframe for error messages
            dataname = deparse(substitute(data))
            
          #2 Is data a data.frame()
                if (class(data)!='data.frame') exit("interprobe says(): the 'data' argument must be a data.frame, but , '",
                                                    dataname,"' is not a data.frame.")
            
            

          #3 Are x,z,y in the dataset
            n1=names(data)
            if (!xvar %in% n1) exit("interprobe() says the focal variable x ('",xvar,    "') is not in the dataset '",dataname,"'.")
            if (!zvar %in% n1) exit("interprobe() says the moderator variable z ('",zvar,"') is not in the dataset '",dataname,"'.")
            if (!yvar %in% n1) exit("interprobe() says the dependent variable y ('",yvar,"') is not in the dataset '",dataname,"'.")
  
          
          }    
   
return(TRUE)
    
    #1.1 Model was entered by default into x,z,y or data instead of model=
        if (
          any(class(model) %in% c('logical','integer','numeric','data.frame','factor')) | 
          any(class(x) %in% c("lm","glm","gam")) |
          any(class(z) %in% c("lm","glm","gam")) |
          any(class(y) %in% c("lm","glm","gam"))
          )
      
          {
            exit(
               "interprobe() says:\n",
               "There is a problem with the set of arguments provided to the function.\n",
               "If you are providing a model as input, make sure to reference it explictly\n",
               "and to enter the variable names in quotes.\n",
                "\n   For example:\n      lm1=lm(y~cond*age)\n      interprobe(model=lm1,x='cond',z='age') "
               )
          }
    
  #---------------------------------------------------------------------------------
  #Cases
    #Case #1: x,z,y
        
        if (is.null(model) & is.null(data)) 
            {
            #Same length  
              if (length(x) != length(z)) exit("interprobe says(): x and z must have the same length")
        }
    
    #-------------------------------------------------------------
    
   
      
  #-------------------------------------------------------------
  
  #Case 3: Model        
   if (!is.null(model))
      {
        modelname = deparse(substitute(model))

     
        if (! any(class(model) %in% c("lm","glm","gam"))) exit("interprobe() says you provided a model but it is not lm, glm, or gam")
        n2=names(model$model)
        if (!xvar %in% n2) exit("interprobe() says the focal variable x ('",xvar    ,"') is not in the model '", modelname,"'")
        if (!zvar %in% n2) exit("interprobe() says the moderator variable z ('",zvar,"') is not in the model '", modelname,"'")
        }
      
    
          
#-------------------------------------------------------------
          
  #Other arguments        
  
          
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
      #if (length(cols)!=3)    exit(("interprobe() says the argument 'cols' must be of length 3"))
      check1 (cols, "cols", 3, 'character')
      
  #10 Draw
      if (!is.logical(draw)) exit(("interprobe() says the argument 'draw' must be either TRUE or FALSE"))
 
  #11 xlim
      if (!is.null(xlim)) check1 (xlim, "xlim", 2, 'numeric')
      
  #12 file
      if (!is.null(file)) {
              
          #Get extension of file name
              extension= tools::file_ext(file)
                  
          #Type of figure file
              if (!extension %in% c('svg','png')) exit("interprobe() says 'file' must be either a .png or .svg format.")
      
              } #End of file check
      
      
  }#End of function
