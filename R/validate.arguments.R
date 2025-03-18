
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
                              save.as,
							                xvar,zvar,yvar,
							                x.ticks, y1.ticks,y2.ticks,
							                moderator.on.x.axis)

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
            if (!xvar %in% n1) exit("interprobe() says the focal variable ('",xvar,    "') is not in the dataset '",dataname,"'.")
            if (!zvar %in% n1) exit("interprobe() says the moderator variable ('",zvar,"') is not in the dataset '",dataname,"'.")
            if (!yvar %in% n1) exit("interprobe() says the dependent variable ('",yvar,"') is not in the dataset '",dataname,"'.")
  
          
          }    
   

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
  
  #Case 3: Model        
   if (!is.null(model))
      {
        modelname = deparse(substitute(model))

     
        if (! any(class(model) %in% c("lm","glm","gam"))) exit("interprobe() says you provided a model but it is not lm, glm, or gam")
        vars <- all.vars(terms(model))[-1]  # Remove the response variable
        if (!xvar %in% vars) exit("interprobe() says the focal variable x ('",xvar    ,"') is not in the model '", modelname,"'")
        if (!zvar %in% vars) exit("interprobe() says the moderator variable z ('",zvar,"') is not in the model '", modelname,"'")
        }
      
    
          
#-------------------------------------------------------------
          
  #Other arguments        
  
          
  #4 k must have length 1 and be an integer
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
      #check1 (draw, "draw", 1, 'character')
      if (!draw %in% c('both','simple slopes','jn')) {
        exit("interprobe() says that the argument 'draw' must be one of three values:\n  - 'both'\n  - 'simple slopes'\n  - 'jn'")
        }

  #11 xlim
      if (!is.null(xlim)) check1 (xlim, "xlim", 2, 'numeric')
      
  #12 file
      if (!is.null(save.as)) {
              
          #Get extension of file name
              extension= tools::file_ext(save.as)
                  
          #Type of figure file
              if (!extension %in% c('svg','png')) exit("interprobe() says 'file' must be either a .png or .svg format.")
      
              } #End of file check
      
      
  #13 If submitted a model, and x is categorical, it must be a factor
      
      
      if (!is.null(model))
        {
        #13.1 Focal predictor must be a factor if nux<=3
            x = model$model[,xvar]
            nux = length(unique(x))
            model_txt = clean_string(deparse(substitute(model)))

            if (nux<=3 & class(x)!='factor') {
            exit(paste0("ERROR.\n   ",
                  "interprobe() says the focal predictor x ('", xvar,"') has only \n   ",
                 nux, " possible valuess. Make sure to define it as a factor before\n   ",
                 "estimating the model '",model_txt,"'. You  can do that by running\n   ",
                 "df$",xvar," <- factor(df$",xvar,"), where df is the name of the \n   ",
                 "data frame containing  '",xvar,"'."))
            }
            
        #13.2 No factors in formula
            formula.txt = paste0(as.character(model$call),collapse=' ')
            if (as.numeric(regexpr('factor\\(', formula.txt)) >0) {
              exit("interprobe() says: you defined a variable as factor\n   ",
                   "within the '",model_txt,"' call. This creates problems.\n   ",
                   "Please define the factor variables as factors in \n   ",
                   "the data before estimating the model \n   e.g., df$x <- factor(df$x)).")
            }
              
      }
      
      
  #14 x-ticks and y-ticks
      #14.1 Numeric or data.frame
      
          if (!is.null(x.ticks) &&  !class(x.ticks) %in% c('numeric','data.frame','integer')) {
            exit("interprobe() says: the argument 'x.ticks' must be either a\n ",
                 "numeric vector or a dataframe, but it is instead '",class(x.ticks),"'.")
          }
 
        if (!is.null(y1.ticks) &&  !class(y1.ticks) %in% c('numeric','data.frame','integer')) {
            exit("interprobe() says: the argument 'y1.ticks' must be either a\n ",
                 "numeric vector or a dataframe, but it is instead '",class(y1.ticks),"'.")
        }
      
      if (!is.null(y2.ticks) &&  !class(y2.ticks) %in% c('numeric','data.frame','integer')) {
        exit("interprobe() says: the argument 'y2.ticks' must be either a\n ",
             "numeric vector or a dataframe, but it is instead '",class(y2.ticks),"'.")
      }
      #If dataframe, 2 cols
        if (!is.null(y1.ticks) &&  class(y1.ticks) =='data.frame' && ncol(y1.ticks)!=2) 
          {
                exit("interprobe() says: the argument 'y1.ticks' must be either a\n",
               "numeric vector or a dataframe with 2 columns, but it is a\n",
               "dataframe with '",ncol(y1.ticks),"' columns.")
          } 
          
      #If dataframe, 2 cols
          if (!is.null(y2.ticks) &&  class(y2.ticks) =='data.frame' && ncol(y2.ticks)!=2) {
            
              exit("interprobe() says: the argument 'y2.ticks' must be either a\n",
                   "numeric vector or a dataframe with 2 columns, but it is a\n",
                   "dataframe with '",ncol(y2.ticks),"' columns.")
            } 
        
      
  #15 moderator.on.x.axis
      check1 (moderator.on.x.axis, "moderator.on.x.axis", 1, 'logical')

      
  #16 Three unique variables were set
      if (length(unique(c(xvar,zvar,yvar)))<3) {
          exit("interprobe() says: you seem to have entered the same variable twice")
        
      }
      
      
  }#End of function
