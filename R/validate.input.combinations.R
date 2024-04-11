

#validate.input.combinations=function(input.xz, input.xyz, input.data, input.model)
validate.input.combinations=function(data, model, x,y,z)


{
  
        input.xz = input.data = input.xyz = input.model <- FALSE
          
        
  #1 if model and y are specified, ignore y
        if (!is.null(model) & !is.null(y)) {
          y=NULL
          message("interprobe() says: You specified both 'model' and 'y', will ignore 'y' and use DV in the model.")
        }

        
  #2 Determine type of input submitted
          if (!is.null(data))                           input.data=TRUE
          if (!is.null(model))                          input.model=TRUE
          if (!is.null(x) & !is.null(z) & !is.null(y))  input.xyz=TRUE
          if (!is.null(x) & !is.null(z) & is.null(y))   input.xz=TRUE
      
   
  
   #3 Specified data & model
        if (input.data + input.model==2) {
              exit("interprobe() says: You may include either a data or a model argument, but you included both.")
              }
   
  
     
   #4 Data without xyz
        if (input.data==TRUE & input.xyz==FALSE) {
          exit("interprobe says: you specified a dataset but not x,y,z, recall that:\n",
               "x: focal predictor\n",
               "z: moderator\n",
               "y: dependent variable")
         }

  
   #5 Model without xz
      if (input.model==TRUE & input.xz==FALSE) {
          message("interprobe says: you specified a model but not x and z, recall that:\n",
               "x: focal predictor\n",
               "z: moderator")
          exit()
         }

  #6 Model with xyz
      if (input.model==TRUE & input.xyz==TRUE) {
          message("interprobe says: you specified both a model and the y argument.\nPlease specify only one of them");
          exit()
         }

            
  
   
        if (input.data + input.xyz + input.model==0) {
            message("interprobe says:\nYou must specify the data by providing at least one of these arguments:\ndata, or 'x,y,z', or model")
            exit()
            }

   
    #7 return input type
          return(namedList(input.data, input.xyz, input.model, input.xz))
   
    }