

#validate.input.combinations=function(input.xz, input.xyz, input.data, input.model)
validate.input.combinations=function(data, model, x,y,z)


{
  
        input.xz = input.data = input.xyz = input.model <- FALSE
         
          if (!is.null(data))                           input.data=TRUE
          if (!is.null(model))                          input.model=TRUE
          if (!is.null(x) & !is.null(z) & !is.null(y))  input.xyz=TRUE
          if (!is.null(x) & !is.null(z) & is.null(y))   input.xz=TRUE
      
  
  
   #1 Specified data & model
        if (input.data + input.model==2) {
              stop("interprobe() says, you may include either a data, or a model statement,\n",
                   "but you specified both.")
              }
   
  
     
   #2 Data without xyz
        if (input.data==TRUE & input.xyz==FALSE) {
          stop("interprobe says: you specified a dataset but not x,y,z, recall that:\n",
               "x: focal predictor\n",
               "z: moderator\n",
               "y: dependent variable")
         }

  
   #3 Model without xz
      if (input.model==TRUE & input.xz==FALSE) {
          stop("interprobe says: you specified a model but not x and z, recall that:\n",
               "x: focal predictor\n",
               "z: moderator")
         }

  #3 Model with xyz
      if (input.model==TRUE & input.xyz==TRUE) {
          stop("interprobe says: you specified both a model and the y argument.\nPlease specify only one of them");
         }

            
  
   
        if (input.data + input.xyz + input.model==0) {
            stop("interprobe says:\nYou must specify the data by providing at least one of these arguments:\ndata, or 'x,y,z', or model")
            }

   
    #4 return input type
          return(namedList(input.data, input.xyz, input.model, input.xz))
   
    }