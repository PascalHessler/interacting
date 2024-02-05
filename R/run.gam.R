  run.gam = function(data,k=NULL)
    {
    
  #Check  data were provided
    if (!is.data.frame(data))                   stop('ran.gam() says:  `data` is not a data.frame)')
    if (!all(names(data) %in% c('x','z','y')))  stop('ran.gam() says: `data` does not contain `x`, `y` and `z` but it should')
    
    
  #Drop observations with any missing values
       #Count OBS
         nbefore=nrow(data)
         
      #Drop missing
         data=data[complete.cases(data),]
      
      #Count again
        ndif = nbefore - nrow(data)
       if (ndif>0) {
         message("interprobe() says: There were ",ndif," observations dropped ",
                 "due to missing values.")
       }
        
        
  #Is x binary?
     binary  <- length(unique(data$x))==2   #True if exactly 2 unique values
       
  #Estimate  model
       
    #Binary model

        if (binary==TRUE) {
        
             #GAM it
               if (!is.null(k)) gam1 = mgcv::gam(y~s(z,by=x,k=k)+x,data=data) 
               if ( is.null(k)) gam1 = mgcv::gam(y~s(z,by=x)+x,data=data) 
            }
       
      #4.2 Non binary
        if (binary==FALSE) {
          
          #k is specified
            if (!is.null(k)) {
              kx  = min(k,length(unique(x)))
              kz  = min(k,length(unique(z)))
              kxz = min(c(k,kx+kz))
              gam1 = mgcv::gam(y~s(x,k=kx)+s(z,k=kz)+ti(x,z,k=kxz), data=data)
  
            }
          #k is not
            if (is.null(k)) gam1=mgcv::gam(y~s(x)+s(z)+ti(x,z))
          
          }
      
       return(gam1)
  }

