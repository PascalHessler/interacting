
  estimate.model = function(nux,data,k)
  { 
    
  #DISCRETE
      if (nux <=3)
               {
             #Make xvar a factor to estimate GAM with it
                  data$x = factor(data$x)
                    
             #Estimate model, with /without k
                  if (!is.null(k)) model = try(mgcv::gam(y~s(z,by=x,k=k)+x, data=data),silent=TRUE )
                  if ( is.null(k)) model = try(mgcv::gam(y~s(z,by=x)+x, data=data),    silent=TRUE )
                  check.gam.error(model) #check.gam.error.R - stops if gam gave an error msg
              } #End nux<4
            
  #CONTINUOUS 
      if (nux>=4)
            {
            if (!is.null(k)) model = try(mgcv::gam(y~s(z,k=k)+s(x,k=k)+ti(x,z,k=k),data=data),silent=TRUE) 
            if ( is.null(k)) model = try(mgcv::gam(y~s(z)  +s(x)    +ti(x,z),data=data),silent=TRUE) 
            check.gam.error(model) #check.gam.error.R - stops if gam gave an error msg

      }
  return(model)  
  }
  
