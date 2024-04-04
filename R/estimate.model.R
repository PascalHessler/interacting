
  estimate.model = function(nux,data,k,xvar,zvar,yvar)
  { 
    
  #First figure out if we will add k
      k_if_specified = ifelse(is.null(k), "" , paste0(",k=",k))
                  
    
  #DISCRETE
      if (nux <=3)
            {
             #Setup model statement with variable names entered by user
                #Make the statement
                    model.text=paste0('try(mgcv::gam(' , yvar, '~' , 's(', zvar, ',by=', xvar , k_if_specified,')', '+' , xvar,", data=data),silent=TRUE)")
             
                    
               #Ensure xvar is a factor
                  data[,xvar] = factor(data[,xvar])
                    
             #Run model
                 model = eval2(model.text)
                    
              #Check for possible error
                 check.gam.error(model) #check.gam.error.R - stops if gam gave an error msg
                 
              } #End nux<=3
            
  #CONTINUOUS 
      if (nux>=4)
            {
            #Make the model statement in text
                model.text=paste0('try(mgcv::gam(' , yvar, '~' , 
                                        's(', zvar, k_if_specified,')', '+' ,
                                        's(', xvar, k_if_specified,')', '+' , 
                                        'ti(',xvar, ',' , zvar, k_if_specified,'),data=data),silent=TRUE)') 
            
            #message(model.text)  
            #exit()
            #Run model
                 model = eval2(model.text)
                               
            #Check  errors
                check.gam.error(model) #check.gam.error.R - stops if gam gave an error msg

            }
  return(model)  
  }
  
 