
  
  #note:
  #To estaimte a model we generate a text string with the command to be run and then we execute it
  #This is needed because the way we reference variables is with data[,xvar], but if we run gam()
  #with that syntax, it cannot run predict(), so we must run it with gam(y~s(xvar)) and to do that we create a text
  #version of the model and we then execute it using function eval2() in utils.R Function 8
  #----------------------------------------------------------------------



estimate.model = function(nux,data,k,xvar,zvar,yvar)
  { 
    
  #First figure out if we will add k
      k_if_specified = ifelse(is.null(k), "" , paste0(",k=",k))
      #Creates empty string, or k restriction, added to GAM() statement            
      
      
    
  #DISCRETE
      if (nux <=3)
            {
             #Setup model statement with variable names entered by user
                #Make the statement
                    model.text=paste0('try(mgcv::gam(' , yvar, '~' , 's(', zvar, ',by=', xvar , k_if_specified,')', '+' , 
                                      xvar,', data=data,method="REML"),silent=TRUE)')
             
                    
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
                                        'ti(',xvar, ',' , zvar, k_if_specified,'),method="REML", data=data),silent=TRUE)') 
          
            #Run model
                 model = eval2(model.text)
                               
            #Check  errors
                check.gam.error(model) #check.gam.error.R - stops if gam gave an error msg

            }
  return(model)  
  }
  
 