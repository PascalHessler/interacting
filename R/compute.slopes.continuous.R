


    compute.slopes.continuous=function(spotlights, data, xs,zs, model,xvar,zvar,moderator.on.x.axis)
    {
    simple.slopes = list()
    j=1
    
    #Simple slopes with x in the x-axis
        if (moderator.on.x.axis==FALSE)
        {
        for (zj in spotlights)
          {
          #Make prediction data
            ndj = expand.grid(z=zj,x=xs)
            names(ndj)=c(zvar,xvar)
            ndj = add.covariates.at.mean(ndj, data)  #utils.R  #Function #7
    
          #Save marginal effects results
            options(warn=-1)
            simple.slopes[[j]] = marginaleffects::predictions(model, newdata = ndj)
            
            
            
            #Note: suppress warnings because `marginaleffects` warns about k as a missing variable when it is specified
                     
            j=j+1 
                    } #End loop
                    
        return(simple.slopes)
          
          
        }
    
    #Simple slopes with z in the x-axis
        if (moderator.on.x.axis==TRUE)
        {
        for (xj in spotlights)
          {
          #Make prediction data
            ndj = expand.grid(z=zs,x=xj)
            names(ndj)=c(zvar,xvar)
            ndj = add.covariates.at.mean(ndj, data)  #utils.R  #Function #7
    
          #Save marginal effects results
            options(warn=-1)
            simple.slopes[[j]] = marginaleffects::predictions(model, newdata = ndj)
            #Note: suppress warnings because `marginaleffects` warns about k as a missing variable when it is specified
                     
            j=j+1 
                    } #End loop
                    
        return(simple.slopes)
          
          
        }
    
    
    
    } #End of function

    
    

    