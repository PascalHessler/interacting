


    compute.slopes.continuous=function(spotlights, data, xs)
    {
    simple.slopes = list()
    j=1
    for (zj in spotlights)
      {
      #Make prediction data
        ndj = expand.grid(z=zj,x=xs)
        ndj = add.covariates.at.mean(ndj, data)

      #Save marginal effects results
        options(warn=-1)
        simple.slopes[[j]] = marginaleffects::predictions(model, newdata = ndj)
        options(warn=-0)
        
        #Note: suppress warnings because `marginaleffects` warns about k as a missing variable when it is specified
                 
        j=j+1 
                } #End loop
                
    return(simple.slopes)
    } #End of function

    
