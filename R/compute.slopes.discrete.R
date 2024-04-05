

compute.slopes.discrete=function(ux, zs, model,xvar,zvar)
  {
  simple.slopes = list()
  j=1
  #loop over the unique values of x
    for (xj in ux)
    {
    #Make prediction data for this particular value of x
      ndj = expand.grid(z=zs,x=xj)
      names(ndj)=c(zvar,xvar)

                  
    #Save marginal effects results
      options(warn=-1)
      simple.slopes[[j]] = marginaleffects::predictions(model, newdata = ndj,by=zvar)
      options(warn=-0)
      #Note: suppress warnings because `marginaleffects` warns about k as a missing variable
                 
    j=j+1 
    } 
    #End loop
    
  
  return(simple.slopes)
}
      