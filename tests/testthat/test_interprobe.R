


#1. Marginal effect for a specific dataset

    

  test_that('GAM on linear produces expected results', {
      
    #Generate data
      set.seed(111)
      n=1000
      x=rnorm(n)
      z=rnorm(n,mean=10,sd=2)
      y.raw = x*z
      e=rnorm(n,sd=sd(y.raw))
      y=y.raw+e
    #Linearr model
      lm1=lm(y~x*z)
      
    #Get interprobe
      t1=interprobe(x,z,y,draw=FALSE)
      t2=interprobe(model=lm1,x=x,z=z,draw=FALSE)
    
    #Check specific floodlight values
      expect_equal(round(t1$floodlight$dydx[21],2), 8.36)
      expect_equal(round(t2$floodlight$dydx[21],2), 8.36)
          }
    )

