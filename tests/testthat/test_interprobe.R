


#1. Marginal effect for a specific dataset

    

  test_that('GAM on linear produces expected results', {
      
    #Generate data
      set.seed(111)
      n=1000
      x1=rnorm(n)
      z1=rnorm(n,mean=10,sd=2)
      y.raw = x1*z1
      e=rnorm(n,sd=sd(y.raw))
      y1=y.raw+e
      
    #df
      df=data.frame(x1,y1,z1)
      
    #Linear model
      lm1=lm(y1~x1*z1)
      
    #Get interprobe
      t1=interprobe(x1,z1,y1,draw=FALSE)
      t2=interprobe(model=lm1,x=x1,z=z1,draw=FALSE)
      
    #Check specific floodlight values
      expect_equal(round(t1$floodlight$dydx[21],2), 8.36)
      expect_equal(round(t2$floodlight$dydx[21],2), 8.36)
          }
    )

