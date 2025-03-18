  library('groundhog')
  pkgs=c('mgcv','devtools','this.path')
  date='2025-02-01'
  groundhog.library(pkgs,date)


  pkg_path <- "c:/git/interacting/r"
  

  library('interacting')

 
  
    n=1000
    x1=rnorm(n,mean=100,sd=5)
    z1=rnorm(n,mean=0,sd=1)
    y1.raw=x1*z1
    e=rnorm(n,sd=.2*sd(y1.raw))
    y1=y1.raw+e

    df1=data.frame(v1=x1,v2=z1,v3=y1)
     
#THINGS THAT SHOULD WORK
          
      #1 INPUT SYNTAX
        #1.1 x,z,y
          r1=interprobe(x1,z1,y1)
          
        
        #1.2 data vars with quotes
          dev.off()
          interprobe('v1','v2','v3',data=df1)
      
        #1.3 data vars without quotes
          dev.off()
          interprobe(v1 , v2 , v3 , data=df1)
      
        #1.4 model linear
          dev.off()
          lm1=lm(y1~x1*z1,data=df1)
          interprobe(model=lm1,x=x1,z=z1)
          r2=interprobe(model=lm1,x='x1',z='z1')
          
        
      
        #1.5 model gam
          g1=mgcv::gam(y1~x1*z1,data=df1)
          interprobe(model=g1,x='x1',z='z1')
          interprobe(model=g1,x=x1,z=z1)
          
      #2 Ouput results to variable
          t1=interprobe(model=g1,x='x1',z='z1')
          print(t1)
          
      #3 Make figure
          fig1=file.path(this.path::this.dir(),"Example 3.svg")
          t1=interprobe(model=g1,x='x1',z='z1',save.as=fig1)
          
          
      #4 Discrete X continuous z
          n=1200
          x1=sample(c(1,2,3),n,replace=TRUE)
          z1=rnorm(n,mean=10,sd=2)
          y.raw = x1*z1
          e=rnorm(n,sd=sd(y.raw))
          y1=y.raw+e
          data1=data.frame(x1,y1,z1)
          interprobe(x='x1',z='z1',y='y1')
      
      #5 Discrete X discrete z    
          n=1200
          x1=sample(c(1,2,3),n,replace=TRUE)
          z1=sample(c(1,2,2,2,3,3,4,4,5,6,6,6,6,6,6,6,7,7,7),n,replace=TRUE)
          y.raw = x1*z1
          e=rnorm(n,sd=sd(y.raw))
          y1=y.raw+e
          data1=data.frame(x1,y1,z1)
          
        k = interprobe(x='x1',z='z1',y='y1')
         ik = interprobe(x='x1',z='z1',y='y1',k=2)
         ik = interprobe(x='x1',z='z1',y='y1',k=3)
         ik = interprobe(x='x1',z='z1',y='y1',k=4)
      
          
      #5 Continuous X discrete z    
          n=1200
          x1=rnorm(n,100,10)
          z1=sample(c(1,2,2,2,3,3,4,4,5,6,6,6,6,6,6,6,7,7,7),n,replace=TRUE)
          y.raw = x1*z1
          e=rnorm(n,sd=sd(y.raw))
          y1=y.raw+e
          data1=data.frame(x1,y1,z1)
          f1=interprobe(x='x1',z='z1',y='y1',k=3)
          interprobe(x='x1',z='z1',y='y1',k=3,moderator.on.x.axis = F)
          
          
      
      #6 Covariates
          n=1200
          x1=rnorm(n,100,10)
          z1=sample(c(1,2,2,2,3,3,4,4,5,6,6,6,6,6,6,6,7,7,7),n,replace=TRUE)
          y.raw = x1*z1
          e=rnorm(n,sd=sd(y.raw))
          y1=y.raw+e
          z2=rnorm(n)
          g1=mgcv::gam(y1~s(z1,k=3)+s(x1)+ti(x1,z1,k=3)+s(z2))
          interprobe(model=g1,x=x1,z=z1)
          
        
      
      #7 Factor
          n=1200
          x1=rnorm(n,100,10)
          z1=sample(c(1,2,2,2,3,3,4,4,5,6,6,6,6,6,6,6,7,7,7),n,replace=TRUE)
          y.raw = x1*z1
          e=rnorm(n,sd=sd(y.raw))
          y1=y.raw+e
          z2=sample(c("A","B","C"),n,replace=TRUE)
          z2=factor(z2)
          g1=mgcv::gam(y1~s(z1,k=3)+s(x1)+ti(x1,z1,k=3)+z2)
          interprobe(model=g1,x=x1,z=z1)
   
          
#THINGS THAT SHOULD NOT WORK
          
   n=1000
    x1=rnorm(n,mean=100,sd=5)
    z1=rnorm(n,mean=0,sd=1)
    y1.raw=x1*z1
    e=rnorm(n,sd=.2*sd(y1.raw))
    y1=y1.raw+e
    x2=rnorm(n)
    x3=rnorm(n)

    df0=data.frame(x1=x1,z1=z1,y1=y1)
    df1=data.frame(v1=x1,v2=z1,v3=y1)
    g1=mgcv::gam(y1~s(x1)+s(z1)+ti(x1,z1)+s(x2))
  
    
    
    #8 specify dv and model (get warning only)
      interprobe(model=g1,x=x1,z=z1,y=y1)
      
    #9 specify var does not exist
      interprobe(model=g1,x=x2,z=z1,y=y1)

    #10 specify var does not exist
      interprobe(model=g1,x=x3,z=z1,y=y1)
      
    #11 specify model and data
       interprobe(model=g1,x=x1,z=z1,data=df0)
       
    #12 must specify x,z,y if specifying data
       interprobe(x=x1,z=z1,data=df0)
          
    #13 must include all 3 in vectors
       interprobe(x=x1,z=z1)
          
       
          