library('groundhog')
pkgs=c('mgcv','devtools','this.path')
date='2025-02-01'
groundhog.library(pkgs,date)


  pkg_path <- "c:/git/interacting/r"
  #pkg_path <- "/Users/andres/Documents/[2] projects/[7] interacting/interacting/R" #in Andres's computer
  
  
#INSTALL
  #devtools::document(pkg_path)
  #devtools::build(pkg_path,path=this.dir())
  devtools::install(pkg_path, dependencies = FALSE, build = TRUE)
  
  library('interacting')

  #y sweat
  #x is excerize
  #z is temperature
  
   n=1000
    x1=rnorm(n,mean=100,sd=5)
    z1=rnorm(n,mean=0,sd=1)
    y1.raw=x1*z1
    e=rnorm(n,sd=.2*sd(y1.raw))
    y1=y1.raw+e

    data1=data.frame(x1,z1,y1)
    r=interprobe(x1,z1,y1)
     
#1 INPUT SYNTAX
  #1.1 x,z,y
    r1=interprobe(x1,z1,y1,k=6)
    
    
  #1.2 data
    interprobe('x1','z1','y1',data=data1)
    interprobe(x1 , z1 , y1 , data=data1)

  #1.3 model linear
    lm1=lm(y1~x1*z1,data=data1)
    interprobe(model=lm1,x=x1,z=z1)
    r2=interprobe(model=lm1,x='x1',z='z1')
    
    r2$regions.jn

  #1.4 model gam
    g1=mgcv::gam(y1~x1*z1,data=data1)
    interprobe(model=g1,x='x1',z='z1')
    interprobe(model=g1,x=x1,z=z1)
    
#2 Ouput results to variable
    t1=interprobe(model=g1,x='x1',z='z1')
    print(t1)
    
#3 Make figure
    fig1=file.path(this.path::this.dir(),"Example 3.svg")
    t1=interprobe(model=g1,x='x1',z='z1',file=fig1)
    
    
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
    
    
    interprobe(x='x1',z='z1',y='y1',file='c:/temp/f1.svg')

    
#5 Continuous X discrete z    
    n=1200
    x1=rnorm(n,100,10)
    z1=sample(c(1,2,2,2,3,3,4,4,5,6,6,6,6,6,6,6,7,7,7),n,replace=TRUE)
    y.raw = x1*z1
    e=rnorm(n,sd=sd(y.raw))
    y1=y.raw+e
    data1=data.frame(x1,y1,z1)
    interprobe(x='x1',z='z1',y='y1')
    
    

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
    
    
    
    g1=mgcv::gam(y1~s(z1,k=3)+s(x1)+ti(x1,z1,k=3)+factor(z2))
    nd=data.frame(x1=100,z1=1,z2="A")
    marginaleffects::slopes(g1,newdata = nd)
    
    
    
    