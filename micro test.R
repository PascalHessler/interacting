library('groundhog')
pkgs=c('mgcv','devtools','this.path')
date='2024-02-01'
groundhog.library(pkgs,date)


  pkg_path <- "c:/git/interacting/r"
  #pkg_path <- "/Users/andres/Documents/[2] projects/[7] interacting/interacting/R" #in Andres's computer
  
  
#INSTALL
  #devtools::document(pkg_path)
  #devtools::build(pkg_path,path=this.dir())
  devtools::install(pkg_path, dependencies = FALSE, build = TRUE)
  
  library('interacting')

  
   n=1000
    x1=rnorm(n)
    z1=rnorm(n,mean=10,sd=2)
    y.raw = x1*z1
    e=rnorm(n,sd=sd(y.raw))
    y1=y.raw+e
    data1=data.frame(x1,y1,z1)
  
#1 INPUT SYNTAX
  #1.1 x,z,y
    interprobe(x1,z1,y1)
    
  #1.2 data
    interprobe(x1,z1,y1,data=data1)
  
  #1.3 model linear
    lm1=lm(y1~x1*z1,data=data1)
    interprobe(model=lm1,x=x1,z=z1)
  
  #1.4 model gam
    g1=mgcv::gam(y1~x1*z1,data=data1)
    interprobe(model=g1,x='x1',z='z1')
    
#2 SAVE
    t1=interprobe(model=g1,x='x1',z='z1')
    print(t1)
    
#3 Make figure
    fig1=file.path(this.dir(),"Example 3.svg")
    t1=interprobe(model=g1,x='x1',z='z1',file=fig1)
    
    
#4 Discrete (BUG PRESENT!)
    n=1200
    x1=sample(c(1,2,3),n/3,replace=TRUE)
    z1=rnorm(n,mean=10,sd=2)
    y.raw = x1*z1
    e=rnorm(n,sd=sd(y.raw))
    y1=y.raw+e
    data1=data.frame(x1,y1,z1)
    interprobe(x='x1',z='z1',y='y1')

    
    
