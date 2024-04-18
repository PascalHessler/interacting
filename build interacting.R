  
rm(list = ls())
pkg_path <- "c:/git/interacting/r"
  #pkg_path <- "/Users/andres/Documents/[2] projects/[7] interacting/interacting/R" #in Andres's computer
  
  
#INSTALL
  #devtools::document(pkg_path)
  #devtools::build(pkg_path)
  devtools::install(pkg_path, dependencies = FALSE, build = TRUE)
  library('interacting')

  install.package(path, repos=NULL)
###################################################################

  
    #SOURCE
      rm(list = ls())

  pkg_path <- "c:/git/interacting/r"
  
  scripts<-list.files(pkg_path,full.names = TRUE)
  for (scriptk in scripts) {
    if (!basename(scriptk) %in% c('interprobe___.R','interprobe___.R','interprobe2.R')) {
    message("next:",basename(scriptk))
    source(scriptk)
    } }
  y=NULL
   data=NULL
  model=NULL
  k=3
  zs=NULL
  spotlights=NULL
  spotlight.labels=NULL
  
  draw = TRUE
  histogram = TRUE
  nbins = NULL
  n.max = 50  #below this sample size we shade to show few observations
  xlab='moderator'
  cols=c('red4','blue4','green4')
  ylab1='Dependent Variable'
  ylab2='Marginal Effect'
  main1="GAM Simple Slopes"
  main2='GAM Floodlight'
  focal.label = 'Focal Predictor (x)'
  xlim=NULL
  force.discrete.freqs=FALSE
   n.bin.continuous = 10
  max.unique=11
  draw.simple.slopes=TRUE
  draw.floodlight=TRUE
  legend.round=c(2,4)
  file=NULL
  
  
  
  
  
   n=1000
    x1=rnorm(n)
    z1=rnorm(n,mean=150,sd=30)
    y.raw = x1*sqrt(z1)
    e=rnorm(n,sd=sd(y.raw))
    y1=y.raw+e
    data1=data.frame(x2=x1,y2=y1,z2=z1)
  
    
  
  interprobe('x2','z2','y2',data=data1)
    interprobe(x2,z2,y2,data=data1)

  #devtools::test()

  library('interacting')
    n=100
    x=sample(c(1,2,3),n,replace=TRUE)
    z=sample(c(1,2,3,4,5,6,6,6,7,7,7),replace=TRUE,size=n)
    y.raw = x*sqrt(z)
    e=rnorm(n,sd=sd(y.raw))
    z1=sample(c(44,55,66),n,r=T)
    y=y.raw+e
    
    
    
    z1=factor(z1)
    x=factor(x)
    
    
    g1=mgcv::gam(y~s(z,by=x,k=3))
    interprobe(x,z,y)
    interprobe(g1,x='x',z='z')
    interprobe(g1,x,z)
    class(model)
    
    
    x=g1
    x=z
    z=y

    class(g1)
    class(arguments$y)
    
    
       r1= interprobe(x=x,z=z,y=y)
       floodlight=fl
       fl[[1]]
       
d=r1$simple.slopes
d2=r1$floodlight
    
    type='floodlight'
    res=floodlight
    ylab='dydx'
    
    
    
    
str(ndj)
    
    xj=ux[2]
    
    ndj
    ndj
    ndj$z2=factor(ndj$z2)
    
    marginaleffects::slopes(model)
    marginaleffects::slopes(g1)
    nd=
    marginaleffects::slopes(model,newdata=ndj[1,])
    marginaleffects::slopes(model,newdata=ndj.hand[1,])
    ndj.hand=data.frame(z=1,x=1,y=4.25,z2='aa')
    str(ndj)
    str(ndj.hand)
    ndj$x=as.numeric(ndj$x)
    ndj$z2=as.character(ndj$z2)
    ndj
    names(ndj)
    names(ndj.hand)
    ndj==ndj.hand
    summary(ndj)
    ndj=data.frame(ndj)
    d=data.frame(z=1,x=1,y=4.25,z2='aa')
    ndj[1,]
    d[1,]=ndj[1,]
    
    names(d)
    names(ndj)
    d2=data.frame(z=1,x=1,y=0,z2='aa')
    model$model
    d=model$model
    d=d[,-1]
    predict
    ndj  
      insight::get_data(model)
    
    
    
    
    interprobe(x=x2,y=y2,z=z2,data=data1)
    x=x1
    z=z1
    y=y1
    
     interprobe(x,z,y)


  
    