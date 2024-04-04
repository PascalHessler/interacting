  pkg_path <- "c:/git/interacting/r"
  #pkg_path <- "/Users/andres/Documents/[2] projects/[7] interacting/interacting/R" #in Andres's computer
  
  
#INSTALL
  devtools::document(pkg_path)
  #devtools::build(pkg_path)
  devtools::install(pkg_path, dependencies = FALSE, build = TRUE)
  library('interacting')

  
  devtools::test()

  
#------------------------------------------------------------------------------------------------

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
  
  draw=TRUE
  histogram=TRUE
  nbins=NULL
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
  library('interacting')

#nux>11 , z>3   (continuous,continuous)
    n=1000
    x1=rnorm(n)
    z1=rnorm(n,mean=10,sd=2)
    y.raw = x1*z1
    e=rnorm(n,sd=sd(y.raw))
    y1=y.raw+e
  
    
    x=x1
    z=z1
    
    lm1=lm(y1~x1*z1)
    model=lm1
    data1=data.frame(x1,z1,y1)
    interprobe(x=x1 ,z=z1,y=y1 )
    
    interprobe(model=lm1,x=x1 ,z=z1)
    
    
    
    
    
    
    n=1000
    x=rnorm(n)
    z=rnorm(n,mean=10,sd=2)
    y.raw = x*z
    e=rnorm(n,sd=sd(y.raw))
    y=y.raw+e
  
    
    lm2=lm(y~x*z)
    #model=lm2
    data1=data.frame(x1,z1,y1)
    interprobe(x=x ,z=z,y=y )
    
    interprobe(model=lm2,x=x ,z=z)
    
    
    
    traceback()
debug(interprobe)

undebug(interprobe)
    x=x1
    z=z1
    model=lm1
    
  g=lme4::lmer(y~x+1|s)
    class(g)
    class(data.frame(x,z))
#nux = 7 , z>3   (continuous K=7  ,  continuous)
  if (case==2) {  
  x=sample(c(1,2,3,4,5,5,5,5,5,6,6,6,6,6,7),size=1000,replace=TRUE)
    z=rnorm(1000,mean=10,sd=5)
    m1=rnorm(1000,mean=10)
    m2=rnorm(1000,mean=15)
    y.raw=x*z+m1+m2
    e=rnorm(1000,sd=sd(y.raw))
    y=y.raw+e
    
  }
  
  
#CASE 3 nux = 3 , z>3   (continuous K=7  ,  continuous)
  if (case==3) {  
  x=sample(c(0,1,2),size=1000,replace=TRUE)
    z=rnorm(1000,mean=10,sd=5)
    m1=rnorm(1000,mean=10)
    m2=rnorm(1000,mean=15)
    y.raw=x*z*4
    e=rnorm(1000,sd=sd(y.raw))
    y=y.raw+e
    
  }
  
  
  interprobe(x,z,y,file='c:/temp/test1.png')
  
  
  
  
  
  lines.total=3
  
  
 res=simple.slopes
 
 res=floodlight
  ylab='dydx'
  main='flooding'
  xlab='z'
  x1s=xs
  
  type='floodlight'
#Discrete x, continuous z
    n=200
    x=rep(c(1,2,3),n)
    z=rnorm(3*n)
    y.raw=x*z
    e=rnorm(length(y.raw),sd=sd(y.raw))
    y=y.raw+e
    
 
    
    
  g=mgcv::gam(y~s(z,by=x,k=3)+x)
  lm1=lm(y~x*z)
  interprobe(model=g)
  interprobe(model=g,x='x',z='z')
  interprobe(model=g,x='x',z='z')
  interprobe(model=lm1,x='x',z='z')

  x='x'
  z='z'
  y=NULL
  
  
  i1=interprobe(x=x,z=z,y=y,k=3)
  i1$fx
  df=i1$simple.slopes
  
  data
  
  svg("c:/temp/f1.svg")
  dev.off()

  
  
    
  
  rsvg::rsvg_png("c:/temp/f1.svg", "c:/temp/f1.svg.png")