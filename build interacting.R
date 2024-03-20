  pkg_path <- "c:/git/interacting/r"
  #pkg_path <- "/Users/andres/Documents/[2] projects/[7] interacting/interacting/R" #in Andres's computer
  
  
#INSTALL
  
  devtools::document(pkg_path)
  #devtools::build(pkg_path)
  devtools::install(pkg_path, dependencies = FALSE, build = TRUE)

  
  library('interacting')
  library('mgcv')
############
  
  
#------------------------------------------------------------------------------------------------
  #SOURCE
  
  
  pkg_path <- "c:/git/interacting/r"
  
  scripts<-list.files(pkg_path,full.names = TRUE)
  for (scriptk in scripts) {
    if (!basename(scriptk) %in% c('interprobe.R','interprobe_dev.R')) {
    message("next:",basename(scriptk))
    source(scriptk)
    } }
  
  
  
  
#DEBUG
   x=NULL
   z=NULL
   y=NULL
  data=NULL
  model=NULL
  k=NULL
  zs=NULL
  spotlights=NULL
  draw=TRUE
  histogram=TRUE
  nbins=NULL
  shade.up.to = 50  #below this sample size we shade to show few observations
  xlab='moderator'
  cols=c('red4','blue4','green4')
  ylab1='Dependent Variable'
  ylab2='Marginal Effect'
  main1="GAM Simple Slopes"
  main2='GAM Floodlight'
  force.discrete.freqs=FALSE
   n.bin.continuous = 10
  max.unique=11
  draw.simple.slopes=TRUE
  draw.floodlight=TRUE
  

    #SOURCE
      rm(list = ls())

  pkg_path <- "c:/git/interacting/r"
  
  scripts<-list.files(pkg_path,full.names = TRUE)
  for (scriptk in scripts) {
    if (!basename(scriptk) %in% c('interprobe.R','interprobe_dev.R')) {
    message("next:",basename(scriptk))
    source(scriptk)
    } }
  
  
  
   data=NULL
  model=NULL
  k=NULL
  zs=NULL
  spotlights=NULL
  draw=TRUE
  histogram=TRUE
  nbins=NULL
  shade.up.to = 50  #below this sample size we shade to show few observations
  xlab='moderator'
  cols=c('red4','blue4','green4')
  ylab1='Dependent Variable'
  ylab2='Marginal Effect'
  main1="GAM Simple Slopes"
  main2='GAM Floodlight'
  force.discrete.freqs=FALSE
   n.bin.continuous = 10
  max.unique=11
  draw.simple.slopes=TRUE
  draw.floodlight=TRUE
  

  
#Continuous
    x=rnorm(1000)
    z=rnorm(1000)
    m1=rnorm(1000,mean=10)
    m2=rnorm(1000,mean=15)
    y.raw=x*z+m1+m2
    e=rnorm(1000,sd=sd(y.raw))
    y=y.raw+e
    
    interprobe_dev(x,z,y)
    
    
    g=gam(y~s(x)+s(z)+ti(x,z)+s(m1)+m2)
    model=g
    x='x'
    z='z'
    y=NULL
    
    
    
#Discrete
    n=200
    x=rep(c(1,2,3),n)
    z=rnorm(3*n)
    y.raw=x*z
    e=rnorm(length(y.raw),sd=sd(y.raw))
    y=y.raw+e
    
 
    
  g=mgcv::gam(y~s(z,by=x,k=3)+x)
  lm1=lm(y~x*z)
  interprobe_dev(model=g)
  interprobe_dev(model=g,x='x',z='z')
  interprobe_dev(model=g,x='x',z='z')
  interprobe_dev(model=lm1,x='x',z='z')

  x='x'
  z='z'
  y=NULL
  
  
  i1=interprobe_dev(x=x,z=z,y=y,k=3)
  i1$fx
  df=i1$simple.slopes
  
  data
  
  svg("c:/temp/f1.svg")
  dev.off()

  
  
    
  
  rsvg::rsvg_png("c:/temp/f1.svg", "c:/temp/f1.svg.png")