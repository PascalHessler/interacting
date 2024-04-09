  pkg_path <- "c:/git/interacting/r"
  #pkg_path <- "/Users/andres/Documents/[2] projects/[7] interacting/interacting/R" #in Andres's computer
  
  
#INSTALL
  devtools::document(pkg_path)
  #devtools::build(pkg_path)
  devtools::install(pkg_path, dependencies = FALSE, build = TRUE)
  library('interacting')

  
  devtools::test()

  
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
  

  
 
  n=1200
    x1=sample(c(1,2,3),n,replace=TRUE)
    z1=sample(c(1,2,3,4,5,6,6,6,7,7,7),replace=TRUE,size=n)
    y.raw = x1*z1
    e=rnorm(n,sd=sd(y.raw))
    y1=y.raw+e
    data1=data.frame(x=x1,y=y1,z=z1)
    
    x=x1
    z=z1
    y=y1
    main='tests'
     type='simple slopes'
  res=simple.slopes
  ylab=ylab1
    
    
    reds=interprobe(x=x1,z=z1,y=y1)