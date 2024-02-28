  pkg_path <- "c:/git/interacting/r"
  #pkg_path <- "/Users/andres/Documents/[2] projects/[7] interacting/interacting/R" #in Andres's computer
  
  
#INSTALL
  
  devtools::document(pkg_path)
  #devtools::build(pkg_path)
  devtools::install(pkg_path, dependencies = FALSE, build = TRUE)

  
  library('interacting')
############
  
  
#------------------------------------------------------------------------------------------------
  #SOURCE
  pkg_path <- "c:/git/interacting/r"
  
  scripts<-list.files(pkg_path,full.names = TRUE)
  for (scriptk in scripts) {
    if (!basename(scriptk) %in% c('interprobe.R','interprobe_dev____.R')) {
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
  pkg_path <- "c:/git/interacting/r"
  
  scripts<-list.files(pkg_path,full.names = TRUE)
  for (scriptk in scripts) {
    if (!basename(scriptk) %in% c('interprobe.R','interprobe_dev____.R')) {
    message("next:",basename(scriptk))
    source(scriptk)
    } }
  n=150
  xn=rep(c(1,2,3),n)
  levels=sort(unique(xn))
  labels=c('low','med','high')
  x=factor(xn,levels=levels,labels=labels)  
  z=sample(c(1,2,3,4,4,4,4,5,6,7,7,7,7),size=length(x),replace=TRUE)
  z=rnorm(length(x),100,10)
  y.raw=xn*z
  e=rnorm(length(x),sd=sd(y.raw))
  y=y.raw+e
  
  svg("c:/temp/f1.svg")
  interprobe_dev(x=x,z=z,y=y,k=3)
  dev.off()
pretty(c(-65,458))
  