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
   
  

    #SOURCE
      rm(list = ls())

  pkg_path <- "c:/git/interacting/r"
  
  scripts<-list.files(pkg_path,full.names = TRUE)
  for (scriptk in scripts) {
    if (!basename(scriptk) %in% c('interprobe.R','interprobe_dev.R','interprobe_dev2.R')) {
    message("next:",basename(scriptk))
    source(scriptk)
    } }
  
  
  case=2
   data=NULL
  model=NULL
  k=3
  zs=NULL
  spotlights=NULL
  spotlight.labels=NULL
  
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
  focal.label = 'Focal Predictor (x)'
  
  force.discrete.freqs=FALSE
   n.bin.continuous = 10
  max.unique=11
  draw.simple.slopes=TRUE
  draw.floodlight=TRUE
  legend.max.d=5
  legend.min.d=2

  
  
  

    #SOURCE
      rm(list = ls())

  pkg_path <- "c:/git/interacting/r"
  
  scripts<-list.files(pkg_path,full.names = TRUE)
  for (scriptk in scripts) {
    if (!basename(scriptk) %in% c('interprobe.R','interprobe_dev.R','interprobe_dev32.R')) {
    message("next:",basename(scriptk))
    source(scriptk)
    } }
  
  case=3
  
#nux>11 , z>3   (continuous,continuous)
   if (case==1)
   {
    age=rnorm(1000)
    z=rnorm(1000)
    m1=rnorm(1000,mean=10)
    m2=rnorm(1000,mean=15)
    y.raw=x*z+m1+m2
    e=rnorm(1000,sd=sd(y.raw))
    y=y.raw+e
   }
  
  g1 = mgcv::gam(y~s(x)+s(z)+ti(x,z))
  t1 = interprobe_dev(model=g1,x='age',z='z',xlab="Years of Experience",ylab1='Salary',ylab2='Effect on Salary')

  names(t1$simple.slopes)
  t1$simple.slopes
  
  df2=t1$floodlight
  df1=t1$simple.slopes.df
  t1$simple.slopes
  names(df2)
  t1$simple.slopes
  
  t2=df1[,-c('rowid')]
      names(t1$simple.slopes.df)

  
  svg("c:/temp/f1.svg",width=12,height=8)  
    interprobe_dev(model=g1,x='x',z='z',xlab="Years of Experience",ylab1='Salary',ylab2='Effect on Salary')
dev.off()
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
  x=sample(c(0,1),size=1000,replace=TRUE)
    z=rnorm(1000,mean=10,sd=5)
    m1=rnorm(1000,mean=10)
    m2=rnorm(1000,mean=15)
    y.raw=x*z*4
    e=rnorm(1000,sd=sd(y.raw))
    y=y.raw+e
    
  }
  
  
  interprobe_dev(x,z,y)
  
  
  
  
  
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