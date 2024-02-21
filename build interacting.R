  pkg_path <- "c:/git/interacting/r"
  #pkg_path <- "/Users/andres/Documents/[2] projects/[7] interacting/interacting/R" #in Andres's computer
  
#SOURCE
  
  scripts<-list.files(pkg_path,full.names = TRUE)
  for (scriptk in scripts) {
    if (basename(scriptk) !='interprobe_dev.R') {
    message("next:",basename(scriptk))
    source(scriptk)
    } }
  
  
  
#INSTALL
  
  devtools::document(pkg_path)
  #devtools::build(pkg_path)
  devtools::install(pkg_path, dependencies = FALSE, build = TRUE)

  
  library('interacting')
############
  
  
  
  x1=rnorm(10)
  z1=rnorm(10)+5
  y1=rnorm(10)+25

  g=mgcv::gam(y1~s(x1)+s(z1))
  interprobe_dev(data=data.frame(x1,z1,y1),x='x1',z='z1',y='y1')

  interprobe_dev(model=g , x='x1', z='z1')


