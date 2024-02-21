  pkg_path <- "c:/git/interacting/r"
  #pkg_path <- "/Users/andres/Documents/[2] projects/[7] interacting/interacting/R" #in Andres's computer
  scripts<-list.files(pkg_path,full.names = TRUE)
  for (scriptk in scripts) {
    if (basename(scriptk) !='interprobe_dev.R') {
    message("next:",basename(scriptk))
    source(scriptk)
        } }