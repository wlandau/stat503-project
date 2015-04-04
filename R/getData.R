studentUSA2012 = function(){
  regions = c("United States of America", "Connecticut (USA)", "Florida (USA)", "Massachusetts (USA)")

  f = "../cache/usaStudent2012.rds"
  if(file.exists(f))
    return(readRDS(f))

  load(url("http://beta.icm.edu.pl/PISAcontest/data/student2012.rda"))
  usaStudent2012 = subset(student2012, CNT %in% regions)
  saveRDS(usaStudent2012, f)
  rm(student2012)
  usaStudent2012
}

schoolUSA2012 = function(){
  regions = c("United States of America", "Connecticut (USA)", "Florida (USA)", "Massachusetts (USA)")

  f = "../cache/usaSchool2012.rds"
  if(file.exists(f))
    return(readRDS(f))

  load(url("http://beta.icm.edu.pl/PISAcontest/data/school2012.rda"))
  usaSchool2012 = subset(school2012, CNT %in% regions)
  saveRDS(usaSchool2012, f)
  rm(school2012)
  usaSchool2012
}