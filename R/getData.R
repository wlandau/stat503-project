dataUSA2012 = function(){
  f = "../cache/usa2012.rds"
  if(file.exists(f)){
    return(readRDS(f))
  } else {
    regions = c("United States of America", "Connecticut (USA)", "Florida (USA)", "Massachusetts (USA)")

    load(url("http://beta.icm.edu.pl/PISAcontest/data/student2012.rda"))
    usaStudent2012 = subset(student2012, CNT %in% regions)
    saveRDS(usaStudent2012, "../cache/usaStudent2012.rds")
    rm(student2012)

    load(url("http://beta.icm.edu.pl/PISAcontest/data/school2012.rda"))
    usaSchool2012 = subset(school2012, CNT %in% regions)
    saveRDS(usaSchool2012, "../cache/usaSchool2012.rds")
    rm(school2012)

    usa2012 = merge(usaStudent2012, usaSchool2012, by="SCHOOLID")
    saveRDS(usa2012, "../cache/usa2012.rds")
    return(usa2012)
  }
}
