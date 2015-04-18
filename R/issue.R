issue = Vectorize(function(v){
  library(gdata)

  if(v %in% c(
    "ST03Q02"
  ))
    return("age")

  if(v %in% c(
    "ST04Q01"
  ))
    return("gender")

  if(v %in% c(
    "ST08Q01",
    "ST09Q01",
    "ST115Q01",
    "ST05Q01",
    "REPEAT"
  ) || startsWith(v, "ST07"))
    return("attendance-truancy-repeat")

  if(startsWith(v, "ST11") && v != "ST115Q01")
    return("family.at.home")

  if(v %in% c("FISCED", "HISCED", "MISCED") || 
  any(startsWith(v, c("ST13", "ST14", "ST15", "ST17", "ST18", "ST19"))))
    return("parent.backgrounds")

  if(v %in% c("IMMIG") ||
  any(startsWith(v, c("ST20", "ST21", "ST25", "EC07", "EC08", "EC09", "EC10", "EC11", "EC12",
    "EC22", "EC23", "EC24"))))
    return("international-language")

 if(v %in% c(
    "ST27Q04",
    "ST26Q13",
    "ST27Q05",
    "ST27Q01",
    "ST26Q02",
    "ST27Q02",
    "ST26Q14",
    paste("IC01Q", c("05", "06", "07", "08"), sep = "")
  ))
    return("posessions.not.school")

  if(v %in% c(
    "ST27Q03",
    "ST28Q01",
    "ST27Q03",
    "ST26Q07",
    "ST26Q11",
    "ST26Q09",
    "ST26Q01",
    "ST26Q08",
    "ST26Q10",
    "ST26Q04",
    "ST26Q05",
    "ST26Q03",
    "ST26Q06",
    "ST26Q12",
    paste("IC01Q0", 1:4, sep = ""),
    paste("IC01Q", c("09", "10", "11"), sep = "")
  ) || any(startsWith(v, c(
    "IC02", "IC06", "IC07"
  ))))
 return("school.posessions")

  if(any(startsWith(v, c("ST29", "ST42", "ST43", "ST44", "ST46", "ST48", "ST88", "ST89", "ST91", "ST93", "IC22"))))
    return("attitude-interest")
 
  if(any(startsWith(v, c("ST35", "ST87"))))
    return("sociality")

  if(any(startsWith(v, c("EC03", "EC04"))))
    return("learn.to.look.for.job")

  if(any(startsWith(v, c("ST37", "ST61", "ST62", "ST94", "ST96"))))
    return("self-efficacy-familiarity-experience")

  if(any(startsWith(v, c("ST49"))))
    return("math-behavior")

  if(any(startsWith(v, c("ST53"))))
    return("learning-strategies")

  if(any(startsWith(v, c("IC03", "IC04", "IC08"))))
    return("use.tech.outside.school")

  if(any(startsWith(v, c("IC10"))))
    return("other.activities.at.school")

  if(any(startsWith(v, c("ST55", "ST57", "IC09"))))
    return("study-learn.outside.school")

  if(any(startsWith(v, paste("ST", 70:71, sep=""))))
    return("number.class.periods")

  if(any(startsWith(v, paste("ST", 72, sep=""))))
    return("class.size")

  if(any(startsWith(v, paste("ST", 73:76, sep=""))))
    return("course.content")

  if(v %in% c("CLUSE", "CLCUSE1", "EASY") ||
  any(startsWith(v, c("ST77", "ST79", "ST80", "ST81", "ST82", "ST83", "ST84", "ST85", "ST86",
      "IC11"))))
    return("teaching")

  return("other")
  
}, "v")