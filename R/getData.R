rawStudent = function(){
  regions = c("United States of America", "Connecticut (USA)", "Florida (USA)", "Massachusetts (USA)")

  f = "../cache/rawStudent.rds"
  if(file.exists(f))
    return(readRDS(f))

  load(url("http://beta.icm.edu.pl/PISAcontest/data/student2012.rda"))
  usaStudent2012 = subset(student2012, CNT %in% regions)
  saveRDS(usaStudent2012, f)
  rm(student2012)
  usaStudent2012
}

censorStudentSuccess = function(){
  stu = rawStudent()
  scorenames = paste(rep(paste("PV", 1:5, sep=""), each = 2), c("MATH", "READ"), sep="")
  scores = stu[,scorenames]
  totscores = apply(scores, 1, sum)
  keep = (totscores < quantile(totscores, .25)) | (totscores > quantile(totscores, .75))
  stu$success = factor(totscores > median(totscores), levels = c(T, F), labels = c("high", "low"))
  stu$PVTOTAL = totscores
  stu = stu[keep,]
  stu
}

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
    return("attendance/truancy/repeat")

  if(startsWith(v, "ST11") && v != "ST115Q01")
    return("family.at.home")

  if(v %in% c("FISCED", "HISCED", "MISCED") || 
  any(startsWith(v, c("ST13", "ST14", "ST15", "ST17", "ST18", "ST19"))))
    return("parent.backgrounds")

  if(v %in% c("IMMIG") ||
  any(startsWith(v, c("ST20", "ST21", "ST25"))))
    return("international")

 if(v %in% c(
    "ST27Q04",
    "ST26Q13",
    "ST27Q05",
    "ST27Q01",
    "ST26Q02",
    "ST27Q02",
    "ST26Q14"
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
    "ST26Q12"
  ))
    return("school.posessions")

  if(any(startsWith(v, c("ST29", "ST42", "ST43", "ST44", "ST46", "ST48", "ST88", "ST89", "ST91", "ST93"))))
    return("attitude/interest")
 
  if(any(startsWith(v, c("ST35", "ST87"))))
    return("sociality")

  if(any(startsWith(v, c("ST37", "ST61", "ST62"))))
    return("self-efficacy/familiarity/experience")

  if(any(startsWith(v, c("ST49"))))
    return("math-behavior")

  if(any(startsWith(v, c("ST53"))))
    return("learning-strategies")

  if(any(startsWith(v, c("ST55", "ST57"))))
    return("outside.school")

  if(any(startsWith(v, paste("ST", 73:76, sep=""))))
    return("course.content")

  if(v %in% c("CLUSE", "CLCUSE1", "EASY") ||
  any(startsWith(v, c("ST77", "ST79", "ST80", "ST81", "ST82", "ST83", "ST84", "ST85", "ST86"))))
    return("teaching")

  if(any(startsWith(v, "ISCED")))
    return("ISCED")

}, "v")

student = function(){
  f = "../cache/student.rds"
  if(file.exists(f))
    return(readRDS(f))

  stu = censorStudentSuccess()
  candidates = stu[,8:500]
  candidates = candidates[, sapply(candidates, 
    function(x){is.factor(x) & length(levels(x)) < 10 & !all(is.na(x))})]

  d = read.csv("../dictionaries/student-dict.csv", head = T)
  rownames(d) = d$variable
  Factor = colnames(candidates)

  Description = as.character(d[as.character(Factor),]$description)
  names(Description) = Factor
  Description = gsub("\x92", "", Description)

  if(Description[83] == Description[226])
    Description[226] = "Perceived Control - Can Succeed with Enough Effort (rep 2)"

  Description[Factor == "ST84Q01"] = "Class Mngmt: Students Interrupt/Teacher Early"
  Description[Factor == "ST84Q03"] = "Class Mngmt: Students Interrupt/Teacher Late"

  remove = grepl("Self-Efficacy", Description) | 
      grepl("Familiarity with", Description) |
      grepl("Experience with", Description) | 
      grepl("Openness for", Description) | 
      grepl("Problem Text Message", Description) | 
      grepl("Problem Route Selection", Description) | 
      grepl("Problem Ticket Machine", Description) | 
      grepl("Student Questionnaire Form", Description) |
      grepl("Overclaiming", Description) | 
      (Factor %in% c("ISCEDD", "ISCEDL", "ISCEDO"))
  
  stu = candidates[, !remove]

  dictionary = data.frame(
    Description = Description[colnames(stu)],
    Factor = colnames(stu),
    Issue = issue(colnames(stu))
  )

  ret = c(data = stu, dictionary = dictionary)
  saveRDS(ret, f)
  ret
}