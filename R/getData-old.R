dataUSA2012 = function(){
  f = "../cache/usa2012.rds"
  if(file.exists(f)){
    return(readRDS(f))
  } else {
    regions = c("United States of America", "Connecticut (USA)", "Florida (USA)", "Massachusetts (USA)")

    load(url("http://beta.icm.edu.pl/PISAcontest/data/student2012.rda"))
    usaStudent2012 = subset(student2012, CNT %in% regions)
    saveRDS(usaStudentl2012, "../cache/usaStudent2012.rds")
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

id.def = c(
  "SCHOOLID" = "school.id", 
  "ST01Q01" = "gender",
  "ST04Q01" = "international.grade",
  "SC01Q01" = "public.or.private"
)
id = names(id.def)

people.def = c(
  "SC05Q01" = "class.size.school",
  "ST72Q01" = "class.size.student",
  "SC07Q01" = "num.boys",
  "SC07Q02" = "num.girls",
  "SMRATIO" = "math.teacher.student.ratio", 
  "SC09Q11" = "num.teachers.fulltime",
  "SC09Q12" = "num.teachers.parttime",		 
  "SC09Q21" = "num.teachers.cert.fulltime",
  "SC09Q22" = "num.teachers.cert.parttime",
  "SC09Q31" = "num.teahers.qual.fulltime",
  "SC09Q32" = "num.teachers.qual.parttime",
  "SC10Q11" = "num.mathteachers.fulltime",
  "SC10Q12" = "num.mathteachers.partime",
  "SC10Q61" = "num.mathteachers.qual.fulltime",
  "SC10Q62" = "num.mathteachers.qual.parttime",
  "SC10Q21" = "num.mathteachers.qual.mathmajor.fulltime",
  "SC10Q22" = "num.mathteachers.qual.mathmajor.parttime", 
  "SC10Q41" = "num.mathteachers.qual.pedagogy.fulltime", 
  "SC10Q42" = "num.mathteachers.qual.pedagogy.parttime", 
  "SC10Q51" = "num.mathteachers.qual.ISCED5B.fulltime", 
  "SC10Q52" = "num.mathteachers.qual.ISCED5B.parttime"
)
people = names(people.def)

funding.def = c(
  "SC02Q01" = "govt.funding", 
  "SC02Q02" = "student.fees.funding",
  "SC02Q03" = "benefactors.funding",
  "SC02Q04" = "other.funding"
)
funding = names(funding.def)

shortage.def = c(
  "SC14Q01" = "shortage.science.teachers", 
  "SC14Q02" = "shortage.math.teachers",
  "SC14Q03" = "shortage.teachers",
  "SC14Q04"= "shortage.other.teachers",
  "SC14Q05" = "shortage.sci.lab.equipment",
  "SC14Q06" = "shortage.instruction.materials",
  "SC14Q07" = "shortage.computers.for.instruction",
  "SC14Q08" = "shortage.internet.connectivity",
  "SC14Q09" = "shortage.computer.software",
  "SC14Q10" = "shortage.library.materials",
  "SC14Q11" = "shortage.buildings.and.grounds",
  "SC14Q12" = "shortage.heating.cooling.lighting",
  "SC14Q13" = "shortage.instructional.space"
)
shortage = names(shortage.def)

equipment.def = c(
  "SC11Q01" = "student.computers",
  "SC11Q02" = "student.computers.for.education",
  "SC11Q03" = "student.computers.with.internet",
  "ST26Q01" = "possessions.desk",
  "ST26Q02" = "possessions.own.room",
  "ST26Q03" = "possessions.study.place",
  "ST26Q04" = "possessions.computer",
  "ST26Q05" = "possessions.software",
  "ST26Q06" = "possessions.internet",
  "ST26Q07" = "possessions.literature",
  "ST26Q08" = "possessions.poetry",
  "ST26Q09" = "possessions.art",
  "ST26Q10" = "possessions.textbooks",
  "ST26Q11" = "possessions.technical.reference.books",
  "ST26Q12" = "possessions.dictionary",
  "ST27Q03" = "how.many.computers",
  "ST28Q01" = "how.many.books.at.home"
)
equipment = names(equipment.def)

attitude.def = c(
  "ST29Q01" = "math.interest.enjoy.reading",
  "ST29Q02" = "instrumental.motivation.worthwhile.for.work",
  "ST29Q03" = "math.interest.forward.to.lessons",
  "ST29Q04" = "math.interest.enjoy.math", 
  "ST29Q05" = "instrumental.motivation.worthwhile.for.career.choices",
  "ST29Q06" = "math.interest.interested", 
  "ST29Q07" = "instrumental.motivation.important.for.future.study", 
  "ST29Q08" = "instrumental.motivation.helps.to.get.job",
  "ST44Q01" = "attributions.to.failure.not.good.at.math.problems",
  "ST44Q03" = "attributions.to.failure.teacher.didnt.explain.well", 
  "ST44Q04" = "attributions.to.failure.bad.guesses", 
  "ST44Q05" = "attributions.to.failure.material.too.hard",
  "ST44Q07" = "attributions.to.failure.teacher.didnt.get.students.interested",
  "ST44Q08" = "attributions.to.failure.unlucky",
  "ST46Q01" = "math.work.ethic.homework.completed.in.time",
  "ST46Q02" = "math.work.ethic.work.hard.on.homework",
  "ST46Q03" = "math.work.ethic.prepared.for.exams",
  "ST46Q04" = "math.work.ethic.study.hard.for.quizzes",
  "ST46Q05" = "math.work.ethic.study.until.i.understand.everything",
  "ST46Q06" = "math.work.ethic.pay.attention.in.classes",
  "ST46Q07" = "math.work.ethic.listen.in.classes",
  "ST46Q08" = "math.work.ethic.avoid.distractions.when.studying",
  "ST46Q09" = "math.work.ethic.keep.work.organized",
  "ST88Q01" = "attitude.toward.school.does.little.to.prepare.me",
  "ST88Q02" = "attitude.toward.school.waste.of.time", 
  "ST88Q03" = "attitude.toward.school.gave.me.confidence",
  "ST88Q04" = "attitude.toward.school.useful.for.job",
  "ST89Q02" = "attitude.toward.school.helps.get.job",
  "ST89Q03" = "attitude.toward.school.prepare.for.college",
  "ST89Q04" = "attitude.toward.school.enjoy.good.grades",
  "ST89Q05" = "attitude.toward.school.trying.hard.is.important",
  "ST91Q01" = "perceived.control.can.succeed.with.enough.effort",
  "ST91Q02" = "perceived.control.my.choice.whether.i.will.be.good", 
  "ST91Q03" = "perceived.control.problems.prevent.putting.effort.into.school",
  "ST91Q04" = "perceived.control.different.teachers.would.make.me.try.harder",
  "ST91Q05" = "perceived.control.could.perform.well.if.i.wanted",
  "ST91Q06" = "perceived.control.perform.poor.regardless",
  "ST93Q01" = "perseverence.give.up.easily",
  "ST93Q03" = "perseverence.put.off.difficult.problems",
  "ST93Q04" = "perseverence.remain.interested",
  "ST93Q06" = "perseverence.continue.to.perfection",
  "ST93Q07" = "perseverence.exceed.expectations"
)
attitude = names(attitude.def)

math.def = c(
  "PV1MATH" = "math1",
  "PV2MATH" = "math2",
  "PV3MATH" = "math3",
  "PV4MATH" = "math4",
  "PV5MATH" = "math5"
)
math = names(math.def)

reading.def = c(
  "PV1READ" = "reading1",
  "PV2READ" = "reading2",
  "PV3READ" = "reading3",
  "PV4READ" = "reading4",
  "PV5READ" = "reading5"
)
reading = names(reading.def)

truancy.def = c(
  "ST08Q01" = "truancy.late",
  "ST09Q01" = "truancy.skip.day",
  "ST115Q01" = "truancy.skip.classes"
)
truancy = names(truancy.def)
