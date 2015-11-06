library(xlsx)
library("lattice")
library("ggplot2")
library("caret")
library("rpart")
library("tree")
library("randomForest")
library("e1071")
library("party")
library("aod")
# Cleaning Data Set for 2013-2014----------------------------------------
D13=read.xlsx("1314Placement4James.xlsx",sheetIndex=1,header=TRUE)
colnames(D13)[2] <- "PEOPLE_ID"
colnames(D13)[4]<-"Gender"
colnames(D13)[5]<-"BorD"
colnames(D13)[6]<-"Form"
colnames(D13)[8]<-"Q.score"
colnames(D13)[9]<-"ScoreCategory"
colnames(D13)[7]<-"Repeat"
colnames(D13)[20]<-"Country"
colnames(D13)[19]<-"States"
colnames(D13)[15]<- "School"
D13 <- D13[!is.na(D13$PEOPLE_CODE_ID),]
D13<- D13[,!(names(D13)%in% c("NA..6","NA..9","OTH.MATH.DESC1","OTH.MATH.DESC2",
                              "Math.Place","Notes","Teacher.rec","FALL.MATH", "Year","Gender",
                              "SUMMER.MATH","MATH.WN.WHERE","MATH.SUM.CRS","PEOPLE_CODE_ID",
                              "BorD","School"))]
#change REPEAT COLUMN into 1 or 0
D13$Repeat <-sapply(D13$Repeat,isEmpty,USE.NAMES=FALSE)
#change Q.Score Column to consistent measurement.
D13$Q.score<-sapply(D13$Q.score,Prelim)
D13$Q.score <- as.numeric(paste(D13$Q.score))
for(i in 1:293){
  if((is.na(D13$ScoreCategory[i]))||(D13$ScoreCategory[i]!="SSAT")){
    D13$Q.score[i] <- QScoreConvert(D13$Q.score[i],D13$ScoreCategory[i])
  }
}
#Matriculated Data
M13=read.xlsx("matric-13.xlsx",sheetIndex=1,header=TRUE)
M13<- M13[,(names(M13)%in% c("PEOPLE_ID","EVENT_ID","Term"))]
D13 <- merge(D13,M13,by ="PEOPLE_ID")
D13$EVENT_ID<-as.character(D13$EVENT_ID)
D13$EVENT_ID <- sapply(D13$EVENT_ID, substring35)
for(i in 1:282){
  if(!(D13$Term[i] %in% c("A","A+","A-","B","B+"))){
    D13$EVENT_ID[i] <- drop(D13$EVENT_ID[i],D13$Form[i])
  }
}
D13$ScoreCategory <- sapply(D13$ScoreCategory,ScoreCategory)
D13<- D13[,!(names(D13)%in% c("Term","Year"))]
colnames(D13)[9]<-"SCHL"
colnames(D13)[10]<-"STATE"
D13$CURR.MATH <- as.character(D13$CURR.MATH)
D13$CURR.MATH<-sapply(D13$CURR.MATH, currentMath)

# Cleaning Data Set For 2012-2013--------------------

D12=read.xlsx("1213Placement4James.xls",sheetIndex=1,header=TRUE)
M12=read.xlsx("matric-12.xlsx",sheetIndex=1,header=TRUE)
M12<- M12[,(names(M12)%in% c("PEOPLE_ID","EVENT_ID","Term"))]
D12 <- merge(D12,M12, by = "PEOPLE_ID")
D12<- D12[,!(names(D12)%in% c("series","teacher","Notes","Place","watch",
                              "FALL.MATH","SUMMER.MATH","MATH.WN.WHERE","MATH.SUM.CRS",
                              "OTH.MATH.DESC1","OTH.MATH.DESC2"))]
D12$GR.APPLY <- sapply(D12$GR.APPLY,ConvertGrade)
D12$CUR.GRADE <- sapply(D12$CUR.GRADE,ConvertGrade)
D12$EVENT_ID <- as.character(D12$EVENT_ID)
D12$EVENT_ID <- sapply(D12$EVENT_ID,substring35)
for(i in 1:264){
  if(!(is.na(D12$Term[i])) && !(D12$Term[i] %in% c("A","B","A+","B+","A-"))){
    D12$EVENT_ID[i] <- drop(D12$EVENT_ID[i],D12$GR.APPLY[i])
  }
  
  if(!is.na(D12$CUR.GRADE[i])){
    if(D12$CUR.GRADE[i]== D12$GR.APPLY[i]){
      D12$CUR.GRADE[i] <- 1
    }else{
      D12$CUR.GRADE[i] <- 0
    }
  }else{  
    D12$CUR.GRADE[i] <- 0
  }
}
colnames(D12)[2]<-"Repeat"
colnames(D12)[3]<-"Form"
D12$SSAT.Q<-sapply(D12$SSAT.Q,Prelim)
D12$SSAT.Q <- as.numeric(paste(D12$SSAT.Q))
D12["ScoreCategory"] <- D12$Test.A

for(i in 1:264){
  if(is.na(D12$SSAT.Q[i])){
    D12$SSAT.Q[i] <- 0
    D12$ScoreCategory[i]<-NA
  }else{
    if(D12$SSAT.Q[i]== 1000){
      D12$SSAT.Q[i] <- 0
      D12$ScoreCategory[i]<-NA
    }else if(D12$SSAT.Q[i]>100){
      D12$SSAT.Q[i] <- QScoreConvert(D12$SSAT.Q[i],"SAT")
      D12$ScoreCategory[i]<-"SAT"
    }else{
      D12$ScoreCategory[i]<-"SSAT"
    }
  }
}

colnames(D12)[4]<-"Q.score"
colnames(D12)[5]<-"Test.A"
colnames(D12)[6]<-"Test.B"
colnames(D12)[3]<-"Form"
colnames(D12)[9]<-"STATE"
colnames(D12)[10]<-"Country"
D12$CURR.MATH <- sapply(D12$CURR.MATH,currentMath)
D12<- D12[,!(names(D12)%in% c("Term"))]

# Cleaning Data for 2011-2012----------
D11=read.xlsx("1112Placement4James.xlsx",sheetIndex=1,header=TRUE)
M11=read.xlsx("matric-11.xlsx",sheetIndex=1,header=TRUE)
M11<- M11[,(names(M11)%in% c("PEOPLE_ID","EVENT_ID","Term"))]
D11 <- merge(D11,M11, by = "PEOPLE_ID")
D11<- D11[,(names(D11)%in% c("PEOPLE_ID","State.Province","Current.Grade",
                             "Entering.Grade","Country","Test.A","Test.B","EVENT_ID",
                             "Term","SSAT_Math_1","SSAT_Math_Prcnt_1",
                             "Please.provide.us.with.the.following.information....Current.School.",
                             "X1...In.what.mathematics.course.are.you.currently.enrolled."))]
colnames(D11)[11] <- "CURR.MATH"
D11$CURR.MATH <- sapply(D11$CURR.MATH, currentMath)
D11$Current.Grade <- sapply(D11$Current.Grade,ConvertGrade)
D11$Entering.Grade <- sapply(D11$Entering.Grade,ConvertGrade)
D11$EVENT_ID <- as.character(D11$EVENT_ID)
D11$EVENT_ID <- sapply(D11$EVENT_ID,substring35)
D11$Entering.Grade[230] <- 5
for(i in 1:255){
  if(!(is.na(D11$Term[i])) && !(D11$Term[i] %in% c("A+","A","A-","B+","B"))){
    D11$EVENT_ID[i] <- drop(D11$EVENT_ID[i],D11$Entering.Grade[i])
  }
  # this is the problem. Levels of factors are different 
  if(!is.na(D11$Current.Grade[i])){
    if(D11$Current.Grade[i]== D11$Entering.Grade[i]){
      D11$Current.Grade[i] <- 1
    }else{
      D11$Current.Grade[i] <- 0
    }
  }else{  
    D11$Current.Grade[i] <- 0
  }
}
D11["ScoreCategory"] <- D11$Test.A
D11$SSAT_Math_Prcnt_1 <- as.numeric(paste(D11$SSAT_Math_Prcnt_1))
for(i in 1:255){
  if(is.na(D11$SSAT_Math_Prcnt_1[i])){
    D11$SSAT_Math_Prcnt_1[i] <- 0
    D11$ScoreCategory[i] <- NA
  }else{
    if(D11$SSAT_Math_Prcnt_1[i]== 1000){
      D11$SSAT_Math_Prcnt_1[i] <- 0
      D11$ScoreCategory[i] <- NA
    }else if(D11$SSAT_Math_Prcnt_1[i]>100){
      D11$SSAT_Math_Prcnt_1[i] <- QScoreConvert(D11$SSAT_Math_Prcnt_1[i],"SAT")
      D11$ScoreCategory[i] <- "SAT"
    }else{
      D11$ScoreCategory[i] <- "SSAT"
    }
  }
}

D11<- D11[,!(names(D11)%in% c("Term","SSAT_Math_1"))]
colnames(D11)[2]<-"STATE"
colnames(D11)[4]<-"Repeat"
colnames(D11)[5]<-"Form"
colnames(D11)[6]<-"Q.score"
colnames(D11)[9]<-"SCHL"

# Combining Together 3 Data sets--------
D <- rbind(D12,D13)
D <- rbind(D11,D)
D["whichTest"] <- D$Q.score
D["testScore"] <- D$Q.score
for(i in 1:801){
  if(is.na(D$Test.A[i]) && !is.na(D$Test.B[i])){
    D$whichTest[i]<- 2
    D$testScore[i] <- D$Test.B[i]
  }else if(!is.na(D$Test.A[i]) && is.na(D$Test.B[i])){
    D$whichTest[i]<- 1
    D$testScore[i] <- D$Test.A[i]
  }else{
    D$whichTest[i] <-0
    D$testScore[i] <- 0
  }
}
D<- D[,!(names(D)%in% c("Test.A","Test.B"))]
c <-c()
for(i in 1:801){
  if(D$whichTest[i]==0 && is.na(D$ScoreCategory[i])){
    c<- append(c,D$PEOPLE_ID[i])
  }
}

D <- D[!D$PEOPLE_ID %in% c,]
D <- D[,!names(D) %in% c("STATE","SCHL")]
for(i in 1:790){
  if(is.na(D$Country[i])){
    D$Country[i] <- "USA"
  }
}

D$Country <- sapply(D$Country, as.character)

for(i in 1:790){
  if(D$Country[i] == "USA"){
    D$Country[i]=0
  }else if(D$Country[i] %in% c("CHN","CN","SK")){
    D$Country[i]=1
  }else{
    D$Country[i]=0
  }
}
for(i in 1:790){
  if(!is.na(D$ScoreCategory[i]))
    if(D$ScoreCategory[i] == "SAT"){
      D$ScoreCategory[i]<-"2"
    }else{
      D$ScoreCategory[i]<-"1"
    }
}
D$Form <- as.numeric(paste(D$Form))

DeachForm <- D[D$Country == "0",]
DeachForm <- DeachForm[DeachForm$Form== "3",]
DeachForm <- DeachForm[DeachForm$CURR.MATH != "0",]
U3 <- sum(DeachForm$CURR.MATH)/nrow(DeachForm)

DeachForm <- D[D$Country == "0",]
DeachForm <- DeachForm[DeachForm$Form== "4",]
DeachForm <- DeachForm[DeachForm$CURR.MATH != "0",]
U4 <- sum(DeachForm$CURR.MATH)/nrow(DeachForm)


DeachForm <- D[D$Form== "5",]
DeachForm <- DeachForm[DeachForm$CURR.MATH != "0",]
A5 <- sum(DeachForm$CURR.MATH)/nrow(DeachForm)

DeachForm <- D[D$Form== "6",]
DeachForm <- DeachForm[DeachForm$CURR.MATH != "0",]
A6 <- sum(DeachForm$CURR.MATH)/nrow(DeachForm)

DeachForm <- D[D$Country == "1",]
DeachForm <- DeachForm[DeachForm$Form== "3",]
DeachForm <- DeachForm[DeachForm$CURR.MATH != "0",]
I3 <- sum(DeachForm$CURR.MATH)/nrow(DeachForm)

DeachForm <- D[D$Country == "1",]
DeachForm <- DeachForm[DeachForm$Form== "4",]
DeachForm <- DeachForm[DeachForm$CURR.MATH != "0",]
I4 <- sum(DeachForm$CURR.MATH)/nrow(DeachForm)

indexOfPeople <- D[D$CURR.MATH != 0,]$PEOPLE_ID

for(i in 1:790){
  if(D$CURR.MATH[i] == 0){
    
    if(D$Form[i] == 3){
      if(D$Country[i] == 0){
        D$CURR.MATH[i] <- U3
      }else{
        D$CURR.MATH[i] <- I3
      }
    }else if(D$Form[i] == 4){
      if(D$Country[i] == 0){
        D$CURR.MATH[i] <- U4
      }else{
        D$CURR.MATH[i] <- I4
      }
    }else if(D$Form[i] == 5){
      D$CURR.MATH[i] <- A5
    }else{
      D$CURR.MATH[i] <- A6
    }
    
  }
}
D$EVENT_ID <- sapply(D$EVENT_ID,courseToChallenge)

#Fill in missing
D$Form <- as.factor(D$Form)
D$Repeat <- as.factor(D$Repeat)
D$Country <- as.factor(D$Country)

backUp <- D
DPlacement <-D[D$whichTest == 0,]
DSAT <- D[is.na(D$ScoreCategory),]
DSAT <-DSAT[,!names(DSAT) %in% c("ScoreCategory")]
Drest <- D[!D$whichTest == 0,]
Drest<-Drest[!is.na(Drest$ScoreCategory),]

Drest$whichTest <- as.factor(Drest$whichTest)
Drest$ScoreCategory <- as.factor(Drest$ScoreCategory)
DPlacement$ScoreCategory <- as.factor(DPlacement$ScoreCategory)
DSAT$whichTest <- as.factor(DSAT$whichTest)


Testfit <-randomForest(whichTest~ ScoreCategory + Form + Repeat + Q.score + CURR.MATH,
                       data = Drest)

pred <- predict(Testfit, DPlacement, type = "class")
DPlacement$whichTest <- pred

scoreFit <- svm(testScore ~ ScoreCategory + Form + Repeat + Q.score + whichTest,
                data =Drest,method="anova")
pred <- predict(scoreFit, DPlacement)
DPlacement$testScore <- pred

SATFit <- randomForest(ScoreCategory ~ Form + Repeat + whichTest + testScore + CURR.MATH,
                       data = Drest)
pred <- predict(SATFit,DSAT)
DSAT["ScoreCategory"] <- pred

QscoreFit <- svm(Q.score ~ ScoreCategory + Form + Repeat + testScore + whichTest,
                 data =Drest,method="anova")
pred <- predict(QscoreFit, DSAT)
DSAT$Q.score<- pred

D <- rbind(Drest,DSAT)
D <- rbind(D,DPlacement)

D$EVENT_ID <- as.factor(D$EVENT_ID)
D["Smartness"] <-D$Q.score
D$Form <- as.numeric(paste(D$Form))
for(i in 1:790){
  D$Smartness[i] <- D$CURR.MATH[i]/D$Form[i]
}
D$Form <- as.factor(D$Form)
#Challengeness:
DChallenge <- D[D$PEOPLE_ID %in% indexOfPeople,]
DChallenge$EVENT_ID <- sapply(DChallenge$EVENT_ID, ChallengeConvert)

#Content
D$EVENT_ID <- sapply(D$EVENT_ID, courseToContent)

#Output
DChallenge$EVENT_ID <- as.factor(DChallenge$EVENT_ID)
D$EVENT_ID <- as.factor(D$EVENT_ID)

output1 <-randomForest(EVENT_ID~ Country + Form + Repeat + Q.score + CURR.MATH + whichTest + ScoreCategory +Smartness+testScore,
                       data = DChallenge)
pred1 <- predict(output1,DChallenge, type = "class")
sum(pred1 == DChallenge$EVENT_ID)


output2 <-randomForest(EVENT_ID~ Country + Form + Repeat + Q.score + CURR.MATH + whichTest + ScoreCategory + testScore,
                       data = D)
pred2 <- predict(output2,D, type = "class")
sum(pred2 == D$EVENT_ID)


pred3 <- predict(output1, D,type="class")
D["Challenge"]<-pred3
D["Content"]<-pred2

