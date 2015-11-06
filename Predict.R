
D15=read.xlsx("1516PlacementJameLiu.xlsx",sheetIndex=1,header=TRUE)

colnames(D15)[2] <- "PEOPLE_ID"
colnames(D15)[4]<-"ScoreCategory"
colnames(D15)[5]<-"Q.score"
colnames(D15)[11]<-"Country"


D15 <- D15[!is.na(D15$PEOPLE_ID),]

D15$Country <- as.character(D15$Country)
for(i in 1:238){
  if(D15$Country[i] == "USA"){
    D15$Country[i]=0
  }else if(D15$Country[i] %in% c("CHN","CN","SK")){
    D15$Country[i]=1
  }else{
    D15$Country[i]=0
  }
}

#change Q.Score Column to consistent measurement.
D15$Q.score<-sapply(D15$Q.score,Prelim)
for(i in 1:238){
  if((is.na(D15$ScoreCategory[i]))||(D15$ScoreCategory[i]!="SSAT")){
    D15$Q.score[i] <- QScoreConvert(D15$Q.score[i],D15$ScoreCategory[i])
  }
}

D15$ScoreCategory <- as.character(D15$ScoreCategory)
for(i in 1:238){
  if(!is.na(D15$ScoreCategory[i])){
    if(D15$ScoreCategory[i] != "SSAT"){
      D15$ScoreCategory[i] <- 2
    }else{
      D15$ScoreCategory[i]<-1
    }
  }
}

D15["testScore"]<- 0
D15["whichTest"]<- 0

for(i in 1:238){
  
  if(!is.na(D15$Test.A[i])){
    D15$whichTest[i]<-1
    D15$testScore[i]<-D15$Test.A[i]
  }else if(!is.na(D15$Test.B[i])){
    D15$whichTest[i]<-2
    D15$testScore[i]<-D15$Test.B[i]
  }else{
    D15$whichTest[i]<-0
    D15$testScore[i]<-0
  }
}
D15$CURR.MATH <- as.character(D15$CURR.MATH)
D15$CURR.MATH<-sapply(D15$CURR.MATH, currentMath)
for(i in 1:238){
  if(D15$CURR.MATH[i]==0){
    D15$CURR.MATH[i]<-currentMath(D15$FALL.MATH[i])
  }
}

D15$Repeat<-as.numeric(is.na(D15$Repeat))
D15<- D15[,!(names(D15) %in% c("code","Teacher.rec","Test.A","Test.B","FALL.MATH"))]

DeachForm <- D15[D15$Country == "0",]
DeachForm <- DeachForm[DeachForm$Form== "3",]
DeachForm <- DeachForm[DeachForm$CURR.MATH != "0",]
U3 <- sum(DeachForm$CURR.MATH)/nrow(DeachForm)

DeachForm <- D15[D15$Country == "0",]
DeachForm <- DeachForm[DeachForm$Form== "4",]
DeachForm <- DeachForm[DeachForm$CURR.MATH != "0",]
U4 <- sum(DeachForm$CURR.MATH)/nrow(DeachForm)


DeachForm <- D15[D15$Form== "5",]
DeachForm <- DeachForm[DeachForm$CURR.MATH != "0",]
A5 <- sum(DeachForm$CURR.MATH)/nrow(DeachForm)

DeachForm <- D15[D15$Form== "6",]
DeachForm <- DeachForm[DeachForm$CURR.MATH != "0",]
A6 <- sum(DeachForm$CURR.MATH)/nrow(DeachForm)

DeachForm <- D15[D15$Country == "1",]
DeachForm <- DeachForm[DeachForm$Form== "3",]
DeachForm <- DeachForm[DeachForm$CURR.MATH != "0",]
I3 <- sum(DeachForm$CURR.MATH)/nrow(DeachForm)

DeachForm <- D15[D15$Country == "1",]
DeachForm <- DeachForm[DeachForm$Form== "4",]
DeachForm <- DeachForm[DeachForm$CURR.MATH != "0",]
I4 <- sum(DeachForm$CURR.MATH)/nrow(DeachForm)


for(i in 1:238){
  if(D15$CURR.MATH[i] == 0){
    
    if(D15$Form[i] == 3){
      if(D15$Country[i] == 0){
        D15$CURR.MATH[i] <- U3
      }else{
        D15$CURR.MATH[i] <- I3
      }
    }else if(D15$Form[i] == 4){
      if(D15$Country[i] == 0){
        D15$CURR.MATH[i] <- U4
      }else{
        D15$CURR.MATH[i] <- I4
      }
    }else if(D15$Form[i] == 5){
      D15$CURR.MATH[i] <- A5
    }else{
      D15$CURR.MATH[i] <- A6
    }
  }
}

D15$Form <- as.factor(D15$Form)
D15$ScoreCategory <- as.factor(D15$ScoreCategory)
D15$Country <- as.factor(D15$Country)
D15$Repeat <- as.factor(D15$Repeat)
D15$whichTest <- as.factor(D15$whichTest)

DFull <- D15[D15$whichTest != 0,]
DFull <- DFull[!is.na(DFull$ScoreCategory),]
DSAT2 <- D15[is.na(D15$ScoreCategory),]
DPlacementTest <-D15[D15$whichTest==0,]

DPlacementTest$whichTest <- predict(Testfit, DPlacementTest, type = "class")
DPlacementTest$testScore <- predict(scoreFit, DPlacementTest)


DSAT2$whichTest <- as.numeric(paste(DSAT2$whichTest))
DSAT2$PEOPLE_ID <- as.character(DSAT2$PEOPLE_ID)
DSAT2$whichTest <-as.factor(as.character(paste(DSAT2$whichTest)))
DSAT2$ScoreCategory <- predict(SATFit,DSAT2)
DSAT2$Q.score <- predict(QscoreFit, DSAT2)

CompleteData <-rbind(DFull,DPlacementTest,DSAT2)
CompleteData$Form <- as.numeric(paste(CompleteData$Form))
CompleteData["Smartness"] <-CompleteData$CURR.MATH/CompleteData$Form
CompleteData$Form <- as.factor(CompleteData$Form)
CompleteData$whichTest <-as.factor(as.character(paste(CompleteData$whichTest)))

CompleteData["Challenge"] <- predict(output1,CompleteData, type = "class")
CompleteData["Content"] <- predict(output2,CompleteData, type = "class")

CompleteData$Challenge <- as.numeric(paste(CompleteData$Challenge))
CompleteData$Content <- as.numeric(paste(CompleteData$Content))
CompleteData["Prediction"] <- NA
for(i in 1:238){
  CompleteData$Prediction[i] <- backToCourses(CompleteData$Content[i],CompleteData$Challenge[i])
}

write.xlsx(file="new-prediction.xlsx",x=CompleteData)