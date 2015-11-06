#Plotting Data
library("lattice")
library("ggplot2")
library("caret")
library("rpart")
library("tree")
library("randomForest")
library("e1071")
library("party")
library("aod")

backUp <- D
backUpPlot <- DPlot
D$Repeat <- as.factor(D$Repeat)
D$Form <- as.factor(D$Form)
D$EVENT_ID <-as.factor(D$EVENT_ID)

#Fill in missing data
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

Dfinal <- rbind(Drest,DSAT)
Dfinal <- rbind(Dfinal,DPlacement)

for(i in 1:629){
  if(Dfinal$Country == "SK"){
    if(Dfinal$CURR.MATH[i] <2){
      Dfinal$CURR.MATH[i] <- 3.0
    } 
  }
}
set.seed(4)

TestSet <- Dfinal[sample(nrow(Dfinal),158),]
index <- TestSet$PEOPLE_ID
TrainSet <- Dfinal[!Dfinal$PEOPLE_ID %in% index,]

TrainSet$Content <-sapply(TrainSet$EVENT_ID,ContentConvert)
TrainSet$Challenge <-sapply(TrainSet$EVENT_ID,ChallengeConvert)
EVENT_ID <- c("125","200","250","301","331","351","421","435","441",
              "450","507","508","512","521","531","562","650")
Content <- c(0,10*y,10*y,20*y,20*y,20*y,30*y,30*y,35*y,30*y,40*y,45*y,45*y,50*y,40*y,45*y,60*y)
Challenge <- c(0,10,20,10,15,25,10,20,16,30,17,17,30,26,32,45,60)
CourseTemplate<- data.frame(EVENT_ID,Content,Challenge)
# regular classification with tree --> not working quite well b/c too many class
# 60% accuracy
FIT <-randomForest(EVENT_ID~ ScoreCategory + testScore +whichTest + Form + Repeat + Q.score + CURR.MATH,
                   data = TrainSet)

pred <- predict(FIT, TestSet, type = "class")
#Trying to figure out the problem 
TestSet["predtree"] <- pred



#Cordinate system? # use SVM better? How to determin proximity?
ContentFit <- svm(Content ~ ScoreCategory + testScore +whichTest + Form + Repeat + Q.score + CURR.MATH,
                  data = TrainSet)
ChallengeFit <- svm(Challenge ~ ScoreCategory + testScore +whichTest + Form + Repeat + Q.score + CURR.MATH,
                    data = TrainSet)
contentPred <- predict(ContentFit,TestSet)
challengePred <- predict(ChallengeFit,TestSet)
TestSet["Content"] <- contentPred
TestSet["Challenge"] <- challengePred


TestSet["predcor"] <- 0
for (i in 1:158){
  distances <- c()
  min <- 100000
  mindex <- 1
  for(j in 1:17){
    if((TestSet$Challenge[i]-CourseTemplate$Challenge[j])^2 + (TestSet$Content[i]-CourseTemplate$Content[j])^2 < min){
      min <-(TestSet$Challenge[i]-CourseTemplate$Challenge[j])^2 + (TestSet$Content[i]-CourseTemplate$Content[j])^2
      mindex <- j
    }
  }
  if(mindex==1){
    TestSet$predcor[i] <- "125"
  }else if(mindex == 2){
    TestSet$predcor[i] <- "200"
  }else if(mindex == 3){
    TestSet$predcor[i] <- "250"
  }else if(mindex == 4){
    TestSet$predcor[i] <- "301"
  }else if(mindex == 5){
    TestSet$predcor[i] <- "331"
  }else if(mindex == 6){
    TestSet$predcor[i] <- "351"
  }else if(mindex == 7){
    TestSet$predcor[i] <- "421"
  }else if(mindex == 8){
    TestSet$predcor[i] <- "435"
  }else if(mindex == 9){
    TestSet$predcor[i] <- "441"
  }else if(mindex == 10){
    TestSet$predcor[i] <- "450"
  }else if(mindex == 11){
    TestSet$predcor[i] <- "507"
  }else if(mindex == 12){
    TestSet$predcor[i] <- "508"
  }else if(mindex == 13){
    TestSet$predcor[i] <- "512"
  }else if(mindex == 14){
    TestSet$predcor[i] <- "521"
  }else if(mindex == 15){
    TestSet$predcor[i] <- "531"
  }else if(mindex == 16){
    TestSet$predcor[i] <- "562"
  }else if(mindex == 17){
    TestSet$predcor[i] <- "650"
  }
  
}


#success rate is only 62.7%

DTestPlot <-TestSet[,names(TestSet) %in% c("EVENT_ID","Content","Challenge")]
DTestPlot <- rbind(DTestPlot,CourseTemplate[!CourseTemplate$EVENT_ID =="650",])

p1 <- ggplot(CourseTemplate, aes(x=Content, 
                                 y =Challenge, label=EVENT_ID))+geom_text(size=5)+ggtitle("template")
p2 <- ggplot(DTestPlot, aes(x=Content, 
                            y =Challenge,color=EVENT_ID))+geom_point(shape=1)+ggtitle("What is suppose to look like")
p3 <- ggplot(TestSet, aes(x=Content, 
                          y =Challenge,color=predtree))+geom_point(shape=1) +ggtitle("Tree")
p4 <- ggplot(TestSet, aes(x=Content, 
                          y =Challenge,color=predcor))+geom_point(shape=1) +ggtitle("Cor")

multiplot(p1,p2,p3,p4,cols=2)
sum(TestSet$predcor==TestSet$EVENT_ID)
sum(TestSet$predtree==TestSet$EVENT_ID)
# Future steps?
# 1. residual using "SCHL"
# 2. average the tree and cord method? When to average?
# 3. Error from estimation.Computed number more accurate?
# 4. put weight?

# Future What to do:
# Find out what's wrong about those points.

