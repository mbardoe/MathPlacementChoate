# Functions ------------
ScoreCategory <- function(x){
  if(is.na(x)){
    return(NA)
  }else if(x == "SSAT" || x == "ISEE"){
    return("SSAT")
  }else{
    return("SAT")
  }
}

ConvertGrade <- function(x){
  if(!is.na(x)){
    if(x == 8){
      return(2)
    }else if(x == 9){
      return(3)
    }else if(x == 10){
      return(4)
    }else if(x ==11){
      return(5)
    }else{
      return(6)
    }
  }else{
    return(x)
  }
}

isEmpty <- function(x){
  if(is.na(x)){
    return(0)
  }
  else
  {
    return(1)
  }
}

Prelim <- function(x){
  if(is.na(x)){
    return(factor(1000))
  }
  else{
    return(x)
  }
}

QScoreConvert <- function(x,str){
  if(!is.na(str)){
    if(str == "SAT"){
      if(x<= 800 && x>= 730){
        return(98)
      }else if(x<730 && x >= 690){
        return(94)
      }else if(x<690 && x >= 650){
        return(89)
      }else if(x<650 && x >= 610){
        return(80)
      }else if(x<610&& x >= 530){
        return(70)
      }else if(x<530 && x >= 490){
        return(45)
      }else if(x<490 && x >= 450){
        return(31)
      }else if(x<450 && x >= 410){
        return(20)
      }else{
        return(8)
      }
    }else if(str == "PSAT"){
      return(QScoreConvert(x*10,"SAT"))
    }else if(str =="ACT"){
      if(x<= 36 && x>= 30){
        return(97)
      }else if(x<30 && x >= 26){
        return(88)
      }else if(x<26 && x >= 22){
        return(70)
      }else if(x<22 && x >= 19){
        return(52)
      }else if(x<19 && x >= 17){
        return(41)
      }else if(x==16){
        return(26)
      }else{
        return(4)
      }
    }else{
      return(x)
    }
  }else{
    return(0)
  }
}

substring35 <- function(x){
  return(substr(x,3,5))
}

drop <- function(x,form){
  if(x == "351"){
    return("331")
  }else if (x =="331"){
    return("301")
  }else if (x =="301"){
    return("125")
  }else if (x =="250"){
    return("200")
  }else if (x =="200"){
    return("125")
  }else if (x =="450"){
    if(form %in% c(factor(5),factor(6))){
      return("435")
    }else{
      return("351")
    }
  }else if (x =="435"){
    if(form %in% c(factor(5),factor(6))){
      return("421")
    }else{
      return("331")
    }
  }else if (x =="562"){
    return("521")
  }else if (x =="521"){
    return("441")
  }else if (x =="512"){
    return("508")
  }else if (x =="531"){
    return("507")
  }else{
    return(x)
  }
}

toCategory <- function(str){
  if(str %in% c("125","200","250","301","331")){
    return(1) 
  }else if(str %in% c("351","418","421","431","435","450")){
    return(2)
  }else{
    return(3)
  }
}

currentMath <- function(str){
  if(is.na(str)){
    return(0)
  }else if(str == "Other" || str == "none" || str == "Math"){
    return(0)
  }else if(grepl("recal",str) ||grepl("prec",str)){
    return(4)
  }else if(grepl("eom",str)){
    return(2)
  }else if(grepl("re-Alg",str)){
    return(0.5)
  }else if(grepl("alc",str)){
    return(5)
  }else if(grepl("stat",str)){
    return(3.5)
  }else if(grepl("Algebra II",str) || grepl("A2",str) || grepl("Alg 2",str)){
    return(3)
  }else if(grepl("Alg",str)){
    return(1)
  }else{
    return(0)
  }
}

ChallengeConvert <- function(x){
  if(x == "125"){
    return(0) 
  }else if(x == "200"){
    return(10)
  }else if(x == "250"){
    return(20)
  }else if(x == "301"){
    return(10)
  }else if(x == "331"){
    return(15)
  }else if(x == "351"){
    return(25)
  }else if(x == "421" ){
    return(10)
  }else if(x == "435" ||x == "433" || x == "431"||x == "418"){
    return(20)
  }else if(x == "450"){
    return(30)
  }else if(x == "441"){
    return(16)
  }else if(x == "507"){
    return(17)
  }else if(x == "531"){
    return(32)
  }else if(x == "508"){
    return(17)
  }else if(x == "512"){
    return(30)
  }else if(x == "562"){
    return(45)
  }else if(x == "521"){
    return(26)
  }else if(x == "650"){
    return(60)
  }
}

ContentConvert <- function(x){
  if(x == "125"){
    return(0*y) 
  }else if(x == "200"){
    return(10*y)
  }else if(x == "250"){
    return(10*y)
  }else if(x == "301"){
    return(20*y)
  }else if(x == "331"){
    return(20*y)
  }else if(x == "351"){
    return(20*y)
  }else if(x == "421" ){
    return(30*y)
  }else if(x == "435" ||x == "433" || x == "431"|| x == "418"){
    return(30*y)
  }else if(x == "450"){
    return(30*y)
  }else if(x == "441"){
    return(35*y)
  }else if(x == "507"){
    return(40*y)
  }else if(x == "531"){
    return(40*y)
  }else if(x == "508"){
    return(45*y)
  }else if(x == "512"){
    return(45*y)
  }else if(x == "562"){
    return(45*y)
  }else if(x == "521"){
    return(50*y)
  }else if(x == "650"){
    return(60*y)
  }
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

ChallengeConvert <- function(x){
  if(x == "125"){
    return(1) 
  }else if(x == "200"){
    return(2)
  }else if(x == "250"){
    return(3)
  }else if(x == "301"){
    return(1)
  }else if(x == "331"){
    return(2)
  }else if(x == "351"){
    return(3)
  }else if(x == "400" ){
    return(1)
  }else if(x == "435"){
    return(2)
  }else if(x == "450"){
    return(3)
  }else if(x == "507"){
    return(1)
  }else if(x == "520"){
    return(2)
  }else{
    return(3)
  }
}

courseToChallenge <- function(x){
  if(x %in% c("421","441","418","433","431")){
    return("400")
  }else if(x %in% c("512","521","531")){
    return("520")
  }else if(x == "650"){
    return("562")
  }else{
    return(x)
  }
}

courseToContent <- function(x){
  if(x == "125"){
    return(1)
  }else if(x %in% c("200","250")){
    return(2)
  }else if(x %in% c("301","331","351")){
    return(3) 
  }else if(x %in% c("400","431","435","450")){
    return(4)
  }else if(x %in% c("507","520","562","531")){
    return(5)
  }else{
    return(1)
  }
}
backToCourses <-function(x,y){
  if(x ==1 && y == 1){
    return("125")
  }else if(x ==2 && y ==3){
    return("250")
  }else if(x == 2 && (y==1 || y==2)){
    return("200")
  }else if(x ==3 && y==1){
    return("301")
  }else if(x ==3 && y==2){
    return("331")
  }else if(x==3&& y==3){
    return("351")
  }else if(x == 4 && y ==1){
    return("400")
  }else if(x==4 && y==2){
    return("435")
  }else if(x ==4 && y ==3){
    return("450")
  }else if(x ==5 && y== 1){
    return("507")
  }else if(x ==5 && y==2){
    return("520")
  }else if(x == 5 && y==3){
    return("562")
  }else{
    return(FALSE)
  }
}