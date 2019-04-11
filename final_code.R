### raw������ ###
key<-"c26a9a0d25cdcea858606babb54cf641"
startdate<-as.Date("2016-01-01")
enddate<-as.Date("2017-12-31")
date<-seq(startdate,enddate,by="1 days")
date<-format(date,format="%Y%m%d")
rawdata<-NULL
for(i in 1:length(date)){
 url<-paste0("http://www.kobis.or.kr/kobisopenapi/webservice/rest/boxoffice/searchDailyBoxOfficeList.json?key=",key,"&targetDt=",date[i])
 tmp<-fromJSON(url)
 rawdata<-rbind(rawdata,tmp$boxOfficeResult$dailyBoxOfficeList)
}
movie <- rawdata


### duplicated ###
a <- movie[!duplicated(movie$movieNm,fromLast=T),]
a <- a[order(a$movieNm),]


### �帣/������/������ ###
library(jsonlite)
key<-"1a7999ee2a2cd3933a864bde8b17e7db"
cd <- unique(a$movieCd)
genre <- NULL
rawdata3 <- matrix(nrow=537,ncol=3)
rawdata3 <- as.data.frame(rawdata3)
for(i in 1:length(cd)){
 url<-paste0("http://www.kobis.or.kr/kobisopenapi/webservice/rest/movie/searchMovieInfo.json?key=",key,"&movieCd=",cd[i])
 tmp<-fromJSON(url)
 genre <- tmp$movieInfoResult$movieInfo$genres$genreNm
 genre <- paste(genre,collapse="/")
 director <- tmp$movieInfoResult$movieInfo$directors$peopleNm[1]
 watchGradeNm <- tmp$movieInfoResult$movieInfo$audits$watchGradeNm[1]
 if(!is.null(genre)) {rawdata3[i,1] <- genre} else {rawdata3[i,1] <- NA}
 if(!is.null(director)) {rawdata3[i,2] <- director} else {rawdata3[i,2] <- NA}
 if(!is.null(watchGradeNm)) {rawdata3[i,3] <- watchGradeNm} else {rawdata3[i,3] <- NA}
}
a$genre <- NULL
for(i in 1:nrow(a)){
a$genre[i] <- strsplit(rawdata3[,1],'/')[[i]][1]
}
a$director <- rawdata3[,2]
a$watchGradeNm <- rawdata3[,3]

a$watchGradeNm[a$watchGradeNm=='15��������'] <- '15���̻������'
a$watchGradeNm[a$watchGradeNm=='�����л��̻������'] <- '15���̻������'
a$watchGradeNm[a$watchGradeNm=='��� �������� ������ �� �ִ� ���'] <- '��ü������'
a$watchGradeNm[a$watchGradeNm=='�����ڰ�����'] <- '��ü������'
a$watchGradeNm[a$watchGradeNm=='18��������'] <- 'û�ҳ�����Ұ�'
a$watchGradeNm[a$watchGradeNm=='�����ڰ����Ұ�'] <- 'û�ҳ�����Ұ�'


### �������� ###
library(jsonlite)
library(dplyr)
library(httr)
library(RSelenium)
library(rvest)

ch=wdman::chrome(port=4567L) 
remDr=remoteDriver(port=4567L, browserName='chrome')
remDr$open()

�������� <- NULL
for(i in 1:537){
  if(is.na(a$movieNm[i])){��������<-c(��������,NA)}else{
  url2 <- paste0("https://search.naver.com/search.naver?where=nexearch&sm=top_hty&fbm=0&ie=utf8&query=",a$movieNm[i])
  remDr$navigate(url2)
  url2 <- remDr$getCurrentUrl()[[1]] 
  number <- read_html(url2)
  numberinfos <- html_nodes(number,css='.desc_detail')
  val2 <- numberinfos%>%html_nodes('span')%>%html_text()
  val2 <- val2[grep('��',val2)]
  val2 <- val2[length(val2)]
  val2 <- gsub('��','',val2)
  val2 <- gsub(',','',val2)
  if(!identical(grep("[1234567890]",val2),integer(0))){val2=val2}else{val2=numeric(0)}
  val2 <- as.numeric(val2)
  if(!identical(val2,numeric(0))) {�������� <- c(��������,val2)} else {�������� <- c(��������,NA)}
 }
 }
a$�������� <- ��������
for(i in 1:nrow(a)){
if(is.na(a$��������[i])) {a$��������[i] <- a$audiAcc[i]}
}
# �ڵ带 ���� ������ ���� ��ȭ�� ���������� ���� �˻��ؼ� ã�Ƴ־���, ���������� ���ͳݿ� ������
# ���� ��ȭ�� �ڽ����ǽ� 10���� ���� ���� ���� ������������ ��ü��.


### ������Ƚ�� ###
a$cumshow <- tapply(as.numeric(movie$showCnt),movie$movieNm,sum)


### weight ###
movie$dayNm <- c(rep(rep(c('��','��','��','��','ȭ','��','��'),each=10),104),rep(c('��','��','��'),each=10))
startdate<-as.Date("2016-01-01")
enddate<-as.Date("2017-12-31")
date<-seq(startdate,enddate,by="1 days")
movie$date <-  rep(date,each=10)
a$weight <- apply(table(movie$movieNm,movie$dayNm)[,5:6],1,sum)
movie$weight2 <- 0
movie$weight2[movie$date=='2016-01-01'|movie$date=='2016-02-08'|movie$date=='2016-02-09'|movie$date=='2016-02-10'|movie$date=='2016-03-01'|movie$date=='2016-04-13'|movie$date=='2016-05-05'|movie$date=='2016-05-06'|movie$date=='2016-06-06'|movie$date=='2016-08-15'|movie$date=='2016-09-14'|movie$date=='2016-09-15'|movie$date=='2016-09-16'|movie$date=='2016-10-03'] <- 1
movie$weight2[movie$date=='2017-01-27'|movie$date=='2017-01-30'|movie$date=='2017-03-01'|movie$date=='2017-05-03'|movie$date=='2017-05-05'|movie$date=='2017-05-09'|movie$date=='2017-06-06'|movie$date=='2017-08-15'|movie$date=='2017-10-02'|movie$date=='2017-10-03'|movie$date=='2017-10-04'|movie$date=='2017-10-05'|movie$date=='2017-10-06'|movie$date=='2017-10-09'|movie$date=='2017-12-25'] <- 1
a$weight2 <- tapply(movie$weight2,movie$movieNm,sum)


### �⿬��� ###
library(jsonlite)
key<-"1a7999ee2a2cd3933a864bde8b17e7db"
actor <- NULL
for(i in 1:nrow(a)){
 url<-paste0("http://www.kobis.or.kr/kobisopenapi/webservice/rest/movie/searchMovieInfo.json?key=",key,"&movieCd=",cd[i])
 tmp<-fromJSON(url)
 val <- tmp$`movieInfoResult`$`movieInfo`$actors$peopleNm[1:3]
 value <- paste0(val[1],',',val[2],',',val[3])
 if(!is.null(val)) {actor <- rbind(actor,value)} else {actor <- rbind(actor,NA)}
}

for(i in 1:length(actor)){
if(is.na(actor[i])) {actor[i] <- c("NA,NA,NA")}
}

a$actor <- NULL
for(i in 1:nrow(a)){
a$actor[i] <- actor[i]
}


### ���� ��ȭ��/���� �Ŀ� ###
library(jsonlite)
library(dplyr)
library(httr)
library(RSelenium)
library(rvest)

ch=wdman::chrome(port=4567L) 
remDr=remoteDriver(port=4567L, browserName='chrome')
remDr$open()

raw <- NULL
key<-"c26a9a0d25cdcea858606babb54cf641"
for(i in 211:length(a$director)){
 url<-paste0("http://www.kobis.or.kr/kobisopenapi/webservice/rest/people/searchPeopleList.json?key=",key,"&peopleNm=",a$director[i])
 remDr$navigate(url)
 url <- remDr$getCurrentUrl()[[1]] 
 tmp<-fromJSON(url)
 tmp <- tmp$`peopleListResult`$peopleList$filmoNames
 if(identical(tmp[grep('\\|',tmp)],character(0))) {val <- tmp}
 if(!identical(tmp[grep('\\|',tmp)],character(0))) {tmp <- tmp[grep('\\|',tmp)]}
 if(!identical(tmp[grep('\\|',tmp)],character(0))) {val <- strsplit(tmp,split="\\|")[[1]][1:10]}
 movie <- NULL
  for(j in 1:10){
   movie <- cbind(movie,val[j])
  }
 raw <- rbind(raw,movie)
}

total <- NULL
for(i in 1:nrow(raw)){
value <- NULL
 for(j in 1:10){
  url2 <- paste0("https://search.naver.com/search.naver?where=nexearch&sm=top_hty&fbm=0&ie=utf8&query=",raw[i,j])
  remDr$navigate(url2)
  url2 <- remDr$getCurrentUrl()[[1]] 
  number <- read_html(url2)
  numberinfos <- html_nodes(number,css='.desc_detail')
  val2 <- numberinfos%>%html_nodes('span')%>%html_text()
  val2 <- val2[grep('��',val2)]
  val2 <- val2[grep('[0-9]',val2)]
  val2 <- gsub('��','',val2)
  val2 <- gsub(',','',val2)
  val2 <- as.numeric(val2)
  if(!identical(val2,numeric(0))) {value <- c(value,val2)} else {value <- c(value,NA)}
 }
total <- rbind(total,value)
}
a$director.mean <- apply(total,1,mean,na.rm=T)


### ��� ��ȭ��/��� �Ŀ� ###
ch=wdman::chrome(port=4567L) 
remDr=remoteDriver(port=4567L, browserName='chrome')
remDr$open()

actor_s<-as.character(a$actor)
actor_s<-strsplit(actor_s,",")
actor_s[[1]][1]="�� ũ���Ž�Ű"

key<-"1a7999ee2a2cd3933a864bde8b17e7db"

raw<-NULL
raw<-c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)

for(i in 2:length(a$actor)){
 for(m in 1:3){
  url<-paste0("http://www.kobis.or.kr/kobisopenapi/webservice/rest/people/searchPeopleList.json?key=",key,
  "&peopleNm=",actor_s[[i]][m])
  remDr$navigate(url)
  url <- remDr$getCurrentUrl()[[1]] 
  tmp<-fromJSON(url)
  tmp <- tmp$`peopleListResult`$peopleList$filmoNames
  if(identical(tmp[grep('\\|',tmp)],character(0))) {val <- tmp[1:10]}
  if(!identical(tmp[grep('\\|',tmp)],character(0))) {tmp <- tmp[grep('\\|',tmp)]}
  if(!identical(tmp[grep('\\|',tmp)],character(0))) {val <- strsplit(tmp,split="\\|")[[1]][1:10]}
  movie <- NULL
  for(j in 1:10){
   movie <- cbind(movie,val[j])
  }
 raw <- rbind(raw,movie)
 }
}

actormovie <- matrix(nrow=537,ncol=30)
for(i in 1:537){
actormovie[i,] <- c(raw[3*i-2,],raw[3*i-1,],raw[3*i,])
}
a$actormovie <- actormovie

total <- NULL
for(i in 1:nrow(a$actormovie)){
value <- NULL
 for(j in 1:30){
  url2 <- paste0("https://search.naver.com/search.naver?where=nexearch&sm=top_hty&fbm=0&ie=utf8&query=",a$actormovie[i,j])
  remDr$navigate(url2)
  url2 <- remDr$getCurrentUrl()[[1]] 
  number <- read_html(url2)
  numberinfos <- html_nodes(number,css='.desc_detail')
  val2 <- numberinfos%>%html_nodes('span')%>%html_text()
  val2 <- val2[grep('��',val2)]
  val2 <- val2[grep('[0-9]',val2)]
  val2 <- gsub('��','',val2)
  val2 <- gsub(',','',val2)
  val2 <- as.numeric(val2)
  if(!identical(val2,numeric(0))) {value <- c(value,val2)} else {value <- c(value,NA)}
 }
total <- rbind(total,value)
}

t1 <- apply(total[,1:10],1,mean,na.rm=T)
t2 <- apply(total[,11:20],1,mean,na.rm=T)
t3 <- apply(total[,21:30],1,mean,na.rm=T)
actor.mean <- mean(c(t1,t2,t3),na.rm=T)
a$actor.mean <- actor.mean


### ����Ŀ�/�����Ŀ� NA�� ȸ�ʹ�ü ###
a2 <- a[!is.na(a$actor.mean),]
a3 <- a[is.na(a$actor.mean),]
a4 <- a[!is.na(a$director.mean),]
a5 <- a[is.na(a$director.mean),]
lm1 <- lm(actor.mean~director.mean+cumshow,data=a2)
lm2 <- lm(director.mean~actor.mean+cumshow,data=a4)
pred1 <- predict(lm1,a3)
pred2 <- predict(lm2,a5)
a$actor.mean[is.na(a$actor.mean)] <- pred1
a$director.mean[is.na(a$director.mean)] <- pred2


### ��޻� ###
ch=wdman::chrome(port=4567L) 
remDr=remoteDriver(port=4567L, browserName='chrome')
remDr$open()

br <- NULL
for(i in a$movieNm){
 url <- paste0('https://search.naver.com/search.naver?sm=tab_hty.top&where=nexearch&query=',i,' ��޻�')
 remDr$navigate(url)
 url <- remDr$getCurrentUrl()[[1]] 
 movie <- read_html(url)
 val <- html_nodes(movie,css='.v')%>%html_text()
 if(!identical(val,character(0))) {br <- rbind(br,val)} else {br <- rbind(br,NA)}
}

for(i in 1:length(br)){
 br[i] <- strsplit(br,',')[[i]][1]
}
a$br <- br

### �ڽ����ǽ� 3���� ���� �ϼ� ###
movie$n <- 1
movie2 <- movie[movie$rank<=3,]
rankNum <- tapply(movie2$n,movie2$movieNm,sum)

a$rankNum <- 0
for(i in 1:length(rankNum)){
a$rankNum[grep(unique(movie2$movieNm)[i],unique(movie$movieNm))] <- rankNum[i]
}


### ���۱��� ###
raw<- NULL
key<-"1a7999ee2a2cd3933a864bde8b17e7db"
for(i in 1:length(a$movieNm)){
 url<-paste0("http://www.kobis.or.kr/kobisopenapi/webservice/rest/movie/searchMovieList.json?key=",key,"&movieNm=",a$movieNm[i])
 remDr$navigate(url)
 url <- remDr$getCurrentUrl()[[1]]
 tmp<-fromJSON(url)
 tmp<-tmp$`movieListResult`$movieList
 val<-tmp[grep(a[a$movieNm==a$movieNm[i],]$movieCd,tmp$movieCd),]$nationAlt
 if(!identical(val,character(0))) {value <- cbind(a$movieNm[i],val)} else {value <- cbind(a$movieNm[i],NA)}
 raw<-rbind(raw,value)
}

for(i in 1:nrow(a)){
a$nation[i] <- strsplit(raw[,2],',')[[i]][1]
}

a$nation2 <- 'etc'
a$nation2[grep('�ѱ�',a$nation)] <- 'KOREA'
a$nation2[grep('�̱�',a$nation)] <- 'USA'
a$nation2[grep('�Ϻ�',a$nation)] <- 'JAPAN'
a$nation2[grep('����',a$nation)] <- 'UK'


### ������ ###
a$weight3 <- a$weight+a$weight2


### �帣2 ###
data <- a
data <- data[-c(13,498),]  # ���ʿ��� ������ ����

data$genre2<-data$genre
data$genre2<-as.character(data$genre2)
data$genre2[data$genre2=="SF"|data$genre2=="��庥ó"|data$genre2=="��Ÿ��"]<-1
data$genre2[data$genre2=="��Ÿ"|data$genre2=="��ť���͸�"|data$genre2=="����"|data$genre2=="����"|data$genre2=="������"|data$genre2=="���"]<-2
data$genre2[data$genre2=="���"|data$genre2=="���"]<-3
data$genre2[data$genre2=="������"|data$genre2=="����(ȣ��)"|data$genre2=="�̽��͸�"]<-4
data$genre2[data$genre2=="�ִϸ��̼�"]<-5
data$genre2[data$genre2=="�׼�"|data$genre2=="����"|data$genre2=="����"]<-6
data$genre2[data$genre2=="�ڹ̵�"]<-7

data$popular<-0
data[data$genre2==1|data$genre2==6,]$popular<-1


### ������2 ###

data$watchGradeNm[grep('����Ű��',data$movieNm)] <- '��ü������'
data$watchnew<-0
data[data$watchGradeNm=="12���̻������"|data$watchGradeNm=="15���̻������",]$watchnew<-1
data[data$watchGradeNm=="��ü������",]$watchnew<-2


### ����2 ###
data$nationnew<-data$nation2
data[data$nation2=="etc"|data$nation2=="JAPAN"|data$nation2=="UK",]$nationnew<-"ETC"


### ��޻�2 ###
br <- rep(3,535)
br[grep('�����',data$br)] <- 1
br[grep('CJ',data$br)] <- 1
br[grep('�Ե�',data$br)] <- 1
br[grep('NEW',data$br)] <- 2
br[grep('�̽ʼ���',data$br)] <- 2
br[grep('���Ϲ���',data$br)] <- 2
br[grep('����',data$br)] <- 2
br[grep('��ڽ�',data$br)] <- 2
br[grep('�Ҵ�',data$br)] <- 3
br[grep('�ް��ڽ�',data$br)] <- 3

br <- as.factor(br)
data$br <- br


### Ư����, ������ ���� ###
data <- data[,c('cumshow','director.mean','weight3','actor.mean','br','rankNum','popular','watchnew','nationnew','��������')]
lm <- lm(��������~.,data=data)
summary(lm)
plot(lm,which=5)

data <- data[-c(2,266,325,535,432),]


### train, test �����ͼ� ������ ###
data$br <- as.factor(data$br)
data$popular <- as.factor(data$popular)
data$watchnew <- as.factor(data$watchnew)
data$nationnew <- as.factor(data$nationnew)

A <- sample(530,530,replace=F)
data <- data[A,]
train <- data[1:500,]
test <- data[501:530,]


### rmse ###
library(nnet)
library(party)
library(randomForest)
library(e1071)
library(caret)


lm <- lm(��������~.,data=train)
p1 <- predict(lm,test)
rmse(p1,test$��������)

dt <- ctree(��������~.,data=train)
p2 <- predict(dt,test)
rmse(p2,test$��������)

rf <- randomForest(��������~.,data=train,mtry=,ntree=500)
p3 <- predict(rf,test)
rmse(p3,test$��������)

set.seed(123)
xgb <- train(
  �������� ~., data = train, method = "xgbTree",
  trControl = trainControl("cv", number = 10)
  )
p4 <- predict(xgb,test)
rmse(p4,test$��������)


### ���� ��ȭ NA�� ��ü ###
cumshow.lm <- lm(cumshow~rankNum+weight3+actor.mean,data=data)
director.lm <- lm(director.mean~actor.mean+cumshow,data=data)
real[1,1] <- predict(cumshow.lm,real)[1]
real[1,2] <- predict(director.lm,real)[1]
real[1,3] <- 15
real[2,2] <- predict(director.lm,real)[2]
real[3,1] <- predict(cumshow.lm,real)[3]


### �𵨸� ###
# �ǻ��������
data <- rbind(train,test)
model.dt <- ctree(��������~.,data=data)
round(predict(model.dt,real))
