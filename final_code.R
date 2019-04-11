### raw데이터 ###
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


### 장르/감독명/관람가 ###
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

a$watchGradeNm[a$watchGradeNm=='15세관람가'] <- '15세이상관람가'
a$watchGradeNm[a$watchGradeNm=='고등학생이상관람가'] <- '15세이상관람가'
a$watchGradeNm[a$watchGradeNm=='모든 관람객이 관람할 수 있는 등급'] <- '전체관람가'
a$watchGradeNm[a$watchGradeNm=='연소자관람가'] <- '전체관람가'
a$watchGradeNm[a$watchGradeNm=='18세관람가'] <- '청소년관람불가'
a$watchGradeNm[a$watchGradeNm=='연소자관람불가'] <- '청소년관람불가'


### 관람객수 ###
library(jsonlite)
library(dplyr)
library(httr)
library(RSelenium)
library(rvest)

ch=wdman::chrome(port=4567L) 
remDr=remoteDriver(port=4567L, browserName='chrome')
remDr$open()

관람객수 <- NULL
for(i in 1:537){
  if(is.na(a$movieNm[i])){관람객수<-c(관람객수,NA)}else{
  url2 <- paste0("https://search.naver.com/search.naver?where=nexearch&sm=top_hty&fbm=0&ie=utf8&query=",a$movieNm[i])
  remDr$navigate(url2)
  url2 <- remDr$getCurrentUrl()[[1]] 
  number <- read_html(url2)
  numberinfos <- html_nodes(number,css='.desc_detail')
  val2 <- numberinfos%>%html_nodes('span')%>%html_text()
  val2 <- val2[grep('명',val2)]
  val2 <- val2[length(val2)]
  val2 <- gsub('명','',val2)
  val2 <- gsub(',','',val2)
  if(!identical(grep("[1234567890]",val2),integer(0))){val2=val2}else{val2=numeric(0)}
  val2 <- as.numeric(val2)
  if(!identical(val2,numeric(0))) {관람객수 <- c(관람객수,val2)} else {관람객수 <- c(관람객수,NA)}
 }
 }
a$관람객수 <- 관람객수
for(i in 1:nrow(a)){
if(is.na(a$관람객수[i])) {a$관람객수[i] <- a$audiAcc[i]}
}
# 코드를 통해 나오지 않은 영화의 관람객수는 직접 검색해서 찾아넣었고, 관람객수가 인터넷에 나오지
# 않은 영화는 박스오피스 10위권 내에 있을 때의 최종관객수로 대체함.


### 누적상영횟수 ###
a$cumshow <- tapply(as.numeric(movie$showCnt),movie$movieNm,sum)


### weight ###
movie$dayNm <- c(rep(rep(c('금','토','일','월','화','수','목'),each=10),104),rep(c('금','토','일'),each=10))
startdate<-as.Date("2016-01-01")
enddate<-as.Date("2017-12-31")
date<-seq(startdate,enddate,by="1 days")
movie$date <-  rep(date,each=10)
a$weight <- apply(table(movie$movieNm,movie$dayNm)[,5:6],1,sum)
movie$weight2 <- 0
movie$weight2[movie$date=='2016-01-01'|movie$date=='2016-02-08'|movie$date=='2016-02-09'|movie$date=='2016-02-10'|movie$date=='2016-03-01'|movie$date=='2016-04-13'|movie$date=='2016-05-05'|movie$date=='2016-05-06'|movie$date=='2016-06-06'|movie$date=='2016-08-15'|movie$date=='2016-09-14'|movie$date=='2016-09-15'|movie$date=='2016-09-16'|movie$date=='2016-10-03'] <- 1
movie$weight2[movie$date=='2017-01-27'|movie$date=='2017-01-30'|movie$date=='2017-03-01'|movie$date=='2017-05-03'|movie$date=='2017-05-05'|movie$date=='2017-05-09'|movie$date=='2017-06-06'|movie$date=='2017-08-15'|movie$date=='2017-10-02'|movie$date=='2017-10-03'|movie$date=='2017-10-04'|movie$date=='2017-10-05'|movie$date=='2017-10-06'|movie$date=='2017-10-09'|movie$date=='2017-12-25'] <- 1
a$weight2 <- tapply(movie$weight2,movie$movieNm,sum)


### 출연배우 ###
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


### 감독 영화명/감독 파워 ###
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
  val2 <- val2[grep('명',val2)]
  val2 <- val2[grep('[0-9]',val2)]
  val2 <- gsub('명','',val2)
  val2 <- gsub(',','',val2)
  val2 <- as.numeric(val2)
  if(!identical(val2,numeric(0))) {value <- c(value,val2)} else {value <- c(value,NA)}
 }
total <- rbind(total,value)
}
a$director.mean <- apply(total,1,mean,na.rm=T)


### 배우 영화명/배우 파워 ###
ch=wdman::chrome(port=4567L) 
remDr=remoteDriver(port=4567L, browserName='chrome')
remDr$open()

actor_s<-as.character(a$actor)
actor_s<-strsplit(actor_s,",")
actor_s[[1]][1]="존 크래신스키"

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
  val2 <- val2[grep('명',val2)]
  val2 <- val2[grep('[0-9]',val2)]
  val2 <- gsub('명','',val2)
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


### 배우파워/감독파워 NA값 회귀대체 ###
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


### 배급사 ###
ch=wdman::chrome(port=4567L) 
remDr=remoteDriver(port=4567L, browserName='chrome')
remDr$open()

br <- NULL
for(i in a$movieNm){
 url <- paste0('https://search.naver.com/search.naver?sm=tab_hty.top&where=nexearch&query=',i,' 배급사')
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

### 박스오피스 3위권 진입 일수 ###
movie$n <- 1
movie2 <- movie[movie$rank<=3,]
rankNum <- tapply(movie2$n,movie2$movieNm,sum)

a$rankNum <- 0
for(i in 1:length(rankNum)){
a$rankNum[grep(unique(movie2$movieNm)[i],unique(movie$movieNm))] <- rankNum[i]
}


### 제작국가 ###
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
a$nation2[grep('한국',a$nation)] <- 'KOREA'
a$nation2[grep('미국',a$nation)] <- 'USA'
a$nation2[grep('일본',a$nation)] <- 'JAPAN'
a$nation2[grep('영국',a$nation)] <- 'UK'


### 공휴일 ###
a$weight3 <- a$weight+a$weight2


### 장르2 ###
data <- a
data <- data[-c(13,498),]  # 불필요한 데이터 제거

data$genre2<-data$genre
data$genre2<-as.character(data$genre2)
data$genre2[data$genre2=="SF"|data$genre2=="어드벤처"|data$genre2=="판타지"]<-1
data$genre2[data$genre2=="기타"|data$genre2=="다큐멘터리"|data$genre2=="가족"|data$genre2=="공연"|data$genre2=="뮤지컬"|data$genre2=="사극"]<-2
data$genre2[data$genre2=="드라마"|data$genre2=="멜로"]<-3
data$genre2[data$genre2=="스릴러"|data$genre2=="공포(호러)"|data$genre2=="미스터리"]<-4
data$genre2[data$genre2=="애니메이션"]<-5
data$genre2[data$genre2=="액션"|data$genre2=="범죄"|data$genre2=="전쟁"]<-6
data$genre2[data$genre2=="코미디"]<-7

data$popular<-0
data[data$genre2==1|data$genre2==6,]$popular<-1


### 관람가2 ###

data$watchGradeNm[grep('젝스키스',data$movieNm)] <- '전체관람가'
data$watchnew<-0
data[data$watchGradeNm=="12세이상관람가"|data$watchGradeNm=="15세이상관람가",]$watchnew<-1
data[data$watchGradeNm=="전체관람가",]$watchnew<-2


### 국가2 ###
data$nationnew<-data$nation2
data[data$nation2=="etc"|data$nation2=="JAPAN"|data$nation2=="UK",]$nationnew<-"ETC"


### 배급사2 ###
br <- rep(3,535)
br[grep('디즈니',data$br)] <- 1
br[grep('CJ',data$br)] <- 1
br[grep('롯데',data$br)] <- 1
br[grep('NEW',data$br)] <- 2
br[grep('이십세기',data$br)] <- 2
br[grep('유니버셜',data$br)] <- 2
br[grep('워너',data$br)] <- 2
br[grep('쇼박스',data$br)] <- 2
br[grep('소니',data$br)] <- 3
br[grep('메가박스',data$br)] <- 3

br <- as.factor(br)
data$br <- br


### 특이점, 영향점 제거 ###
data <- data[,c('cumshow','director.mean','weight3','actor.mean','br','rankNum','popular','watchnew','nationnew','관람객수')]
lm <- lm(관람객수~.,data=data)
summary(lm)
plot(lm,which=5)

data <- data[-c(2,266,325,535,432),]


### train, test 데이터셋 나누기 ###
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


lm <- lm(관람객수~.,data=train)
p1 <- predict(lm,test)
rmse(p1,test$관람객수)

dt <- ctree(관람객수~.,data=train)
p2 <- predict(dt,test)
rmse(p2,test$관람객수)

rf <- randomForest(관람객수~.,data=train,mtry=,ntree=500)
p3 <- predict(rf,test)
rmse(p3,test$관람객수)

set.seed(123)
xgb <- train(
  관람객수 ~., data = train, method = "xgbTree",
  trControl = trainControl("cv", number = 10)
  )
p4 <- predict(xgb,test)
rmse(p4,test$관람객수)


### 예측 영화 NA값 대체 ###
cumshow.lm <- lm(cumshow~rankNum+weight3+actor.mean,data=data)
director.lm <- lm(director.mean~actor.mean+cumshow,data=data)
real[1,1] <- predict(cumshow.lm,real)[1]
real[1,2] <- predict(director.lm,real)[1]
real[1,3] <- 15
real[2,2] <- predict(director.lm,real)[2]
real[3,1] <- predict(cumshow.lm,real)[3]


### 모델링 ###
# 의사결정나무
data <- rbind(train,test)
model.dt <- ctree(관람객수~.,data=data)
round(predict(model.dt,real))

