#단어 빈도 데이터 가져오기
word<-read.csv("C:\\Users\\Mellisa\\Desktop\\word.csv", head=T)
head(word)

dim(word) #[1] 4273    2
str(word)

#워드클라우드 패키지 및 라이브러리
#KoNLP 한글사전을 기반으로 제공하는 패키지
#tm 텍스트마이닝 관련 패키지
install.packages(c("KoNLP", "tm", "wordcloud"))

#패키지 로딩
library(KoNLP) #한글사전 제공
library(tm)    #텍스트를 전처리하는 패키지
library(wordcloud)     #단어 구름을 시각화
library(stringr)

#상위100개 아이템 가져오기
top100word<-word[1:100,]
head(top100word)

name<-top100word$name
fre<-top100word$구매량

word.df = data.frame(word=name, freq=fre) 
head(word.df)

pal <- brewer.pal(12,"Paired") 
windowsFonts(malgun=windowsFont("맑은 고딕")) 

wordcloud(word.df$word, word.df$freq,random.order=F, 
          rot.per=.1, colors=pal, family="malgun")