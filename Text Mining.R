#######################################################################################################################################
#Individual Emperical Assignment for AFA course
#Anthor: XUAN FENG
#Student ID: 558933xf
#######################################################################################################################################

# clean workspace/environment
rm(list = ls())

#load necessary packages
library('rvest')
library('tidyverse')
library('tm')
library('tidytext')
library('stringr')
library('wordcloud')
library('pdftools') # for reading pdf documents and transfer them to texts
library('httr') # deal with outime problem-in for loop, if a pdf is too large and consume too much time for reading, then skip
library('tcltk') #to show the running process
library('qdapDictionaries') # use the English dictionary 'GradyAugmented' for data cleaning - remove meaningless words.
library('lubridate')

########################################################################################################################################
# STEP 1: load texts 

########################################################################################################################################
# following codes are used for load all new articles of NewYork Times Journal (1852 to 2021)
web<-'https://www.nytimes.com/sitemap/'
dir.create("D:/NYtimes_yearly") #generate the main output directory - each txt file represents each year
dir.create("D:/NYtimes_monthly") #generate the main output directory - each txt file represents each month
wholetitle<-list() #create an empty list directory for further write-in news titles

# define a function that will further be used in the for-loop to extract news titles
gettitle<-function(url){
  url%>%read_html()%>%html_nodes(xpath = '//*[@id="site-content"]/div/ul/li')%>%html_text()
}

pb <- tkProgressBar(title="process",label="finished %", min=0, max=100, initial = 0, width = 300) #create a process bar
# use a double for-loop plus a lapply funtion to read the webpage of each day in each month in each year
for (i in 1852:2021) { #first loop based on year - open each year's web
  Ytitlelist<-list()
  dir.create(paste0("D:/NYtimes_monthly/",i))
  for (j in 1:12) { #second loop based on month - open each month's web in each year
    try({ #try is used to jump errors/warnings and continue the loop
      url0<-paste0(web,i,"/",ifelse(j<10,paste0(0,j),j),"/") #generate the web url for each month
      Ndays<-url0%>%read_html()%>%html_nodes(xpath='//*[@id="site-content"]/div/ol/li')%>%html_text() #get the number of days in each month
      Ndays<-as.numeric(Ndays)
      url1<-paste0(url0,ifelse(Ndays<10,paste0(0,Ndays),Ndays)) # generate the web url for each day's news
      url1<-split(url1,1:length(url1)) #transfer to a list, which can be used for 'lapply'
      Mtitlelist<-lapply(url1,gettitle) #'third loop' - lapply the 'gettittle' function on each day, output a list containing all tittles in each month
      Ytitlelist<-c(Ytitlelist,Mtitlelist) #paste each month's title and output a list containing all titles in each year
      txt<-unlist(Mtitlelist)
      write(txt,paste0("D:/NYtimes_monthly/",i,"/",j,".txt"))
    }, silent=TRUE) #end try, don't show error/warning information
  }
  try({ #try is used to jump errors/warnings and continue the loop
    text<-unlist(Ytitlelist)
    wholetitle<-c(wholetitle,list(text)) #output a list directory including all news titles
    write(text,paste0("D:/NYtimes_yearly/",i,".txt")) #write the new titles in "D:/Nytimes" as txt document, each document for each year
  },silent = TRUE) #end try, don't show error/warning information
  #following two lines are used for the process bar
  info<- sprintf("finished %d%%", round((i-2001)*100/(2021-2001)))  
  setTkProgressBar(pb, value = (i-2001)*100/(2020-2001), title = sprintf("process (%s)",info),label = info)
}
close(pb) #close the process bar

########################################################################################################################################
# following codes are used for 104th-117th congresses, which have the websites in the same format
web<-'https://www.congress.gov/'
pdflinklist<-list()
dir.create("D:/congress") #generate the main output directory

# first use a for loop to read all congresses' website (mainly use the 'rvest' package)
for (i in 104:117) {
  url<-paste0('https://www.congress.gov/congressional-record/',i,'th-congress/browse-by-date')
  pdflink<-read_html(url)%>%html_nodes(xpath = '//td[6]/a')%>%html_attr("href") #find each record/pdf's web link
  pdflink<-tibble(link=paste0(web,pdflink)) #generate the full web-link for each record/pdf
  pdflinklist<-c(pdflinklist,list(pdflink)) #generate a list that save all the web-link for all record, each item in the list represents one congress
  dir.create(paste0("D:/congress/",i)) # create a folder under the main folder-'D:/congress' for each congress
  # then use a for loop to read all pdf in each congress's website, convert them to txt files, and save them in each congress's folder
  for (j in 1:nrow(pdflink)) {
    text<-pdf_text(pdflink$link[j])
    write(text,paste0("D:/congress/",i,"/",j,".txt"))
  }
}

##########################################################################################################################################
# following codes are used for loading 82nd-103rd congresses speeches, which have the websites in the same format
# define the suffix for each congress, because the suffix is included in each congress's wesite/url location
suffix<-function (i) {
  if (i %in% c(81,91,101)){
    "st"
  } else if (i %in% c(82,92,102)){
    "nd"
  } else if (i %in% c(83,93,103)){
    "rd"
  } else {
    "th"
  }
}
# first use a double for loop to read all congresses' website (mainly use the 'rvest' package)
# we need to first open the congress website including all records' link, then open the website for each record, then open the pdf link
for (i in 82:103) {
  url<-paste0('https://www.congress.gov/congressional-record/',i,suffix(i),'-congress/browse-by-date')
  dir.create(paste0("D:/congress/",i)) # create a folder under the main folder-'D:/congress' for each congress
  link0<-url%>%read_html()%>%html_nodes(xpath = '//td[5]/a')%>%html_attr("href")
  link0<-tibble(link=paste0(web,link0))
  pdflink<-link0
  # then use a for loop to read all pdf in each congress's website, convert them to txt files, and save them in each congress's folder
  for (n in 1:nrow(link0)){
    try({
      pdflink$link[n]<-link0$link[n]%>%GET(.,timeout(60))%>%read_html()%>%html_nodes(xpath = '//*[@id="content"]/div[2]/p[2]/a')%>%html_attr("href")
      pdflink$link[n]<-paste0(web,pdflink$link[n])
      text<-pdf_text(pdflink$link[n])
      write(text,paste0("D:/congress/",i,"/",n,".txt"))
    }, silent = TRUE)
  }
  pdflinklist<-c(pdflinklist,list(pdflink))
}


##########################################################################################################################################
#STEP 2: Data Cleaning 
##########################################################################################################################################
# define 'remove' vector, which includes the words that appear frequently but are not interested
remove<-c('article','no title','front page','corrections','correction','for the record','letters','to the editor',
          'paid notice: deaths','news summary','weddings','new york','news','will','month','year','day','jkt','frm',
          'sfmt','fmt','next','last','one','two','three','first','second','new','time','times','briefing','dies',
          'says','say','can','review')

months<-tolower(c(month.name,month.abb))#generate a vector included all months(including abbreviations) for data clean
isoletters<-c(letters) #generate a vector of 26 letters for data clean - the meaningless isolated letters may appear due to other data clean steps

#Define a function 'textclean' - use it for all txt documents
textclean<-function(dir){
  document<-Corpus(DirSource(dir))
  document<-tm_map(document,tolower) #transfer all text to lower-case
  document<-tm_map(document,removeNumbers) #remove all numbers
  document<-tm_map(document,removeWords,remove) #remove all uninteresting words defined before in 'remove' vector
  f<-content_transformer(function(x,pattern) gsub(pattern,"",x))
  document<-tm_map(document,f,"[[:punct:]]")  #define a function to remove punctuations
  document<-tm_map(document,removeWords,stopwords("en")) #remove all English stopwords
  document<-tm_map(document,removeWords,months) #remove all months
  document<-tm_map(document,removeWords,isoletters) #remove meaningless isoletters
  document<-tm_map(document,stripWhitespace) #remove white space
}

# Use the 'textclean' function for all new title records from 2020 to 2021, by month
NYtimes<-textclean(dir="D:/NYtimes_monthly") #generate a corpus list including all after-cleaning texts
congress<-textclean(dir="D:/congress/selected/")

# further clean - clean non-english words
#following codes is used to remove non-english words, reference: https://www.debugcn.com/article/48447293.html
tdmnyt<-TermDocumentMatrix(NYtimes)
all_tokens1<-findFreqTerms(tdmnyt,2) #recognize words that appears over 2 times - these words are worth consideration
tokens_to_remove1 = setdiff(all_tokens1, c(GradyAugmented,"covid","corona","coronavirus")) # add 'covid' related new terms
NYtimes <- tm_map(NYtimes, removeWords, tokens_to_remove1)
Nytimes<-tm_map(NYtimes,stripWhitespace)

tdmcgs<-TermDocumentMatrix(congress)
all_tokens2<-findFreqTerms(tdmcgs,2)
tokens_to_remove2 = setdiff(all_tokens2, c(GradyAugmented,"covid","corona","coronavirus"))
congress <- tm_map(congress, removeWords, tokens_to_remove2)
congress<-tm_map(congress,stripWhitespace)

##########################################################################################################################################
#STEP3: Text Analysis
##########################################################################################################################################
# PART 1: News analysis
# 3.1 - Overall pattern
tdmnyt<-TermDocumentMatrix(NYtimes)
Mnyt<-as.matrix(tdmnyt)
commonality.cloud(Mnyt,max.words = 100,colors=brewer.pal(3,"Dark2"),random.order=FALSE) #word cloud for NewYork times news title

tdmcgs<-TermDocumentMatrix(congress)
Mcgs<-as.matrix(tdmcgs)
commonality.cloud(Mcgs,max.words = 100,colors=brewer.pal(3,"Dark2"),random.order=FALSE) #word cloud for congress speeches

# 3.2 - The change of focus over time - top three frenquent words for both corpuses
Tnyt<-as_tibble(Mnyt,rownames=pkgconfig::get_config("tibble::rownames", NA)) #transfer matrix to dataframe and keep row names
Tcgs<-as_tibble(Mcgs,rownames=pkgconfig::get_config("tibble::rownames", NA)) #transfer matrix to dataframe and keep row names
Tnyt1<-Tnyt #as a copy
Tcgs1<-Tcgs #as a copy

#define a function to explore each corpus's top three frequent word in each month (2020-2021)
top3nyt<-function(x){
  first<-rownames(Tnyt1)[which(x==max(x))]
  x[which(x==max(x))]=0
  second<-rownames(Tnyt1)[which(x==max(x))]
  x[which(x==max(x))]=0
  third<-rownames(Tnyt1)[which(x==max(x))]
  top<-c(first,second,third)
}

topnyt<-apply(Tnyt1,2,top3nyt)
#congress speeches have more meaningless strings, so pick top 5.
top3cgs<-function(x){
  first<-rownames(Tcgs1)[which(x==max(x))]
  x[which(x==max(x))]=0
  second<-rownames(Tnyt1)[which(x==max(x))]
  x[which(x==max(x))]=0
  third<-rownames(Tnyt1)[which(x==max(x))]
  x[which(x==max(x))]=0
  forth<-rownames(Tnyt1)[which(x==max(x))]
  x[which(x==max(x))]=0
  fifth<-rownames(Tnyt1)[which(x==max(x))]
  top<-c(first,second,third,forth,fifth)
}

topcgs<-apply(Tcgs1,2,top3cgs)

# 3.3 - Sensitivity Analysis - The change of JF factor

JF<-function(x){
  text_df<-tibble(text=as.character(x))
  tokentext<-text_df%>%unnest_tokens(word,text)
  negwords<-tibble(get_sentiments('loughran'))%>%filter(sentiment=="negative")
  tokentext1<-tokentext%>%inner_join(negwords)%>%count(word,sentiment,sort = TRUE)
  nneg<-sum(tokentext2$n)
  pstwords<-tibble(get_sentiments('loughran'))%>%filter(sentiment=="positive")
  tokentext2<-tokentext%>%inner_join(pstwords)%>%count(word,sentiment,sort = TRUE)
  npst<-sum(tokentext2$n)
  total<-nneg+npst
  JF<-ifelse(npst>nneg,(npst^2-npst*nneg)/(total^2),(npst*nneg-nneg^2)/(total^2))
  round(JF,2)
}

JF_total<-JF(Nytimes)

JF_monthly<-lapply(Nytimes,JF)
JF_monthly<-as_tibble(JF_monthly)
JF_monthly<-t(JF_monthly)
JF_monthly<-as_tibble(JF_monthly,rownames=pkgconfig::get_config("tibble::rownames", NA))
JF_monthly$time<-c('2020/01','2020/02','2020/03','2020/04','2020/05','2020/06','2020/07','2020/08','2020/09',
                   '2020/10','2020/11','2020/12','2021/01','2021/02','2021/03','2021/04')
JF_monthly$time<-factor(JF_monthly$time)
JF_monthly%>%ggplot(aes(x=JF_monthly$time,y=JF_monthly$`JF coefficient`))+geom_point()+theme(plot.title=element_text(hjust=0.5))+
  coord_flip()+ggtitle("JF coefficient in each month")+ylim(-0.5,1)+ylab('JF_coefficient')+xlab('time')

# 3.4 - Key words analysis
analysis<-c("coronavirus","covid","pandemic", "trump", "election", "vaccine")
ananyt<-Tnyt%>%filter(rownames(Tnyt)%in%analysis)
ananyt1<-as_tibble(t(ananyt),rownames=pkgconfig::get_config("tibble::rownames", NA))
ananyt1$time<-c('2020/01','2020/02','2020/03','2020/04','2020/05','2020/06','2020/07','2020/08','2020/09',
                '2020/10','2020/11','2020/12','2021/01','2021/02','2021/03','2021/04')
ananyt1$nmonth<-1:16
ananyt1<-ananyt1[,c(7,1:6)]
ananyt1$sum_covid_19<-ananyt1$coronavirus+ananyt1$pandemic+ananyt1$covid
ananyt1<-ananyt1%>%gather('words','number',c("coronavirus","covid","pandemic", "trump", "election", "vaccine","sum_covid_19"))
ananyt1%>%ggplot(aes(x=ananyt1$nmonth,y=ananyt1$number,color=words))+theme(plot.title=element_text(hjust=0.5))+
  geom_line(size=1)+ylim(0,1500)+ggtitle("Distribution of high-frequency worlds in 16 months (Jan,2020 - Apr,2021)")+
  xlab("No. month since Jan,2020")+ylab("Words number")
  

##########################################################################################################################################
# End 22nd, April, 2021
##########################################################################################################################################