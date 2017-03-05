# Required Packages

require(rCharts)
require(googleVis)

# Function to calculate sentiment score for every article

nlzr <- function(file_name_vec,key_word){
  fil <- read.csv(file_name_vec)
  txt <- fil$Text
  exwords <- c("Mr.","Ms.","Mrs.","etc","\n")
  txt <- gsub(x=txt,pattern = paste(exwords, collapse = "|"), replacement = "")
  txt <- tolower(txt)
  sentence <- strsplit(txt, split=".", fixed=TRUE)
  word <- strsplit(txt,split=" ", fixed=TRUE)
  
  cname <- c("words","value")
  afinn <- read.delim2("AFINN-111.txt", header=FALSE, col.names = cname)
  
  for(i in 1:length(txt)) 
  {
    word[[i]]  <- gsub("[[:punct:]]", "", word[[i]])
    
    sentence[[i]] <- gsub("[[:punct:]]", " ", sentence[[i]])
    
    inds <- which(afinn$words %in% word[[i]])
    ifelse(length(inds) == 0, txt_score <- 0, txt_score<- mean(afinn$value[inds]))
    fil$Text_Score[i] <- txt_score
    
    pos <- grep(key_word, sentence[[i]])
    hold_pos <- sentence[[i]][pos]
    sent_score <- c(0)
    if(length(pos) != 0){ 
    for (j in 1:length(hold_pos)){
      word_from_sent <- strsplit(hold_pos[j],split=" ", fixed=TRUE)
      inds <- which(afinn$word %in% word_from_sent[[length(word_from_sent)]])
      
      if(length(inds)>0){
        sent_score <- sent_score + mean(afinn$value[inds])
      }
      
    }
    }
    if(length(hold_pos) == 0){
      fil$Sentence_Score[i] <- 0
    } else{
      sent_score <- sent_score/(length(hold_pos))
    }
    fil$Sentence_Score[i] <- sent_score
    
    net_score <- c(0)
    ifelse(sign(txt_score) == sign(sent_score), 
           net_score <- sign(sent_score)*abs(abs(txt_score)-abs(sent_score)), 
           net_score <- sign(sent_score)*abs(abs(txt_score)+abs(sent_score)))
    
    fil$Net_Score[i] <- net_score 
  }
  ret <- fil[,c(1,6,7,8,4,5)]
  return(ret)
}

# Applying the function accros all the input files.
# I can make this better by having a proper series of file names, but because of the time constraint 
# left it like this.

path <- "C:/Users/ashok_000/Desktop/CS 504 Project/Sentiment Analysis on News Articles/All Agencies/Extracted Text"
input <- paste0('input',1:30)
output <- paste0('output',1:30)
input[1] <- paste0(path, "/1_NYT_Hillary.csv")
input[2] <- paste0(path, "/2_NYT_Republicans.csv")
input[3] <- paste0(path, "/3_NYT_Ben Carson.csv")
input[4] <- paste0(path, "/4_NYT_Democrats.csv")
input[5] <- paste0(path, "/5_NYT_Donald Trump.csv")
input[6] <- paste0(path, "/6_NYT_Bernie Sanders.csv")
input[7] <- paste0(path, "/7_FOX_Hillary_Clinton.csv")
input[8] <- paste0(path, "/8_FOX_Republicans.csv")
input[9] <- paste0(path, "/9_FOX_Ben_Carson.csv")
input[10] <- paste0(path, "/10_FOX_Democrats.csv")
input[11] <- paste0(path, "/11_FOX_Donald_Trump.csv")
input[12] <- paste0(path, "/12_FOX_Bernie_Sanders.csv")
input[13] <- paste0(path, "/13_USAToday_Hillary.csv")
input[14] <- paste0(path, "/14_USAToday_Republicans.csv")
input[15] <- paste0(path, "/15_USAToday_Ben Carson.csv")
input[16] <- paste0(path, "/16_USAToday_Democrats.csv")
input[17] <- paste0(path, "/17_USAToday_Donald Trump.csv")
input[18] <- paste0(path, "/18_USAToday_Bernie Sanders.csv")
input[19] <- paste0(path, "/19_WSP_Hillary Clinton.csv")
input[20] <- paste0(path, "/20_WSP_Republicans.csv")
input[21] <- paste0(path, "/21_WSP_Ben Carson.csv")
input[22] <- paste0(path, "/22_WSP_Democrats.csv")
input[23] <- paste0(path, "/23_WSP_Donald Trump.csv")
input[24] <- paste0(path, "/24_WSP_Bernie Sanders.csv")
input[25] <- paste0(path, "/25_NewYorker_Hillary.csv")
input[26] <- paste0(path, "/26_NewYorker_Republicans.csv")
input[27] <- paste0(path, "/27_NewYorker_Ben Carson.csv")
input[28] <- paste0(path, "/28_NewYorker_Democrats.csv")
input[29] <- paste0(path, "/29_NewYorker_Donald Trump.csv")
input[30] <- paste0(path, "/30_NewYorker_Bernie Sanders.csv")


candidate <- c("Hillary Clinton", "Republicans", "Ben Carson", "Democrats","Donald Trump", "Bernie Sanders")
candidate1 <- c("clinton", "republican", "carson", "democrat","trump", "sanders")

#  Generating output in loop (not working as of now)
for (i in 1:30){
  paste('output', formatC(i, width = 2, flag = "0"), sep = "") <- nlzr(input[i], paste(candidate1[ifelse(i%%6 ==0,6,i%%6)]))
  }
View(output[1])

output <- 0 
for (i in 1:10){
  get(paste('output', formatC(i, width = 1), sep = "")) <- nlzr(input[i], paste(candidate1[ifelse(i%%6 ==0,6,i%%6)]))
}
View(output[1])

a <- nlzr(input[1], "clinton")

# output1 <- nlzr(input[1], "clinton")
# output2 <- nlzr(input[2], 'republican')
# output3 <- nlzr(input[3], 'carson')
# output4 <- nlzr(input[4], 'democrat')
# output5 <- nlzr(input[5], 'trump')
# output6 <- nlzr(input[6], 'sanders')
# output7 <- nlzr(input[7], 'clinton')
# output8 <- nlzr(input[8], 'republican')
# output9 <- nlzr(input[9], 'carson')
# output10 <- nlzr(input[10], 'democrat')
output11 <- nlzr(input[11], 'trump')
# output12 <- nlzr(input[12], 'sanders')
# output13 <- nlzr(input[13], 'clinton')
# output14 <- nlzr(input[14], 'republican')
# output15 <- nlzr(input[15], 'carson')
# output16 <- nlzr(input[16], 'democrat')
# output17 <- nlzr(input[17], 'trump')
# output18 <- nlzr(input[18], 'sanders')
# output19 <- nlzr(input[19], 'clinton')
# output20 <- nlzr(input[20], 'republican')
# output21 <- nlzr(input[21], 'carson')
# output22 <- nlzr(input[22], 'democrat')
# output23 <- nlzr(input[23], 'trump')
# output24 <- nlzr(input[24], 'sanders')
 output25 <- nlzr(input[25], 'clinton')
 output26 <- nlzr(input[26], 'republican')
 output27 <- nlzr(input[27], 'carson')
output28 <- nlzr(input[28], 'democrat')
 output29 <- nlzr(input[29], 'trump')
 output30 <- nlzr(input[30], 'sanders')

# Writing the output

for (i in 1:30) {
  if(i!=11){
  write.csv(get(paste('output', formatC(i, width = 1), sep = "")), paste('output', formatC(i, width = 2, flag = "0"), 
                           ".csv", sep = ""))
  }
}  
 
 output25 <- output25[,c(1,3,4,5,6)]
write.csv(output11, file = "output11.csv")

  
# Sample file execution
a <- nlzr(input[1], 'clinton')
a$Date <- as.Date(a$Date)
a$x <- c(0)

# Visulization

# Sample
samplevis1 <- read.csv("sample.csv", header = TRUE)
samplevis1$Date <- as.Date(samplevis1$Date)
## Visualization using GoogleVis Package
vis1 <- read.csv("All_output.csv", header = TRUE)
vis1$Date <- as.Date(vis1$Date)

# Custom settings for the motion chart
myStateSettings <-'
{"xZoomedDataMin":1199145600000,"colorOption":"2",
"duration":{"timeUnit":"Y","multiplier":1},"yLambda":1,
"yAxisOption":"4","sizeOption":"_UNISIZE",
"iconKeySettings":[],"xLambda":1,"nonSelectedAlpha":0,
"xZoomedDataMax":1262304000000,"iconType":"LINE",
"dimensions":{"iconDimensions":["dim0"]},
"showTrails":false,"uniColorForNonSelected":false,
"xAxisOption":"_TIME","orderedByX":false,"playDuration":15000,
"xZoomedIn":false,"time":"2010","yZoomedDataMin":0,
"yZoomedIn":false,"orderedByY":false,"yZoomedDataMax":100}'


# Visualzing data for every news Agency

# New York Times
nytimes <- read.csv("NYTimes.csv", header = TRUE)
head(nytimes)
nytimes$Date <- as.Date(nytimes$Date)
NYT = gvisMotionChart(nytimes, 
                       idvar="Political.Contestent", 
                       timevar="Date",
                       options = list(width=600, height=450, state = myStateSettings, title='New York Times'))

plot(NYT)

# Fox News
foxnews <- read.csv("FOXNews.csv", header = TRUE)
head(foxnews)
foxnews$Date <- as.Date(foxnews$Date)
FoxNews = gvisMotionChart(foxnews, 
                      idvar="Political.Contestent", 
                      timevar="Date",
                      options = list(width=1200, height=700, state = myStateSettings))

plot(FoxNews)
# USA Today
usatoday <- read.csv("USAToday.csv", header = TRUE)
head(usatoday)
usatoday$Date <- as.Date(usatoday$Date)
USAToday = gvisMotionChart(usatoday, 
                          idvar="Political.Contestent", 
                          timevar="Date",
                          options = list(width=600, height=450, state = myStateSettings))


# Washington Post
washingtonpost <- read.csv("WashingtonPost.csv", header = TRUE)
head(washingtonpost)
washingtonpost$Text_Score <- as.numeric(washingtonpost$Text_Score)
washingtonpost$Sentence_Score <- as.numeric(washingtonpost$Sentence_Score)
washingtonpost$Net_Score <- as.numeric(washingtonpost$Net_Score)

washingtonpost$Date <- as.Date(washingtonpost$Date)
WashingtonPost = gvisMotionChart(washingtonpost, 
                          idvar="Political.Contestent", 
                          timevar="Date",
                          options = list(width=400, height=400, state = myStateSettings))


# New Yorker
newyorker <- read.csv("NewYorker.csv", header = TRUE)
head(newyorker)
newyorker$Date <- as.Date(newyorker$Date)
newyorker$Text_Score <- as.numeric(newyorker$Text_Score)
washingtonpost$Sentence_Score <- as.numeric(newyorker$Sentence_Score)
newyorker$Net_Score <- as.numeric(newyorker$Net_Score)

NewYorker = gvisMotionChart(newyorker, 
                          idvar="Political.Contestent", 
                          timevar="Date",
                          options = list(width=400, height=400, state = myStateSettings))

## Plotting the data
plot(NYT)
plot(FoxNews)
plot(USAToday)
plot(WashingtonPost)
plot(NewYorker)

## Generating a merged plot
AllAgencies <- gvisMerge(gvisMerge(NYT,FoxNews, USAToday, horizontal = TRUE), 
                         gvisMerge(WashingtonPost, NewYorker, horizontal= TRUE),
                         tableOptions="bgcolor=\"#CCCCCC\" cellspacing=10")

NYTFox <- gvisMerge(NYT,FoxNews, horizontal = TRUE, tableOptions = "cellspacing=20")
NYTFoxUSA <- gvisMerge(gvisMerge(NYT,FoxNews, horizontal = FALSE), USAToday, horizontal = TRUE,
                       tableOptions="bgcolor=\"#CCCCCC\" cellspacing=10")

NYTFoxUSA <- gvisMerge(NYTFoxNews, USAToday, horizontal = FALSE,
                       tableOptions="bgcolor=\"#CCCCCC\" cellspacing=10")


plot(AllAgencies)
plot(NYTFox)
plot(NYTFoxUSA)


# Retrieving unique rows
vis2[,c(1,2,5,6)] <- unique(vis2[,c(1,2,5,6)])
vis2 <- vis2[ !duplicated(vis2), ] 
vis2 <- unique(vis1[,c(1,2)])
vis2$Political_Contestent <- vis1$Political_Contestent
nrow(vis2)
head(vis2)
write.csv(vis2,"unique1.csv")

## Visualizing the motion chart
vis2$Number <- c(1:6283)
vis1$Net_Score <- as.numeric(vis1$Net_Score)
vis1$Sentence_Score <- as.numeric(vis1$Sentence_Score)
vis1$Text_Score <- as.numeric(vis1$Text_Score)

vis1 <- read.csv("unique1.csv", header = TRUE)
vis1$Date <- as.Date(vis1$Date)
Motion=gvisMotionChart(vis1, 
                       idvar="Text_Score", 
                       timevar="Date",
                       options = list(width=1200, height=700, state = myStateSettings))

plot(Motion)



## A ggplot visualization

library(ggplot2)
library(grid)
library(fit.models)

hw <- theme_gray() + theme(
  strip.background=element_rect(fill=rgb(.9,.95,1),
                                colour=gray(.5), size=.2),
  panel.border=element_rect(fill=FALSE,colour=gray(.50)),
  axis.text=element_text(colour="black"),
  axis.ticks=element_blank(),
  axis.ticks.margin=unit(-0.05,"cm"),
  panel.grid.minor = element_blank()
)

ggplot(nytimes,aes(x=Date,y=Text_Score))+
    geom_point(shape=21,fill="blue",color="black",size=3.2)+
  labs(x="Date", y="TextScore",
       title=paste("Q-Q Plot","Blue Lines At Medians",
                   "Black Line Thru 1st and 3rd Quartiles",sep="\n"))+hw

