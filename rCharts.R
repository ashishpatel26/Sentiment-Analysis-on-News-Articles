x <- rPlot(mpg~wt|am + vs, color='gear', data = mtcars, type='point')
x
hair_eye_male <- subset(as.data.frame(HairEyeColor), Sex == "Male")
n1 <- nPlot(Freq ~ Hair, group = "Eye", data = hair_eye_male, 
            type = 'multiBarChart')
require(googleVis)

gvis <- gvisLineChart(iris, xvar = 'Sepal.Length', yvar = 'Sepal.Width', options = list(width=600, height=600))
plot(gvis)
head(iris)
      
vis1 <- read.csv("All_output.csv", header = TRUE)
vis1$Date <- as.Date(vis1$Date)
n1 <- nPlot(Text_Score ~ Political_Contestent+News_Agency, group = "Date", data = vis1, 
            type = 'multiBarChart')

n1
n1$publish('Sentiment Score')
devtools::install_github('rstudio/shinyapps')
shinyapps::setAccountInfo(name='ashokvardhan', token='7D5637F241BB79603AC18B6D964D4000', secret='mqqaSZfpJcd97qoumeDddNBG8JmFa7JJuh78sgY7')
library(shinyapps)
devtools::install_github('rstudio/rsconnect')
library(rsconnect)
