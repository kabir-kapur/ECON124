library(readstata13)
library(scales)

surveyData <- read.dta13("/Users/kabirkapur/Desktop/SCHOOL/ECON124/HW1/survey.dta")
fakeNewsData <- read.dta13("/Users/kabirkapur/Desktop/SCHOOL/ECON124/HW1/fakenews.dta")

# question 2
na.omit(surveyData$fb_share)
sum(fakeNewsData$fb_share)

View(surveyData)
View(fakeNewsData)

fakeNewsBias <- table(fakeNewsData$pro)
surveyClinton <- table(surveyData$ArticleProClinton)
surveyTrump <- table(surveyData$ArticleProTrump)

x1 <- fakeNewsBias[2] + surveyTrump[2]
y1 <- fakeNewsBias[1] + surveyClinton[2]
x <- sum(na.omit(fakeNewsData$fb_share[fakeNewsData$pro=="Trump"])) * 1000


cat(x, "pro-Trump articles were shared on Facebook, and", y, "pro-Clinton articles were shared, in this dataset.")

# question 3
snopes <- sum(fakeNewsData$buzzfeed[])
buzzfeed <- table(fakeNewsData$buzzfeed)[2]
politifact <- table(fakeNewsData$politifact)[2]

snopes

# question 4
sumMedia <- sum(surveyData$MediaMinutesPerDay)

socialMediaRate = sum(surveyData$SocialMediaMinutesPerDay) / sumMedia

socialMediaPercentage <- label_percent()(socialMediaRate)

cat("Respondents spent", sumMedia, "total minutes consuming media per day, and", socialMediaPercentage, "of that time on social media.")

# question 5
mediaVector <- table(surveyData$MostImportantSource)
unlist(mediaVector)
sort(mediaVector)

par(mar = c(1,1,1,1))
barplot(mediaVector, main = "Media Consumption Preferences of Surveyed Individuals", xlab = "Preferred Media Source", ylab = "Number of Respondents", las = 2)

