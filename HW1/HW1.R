library(readstata13)
library(scales)

surveyData <- read.dta13("/Users/kabirkapur/Desktop/SCHOOL/ECON124/HW1/survey1.dta")
fakeNewsData <- read.dta13("/Users/kabirkapur/Desktop/SCHOOL/ECON124/HW1/fakenews1.dta")

# question 2
x <- sum(na.omit(fakeNewsData$fb_share[fakeNewsData$pro=="Trump"])) * 1000
y <- sum(na.omit(fakeNewsData$fb_share[fakeNewsData$pro == "Clinton"])) * 1000

cat(x, "pro-Trump articles were shared on Facebook, and", y, "pro-Clinton articles were shared, in this dataset.")

# question 3
snopes <- sum(fakeNewsData$snopes)
buzzfeed <- sum(fakeNewsData$buzzfeed)
politifact <- sum(fakeNewsData$politifact)

snopesExclusive <- sum(fakeNewsData$snopes[fakeNewsData$buzzfeed == 0 & fakeNewsData$politifact == 0])
buzzfeedExclusive <- sum(fakeNewsData$buzzfeed[fakeNewsData$snopes == 0 & fakeNewsData$politifact == 0])
politifactExclusive <- sum(fakeNewsData$politifact[fakeNewsData$snopes == 0 & fakeNewsData$buzzfeed == 0])

cat(snopesExclusive/snopes, buzzfeedExclusive/buzzfeed, politifactExclusive/politifact)

# question 4
cat(sum(surveyData$NewsSource_Social) / sum(surveyData$NewsSource_Web))
meanMedia <- round(mean(surveyData$MediaMinutesPerDay)) # mean number of minutes spent consuming media
sumMedia <- sum(surveyData$MediaMinutesPerDay) # total number of minutes spent consuming media

socialMediaRate <- sum(surveyData$SocialMediaMinutesPerDay[surveyData$MediaMinutesPerDay > 0]) / sum(surveyData$MediaMinutesPerDay[surveyData$MediaMinutesPerDay > 0])

socialMediaPercentage <- label_percent()(socialMediaRate)

cat("The average respondent spent", meanMedia, "minutes per day consuming media, and", socialMediaPercentage, "of that time on social media.")

# question 5
x <- tapply(rep(1, nrow(surveyData)), surveyData$MostImportantSource, sum)

barplot(sort(x), las = 2)

