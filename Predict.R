library("ggplot2", lib.loc="~/R/win-library/3.2")

train <- read.csv("C:/Ramesh/RDevelop/July12AV/Data/train.csv", header=TRUE, stringsAsFactors=FALSE)

train$Category_article <- as.factor(train$Category_article)
train$Day_of_publishing <- as.factor(train$Day_of_publishing)


train.viral <- subset(train, shares >= 10000)
str(train)

#n_tokens_title
table(train$n_tokens_title)

brx <- pretty(range(train.viral$n_tokens_title), n = nclass.Sturges(train.viral$n_tokens_title), min.n = 1)

ggplot(train.viral, aes(x = n_tokens_title)) + 
                geom_histogram(color="darkgray",fill="white", breaks=brx) + 
                scale_x_continuous("n_tokens_title") + 
                theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))
#n_tokens_title
# This follows a normal distribution when shares are 10000. Above this values appear to be bunched between 8 to 12.

#n_tokens_content
brx <- pretty(c(0, 2000), n = 20, min.n = 1)

ggplot(train.viral, aes(x = n_tokens_content)) + 
  geom_histogram(color="darkgray",fill="white", breaks=brx) + 
  scale_x_continuous("n_tokens_content") + 
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))
#n_tokens_content
#The shares appear to be large when the word count is between 100 and 500 in the histogram
#In the scatter plot there appears to be no correlation

#n_unique_tokens
brx <- pretty(range(train.viral$n_unique_tokens), n = 20, min.n = 1)

ggplot(train.viral, aes(x = n_unique_tokens)) + 
  geom_histogram(color="darkgray",fill="white", breaks=brx) + 
  scale_x_continuous("n_unique_tokens") + 
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))
#n_unique_tokens
#The shares appear to be large when the unique token is between 0.4 to 0.8

# n_non_stop_words
brx <- pretty(range(train.viral$n_non_stop_words), n = nclass.Sturges(train.viral$n_non_stop_words), min.n = 1)

ggplot(train.viral, aes(x = n_non_stop_words)) + 
  geom_histogram(color="darkgray",fill="white", breaks=brx) + 
  scale_x_continuous("n_non_stop_words") + 
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))
# n_non_stop_words
# This column as no significance and can be ignored.

# n_non_stop_unique_tokens
brx <- pretty(range(train.viral$n_non_stop_unique_tokens), n = 20, min.n = 1)

ggplot(train.viral, aes(x = n_non_stop_unique_tokens)) + 
  geom_histogram(color="darkgray",fill="white", breaks=brx) + 
  scale_x_continuous("n_non_stop_unique_tokens") + 
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))
# n_non_stop_unique_tokens
#The shares appear to be large when the unique token is between 0.5 to 0.9

#num_hrefs
brx <- pretty(c(0, 80), n = 20, min.n = 1)

ggplot(train.viral, aes(x = num_hrefs)) + 
  geom_histogram(color="darkgray",fill="white", breaks=brx) + 
  scale_x_continuous("num_hrefs") + 
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))
#num_hrefs
#The shares appear to be large when num_hrefs is below 25
