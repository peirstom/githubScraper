library(dplyr)
library(psych)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(qdap)
library(tidyr)
library(ggplot2)
library(udpipe)
library(readxl)
library(stringr)
library(ggpubr)
# Import Datasets
df <- read.csv("C:/Users/peirs/Desktop/Ecosystems/data/github_data.csv", sep=";", na.strings=c("","NA"))
topics <- read.csv("C:/Users/peirs/Desktop/Ecosystems/data/topics.csv", sep=";", na.strings=c("","NA"))

# Data Preperation. # Convert Language column to upper to avoid duplicates.
df$Language <- toupper(df$Language);
df$Language <- as.factor(df$Language);
df$Description <- tolower(as.character(df$Description));
#Split data in different groups
split <- split(df, df$Language)
df_JAVA <- split$JAVA;
df_PYTHON <- split$PYTHON;
df_R <- split$R;
df_MATLAB <- split$MATLAB;

##### JAVA CORPUS #####
JAVA_Corpus <- Corpus(VectorSource(df_JAVA$Description))
JAVA_Corpus <- JAVA_Corpus %>%
  tm_map(stripWhitespace)
JAVA_Corpus <- tm_map(JAVA_Corpus, content_transformer(tolower));
JAVA_Corpus <- tm_map(JAVA_Corpus, removeWords, c(stopwords("english"),"java"));

TDM_JAVA <- TermDocumentMatrix(JAVA_Corpus) ;
DTM_JAVA <- DocumentTermMatrix(JAVA_Corpus);
matrix_JAVA <- as.matrix(TDM_JAVA);
words_JAVA <- sort(rowSums(matrix_JAVA),decreasing=TRUE);
JAVA_DF <- data.frame(word = names(words_JAVA),freq=words_JAVA, percentage= words_JAVA/sum(words_JAVA)*100)
JAVA_DF <-  subset(JAVA_DF, JAVA_DF$word %in% topics$Topics)

##### PYTHON CORPUS #####  
PYTHON_Corpus <- Corpus(VectorSource(df_PYTHON$Description))
PYTHON_Corpus <- docs_PYTHON %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace)
PYTHON_Corpus <- tm_map(PYTHON_Corpus, content_transformer(tolower))
PYTHON_Corpus <- tm_map(PYTHON_Corpus, removeWords, c(stopwords("english"),"python"))

TDM_PYTHON <- TermDocumentMatrix(PYTHON_Corpus) 
DTM_PYTHON <- DocumentTermMatrix(PYTHON_Corpus)
matrix_PYTHON <- as.matrix(TDM_PYTHON) 
words_PYTHON <- sort(rowSums(matrix_PYTHON),decreasing=TRUE) 
PYTHON_DF <- data.frame(word = names(words_PYTHON),freq=words_PYTHON, percentage = words_PYTHON/sum(words_PYTHON)*100)
PYTHON_DF <-  subset(PYTHON_DF, PYTHON_DF$word  %in% topics$Topics)

##### R CORPUS #####  
R_CORPUS <- Corpus(VectorSource(df_R$Description))
R_CORPUS <- R_CORPUS %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace)
R_CORPUS <- tm_map(R_CORPUS, content_transformer(tolower))
R_CORPUS <- tm_map(R_CORPUS, removeWords, c(stopwords("english"),"rlanguage"))

TDM_R <- TermDocumentMatrix(R_CORPUS) 
DTM_R <- DocumentTermMatrix(R_CORPUS)
matrix_R <- as.matrix(TDM_R) 
words_R <- sort(rowSums(matrix_R),decreasing=TRUE) 
R_DF <- data.frame(word = names(words_R),freq=words_R, percentage = words_R/sum(words_R)*100)
R_DF <-  subset(R_DF, R_DF$word  %in% topics$Topics)

##### MATLAB CORPUS #####  
MATLAB_Corpus <- Corpus(VectorSource(df_MATLAB$Description))
MATLAB_Corpus <- MATLAB_Corpus %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace)
MATLAB_Corpus <- tm_map(MATLAB_Corpus, content_transformer(tolower))
MATLAB_Corpus <- tm_map(MATLAB_Corpus, removeWords, c(stopwords("english"),"matlab"))

TDM_MATLAB <- TermDocumentMatrix(MATLAB_Corpus) 
DTM_MATLAB <- DocumentTermMatrix(MATLAB_Corpus)
matrix_MATLAB <- as.matrix(TDM_MATLAB) 
words_MATLAB <- sort(rowSums(matrix_MATLAB),decreasing=TRUE) 
MATLAB_DF <- data.frame(word = names(words_MATLAB),freq=words_MATLAB, percentage = words_MATLAB / sum(words_MATLAB)*100)
MATLAB_DF <-  subset(MATLAB_DF, MATLAB_DF$word %in% topics$Topics)

##### GENERAL CORPUS #####
TDM <- c(TDM_JAVA, TDM_PYTHON, TDM_R, TDM_MATLAB);
DTM <- c(DTM_JAVA, DTM_PYTHON, DTM_R, DTM_MATLAB);
matrix <- as.matrix(TDM) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df_all <- data.frame(word = names(words),freq=words, percentage = words / sum(words)*100)
df_all <-  subset(df_all, df_all$word %in% topics$Topics)


## calculate topics per repository
KeyWords<-topics$Topics;
SearchFunc<-function(X){Xsep<-unlist(strsplit(X, " ")); length(which(Xsep%in%KeyWords))}

df_JAVA$Description <- removeWords(df_JAVA$Description, "java");
df_PYTHON$Description <- removeWords(df_PYTHON$Description, "python")
df_R$Description <- removeWords(df_R$Description,"rlanguage")
df_MATLAB$Description <- removeWords(df_MATLAB$Description,"matlab")
df$Description <- tolower(as.character(df$Description));
df$Variety <- 0;
df <- do.call("rbind", list(df_JAVA, df_PYTHON, df_R, df_MATLAB))
df$Variety<-unlist(lapply(t(df$Description), SearchFunc))


# Populate unique number of topics for each language.
df$Variety.Language <- 0;
df$Variety.Language[which(df$Language=="JAVA")] <-  nrow(filter(JAVA_DF, JAVA_DF$word %in% topics$Topics))
df$Variety.Language[which(df$Language=="PYTHON")] <- nrow(filter(PYTHON_DF, PYTHON_DF$word %in% topics$Topics))
df$Variety.Language[which(df$Language=="R")] <- nrow(filter(R_DF, R_DF$word %in% topics$Topics))
df$Variety.Language[which(df$Language=="MATLAB")] <- nrow(filter(MATLAB_DF, MATLAB_DF$word %in% topics$Topics))

# create a new column. Programming Language Type
df$LanguageType = "";
df$LanguageType[df$Language=="JAVA"] <- "GENERAL";
df$LanguageType[df$Language=="PYTHON"] <- "GENERAL";
df$LanguageType[df$Language=="R"] <- "SPECIFIC";
df$LanguageType[df$Language=="MATLAB"] <- "SPECIFIC";
df$LanguageType <- as.factor(df$LanguageType);
##############################################################################################################
##############################################################################################################
##############################################################################################################

### For every type of programming language, \{technical variety} correlates with the number of active contributors.
res.man <- manova(cbind(df$Variety, df$Active.Contributors) ~ df$Language, data = df)
summary(res.man)
# Look to see which differ
summary.aov(res.man)

######## Descriptives #########
### 1) Descriptives per language ###
describeBy(df, df$Language)
### 2) ANOVA per language ###
df.Java <- df[df$Language=="JAVA",]
df.Python <- df[df$Language=="PYTHON",]
df.R <- df[df$Language=="R",]
df.Matlab <- df[df$Language=="MATLAB",]
# ANOVA per language variety vs contributors
summary(aov(df.Java$Variety ~ df.Java$Active.Contributors))
summary(aov(df.Python$Variety ~ df.Python$Active.Contributors))
summary(aov(df.R$Variety ~ df.R$Active.Contributors))
summary(aov(df.Matlab$Variety ~ df.Matlab$Active.Contributors))
# ANOVA per language: variety vs commits
summary(aov(df.Java$Variety ~ df.Java$Commits.2019))
summary(aov(df.Python$Variety ~ df.Python$Commits.2019))
summary(aov(df.R$Variety ~ df.R$Commits.2019))
summary(aov(df.Matlab$Variety ~ df.Matlab$Commits.2019))
### 3) Correlations between variety and commits per language
cor.test(df.Java$Variety, df.Java$Commits.2019, method = c("pearson","kendall","spearman"))
cor.test(df.Python$Variety, df.Python$Commits.2019, method = c("pearson","kendall","spearman"))
cor.test(df.R$Variety, df.R$Commits.2019, method = c("pearson","kendall","spearman"))
cor.test(df.Matlab$Variety, df.Matlab$Commits.2019, method = c("pearson","kendall","spearman"))
### 4) Correlations between variety and active contributors per langauge
cor.test(df.Java$Variety, df.Java$Active.Contributors, method = c("pearson","kendall","spearman"))
cor.test(df.Python$Variety, df.Python$Active.Contributors, method = c("pearson","kendall","spearman"))
cor.test(df.R$Variety, df.R$Active.Contributors, method = c("pearson","kendall","spearman"))
cor.test(df.Matlab$Variety, df.Matlab$Active.Contributors, method = c("pearson","kendall","spearman"))


#### Desciptives all languages together
describeBy(df)
# ANOVA TEST for complete dataset
#Technical variety has a positive effect  on active  contributors.
summary(aov(df$Variety ~ df$Active.Contributors))
#Technical variety has a positive effect  on commits.
summary(aov(df$Variety ~ df$Commits.2019))
# KRUSKAL WALLIS TEST (done if not a normal distribution)
kruskal.test(df$Variety, df$Active.Contributors)
kruskal.test(df$Variety, df$Commits.2019)

#Correlation test between two variables
#Pearson correlation (r), which measures a linear dependence between two variables (x and y). It's also known as a parametric correlation test because it depends to the distribution of the data. It can be used only when x and y are from normal distribution. The plot of y = f(x) is named the linear regression curve.
#Kendall tau and Spearman rho, which are rank-based correlation coefficients (non-parametric
cor.test(df$Variety, df$Commits.2019, method = c("pearson","kendall","spearman"))
cor.test(df$Variety, df$Active.Contributors, method = c("pearson","kendall","spearman"))


########## Visualisation #########
#Create Wordcloud(s) -> change DF respectively
set.seed(1234) # for reproducibility 
wordcloud(words = JAVA_DF$word, freq = JAVA_DF$freq, min.freq = 1,max.words=100,
          random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))
wordcloud(words = PYTHON_DF$word, freq = PYTHON_DF$freq, min.freq = 1,max.words=120,
          random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))
wordcloud(words = R_DF$word, freq = R_DF$freq, min.freq = 1,max.words=100,
          random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))
wordcloud(words = MATLAB_DF$word, freq = MATLAB_DF$freq, min.freq = 1,max.words=100,
          random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))
wordcloud(words = df_all$word, freq = df_all$freq, min.freq = 1,max.words=150,
          random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

#Create frequency plot of topics
wf1 <- data.frame(word=JAVA_DF$word, freq=JAVA_DF$freq,percentage = JAVA_DF$percentage)
wf2 <- data.frame(word=PYTHON_DF$word, freq=PYTHON_DF$freq, percentage = PYTHON_DF$percentage)
wf3 <- data.frame(word=MATLAB_DF$word, freq=MATLAB_DF$freq, percentage = MATLAB_DF$percentage)
wf4 <- data.frame(word=R_DF$word, freq=R_DF$freq, percentage = R_DF$percentage)
wf <- data.frame(word=df_all$word, freq=df_all$freq, percentage=df_all$percentage)
x1 <- wf1[1:20,]
x2 <- wf2[1:20,]
x3 <- wf3[1:20,]
x4 <- wf4[1:20,]
x <- wf[1:20,]
g1<-ggplot(x1)+
  geom_col(aes(x = reorder(word, -freq), y = percentage)) +
  geom_text(aes(x=word, y = percentage + 0.2, label=freq)) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab("Topics") + ylab("Frequency") +
  scale_y_continuous(labels = function(x)paste0(x, "%"), limits = c(0,6))
g2<-ggplot(x2)+
  geom_col(aes(x = reorder(word, -freq), y = percentage)) +
  geom_text(aes(x=word, y = percentage + 0.2, label=freq)) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab("Topics") + ylab("Frequency") + 
  scale_y_continuous(labels = function(x)paste0(x, "%"), limits = c(0,6))
g3<-ggplot(x3)+
  geom_col(aes(x = reorder(word, -freq), y = percentage)) +
  geom_text(aes(x=word, y = percentage + 0.2, label=freq)) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab("Topics") + ylab("Frequency") + 
  scale_y_continuous(labels = function(x)paste0(x, "%"), limits = c(0,6))
g4<-ggplot(x4)+
  geom_col(aes(x = reorder(word, -freq), y = percentage)) +
  geom_text(aes(x=word, y = percentage + 0.2, label=freq)) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab("Topics") + ylab("Frequency") + 
  scale_y_continuous(labels = function(x)paste0(x, "%"), limits = c(0,6))
gg<-ggplot(x)+
  geom_col(aes(x = reorder(word, -freq), y = percentage)) +
  geom_text(aes(x=word, y = percentage + 0.2, label=freq)) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab("Topics") + ylab("Frequency")+
  scale_y_continuous(labels = function(x)paste0(x, "%"), limits = c(0,6))
figure <- ggarrange(g1,g2,g3,g4,
                    #labels = c("Java", "Python", "Matlab", "R"),
                    ncol = 2, nrow = 2)
figure


t.test(df$Variety~df$LanguageType)
cor.test(df$Variety, df$Active.Contributors)
cor.test(df$Variety, df$Commits.2019)

# correlation results:
library("ggpubr")
ggscatter(df, x = "Active.Contributors", y = "Variety", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Number of active contributors", ylab = "Number of topics (variety)")


split <- split(df, df$Language)
df_JAVA <- split$JAVA;
df_PYTHON <- split$PYTHON;
df_R <- split$R;
df_MATLAB <- split$MATLAB;

# cor.test(df_JAVA$Variety, df_JAVA$Commits.2019)
# cor.test(df_PYTHON$Variety, df_PYTHON$Commits.2019)
# cor.test(df_R$Variety, df_R$Commits.2019)
# cor.test(df_MATLAB$Variety, df_MATLAB$Commits.2019)
# cor.test(df_JAVA$Variety, df_JAVA$Active.Contributors)
# cor.test(df_PYTHON$Variety, df_PYTHON$Active.Contributors)
# cor.test(df_R$Variety, df_R$Active.Contributors)
# cor.test(df_MATLAB$Variety, df_MATLAB$Active.Contributors)
