# Taylor Swearingen
# Text Anlytics @ Hult
# Assignment A2


# Set WD
setwd("C:/Users/Taylor's Laptop/Desktop/Text Analytics/Hult_NLP_student_intensive/assignments/A2 NLP Visualizations EDA")


# Load Libraries
library(tm)
#library(qdap)
library(ggplot2)
library(ggthemes)
library(ggdendro)
library(RCurl)
library(plotrix)
library(wordcloud)
library(RColorBrewer)
library(lexicon)
library(tidytext)
library(dplyr)
library(echarts4r)
library(tidyr)
library(corpus)

# Options & Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  y <- NA
  tryError <- tryCatch(tolower(x), error = function(e) e)
  if (!inherits(tryError, 'error'))
    y <- tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  #corpus <- tm_map(corpus, content_transformer(replace_contraction)) 
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Create custom stop words
stops <- c(stopwords('SMART'))

# Bigram token maker
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}

# read in data
  # reps
  textdems  <- read.csv('dems.csv')
  txtCorpusdems <- VCorpus(VectorSource(textdems$text))
  txtCorpusdems <- cleanCorpus(txtCorpusdems, stops)
  tweetTDMdems  <- TermDocumentMatrix(txtCorpusdems)
  # dems
  textreps  <- read.csv('reps.csv')
  txtCorpusreps <- VCorpus(VectorSource(textreps$text))
  txtCorpusreps <- cleanCorpus(txtCorpusreps, stops)
  tweetTDMreps  <- TermDocumentMatrix(txtCorpusreps)
  # both
  # Read in multiple files as individuals
  dems.reps.files <- list.files(pattern = 'dems|reps', full.names = T, ignore.case = T)
  # Bring them in 1 at a time instead of in a loop
  dems.reps.list <- lapply(dems.reps.files, read.csv)

# reduce and organize both  
  # Reduce TDM
  reducedTDMdems <- removeSparseTerms(tweetTDMdems, sparse=0.96) #shoot for ~50 terms; 1.5% of cells in row have a value  
  reducedTDMdems
  # Reduce TDM
  reducedTDMreps <- removeSparseTerms(tweetTDMreps, sparse=0.95) #shoot for ~50 terms; 1.5% of cells in row have a value  
  reducedTDMreps
  # Organize the smaller TDM
  reducedTDMdems <- as.data.frame(as.matrix(reducedTDMdems))
  # Organize the smaller TDM
  reducedTDMreps <- as.data.frame(as.matrix(reducedTDMreps))

  
### Dendrogram - work and plot
  # dems
  # Basic Hierarchical Clustering
  hcdems <- hclust(dist(reducedTDMdems))
  ggdendrogram(hcdems, rotate=FALSE) 
  # end for dem.csv file
  
  #reps

  # Basic Hierarchical Clustering
  hcreps <- hclust(dist(reducedTDMreps))
  ggdendrogram(hcreps, rotate=FALSE)
  
  # end of dendrogram
  
### Pyramid Plot - work and plot
  #  Apply steps to each list element; Small changes since we need a TDM then simple matrix
  for(i in 1:length(dems.reps.list)){
    print(paste('working on',i, 'of', length(dems.reps.list)))
    tmp <- paste(dems.reps.list[[i]]$text, collapse = ' ')
    tmp <- VCorpus(VectorSource(tmp))
    tmp <- cleanCorpus(tmp, stops)
    tmp <- TermDocumentMatrix(tmp)
    dems.reps.list[[i]] <- as.matrix(tmp)
  }
  
  
  # FYI
  lapply(dems.reps.list, dim)
  
  
  # Merge based on the row attributes (terms)
  df <- merge(dems.reps.list[[1]], dems.reps.list[[2]], by ='row.names')
  
  # Programmatically assign names
  txtNames  <- sapply(strsplit(dems.reps.files, '/'), tail, 1)
  names(df) <- c('terms', txtNames)
  
  # Examine
  df[6:10,]
  
  # Calculate the absolute differences among in common terms
  df$diff <- abs(df[,2] - df[,3])
  
  # Organize df for plotting
  df<- df[order(df$diff, decreasing=TRUE), ]
  top35 <- df[1:35, ]
  
  # Pyarmid Plot
  pyramid.plot(lx         = top35[,2], #left
               rx         = top35[,3],  #right
               labels     = top35[,1],  #terms
               top.labels = c( names(top35)[2], names(top35)[1],  names(top35)[3]), #corpora
               gap        = 200, # space for terms to be read
               main       = 'Words in Common', # title
               unit       = 'wordFreq') 
  
  # End
  
  
### Word Cloud
  
  # As of tm version 0.7-3 tabular was deprecated
  names(textreps)[1] <-'doc_id' 
  names(textdems)[1] <-'doc_id'
  
  # Make a volatile corpus
  txtCorpusreps <- VCorpus(DataframeSource(textreps))
  txtCorpusdems <- VCorpus(DataframeSource(textreps))
  
  # Preprocess the corpus
  txtCorpusreps <- cleanCorpus(txtCorpusreps, stops)
  txtCorpusdems <- cleanCorpus(txtCorpusdems, stops)
  
  # Make bi-gram TDM according to the tokenize control & convert it to matrix
  repsTDM  <- TermDocumentMatrix(txtCorpusreps, 
                                 control=list(tokenize=bigramTokens))
  repsTDMm <- as.matrix(repsTDM)
  demsTDM  <- TermDocumentMatrix(txtCorpusdems, 
                                 control=list(tokenize=bigramTokens))
  demsTDMm <- as.matrix(demsTDM)
  
  # Get Row Sums & organize
  repsTDMv <- sort(rowSums(repsTDMm), decreasing = TRUE)
  repsDF   <- data.frame(word      = names(repsTDMv), 
                         freq      = repsTDMv,
                         row.names = NULL)
  demsTDMv <- sort(rowSums(demsTDMm), decreasing = TRUE)
  demsDF   <- data.frame(word      = names(demsTDMv), 
                         freq      = demsTDMv,
                         row.names = NULL)
  
  # Choose a color & drop light ones
  palreps <- brewer.pal(8, "YlOrRd")
  palreps <- palreps[-(1:2)]
  paldems <- brewer.pal(8, "Blues")
  paldems <- paldems[-(1:2)]
  
  # Make simple word cloud
  # Reminder to expand device pane
  set.seed(1234)
  wordcloud(repsDF$word,
            repsDF$freq,
            max.words    = 50,
            random.order = FALSE,
            colors       = palreps,
            scale        = c(2,1))
  wordcloud(demsDF$word,
            demsDF$freq,
            max.words    = 50,
            random.order = FALSE,
            colors       = paldems,
            scale        = c(2,1))
  
  # End

### Sentiment Analysis
  
  # Ignoring authorship/news political leanings, overall let's examine the emotional words used in these articles
  txtrepsDTM <- VCorpus(VectorSource(textreps$text))
  txtrepsDTM <- cleanCorpus(txtrepsDTM, stops)
  txtrepsDTM <- DocumentTermMatrix(txtrepsDTM)
  
  txtdemsDTM <- VCorpus(VectorSource(textdems$text))
  txtdemsDTM <- cleanCorpus(txtdemsDTM, stops)
  txtdemsDTM <- DocumentTermMatrix(txtdemsDTM)
  
  # Examine 
  as.matrix(txtrepsDTM[1:5,100:105])
  dim(txtrepsDTM)
  
  as.matrix(txtdemsDTM[1:5,100:105])
  dim(txtdemsDTM)
  
  # Examine Tidy & Compare
  tidyreps <- tidy(txtrepsDTM)
  tidyreps[100:105,]
  dim(tidyreps)
  
  tidydems <- tidy(txtdemsDTM)
  tidydems[100:105,]
  dim(tidydems)
  
  # Get bing lexicon
  # "afinn", "bing", "nrc", "loughran"
  bing <- get_sentiments(lexicon = c("bing"))
  head(bing)
  
  # Perform Inner Join
  bingrepsSent <- inner_join(tidyreps, bing, by=c('term' = 'word'))
  bingrepsSent
  
  bingdemsSent <- inner_join(tidydems, bing, by=c('term' = 'word'))
  bingdemsSent
  
  # Quick Analysis
  table(bingrepsSent$sentiment) #tally ignoring count
  table(bingrepsSent$sentiment, bingrepsSent$count) #only a few with more than 1 term
  aggregate(count~sentiment,bingrepsSent, sum) #correct way to sum them
  
  table(bingdemsSent$sentiment) #tally ignoring count
  table(bingdemsSent$sentiment, bingdemsSent$count) #only a few with more than 1 term
  aggregate(count~sentiment,bingdemsSent, sum) #correct way to sum them
  
  # Get nrc lexicon; deprecated in tidytext, use library(lexicon)
  #nrc <- read.csv('nrcSentimentLexicon.csv')
  nrc <- nrc_emotions
  head(nrc)
  
  # Tidy this up
  nrc <- nrc %>% pivot_longer(-term, names_to = "emotion", values_to = "freq")
  nrc <-subset(nrc, nrc$freq>0 )
  head(nrc)
  nrc$freq <- NULL #no longer needed
  
  # Perform Inner Join
  nrcSentreps <- inner_join(tidyreps,nrc, by=c('term' = 'term'))
  nrcSentreps
  nrcSentdems <- inner_join(tidydems,nrc, by=c('term' = 'term'))
  nrcSentdems
  
  # Radar chart
  table(nrcSentreps$emotion)
  emosreps <- data.frame(table(nrcSentreps$emotion))
  names(emosreps) <- c('emotion', 'termsCt')
  emosreps %>% 
    e_charts(emotion) %>% 
    e_radar(termsCt, max = max(emosreps$termsCt), name = "Reps.csv") %>%
    e_tooltip(trigger = "item") %>% e_theme("dark-mushroom")
  
  table(nrcSentdems$emotion)
  emosdems <- data.frame(table(nrcSentdems$emotion))
  names(emosdems) <- c('emotion', 'termsCt')
  emosdems %>% 
    e_charts(emotion) %>% 
    e_radar(termsCt, max = max(emosdems$termsCt), name = "Dems.csv") %>%
    e_tooltip(trigger = "item") %>% e_theme("dark-mushroom")
  
  # end

### Frequency Associations
  
  # organize further
  tweetTDMm.reps <- as.matrix(tweetTDMreps)
  tweetTDMm.dems <- as.matrix(tweetTDMdems)
  
  # Frequency Data Frame
  tweetSums.reps <- rowSums(tweetTDMm.reps)
  tweetFreq.reps <- data.frame(word=names(tweetSums.reps),frequency=tweetSums.reps)
  
  tweetSums.dems <- rowSums(tweetTDMm.dems)
  tweetFreq.dems <- data.frame(word=names(tweetSums.dems),frequency=tweetSums.dems)
  
  # Review a section
  tweetFreq.reps[50:55,]
  tweetFreq.dems[50:55,]
  
  # Remove the row attributes meta family
  rownames(tweetFreq.reps) <- NULL
  tweetFreq.reps[50:55,]
  
  rownames(tweetFreq.dems) <- NULL
  tweetFreq.dems[50:55,]
  
  
  # Simple barplot; values greater than 65
  topWords.reps      <- subset(tweetFreq.reps, tweetFreq.reps$frequency >= 65) 
  topWords.reps      <- topWords.reps[order(topWords.reps$frequency, decreasing=F),]
  
  topWords.dems      <- subset(tweetFreq.dems, tweetFreq.dems$frequency >= 65) 
  topWords.dems      <- topWords.dems[order(topWords.dems$frequency, decreasing=F),]
  
  # Chg to factor for ggplot
  topWords.reps$word <- factor(topWords.reps$word, 
                               levels=unique(as.character(topWords.reps$word))) 
  
  topWords.dems$word <- factor(topWords.dems$word, 
                               levels=unique(as.character(topWords.dems$word))) 
  
  ggplot(topWords.reps, aes(x=word, y=frequency)) + 
    geom_bar(stat="identity", fill='darkred') + 
    coord_flip()+ theme_gdocs() +
    geom_text(aes(label=frequency), colour="white",hjust=1.25, size=1.0)
  
  ggplot(topWords.dems, aes(x=word, y=frequency)) + 
    geom_bar(stat="identity", fill='darkred') + 
    coord_flip()+ theme_gdocs() +
    geom_text(aes(label=frequency), colour="white",hjust=1.25, size=1.0)
  
  # Inspect word associations
  associations.reps <- findAssocs(tweetTDMreps, 'opposed', 0.30)
  associations.reps
  
  associations.dems <- findAssocs(tweetTDMdems, 'opposed', 0.30)
  associations.dems
  
  # Organize the word associations
  assocDF.reps <- data.frame(terms     = names(associations.reps[[1]]),
                             value    = unlist(associations.reps), 
                             row.names = NULL)
  assocDF.reps$terms <- factor(assocDF.reps$terms, levels=assocDF.reps$terms)
  assocDF.reps
  
  assocDF.dems <- data.frame(terms     = names(associations.dems[[1]]),
                             value    = unlist(associations.dems), 
                             row.names = NULL)
  assocDF.dems$terms <- factor(assocDF.dems$terms, levels=assocDF.dems$terms)
  assocDF.dems
  
  # Make a dot plot
  ggplot(assocDF.reps, aes(y=terms)) +
    geom_point(aes(x=value), data=assocDF.reps, col='#c00c00') +
    theme_gdocs() + 
    geom_text(aes(x=value,label=value), 
              colour="red",hjust="inward", vjust ="inward" , size=3) 
  
  ggplot(assocDF.dems, aes(y=terms)) +
    geom_point(aes(x=value), data=assocDF.dems, col='blue') +
    theme_gdocs() + 
    geom_text(aes(x=value,label=value), 
              colour="blue",hjust="inward", vjust ="inward" , size=3)
  # End
  

  




