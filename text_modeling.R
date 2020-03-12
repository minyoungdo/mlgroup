library(tm)
library(grid)
library(wordcloud)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(ggplot2)
library(textdata)
library(sjmisc)

texts <- file.path("C:/Users/Administrator/Documents/ML_Project", "text_mining")
docs <- VCorpus(DirSource(texts))

# remove punctuation + extras that may have been missed
docs <- tm_map(docs, removePunctuation)
for (j in seq(docs)) {
  docs[[j]] <- gsub("/", " ", docs[[j]])
  docs[[j]] <- gsub("'", "", docs[[j]])
  docs[[j]] <- gsub("-", " ", docs[[j]])
  docs[[j]] <- gsub("|", "", docs[[j]])
  docs[[j]] <- gsub("@", "", docs[[j]])
  docs[[j]] <- gsub("\u2028", "", docs[[j]])
  docs[[j]] <- gsub("â\200", "", docs[[j]])
  docs[[j]] <- gsub(""", "", docs[[j]])
  docs[[j]] <- gsub("  ", " ", docs[[j]])
}

# remove numbers
docs <- tm_map(docs, removeNumbers)

# replace uppercase with lowercase
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, PlainTextDocument)

# remove stop words and also "will" since that's pretty common
docs <- tm_map(docs, 
               removeWords, 
               stopwords("english"))
docs <- tm_map(docs, removeWords, c("will"))
docs <- tm_map(docs, PlainTextDocument)

# remove extra whitespace
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument)

set.seed(100)
for (j in seq(docs)) {
wordcloud(docs[j],
          max.words = 150,
          colors = brewer.pal(8, "Dark2"),
          random.order = FALSE, 
          rot.per = 0.30, 
          random.color = TRUE)
}



###### Sentiment Analysis

clist <- vector("list",6)
clist[[1]] <- read_file("C:/Users/Administrator/Documents/ML_Project/text_mining/amy_clean.txt")
clist[[2]] <- read_file("C:/Users/Administrator/Documents/ML_Project/text_mining/bernie_clean.txt")
clist[[3]] <- read_file("C:/Users/Administrator/Documents/ML_Project/text_mining/bloomberg_clean.txt")
clist[[4]] <- read_file("C:/Users/Administrator/Documents/ML_Project/text_mining/pete_clean.txt")
clist[[5]] <- read_file("C:/Users/Administrator/Documents/ML_Project/text_mining/warren_clean.txt")
clist[[6]] <- read_file("C:/Users/Administrator/Documents/ML_Project/text_mining/biden_clean.txt")

clist_df <- vector("list",6)
for (j in seq(docs)) {
  clist_df[[j]] <- tibble(clist[[j]])
}

clist_ut <- vector("list",6)
for (j in seq(docs)) {
  clist_ut[[j]] <- clist_df[[j]] %>%
    unnest_tokens(word,clist[[j]])
}

clist_sentiment_bing <- vector("list",6)
for (j in seq(docs)) {
  clist_sentiment_bing[[j]] <- clist_ut[[j]] %>%
    inner_join(get_sentiments("bing")) %>%
    count(sentiment) %>%
    spread(sentiment, n, fill=0) %>%
    mutate(sentiment = positive - negative)
  clist_sentiment_bing[[j]]
}

clist_sentiment_afinn <- vector("list",6)
clist_sentiment_afinnTotal <- vector("list",6)
for (j in seq(docs)) {
  clist_sentiment_afinn[[j]] <- clist_ut[[j]] %>%
    inner_join(get_sentiments("afinn")) %>%
    count(value) %>%
    spread(value, n, fill=0)
  clist_sentiment_afinnTotal[[j]] <- sum( as.double(names(clist_sentiment_afinn[[j]])) * clist_sentiment_afinn[[j]])
  clist_sentiment_afinnTotal
}





# LDA on k=5
clist_dtm <- vector("list",6)
clist_ntokens <- vector("list",6)
  clist_ldaRaw3  <- vector("list",3)
  clist_ldaRaw5  <- vector("list",6)
  clist_ldaRaw10 <- vector("list",10)
  clist_ldaRaw15 <- vector("list",15)
  clist_ldaRaw20 <- vector("list",20)
  clist_ldaRaw25 <- vector("list",25)
clist_perp3  <- vector("list",6)
clist_perp5  <- vector("list",6)
clist_perp10 <- vector("list",6)
clist_perp15 <- vector("list",6)
clist_perp20 <- vector("list",6)
clist_perp25 <- vector("list",6)
for (j in seq(docs)) {
  clist_dtm[[j]] <- DocumentTermMatrix(docs[j])
  clist_ntokens[[j]] <- clist_dtm[[j]]$ncol
    clist_ldaRaw3[[j]]  <- LDA(clist_dtm[[j]], k=3, method="gibbs")
    clist_ldaRaw5[[j]]  <- LDA(clist_dtm[[j]], k=5, method="gibbs")
    clist_ldaRaw10[[j]] <- LDA(clist_dtm[[j]], k=10, method="gibbs")
    clist_ldaRaw15[[j]] <- LDA(clist_dtm[[j]], k=15, method="gibbs")
    clist_ldaRaw20[[j]] <- LDA(clist_dtm[[j]], k=20, method="gibbs")
    clist_ldaRaw25[[j]] <- LDA(clist_dtm[[j]], k=25, method="gibbs")
  #tidy(clist_ldaRaw5[[j]])
  #terms(clist_ldaRaw5[[j]], 10)
  clist_perp3[[j]]  <- exp(-1 * (clist_ldaRaw3[[j]]@loglikelihood   / clist_ntokens[[j]]) )
  clist_perp5[[j]]  <- exp(-1 * (clist_ldaRaw5[[j]]@loglikelihood   / clist_ntokens[[j]]) )
  clist_perp10[[j]] <- exp(-1 * (clist_ldaRaw10[[j]]@loglikelihood  / clist_ntokens[[j]]) )
  clist_perp15[[j]] <- exp(-1 * (clist_ldaRaw15[[j]]@loglikelihood  / clist_ntokens[[j]]) )
  clist_perp20[[j]] <- exp(-1 * (clist_ldaRaw20[[j]]@loglikelihood  / clist_ntokens[[j]]) )
  clist_perp25[[j]] <- exp(-1 * (clist_ldaRaw25[[j]]@loglikelihood  / clist_ntokens[[j]]) )
}
# same for-loop, without having to re-generate the LDA models
for (j in seq(docs)) {
  tidy(clist_ldaRaw5[[j]])
  terms(clist_ldaRaw5[[j]], 10)
}
# plot perplexities
cvector_perp_amy       <- c(clist_perp3[[1]],clist_perp5[[1]],clist_perp10[[1]],clist_perp15[[1]],clist_perp20[[1]],clist_perp25[[1]])
cvector_perp_bernie    <- c(clist_perp3[[2]],clist_perp5[[2]],clist_perp10[[2]],clist_perp15[[2]],clist_perp20[[2]],clist_perp25[[2]])
cvector_perp_biden <- c(clist_perp3[[3]],clist_perp5[[3]],clist_perp10[[3]],clist_perp15[[3]],clist_perp20[[3]],clist_perp25[[3]])
cvector_perp_bloomberg <- c(clist_perp3[[4]],clist_perp5[[4]],clist_perp10[[4]],clist_perp15[[4]],clist_perp20[[4]],clist_perp25[[4]])
cvector_perp_buttigieg    <- c(clist_perp3[[5]],clist_perp5[[5]],clist_perp10[[5]],clist_perp15[[5]],clist_perp20[[5]],clist_perp25[[5]])
cvector_perp_warren     <- c(clist_perp3[[6]],clist_perp5[[6]],clist_perp10[[6]],clist_perp15[[6]],clist_perp20[[6]],clist_perp25[[6]])
cdataframe_perp <- data.frame(cvector_perp_amy, cvector_perp_bernie, cvector_perp_biden, cvector_perp_bloomberg, cvector_perp_buttigieg, cvector_perp_warren)

# plot perplexity of all candidates together, and see that we run into yscaling issues (i.e. can't make out details)
ggplot(cdataframe_perp, aes(c(3,5,10,15,20,25))) + 
  xlab("number of topics") +
  ylab("perplexity") +
  scale_y_log10() +
  geom_line(aes(y=cvector_perp_amy,      colour="klobuchar")) +
  geom_line(aes(y=cvector_perp_bernie,   colour="sanders")) +
  geom_line(aes(y=cvector_perp_bloomberg,colour="biden")) +
  geom_line(aes(y=cvector_perp_buttigieg,colour="bloomberg")) +
  geom_line(aes(y=cvector_perp_warren,   colour="buttigieg")) +
  geom_line(aes(y=cvector_perp_biden,    colour="warren"))

# plot them individually...  
ggplot(cdataframe_perp, aes(c(3,5,10,15,20,25))) + 
  xlab("number of topics") +
  ylab("perplexity") +
  geom_line(aes(y=cvector_perp_amy,      colour="klobuchar"))
ggplot(cdataframe_perp, aes(c(3,5,10,15,20,25))) + 
  xlab("number of topics") +
  ylab("perplexity") +
  geom_line(aes(y=cvector_perp_bernie,      colour="sanders"))
ggplot(cdataframe_perp, aes(c(3,5,10,15,20,25))) + 
  xlab("number of topics") +
  ylab("perplexity") +
  geom_line(aes(y=cvector_perp_bloomberg,      colour="biden"))
ggplot(cdataframe_perp, aes(c(3,5,10,15,20,25))) + 
  xlab("number of topics") +
  ylab("perplexity") +
  geom_line(aes(y=cvector_perp_buttigieg,      colour="bloomberg"))
ggplot(cdataframe_perp, aes(c(3,5,10,15,20,25))) + 
  xlab("number of topics") +
  ylab("perplexity") +
  geom_line(aes(y=cvector_perp_warren,      colour="buttigieg"))
ggplot(cdataframe_perp, aes(c(3,5,10,15,20,25))) + 
  xlab("number of topics") +
  ylab("perplexity") +
  geom_line(aes(y=cvector_perp_biden,      colour="warren"))
# ...and we see that, roughly, k=15 is the best





clist_lda5 <- vector("list",6)
clist_lda15 <- vector("list",6)
clist_sorted5 <- vector("list",6)
clist_sorted15 <- vector("list",6)
clist_5_t1 <- vector("list",6)
clist_5_t2 <- vector("list",6)
clist_5_t3 <- vector("list",6)
clist_5_t4 <- vector("list",6)
clist_5_t5 <- vector("list",6)
clist_15_t1 <- vector("list",6)
clist_15_t2 <- vector("list",6)
clist_15_t3 <- vector("list",6)
clist_15_t4 <- vector("list",6)
clist_15_t5 <- vector("list",6)
clist_15_t6 <- vector("list",6)
clist_15_t7 <- vector("list",6)
clist_15_t8 <- vector("list",6)
clist_15_t9 <- vector("list",6)
clist_15_t10 <- vector("list",6)
clist_15_t11 <- vector("list",6)
clist_15_t12 <- vector("list",6)
clist_15_t13 <- vector("list",6)
clist_15_t14 <- vector("list",6)
clist_15_t15 <- vector("list",6)
numberTerms <- 10
for (j in seq(docs)) {
  # for (i in 1:15) {
  #   
  # }
  clist_lda5[[j]] <- tidy(clist_ldaRaw5[[j]])
  clist_lda15[[j]] <- tidy(clist_ldaRaw15[[j]])
  clist_sorted5[[j]] <- clist_lda5[[j]][order(-clist_lda5[[j]]$beta),]
  clist_sorted15[[j]] <- clist_lda15[[j]][order(-clist_lda15[[j]]$beta),]
  
  
  clist_5_t1[[j]] <- filter(clist_sorted5[[j]], topic==1)
  clist_5_t2[[j]] <- filter(clist_sorted5[[j]], topic==2)
  clist_5_t3[[j]] <- filter(clist_sorted5[[j]], topic==3)
  clist_5_t4[[j]] <- filter(clist_sorted5[[j]], topic==4)
  clist_5_t5[[j]] <- filter(clist_sorted5[[j]], topic==5)
  
  clist_5_t1[[j]] <- clist_5_t1[[j]][1:numberTerms,]
  clist_5_t2[[j]] <- clist_5_t2[[j]][1:numberTerms,]
  clist_5_t3[[j]] <- clist_5_t3[[j]][1:numberTerms,]
  clist_5_t4[[j]] <- clist_5_t4[[j]][1:numberTerms,]
  clist_5_t5[[j]] <- clist_5_t5[[j]][1:numberTerms,]
  
  
  clist_15_t1[[j]] <- filter(clist_sorted15[[j]], topic==1)
  clist_15_t2[[j]] <- filter(clist_sorted15[[j]], topic==2)
  clist_15_t3[[j]] <- filter(clist_sorted15[[j]], topic==3)
  clist_15_t4[[j]] <- filter(clist_sorted15[[j]], topic==4)
  clist_15_t5[[j]] <- filter(clist_sorted15[[j]], topic==5)
  clist_15_t6[[j]] <- filter(clist_sorted15[[j]], topic==6)
  clist_15_t7[[j]] <- filter(clist_sorted15[[j]], topic==7)
  clist_15_t8[[j]] <- filter(clist_sorted15[[j]], topic==8)
  clist_15_t9[[j]] <- filter(clist_sorted15[[j]], topic==9)
  clist_15_t10[[j]] <- filter(clist_sorted15[[j]], topic==10)
  clist_15_t11[[j]] <- filter(clist_sorted15[[j]], topic==11)
  clist_15_t12[[j]] <- filter(clist_sorted15[[j]], topic==12)
  clist_15_t13[[j]] <- filter(clist_sorted15[[j]], topic==13)
  clist_15_t14[[j]] <- filter(clist_sorted15[[j]], topic==14)
  clist_15_t15[[j]] <- filter(clist_sorted15[[j]], topic==15)
  
  clist_15_t1[[j]] <- clist_15_t1[[j]][1:numberTerms,]
  clist_15_t2[[j]] <- clist_15_t2[[j]][1:numberTerms,]
  clist_15_t3[[j]] <- clist_15_t3[[j]][1:numberTerms,]
  clist_15_t4[[j]] <- clist_15_t4[[j]][1:numberTerms,]
  clist_15_t5[[j]] <- clist_15_t5[[j]][1:numberTerms,]
  clist_15_t6[[j]] <- clist_15_t6[[j]][1:numberTerms,]
  clist_15_t7[[j]] <- clist_15_t7[[j]][1:numberTerms,]
  clist_15_t8[[j]] <- clist_15_t8[[j]][1:numberTerms,]
  clist_15_t9[[j]] <- clist_15_t9[[j]][1:numberTerms,]
  clist_15_t10[[j]] <- clist_15_t10[[j]][1:numberTerms,]
  clist_15_t11[[j]] <- clist_15_t11[[j]][1:numberTerms,]
  clist_15_t12[[j]] <- clist_15_t12[[j]][1:numberTerms,]
  clist_15_t13[[j]] <- clist_15_t13[[j]][1:numberTerms,]
  clist_15_t14[[j]] <- clist_15_t14[[j]][1:numberTerms,]
  clist_15_t15[[j]] <- clist_15_t15[[j]][1:numberTerms,]
}



clist_NAMES <- c("amy", "bernie", "biden", "bloomberg", "buttigieg", "warren")
clist_5_p1 <- vector("list",6)
clist_5_p2 <- vector("list",6)
clist_5_p3 <- vector("list",6)
clist_5_p4 <- vector("list",6)
clist_5_p5 <- vector("list",6)
clist_15_p1 <- vector("list",6)
clist_15_p2 <- vector("list",6)
clist_15_p3 <- vector("list",6)
clist_15_p4 <- vector("list",6)
clist_15_p5 <- vector("list",6)
clist_15_p6 <- vector("list",6)
clist_15_p7 <- vector("list",6)
clist_15_p8 <- vector("list",6)
clist_15_p9 <- vector("list",6)
clist_15_p10 <- vector("list",6)
clist_15_p11 <- vector("list",6)
clist_15_p12 <- vector("list",6)
clist_15_p13 <- vector("list",6)
clist_15_p14 <- vector("list",6)
clist_15_p15 <- vector("list",6)
# plot the results of our LDA, k=5 models for each candidate
for (j in seq(docs)) {
  xtitle <- paste("terms:", clist_NAMES[j])
  ytitle <- paste("beta:" , clist_NAMES[j])
  clist_5_p1[[j]] <- ggplot(clist_5_t1[[j]], aes(x=clist_5_t1[[j]]$term, y=clist_5_t1[[j]]$beta)) + geom_col() + coord_flip() + xlab(xtitle) + ylab(ytitle)
  clist_5_p2[[j]] <- ggplot(clist_5_t2[[j]], aes(x=clist_5_t2[[j]]$term, y=clist_5_t2[[j]]$beta)) + geom_col() + coord_flip() + xlab(xtitle) + ylab(ytitle)
  clist_5_p3[[j]] <- ggplot(clist_5_t3[[j]], aes(x=clist_5_t3[[j]]$term, y=clist_5_t3[[j]]$beta)) + geom_col() + coord_flip() + xlab(xtitle) + ylab(ytitle)
  clist_5_p4[[j]] <- ggplot(clist_5_t4[[j]], aes(x=clist_5_t4[[j]]$term, y=clist_5_t4[[j]]$beta)) + geom_col() + coord_flip() + xlab(xtitle) + ylab(ytitle)
  clist_5_p5[[j]] <- ggplot(clist_5_t5[[j]], aes(x=clist_5_t5[[j]]$term, y=clist_5_t5[[j]]$beta)) + geom_col() + coord_flip() + xlab(xtitle) + ylab(ytitle)
  multiplot(clist_5_p1[[j]], clist_5_p2[[j]], clist_5_p3[[j]], clist_5_p4[[j]], clist_5_p5[[j]], cols=2)
}
# plot the results of our LDA, k=15 models for each candidate
for (j in seq(docs)) {
  xtitle <- paste("terms:", clist_NAMES[j])
  ytitle <- paste("beta:" , clist_NAMES[j])
  clist_15_p1[[j]]  <- ggplot(clist_15_t1[[j]] , aes(x=clist_15_t1[[j]]$term , y=clist_15_t1[[j]]$beta))  + geom_col() + coord_flip() + xlab(xtitle) + ylab(ytitle)
  clist_15_p2[[j]]  <- ggplot(clist_15_t2[[j]] , aes(x=clist_15_t2[[j]]$term , y=clist_15_t2[[j]]$beta))  + geom_col() + coord_flip() + xlab(xtitle) + ylab(ytitle)
  clist_15_p3[[j]]  <- ggplot(clist_15_t3[[j]] , aes(x=clist_15_t3[[j]]$term , y=clist_15_t3[[j]]$beta))  + geom_col() + coord_flip() + xlab(xtitle) + ylab(ytitle)
  clist_15_p4[[j]]  <- ggplot(clist_15_t4[[j]] , aes(x=clist_15_t4[[j]]$term , y=clist_15_t4[[j]]$beta))  + geom_col() + coord_flip() + xlab(xtitle) + ylab(ytitle)
  clist_15_p5[[j]]  <- ggplot(clist_15_t5[[j]] , aes(x=clist_15_t5[[j]]$term , y=clist_15_t5[[j]]$beta))  + geom_col() + coord_flip() + xlab(xtitle) + ylab(ytitle)
  clist_15_p6[[j]]  <- ggplot(clist_15_t6[[j]] , aes(x=clist_15_t6[[j]]$term , y=clist_15_t6[[j]]$beta))  + geom_col() + coord_flip() + xlab(xtitle) + ylab(ytitle)
  clist_15_p7[[j]]  <- ggplot(clist_15_t7[[j]] , aes(x=clist_15_t7[[j]]$term , y=clist_15_t7[[j]]$beta))  + geom_col() + coord_flip() + xlab(xtitle) + ylab(ytitle)
  clist_15_p8[[j]]  <- ggplot(clist_15_t8[[j]] , aes(x=clist_15_t8[[j]]$term , y=clist_15_t8[[j]]$beta))  + geom_col() + coord_flip() + xlab(xtitle) + ylab(ytitle)
  clist_15_p9[[j]]  <- ggplot(clist_15_t9[[j]] , aes(x=clist_15_t9[[j]]$term , y=clist_15_t9[[j]]$beta))  + geom_col() + coord_flip() + xlab(xtitle) + ylab(ytitle)
  clist_15_p10[[j]] <- ggplot(clist_15_t10[[j]], aes(x=clist_15_t10[[j]]$term, y=clist_15_t10[[j]]$beta)) + geom_col() + coord_flip() + xlab(xtitle) + ylab(ytitle)
  clist_15_p11[[j]] <- ggplot(clist_15_t11[[j]], aes(x=clist_15_t11[[j]]$term, y=clist_15_t11[[j]]$beta)) + geom_col() + coord_flip() + xlab(xtitle) + ylab(ytitle)
  clist_15_p12[[j]] <- ggplot(clist_15_t12[[j]], aes(x=clist_15_t12[[j]]$term, y=clist_15_t12[[j]]$beta)) + geom_col() + coord_flip() + xlab(xtitle) + ylab(ytitle)
  clist_15_p13[[j]] <- ggplot(clist_15_t13[[j]], aes(x=clist_15_t13[[j]]$term, y=clist_15_t13[[j]]$beta)) + geom_col() + coord_flip() + xlab(xtitle) + ylab(ytitle)
  clist_15_p14[[j]] <- ggplot(clist_15_t14[[j]], aes(x=clist_15_t14[[j]]$term, y=clist_15_t14[[j]]$beta)) + geom_col() + coord_flip() + xlab(xtitle) + ylab(ytitle)
  clist_15_p15[[j]] <- ggplot(clist_15_t15[[j]], aes(x=clist_15_t15[[j]]$term, y=clist_15_t15[[j]]$beta)) + geom_col() + coord_flip() + xlab(xtitle) + ylab(ytitle)
  multiplot(clist_15_p1[[j]], clist_15_p2[[j]], clist_15_p3[[j]], clist_15_p4[[j]], clist_15_p5[[j]],
            clist_15_p6[[j]], clist_15_p7[[j]], clist_15_p8[[j]], clist_15_p9[[j]], clist_15_p10[[j]],
            clist_15_p11[[j]], clist_15_p12[[j]], clist_15_p13[[j]], clist_15_p14[[j]], clist_15_p15[[j]],
            cols=3)
}

# if multiplot fails, define the custom function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



