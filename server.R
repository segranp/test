library(LDAvis) 
library(shiny) 

shinyServer(function(input, output, session) { 
  
  
  library(shiny)
  library(knitr)
  library(tm)
  library(RWeka)
  library(stringr)
  library(LDAvis)
  library(text2vec)
  
  
  stop_words <- stopwords("SMART") #from tm package
  
  
  file <-"fact.txt"
  con <-file(file,open="r")
  lines <- readLines(con)
  
  close(con)
  
  
  
  textdata = gsub("[[:punct:]]", "", lines)
  textdata <- gsub("[[:digit:]]{2} October 2017","",textdata)
  textdata <- gsub("[[:digit:]]{2} September 2017","",textdata)
  textdata = gsub("[[:digit:]]", "", textdata)
  textdata = gsub("http\\w+", "", textdata)
  textdata = gsub("[ \t]{2,}", "", textdata)
  textdata = gsub("^\\s+|\\s+$", "", textdata)
  
  textdata <- gsub("'", "", textdata)  # remove apostrophes
  textdata <- gsub("[[:punct:]]", " ", textdata)  # replace punctuation with space
  textdata <- gsub("[[:cntrl:]]", " ", textdata)  # replace control characters with space
  textdata <- gsub("^[[:space:]]+", "", textdata) # remove whitespace at beginning of documents
  textdata <- gsub("[[:space:]]+$", "", textdata) # remove whitespace at end of documents
  textdata <- tolower(textdata)  # force to lowercase
  
  textdata <-removeWords(textdata,stop_words)
  #textdata <-removeWords(textdata,myStopwords)
  
  #Other words 
  require("tm")
  otherStop <-c("october","september","2017","limited","sunday tribune","year", "english", "percent","document","jse","jselisted","rand","bus","south","south africa","valley", "ms333", "2016", "group", "acting", "shares", "placing","river","stock","exchange","corporate", "words","sold","independent newspapers pty","sundtb")
  textdata <-removeWords(textdata,otherStop)
  
  textdata <- gsub(paste(otherStop, collapse="|"), "", textdata)
  textdata <- gsub("[[:digit:]]{1,6}? words","",textdata)
  
  myCorpus <- VCorpus(VectorSource(textdata))
  
  removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
  
  myCorpus1 <- myCorpus %>%
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removePunctuation) %>%
    #tm_map(removeNumPunct)%>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(stemDocument)%>%
    tm_map(stripWhitespace)
  
  options(mc.cores=1)
  
  tokens = textdata %>%  tolower %>%   word_tokenizer
  it = itoken(tokens, progressbar = FALSE)
  v = create_vocabulary(it,ngram = c(ngram_min = 2L, ngram_max =
                                       2L)) %>% 
    prune_vocabulary(term_count_min = 10, doc_proportion_max = 0.2)
  vectorizer = vocab_vectorizer(v)
  dtm = create_dtm(it, vectorizer, type = "dgTMatrix")
  
  lda_model = LDA$new(n_topics = 10, doc_topic_prior = 0.1, topic_word_prior = 0.01)
  doc_topic_distr = 
    lda_model$fit_transform(x = dtm, n_iter = 1000, 
                            convergence_tol = 0.001, n_check_convergence = 25, 
                            progressbar = FALSE)
  
  #renderPlot({barplot(doc_topic_distr[1, ], xlab = "topic", 
                      #ylab = "proportion", ylim = c(0, 1), 
                      #names.arg = 1:ncol(doc_topic_distr))
  #})
  
  #25 w0rds
  #lda_model$get_top_words(n = 25, topic_number = c(1L, 5L, 10L), lambda = 1)
  
  
  #lda_model$get_top_words(n = 50, topic_number = c(1L, 5L, 10L), lambda = 0.2)
  #unlist(lda_model$get_top_words(n = 10, topic_number = c(1L, 5L, 10L), lambda = 0.6))
  
  #perplexity(dtm, topic_word_distribution = lda_model$topic_word_distribution, doc_topic_distribution = doc_topic_distr)
  
  
output$myChart <- renderVis({ createJSON(plot(lda_model),R = input$nTerms)}) 
}) 
