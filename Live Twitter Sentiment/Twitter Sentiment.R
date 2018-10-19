#Installing required packages

list.of.packages <- c('streamR','ROAuth','RColorBrewer','tm','wordcloud','base64enc','plyr','stringr',
                      'twitteR','httr','SnowballC','syuzhet','devtools','rtweet','httpuv','packcircles',
                      'textclean','dplyr','tidytext','qdapRegex')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,'Package'])]

library('pacman')
pacman::p_load(streamR,ROAuth,RColorBrewer,tm,wordcloud,base64enc,plyr,dplyr,stringr,twitteR,httr,SnowballC
               ,syuzhet,devtools,rtweet,httpuv,packcircles,ggplot2,textclean,tidytext,qdapRegex)

#Downloading curl certification

#download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

## authenticate via access token
token <- create_token(
  app = "my_twitter_research_app",
  consumer_key = "4DpqmE0sPa6jEfRyN6XYmFI5X",
  consumer_secret = "722i8dcrThcBk7ULXsTtZiNq8c1Bm3l9LQDMNMuroVYcEeKomc",
  access_token = "1451694792-1hxdbNgB1ZgRLJ8txn5X2mDzhPhIL81rIYiBZMj",
  access_secret = "LeB7vkMO6K2sWj9n3TvQDHhoRrLi9y8xPntWhctmYZGYJ")

search_word <- "#AyodhyaVerdict"

## Stream time in seconds so for one minute set timeout = 60
## For larger chunks of time, I recommend multiplying 60 by the number
## of desired minutes. This method scales up to hours as well
## (x * 60 = x mins, x * 60 * 60 = x hours)
## Stream for 30 minutes
## streamtime <- 30 * 60 

streamtime <- 60 * 2

start <- Sys.time()
end <- start + (60*2)
filename <- "tweets.json"

#Creating dataframes

full_tweets_main <- c()
tweets_main <- c()
text_corpus_main <- Corpus(VectorSource(tweets_main))
text_mat_main <- TermDocumentMatrix(text_corpus_main)
text_mat_main1 <- as.matrix(text_mat_main)
text_mat_main_df <- sort(rowSums(text_mat_main1),decreasing=TRUE)
emotion.df<-data.frame()
emotion.df2.main <- data.frame()
sent.value.main <- c()
category_senti_main <- c()
category_senti2_main <- data.frame()


current <- Sys.time()
while(current < end){
  
  rt <- stream_tweets(q = search_word, timeout = streamtime, file_name = filename,language = "en")
  #t<-stream_tweets("#TuesdayThoughts", timeout = 90)
  #t <- get_timelines(c("iamsrk"), n = 1000)
  #rt<-t
  
  if(!is.null(rt))
  {
    
    ########################################### Data Cleaning ########################################################
    
    stopwords <- c('i', 'me', 'my', 'myself', 'we', 'our', 'ours', 'ourselves', 'you','your', 'yours', 'yourself', 'yourselves', 'he', 'him', 'his', 'himself', 'she', "she's", 'her', 'hers', 'herself', 'it', 'its', 'itself', 'they', 'them', 'their', 'theirs', 'themselves', 'what', 'which', 'who', 'whom', 'this', 'that', 'these', 'those', 'am', 'is', 'are', 'was', 'were', 'be', 'been', 'being', 'have', 'has', 'had', 'having', 'do', 'does', 'did', 'doing', 'a', 'an', 'the', 'and', 'but', 'if', 'or', 'because', 'as', 'until', 'while', 'of', 'at', 'by', 'for', 'with', 'about', 'between', 'into', 'through', 'during', 'before', 'after', 'above', 'below', 'to', 'from', 'up', 'down', 'in', 'out', 'on', 'off', 'over', 'under', 'again', 'further', 'then', 'once', 'here', 'there', 'when', 'where', 'why', 'how', 'all', 'any', 'both', 'each', 'few', 'more', 'most', 'other', 'some', 'such', 'only', 'own', 'same', 'so', 'than', 'too', 'very', 's', 't', 'can', 'will', 'just', 'don','should','now', 'd', 'll', 'm', 'o', 're', 've', 'y', 'ain', 'aren', 'couldn', 'didn',  'doesn', 'hadn', 'hasn','haven','isn', 'ma', 'mightn', 'mustn', 'needn', 'shan', 'shouldn',  'wasn',  'weren', 'won', 'wouldn')
    
    full_tweets<-rt$text
    
    #Removing contractions
    
    tweets <- replace_contraction(full_tweets)
    
    #Removing hyperlinks
    tweets <- rm_url(tweets)
    
    #Removing punctuations
    tweets <- gsub("[[:punct:]]", " ", tweets)
    
    #Removing Non-ASCII characters
    tweets <- rm_non_ascii(tweets)
    
    #Removing stopwords
    tweets <- removeWords(tweets,stopwords)
    
    #Removing non wors
    tweets <- rm_non_words(tweets)
    
    #Changing all cases to lower
    tweets <- tolower(tweets)
    
    temp_vec <- c(text_corpus_main,text_corpus,recursive = TRUE)
    text_corpus_main <- Corpus(VectorSource(temp_vec))
    
    #Creating Document Matrix
    
    text_mat <- TermDocumentMatrix(text_corpus)
    
    text_mat_main <- c(text_mat_main,text_mat)
    
    text_mat_main1 <- as.matrix(text_mat_main)
    text_mat_main1 <- sort(rowSums(text_mat_main1),decreasing=TRUE)
    
    #Converting words to dataframe in corpus
    
    text_mat_main_df <- data.frame(word = names(text_mat_main1),freq=text_mat_main1)
    
    ################################################ Analysing sentiments #########################################################
    
    tweets<-text
    
    rt$text<-text
    
    review_words <- rt %>%
      select(status_id,text) %>%
      unnest_tokens(word, text) 
    # %>%
    #   filter(!word %in% stop_words$word,
    #          str_detect(word, "^[a-z']+$"))
    
    AFINN <- sentiments %>%
      filter(lexicon == "AFINN") %>%
      select(word, afinn_score = score)
    
    
    reviews_sentiment <- review_words %>%
      inner_join(AFINN, by = "word") %>%
      group_by(status_id) %>%
      summarize(sentiment = mean(afinn_score)) %>%
      ungroup()
    
    rt2<-inner_join(reviews_sentiment,rt)
    
    word.df <- as.vector(rt$text)
    #emotion.df <- get_nrc_sentiment(word.df)
    emotion.df <- get_nrc_sentiment(word.df)
    status_id<-rt$status_id
    emotion<-cbind(status_id,emotion.df)
    
    rt3<-inner_join(rt2,emotion)
    rt<-rt3
    
    emotion.df2 <- cbind(tweets, emotion.df)
    emotion.df2.main <- rbind( emotion.df2.main, emotion.df2)
    sent.value <- get_sentiment(word.df)
    sent.value.main <- c(sent.value.main,sent.value)
    
    #sent.value.main <- sentiment(get_sentences(word.df))$sentiment
    
    category_senti <- ifelse(rt$sentiment < -1.5, "Negative", ifelse(rt$sentiment > 1.5, "Positive", "Neutral"))
    category_senti2 <- cbind(rt$text,category_senti)
    
    category_senti_main <- c(category_senti_main,category_senti)
    category_senti2_main <- rbind(category_senti2_main,category_senti2)
    
    Freq=table(category_senti_main)
    
    #############################  Creating bar plot of sentiment tweets ##########################
    
    barplot(Freq,
            main="Tweets", 
            xlab="Sentiments",
            ylab="Frequency",
            col = rainbow(5)
    )
    
    ###################################### Plotting top words ############################################
    
    barplot(text_mat_main_df[1:10,]$freq, las = 2, names.arg = text_mat_main_df[1:10,]$word,
            
            col ="orange", main ="Most frequent words",
            
            ylab = "Word frequencies")
    
    ###################################### Creating word bubbles #########################################
    
    # Create data
    data=data.frame(group=paste(text_mat_main_df[1:15,]$word), value=sample(seq(1,100),15)) 
    
    # Generate the layout. This function return a dataframe with one line per bubble. 
    # It gives its center (x and y) and its radius, proportional of the value
    packing <- circleProgressiveLayout(data$value, sizetype='area')
    
    # We can add these packing information to the initial data frame
    data = cbind(data, packing)
    
    # Check that radius is proportional to value. We don't want a linear relationship, since it is the 
    #AREA that must be proportionnal to the value
    #plot(data$radius, data$value)
    
    # The next step is to go from one center + a radius to the coordinates of a circle that
    # is drawn by a multitude of straight lines.
    dat.gg <- circleLayoutVertices(packing, npoints=250)
    
    # Make the plot
    ggplot() + 
      
      # Make the bubbles
      geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.2) +
      
      # Add text in the center of each bubble + control its size
      geom_text(data = data, aes(x, y, size=value, label = group)) +
      scale_size_continuous(range = c(1,5)) +
      
      # General theme:
      theme_void() + 
      theme(legend.position="none") +
      coord_equal()
    
    
    ############################################## Creating Word Cloud #################################################
    
    set.seed(123)
    wordcloud(text_corpus_main,
              min.freq=1,
              max.words=80,
              scale=c(2.2,1), 
              colors=brewer.pal(8, "Dark2"),
              random.color=TRUE,
              random.order=FALSE
    )
    
    ############################################# Gettion location of tweets ###########################################
    
    rt_loc<-rt
    
    location<-tolower(rt$location)
    location<-gsub("?(f|ht)tp(s?)://(.*)[.][a-z]+", "", location)
    location<-gsub("[[:punct:]]", " ", location)
    location <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", location)
    location <- str_replace_all(location,"[^a-zA-Z\\s]", " ")
    location <- str_replace_all(location,"[\\s]+", " ")
    location<-replace_non_ascii(location)
    rt_loc$location<-location
    
    p<-rt_loc %>%
      count(location, sort = TRUE) %>%
      mutate(location = reorder(location, n)) %>%
      na.omit() %>%
      top_n(20) %>%
      ggplot(aes(x = location, y = n)) +
      geom_col() +
      coord_flip() +
      labs(x = "Count",
           y = "Location",
           title = "Where Twitter users are from - unique locations ")
    print(p)
    
    ############################################ Plotting line chart ##################################################
    
    def<-cbind(rt,category_senti2)
    chart<-def %>%
      #dplyr::filter(created_at > "2017-10-29") %>%
      dplyr::group_by(category_senti) %>%
      ts_plot("secs", trim = 1L) +
      ggplot2::geom_point() +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.title = ggplot2::element_blank(),
        legend.position = "bottom",
        plot.title = ggplot2::element_text(face = "bold")) +
      ggplot2::labs(
        x = NULL, y = NULL,
        title = "Frequency of Twitter statuses posted by users"
        #  subtitle = "Twitter status (tweet) counts aggregated by day from October/November 2017",
        #  caption = "\nSource: Data collected from Twitter's REST API via rtweet"
      )
    print(chart)
    
    Sys.sleep(20)
    
    current <- Sys.time()
  }
  
}


