#################################################
#               Basic Text Analysis             #
#################################################

library("shiny")
library("tidytext")
library("tidyr")
library("dplyr")
library("ggplot2")

tidy.sentiment = function(text) {
  
  # require(tidytext)
  # require(tidyr)
  # require(dplyr)
  
  if (length(text[text !=""]) == 0 ) stop (print("Null Vector :( "))
  
  if (length(text) == 1) {
    textdf = data_frame(text0 = text) %>% 
      unnest_tokens(text, text0, token = "sentences")
  } else {
    textdf = data_frame(text = text)  
  }
  
  # if (lexicon == "nrc") {
  sent.nrc = textdf %>%
    mutate(linenumber = row_number()) %>%
    ungroup() %>%
    unnest_tokens(word, text) %>%
    inner_join(get_sentiments("nrc")) %>%
    count(sentiment, index = linenumber %/% 1, sort = TRUE) %>%
    mutate(method = "nrc")
  # }
  
  # if (lexicon == "bing") {
  sent.bing = textdf %>%
    mutate(linenumber = row_number()) %>%
    ungroup() %>%
    unnest_tokens(word, text) %>%
    inner_join(get_sentiments("bing")) %>%
    count(sentiment, index = linenumber %/% 1, sort = TRUE) %>%
    mutate(method = "bing")
  # }
  
  # if (lexicon == "afinn") {
  sent.afinn = textdf %>%
    mutate(linenumber = row_number()) %>%
    ungroup() %>%
    unnest_tokens(word, text) %>%
    inner_join(get_sentiments("afinn")) %>%
    group_by(index = linenumber %/% 1) %>% 
    summarise(sentiment = sum(score)) %>% 
    mutate(method = "afinn")
  # }
  
  # if (lexicon == "loughran") {
  sent.loughran = textdf %>%
    mutate(linenumber = row_number()) %>%
    ungroup() %>%
    unnest_tokens(word, text) %>%
    inner_join(get_sentiments("loughran")) %>%
    count(sentiment, index = linenumber %/% 1, sort = TRUE) %>%
    mutate(method = "loughran")
  # }
  
  # all = rbind(sent.nrc,sent.bing,sent.loughran)
  
  a = data.frame(sent.nrc %>% spread(sentiment, n, fill = 0))
  b = data.frame(sent.bing %>% spread(sentiment, n, fill = 0))
  c = data.frame(sent.afinn)
  d = data.frame(sent.loughran %>% spread(sentiment, n, fill = 0))
  
  a1 = c(setdiff(setdiff(unique(tidytext::sentiments$sentiment),NA),names(a)),"sentiment")
  a11 = data.frame(matrix(0,nrow(a),length(a1))); colnames(a11) = a1
  a = cbind(a,a11)
  
  b1 = c(setdiff(setdiff(unique(tidytext::sentiments$sentiment),NA),names(b)),"sentiment")
  b11 = data.frame(matrix(0,nrow(b),length(b1))); colnames(b11) = b1
  b = cbind(b,b11)
  
  c1 = c(setdiff(setdiff(unique(tidytext::sentiments$sentiment),NA),names(c)))
  c11 = data.frame(matrix(0,nrow(c),length(c1))); colnames(c11) = c1
  c = cbind(c,c11)
  
  d1 = c(setdiff(setdiff(unique(tidytext::sentiments$sentiment),NA),names(d)),"sentiment")
  d11 = data.frame(matrix(0,nrow(d),length(d1))); colnames(d11) = d1
  d = cbind(d,d11)
  
  all.sentiments = rbind(a,b,c,d)
  
  out = list(sent.nrc = sent.nrc, sent.bing = sent.bing, sent.afinn = sent.afinn, sent.loughran = sent.loughran, all.sentiments = all.sentiments)
  return(out)
}

shinyServer(function(input, output,session) {
  set.seed=2092014   

  dataset <- reactive({
        if (is.null(input$file)) {return(NULL)}
    else {
      return(readLines(input$file$datapath))}
      })
  
  sentiments =  reactive ({
    return(tidy.sentiment(dataset()))
  })

  output$nrc.plot <- renderPlot ({
  
    ggplot(sentiments()$sent.nrc, 
           aes(index, n, fill = sentiment)) +     # index is x col, n is y col. fill=?
      geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +     # stat=?
      facet_wrap(~sentiment, ncol = 2, scales = "free_x")     # so cool.
    })
  
  output$afinn.plot <- renderPlot ({
    ggplot(sentiments()$sent.afinn, 
           aes(index, sentiment)) +     # index is x col, n is y col. fill=?
      geom_bar(alpha = 1, stat = "identity", position = "identity", show.legend = FALSE)      # stat=?
    
  })
  output$bing.plot <- renderPlot ({
    ggplot(sentiments()$sent.bing, 
           aes(index, n, fill = sentiment)) +     # index is x col, n is y col. fill=?
      geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +     # stat=?
      facet_wrap(~sentiment, ncol = 2, scales = "free_x")     # so cool.
    
  })
  output$loughran.plot <- renderPlot ({
    ggplot(sentiments()$sent.loughran, 
           aes(index, n, fill = sentiment)) +     # index is x col, n is y col. fill=?
      geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +     # stat=?
      facet_wrap(~sentiment, ncol = 2, scales = "free_x")     # so cool.
    
  })
  
  output$downloadData1 <- downloadHandler(
    filename = function() { "Nokia_Lumia_reviews.txt" },
    content = function(file) {
      writeLines(readLines("data/Nokia_Lumia_reviews.txt"), file)
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function() { "Sentiments Scores.csv" },
    content = function(file) {
      write.csv(sentiments()$all.sentiments, file, row.names=F)
    })

})
