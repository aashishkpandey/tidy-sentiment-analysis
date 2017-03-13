#################################################
#tidy sentiments                               #
#################################################

library("shiny")
library("tidytext")
library("tidyr")
library("dplyr")
library("ggplot2")
library("DT")

shinyServer(function(input, output,session) {
  set.seed=2092014   

  dataset <- reactive({
        if (is.null(input$file)) {return(NULL)}
    else {
      text  = readLines(input$file$datapath)
      
      if (length(text[text !=""]) == 0 ) stop (print("Null Vector :( "))
      
      if (length(text) == 1) {
        textdf = data_frame(text0 = text) %>% 
          unnest_tokens(text, text0, token = "sentences")
      } else {
        textdf = data_frame(text = text)  
      }
      
    }
    return(textdf)
      })
  
  
  sentiments =  reactive ({
    # return(tidy.sentiment(dataset(), lexicon = input$lexicon))
    textdf = dataset()
    
    if (input$lexicon == "nrc") {
      sent = textdf %>%
        mutate(linenumber = row_number()) %>%
        ungroup() %>%
        unnest_tokens(word, text) %>%
        inner_join(get_sentiments("nrc")) %>%
        count(sentiment, index = linenumber %/% 1, sort = TRUE) %>%
        mutate(method = "nrc")
    }
    
    if (input$lexicon == "bing") {
      sent = textdf %>%
        mutate(linenumber = row_number()) %>%
        ungroup() %>%
        unnest_tokens(word, text) %>%
        inner_join(get_sentiments("bing")) %>%
        count(sentiment, index = linenumber %/% 1, sort = TRUE) %>%
        mutate(method = "bing")
    }
    
    if (input$lexicon == "afinn") {
      sent = textdf %>%
        mutate(linenumber = row_number()) %>%
        ungroup() %>%
        unnest_tokens(word, text) %>%
        inner_join(get_sentiments("afinn")) %>%
        group_by(index = linenumber %/% 1) %>% 
        summarise(sentiment = sum(score)) %>% 
        mutate(method = "afinn")
    }
    
    if (input$lexicon == "loughran") {
      sent = textdf %>%
        mutate(linenumber = row_number()) %>%
        ungroup() %>%
        unnest_tokens(word, text) %>%
        inner_join(get_sentiments("loughran")) %>%
        count(sentiment, index = linenumber %/% 1, sort = TRUE) %>%
        mutate(method = "loughran")
    }
    
    return(sent)
    
  })

  output$sent.plot <- renderPlot ({
  
    if (input$lexicon != "afinn") {
      ggplot(sentiments(), 
             aes(index, n, fill = sentiment)) +     # index is x col, n is y col. fill=?
        geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +     # stat=?
        facet_wrap(~sentiment, ncol = 2, scales = "free_x")     # so cool.    
    }   else  {
      ggplot(sentiments(), 
             aes(index, sentiment)) +     # index is x col, n is y col. fill=?
        geom_bar(alpha = 1, stat = "identity", position = "identity", show.legend = FALSE)      # stat=?  
    }

    })


  t1 = reactive({
    if (is.null(input$file)) {return(NULL)}
    else {
      
      tb = sentiments()
      y1 = data.frame(dataset() , index= 1:nrow(dataset()))
      
      test = merge(tb,y1 ,by.x ="index", by.y= "index", all.y=T)
      return(test)
    }
    
  })
  
  output$table <- renderDataTable({
    datatable(t1(), filter = 'top')
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
  
  
  #----------------------------------------------------#
  
  sentiments.index =  reactive ({
    # return(tidy.sentiment(dataset(), lexicon = input$lexicon))
    textdf = dataset()[input$index,] %>% unnest_tokens(text, text, token = "sentences")
  
    if (input$lexicon == "nrc") {
      sent = textdf %>%
        mutate(linenumber = row_number()) %>%
        ungroup() %>%
        unnest_tokens(word, text) %>%
        inner_join(get_sentiments("nrc")) %>%
        count(sentiment, index = linenumber %/% 1, sort = TRUE) %>%
        mutate(method = "nrc")
    }
    
    if (input$lexicon == "bing") {
      sent = textdf %>%
        mutate(linenumber = row_number()) %>%
        ungroup() %>%
        unnest_tokens(word, text) %>%
        inner_join(get_sentiments("bing")) %>%
        count(sentiment, index = linenumber %/% 1, sort = TRUE) %>%
        mutate(method = "bing")
    }
    
    if (input$lexicon == "afinn") {
      sent = textdf %>%
        mutate(linenumber = row_number()) %>%
        ungroup() %>%
        unnest_tokens(word, text) %>%
        inner_join(get_sentiments("afinn")) %>%
        group_by(index = linenumber %/% 1) %>% 
        summarise(sentiment = sum(score)) %>% 
        mutate(method = "afinn")
    }
    
    if (input$lexicon == "loughran") {
      sent = textdf %>%
        mutate(linenumber = row_number()) %>%
        ungroup() %>%
        unnest_tokens(word, text) %>%
        inner_join(get_sentiments("loughran")) %>%
        count(sentiment, index = linenumber %/% 1, sort = TRUE) %>%
        mutate(method = "loughran")
    }
    
    return(sent)
    
  })
  
  
  output$sent.plot.index <- renderPlot ({
    
    if (input$lexicon != "afinn") {
      ggplot(sentiments.index(), 
             aes(index, n, fill = sentiment)) +     # index is x col, n is y col. fill=?
        geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +     # stat=?
        facet_wrap(~sentiment, ncol = 2, scales = "free_x")     # so cool.    
    }   else  {
      ggplot(sentiments.index(), 
             aes(index, sentiment)) +     # index is x col, n is y col. fill=?
        geom_bar(alpha = 1, stat = "identity", position = "identity", show.legend = FALSE)      # stat=?  
    }
    
  })
  
  
  t2 = reactive({
    if (is.null(input$file)) {return(NULL)}
    else {
      
      tb = sentiments.index()
      tx = dataset()[input$index,] %>% unnest_tokens(text, text, token = "sentences")
      
      y1 = data.frame(tx, index= 1:nrow(tx))
      
      test = merge(tb,y1 ,by.x ="index", by.y= "index", all.y=T)
      return(test)
    }
    
  })
  
  output$table2 <- renderDataTable({
    datatable(t2(), filter = 'top')
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
  
  
  #----------------------------------------------------#
  
  output$downloadData1 <- downloadHandler(
    filename = function() { "Nokia_Lumia_reviews.txt" },
    content = function(file) {
      writeLines(readLines("data/Nokia_Lumia_reviews.txt"), file)
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function() { "Sentiments Scores.csv" },
    content = function(file) {
      write.csv(sentiments(), file, row.names=F)
    })

})
