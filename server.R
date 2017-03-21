#################################################
#         tidy sentiments                       #
#################################################

library("shiny")
library("tidytext")
library("tidyr")
library("dplyr")
library("ggplot2")
library("DT")
library("reshape2")
library("wordcloud")
library("plotly")

#--------------------------------------------#

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
    
    if (input$lexicon != "afinn") {
        sent = textdf %>%
        mutate(linenumber = row_number()) %>%
        ungroup() %>%
        unnest_tokens(word, text) %>%
        inner_join(get_sentiments(input$lexicon)) %>%
        count(sentiment, index = linenumber %/% 1, sort = TRUE) %>%
        mutate(method = input$lexicon)
    } else {
        sent = textdf %>%
        mutate(linenumber = row_number()) %>%
        ungroup() %>%
        unnest_tokens(word, text) %>%
        inner_join(get_sentiments(input$lexicon)) %>%
        group_by(index = linenumber %/% 1) %>% 
        summarise(sentiment = sum(score)) %>% 
        mutate(method = input$lexicon)
    }
    
    return(sent)
    
  })

  dat = reactive({
    
    dat1 = sentiments()[(sentiments()$sentiment %in% c("positive", "negative") ),]
    dat2 = sentiments()[(sentiments()$sentiment %in% c("uncertainty","litigious","constraining","superfluous") ),]
    dat3 = sentiments()[(sentiments()$sentiment %in% c("joy", "trust","surprise","anticipation") ),]
    dat4 = sentiments()[(sentiments()$sentiment %in% c("anger", "disgust","fear", "sadness") ),]
    
    if (input$lexicon == "afinn") {
      out = list(sentiments())
    } else if (input$lexicon == "nrc") {
      out = list(dat3,dat4,dat1)
    } else if (input$lexicon == "bing") {
      out = list(dat1)
    } else if (input$lexicon == "loughran") {
      out = list(dat2,dat1)
    }
    return(out)
  })
  
    output$sent.plots <- renderUI({
    if (is.null(input$file)) {return(NULL)}
    else {
      
      if (input$lexicon == "afinn") {
        k = 1
      } else if (input$lexicon == "nrc") {
        k = 3
      } else if (input$lexicon == "bing") {
        k = 1
      } else if (input$lexicon == "loughran") {
        k = 2
      }
        plot_output_list <- lapply(1:k, function(i) {
        plotname <- paste("plot", i, sep="")
        plotlyOutput(plotname, height = 700, width = 700)
        
        # plotOutput(plotname, height = 700, width = 700)
      })
      # Convert the list to a tagList - this is necessary for the list of items
      # to display properly.
      do.call(tagList, plot_output_list)
    }
  })
  
  
  max_plots = 3
  
  for (i in 1:max_plots) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      
      my_i <- i 
      plotname <- paste("plot", my_i, sep="")
      
        output[[plotname]] <- renderPlotly({
          if (input$lexicon == "afinn") {
            plot_ly(dat()[[my_i]],x = ~index, y = ~sentiment,type = "bar")
            #      aes(index, sentiment)) +     # index is x col, n is y col. fill=?
            # geom_bar(alpha = 1, stat = "identity", position = "identity", show.legend = FALSE)      # stat=?
          } else {
            
          ggplot(dat()[[my_i]], 
            aes(index, n, fill = sentiment)) +     # index is x col, n is y col. fill=?
            geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +     # stat=?
            facet_wrap(~sentiment, ncol = 2, scales = "free_x")     # so cool.    
          }
        })
      })
  }
  
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })
  
  
  output$word.cloud <- renderPlot({
    textdf = dataset()

    if (input$lexicon != "afinn") {
      textdf %>%
      mutate(linenumber = row_number()) %>%
      ungroup() %>%
      unnest_tokens(word, text) %>%
      inner_join(get_sentiments(input$lexicon)) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
       comparison.cloud( #colors = c("#F8766D", "#00BFC4"),
                       max.words = 300)
    } else {
      
        textdf %>%
        mutate(linenumber = row_number()) %>%
        ungroup() %>%
        unnest_tokens(word, text) %>%
        inner_join(get_sentiments(input$lexicon)) %>%
        count(word, score, sort = TRUE) %>%
        acast(word ~ score, value.var = "n", fill = 0) %>%
        comparison.cloud( #colors = c("#F8766D", "#00BFC4"),
          max.words = 100)
    }
     
  })
  
  
  output$count <- renderDataTable({
    textdf = dataset()
    
    if (input$lexicon != "afinn") {
      wc = textdf %>%
        mutate(linenumber = row_number()) %>%
        ungroup() %>%
        unnest_tokens(word, text) %>%
        inner_join(get_sentiments(input$lexicon)) %>%
        count(word, sentiment, sort = TRUE) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) 
        #   %>% comparison.cloud( #colors = c("#F8766D", "#00BFC4"),
        #   max.words = 300)
      
    } else {
      
      wc = textdf %>%
        mutate(linenumber = row_number()) %>%
        ungroup() %>%
        unnest_tokens(word, text) %>%
        inner_join(get_sentiments(input$lexicon)) %>%
        count(word, score, sort = TRUE) %>%
        acast(word ~ score, value.var = "n", fill = 0) 
        # %>% comparison.cloud( #colors = c("#F8766D", "#00BFC4"),
        #   max.words = 100)
    }
    
    wc1 = data.frame(wc)

    if (input$lexicon == "afinn"){
     neg =grep("\\.",colnames(wc1))
     vec = c(rep("neg_",length(neg)), rep("pos_",(length(colnames(wc1))-length(neg))))
     cnames = paste(vec,gsub("X|X\\.","",colnames(wc1)))
     colnames(wc1) = cnames
    }
    wc1 = wc1[order(wc1[,1], decreasing = T),]
    return(wc1)
    
  })
  
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
        count(sentiment, Sentence.No = linenumber %/% 1, sort = TRUE) %>%
        mutate(method = "nrc")
    }
    
    if (input$lexicon == "bing") {
      sent = textdf %>%
        mutate(linenumber = row_number()) %>%
        ungroup() %>%
        unnest_tokens(word, text) %>%
        inner_join(get_sentiments("bing")) %>%
        count(sentiment, Sentence.No = linenumber %/% 1, sort = TRUE) %>%
        mutate(method = "bing")
    }
    
    if (input$lexicon == "afinn") {
      sent = textdf %>%
        mutate(linenumber = row_number()) %>%
        ungroup() %>%
        unnest_tokens(word, text) %>%
        inner_join(get_sentiments("afinn")) %>%
        group_by(Sentence.No = linenumber %/% 1) %>% 
        summarise(sentiment = sum(score)) %>% 
        mutate(method = "afinn")
    }
    
    if (input$lexicon == "loughran") {
      sent = textdf %>%
        mutate(linenumber = row_number()) %>%
        ungroup() %>%
        unnest_tokens(word, text) %>%
        inner_join(get_sentiments("loughran")) %>%
        count(sentiment, Sentence.No = linenumber %/% 1, sort = TRUE) %>%
        mutate(method = "loughran")
    }
    
    return(sent)
    
  })
  

  # output$sent.plot.index <- renderPlot ({
  # 
  #   if (input$lexicon != "afinn") {
  #     ggplot(sentiments.index(),
  #            aes(index, n, fill = sentiment)) +     # index is x col, n is y col. fill=?
  #       geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +     # stat=?
  #       facet_wrap(~sentiment, ncol = 2, scales = "free_x")     # so cool.
  #   }   else  {
  #     ggplot(sentiments.index(),
  #            aes(index, sentiment)) +     # index is x col, n is y col. fill=?
  #       geom_bar(alpha = 1, stat = "identity", position = "identity", show.legend = FALSE)      # stat=?
  #   }
  # 
  # })
  
  t1 = reactive({
    if (is.null(input$file)) {return(NULL)}
    else {
      
      # tb = sentiments()
      # y1 = data.frame(dataset() , index= 1:nrow(dataset()))
      
      textdf = dataset()
      
      worddf = textdf %>%
        mutate(linenumber = row_number()) %>%
        ungroup() %>%
        unnest_tokens(word, text) %>%
        inner_join(get_sentiments(input$lexicon)) %>% 
        unique()
      
      worddf = data.frame(worddf)
      
      if (input$lexicon != "afinn") {
        wdf = data.frame(NULL)
        for (i in unique(worddf$linenumber)) {
          tempd = worddf[worddf$linenumber == i,]
          se = unique(tempd$sentiment)
          se = se[order(se)]
          for (s in se){
            t = paste(tempd[tempd$sentiment == s,'word'],collapse = ", ")
            dft = data.frame(index = i, sentiment = s, words = t)
            wdf = rbind(wdf, dft)
          }
        } 
      }   else {
        wdf = data.frame(NULL)
        for (i in unique(worddf$linenumber)) {
          tempd = worddf[worddf$linenumber == i,]
          se = unique(tempd$score)
          se = se[order(se)]
          for (s in se){
            t = paste(tempd[tempd$score == s,'word'],collapse = ", ")
            dft = data.frame(index = i, sentiment = s, words = t)
            wdf = rbind(wdf, dft)
          }
        }
        
      }
      
      wdf1 = wdf[wdf$index == input$index,]
      # test = merge(tb,wdf ,by.x ="index", by.y= "index", all.y=T)
      return(wdf1)
    }
    
  })
  
  output$table <- renderDataTable({
    datatable(t1(), rownames = F)#
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
  
  
  
  t2 = reactive({
    if (is.null(input$file)) {return(NULL)}
    else {
      
      tb = sentiments.index()
      tx = dataset()[input$index,] %>% unnest_tokens(text, text, token = "sentences")
      
      y1 = data.frame(tx, Sentence.No= 1:nrow(tx))
      
      test = merge(tb,y1 ,by.x ="Sentence.No", by.y= "Sentence.No", all.y=T)
      return(test)
    }
    
  })
  
  output$table2 <- renderDataTable({
    datatable(t2(), rownames = F)
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
