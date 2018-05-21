
library("shiny")
library("shinythemes")
library("ggplot2")
library("ggthemes")
library("grDevices")
library("grid")
library("gridExtra")
library("extrafont")
library("feather")
library("data.table")
library("scales")
library("TTR")

### TODO: add Andres NBB funding
### TODO: faster?
### TODO: clean once and for always

# load input data with all relevant information, including sentiment scores (data not included in the repo!)
out <- as.data.table(feather::read_feather("OUT.feather"))

############################

plot_sent <- function(data, timeGap, type = "multi") {
  if (type == "multi") p <- ggplot(data = data, aes(x = date, y = net_sent, color = name))
  else if (type == "full") p <- ggplot(data = data, aes(x = date, y = net_sent_full))
  p <- p +
    geom_line(aes(y = sma), size = 1.25) +
    geom_hline(yintercept = 0, size = 0.70, linetype = "dotted") +
    scale_y_continuous(name = "Sentiment") + 
    scale_x_date(name = "Date",
                 labels = ifelse(timeGap > 90, date_format("%m-%Y"), date_format("%d-%m-%y"))) +
    theme_tufte(ticks = TRUE) +
    theme(legend.title = element_blank(), legend.position = "top", text = element_text(size = 14),
          plot.margin = unit(c(2.5, 2.5, 0, 7.5), "mm"), # top, right, bottom, left
          axis.text.x = element_text(angle = 60, hjust = 1), axis.ticks = element_blank())
  return(p)
}

plot_docs <- function(data, color = "language") {
  p <- ggplot(data = data, aes_string(x = "date", y = "documents", color = color)) +
    geom_col(aes_string(fill = color)) +
    scale_y_continuous(name = "Documents") +
    scale_x_date(name = "") +
    theme_tufte(ticks = TRUE) +
    theme(legend.title = element_blank(), legend.position = "none", text = element_text(size = 14),
          plot.margin = unit(c(5, 2.5, 0, 7.5), "mm"), # top, right, bottom, left
          axis.text.x = element_blank(), axis.ticks = element_blank())
  return(p)
}

validator <- function(inp) { 
  v <- (length(inp$language) > 0 
  & length(inp$lexicon) > 0 
  & as.integer(inp$nSMA) > 0 
  & as.integer(inp$minWords) > 0 
  & as.integer(inp$minDocs) > 0
  & (inp$dates[2] - inp$dates[1]) > 0)
  return(v)
}

validator2 <- function(inp) { # suffix 2 generally indicates first tab (fully aggregated time series)
  v <- (as.integer(inp$nSMA2) > 0 
        & as.integer(inp$minWords2) > 0 
        & as.integer(inp$minDocs2) > 0
        & (inp$dates2[2] - inp$dates2[1]) > 0)
  return(v)
}

get_leads <- function(out, inp, lang, lex) {
  data <- out[keyword %in% inp$keywords & language == lang, ]
  data <- data[word_count >= inp$minWords3, ]
  data <- data[date >= inp$dates3[1] & date <= inp$dates3[2], ]
  data[, "net_sent_abs" := abs(data$net_sent)]
  topLeads <- data[order(-net_sent_abs)]
  top <- topLeads[lexicon == lex, c("net_sent", "date", "lead")][1:inp$nLeads, ]
  top$date <- as.character(top$date)
  colnames(top) <- c("Score", "Date", "Article's Lead")
  return(top)
}

make_grid <- function(top, bottom) {
  gA <- ggplot_gtable(ggplot_build(top))
  gB <- ggplot_gtable(ggplot_build(bottom))
  maxWidth <- grid::unit.pmax(gA$widths, gB$widths)
  gA$widths <- as.list(maxWidth)
  gB$widths <- as.list(maxWidth)
  grid.newpage()
  grid.arrange(arrangeGrob(gA, gB, nrow = 2, heights = c(.3, .7)))
}

fill_dates <- function(data, dates, type) {
  st <- as.POSIXct(min(dates[1]), format = "%Y-%m-%d")
  end <- as.POSIXct(max(dates[2]), format = "%Y-%m-%d")
  date <- as.Date(seq.POSIXt(st, end, by = "day"))
  fill <- data[1, 2:ncol(data)]
  fill[, (type) := 0]
  fill[, c("words", "documents") := NA]
  oldIn <- date %in% data$date
  dataNew <- merge(as.data.table(date), data, by = "date", all = TRUE)
  dataNew[!oldIn, 2:ncol(dataNew)] <- fill
  return(dataNew)
}

############################

function(input, output) {
  
  computeData <- reactive({
    data <- out[keyword %in% input$keywords, ] # selected topic
    if (length(input$desk) > 0) {
      if (length(input$language) == 2) {
        desks <- unlist(strsplit(input$desk, " "))
      } else {
        if (input$language == "nl") desks <- unlist(strsplit(input$desk, " "))[seq(1, length(input$desk) * 2, by = 2)]
        else desks <- unlist(strsplit(input$desk, " "))[seq(1, length(input$desk) * 2, by = 2) + 1]
      }
      data <- data[desk %in% desks, ]
    }
    data <- data[word_count >= input$minWords, ] # selection based on minimum words input
    data <- data[, list(net_sent = sum(net_sent, na.rm = TRUE), # average net sentiment score
                        words = sum(word_count), # total number of words
                        documents = sum(doc_counter)), # total number of documents
                 by = list(date, lexicon, language)] # aggregate over date based on selected keywords
    data <- data[language %in% input$language, ] # selection based on language input
    data <- data[lexicon %in% input$lexicon, ] # selection based on lexicons input
    data <- data[date >= input$dates[1] & date <= input$dates[2], ] # selection based on time frame input
    data <- data[documents >= input$minDocs, ][order(lexicon, language, date)] # selection based on minimum documents input
    data[, "name" := paste0(data$lexicon, " (", data$language, ")")]
    if (nrow(data) > 0) {
      nvals <- data[, list(n = length(net_sent)), by = list(lexicon, language)]$n
      # fill in missing dates with 0 for net_sent, words and documents
      dataNew <- c()
      for (nm in unique(data$name)) {
        dt <- subset(data, name == nm)
        dtNew <- fill_dates(dt, input$dates, "net_sent")
        dataNew <- rbind(dataNew, dtNew)
      }
      data <- dataNew
    } else nvals <- 0 # in case data is empty
    if (any(nvals < 10)) { # if at least one of the series has less than 10 data points, no plot is shown
      valid <- FALSE
    } else {
      valid <- TRUE
      if (input$normalised) {
        # normalisation within selection
        norm <- data[, list(vals = (net_sent - mean(net_sent, na.rm = TRUE)) / sd(net_sent, na.rm = TRUE)),
                     by = list(lexicon, language)]
        data[, "net_sent" := norm$vals]
      }
      sma <- data[, list(vals = SMA(net_sent, n = as.integer(input$nSMA))), by = list(lexicon, language)]
      data[, "sma" := sma$vals]
    }
    return(list(data = data, valid = valid))
  })

  computeDataFull <- reactive({
    data <- out[keyword %in% input$keywords, ] 
    data <- data[word_count >= input$minWords2, ]
    data <- data[, list(net_sent = sum(net_sent, na.rm = TRUE), 
                        words = sum(word_count), 
                        documents = sum(doc_counter)), 
                 by = list(date, lexicon, language)] 
    data <- data[date >= input$dates2[1] & date <= input$dates2[2], ][order(lexicon, language)]
    data[, "name" := paste0(data$lexicon, " (", data$language, ")")]
    data <- data[documents >= input$minDocs2, ]
    if (nrow(data) > 0) {
      docs <- data[, .(n = sum(documents, na.rm = TRUE)), by = name]
      words <- data[, .(n = round(mean(words / documents, na.rm = TRUE), 0)), by = name]
    }
    weightsLex <- rep(input$wLex / 100, nrow(data)) # General
    weightsLex[data$lexicon == "Financial"] <- 1 - (input$wLex / 100)
    weightsLang <- rep(input$wLan / 100, nrow(data)) # French
    weightsLang[data$language == "nl"] <- 1 - (input$wLan / 100)
    data[, "weightsLex" := weightsLex]
    data[, "weightsLang" := weightsLang]
    data <- data[, list(net_sent_full = sum(net_sent * weightsLex * weightsLang, na.rm = TRUE),
                        documents = sum(documents, na.rm = TRUE) / 2,
                        words = sum(words, na.rm = TRUE) / 2),
                 by = list(date)][order(date)]
    nvals <- length(data$date)
    if (nvals >= 10) {
      valid <- TRUE
      data <- fill_dates(data, input$dates2, "net_sent_full")
      if (input$normalised2) {
        norm <- (data$net_sent_full - mean(data$net_sent_full, na.rm = TRUE)) / sd(data$net_sent_full, na.rm = TRUE)
        data[, "net_sent_full" := norm]
      }
      data[, "sma" := SMA(data$net_sent_full, n = as.integer(input$nSMA2))]
    } else {
      nvals <- docs <- words <- 0
      valid <- FALSE
    }
    return(list(data = data, valid = valid, docs = docs, words = words))
  })

  # initialize reactive values
  selData <- reactiveValues()
  selDataFull <- reactiveValues()

  output$boudt <- renderImage({
    return(list(
      src = "images/boudt.jpg",
      filetype = "image/jpeg"
    ))
  }, deleteFile = FALSE)
  outputOptions(output, "boudt", suspendWhenHidden = FALSE)
  
  output$ardia <- renderImage({
    return(list(
      src = "images/ardia.jpg",
      filetype = "image/jpeg"
    ))
  }, deleteFile = FALSE)
  outputOptions(output, "ardia", suspendWhenHidden = FALSE)
  
  output$bluteau <- renderImage({
    return(list(
      src = "images/bluteau.jpg",
      filetype = "image/jpeg"
    ))
  }, deleteFile = FALSE)
  outputOptions(output, "bluteau", suspendWhenHidden = FALSE)
  
  output$borms <- renderImage({
    return(list(
      src = "images/borms.jpg",
      filetype = "image/jpeg"
    ))
  }, deleteFile = FALSE)
  outputOptions(output, "borms", suspendWhenHidden = FALSE)
  
  output$algaba <- renderImage({
    return(list(
      src = "images/algaba.jpg",
      filetype = "image/jpeg"
    ))
  }, deleteFile = FALSE)
  outputOptions(output, "algaba", suspendWhenHidden = FALSE)
  
  output$belga <- renderImage({
    return(list(
      src = "images/belga.jpg",
      filetype = "image/jpeg"
    ))
  }, deleteFile = FALSE)
  outputOptions(output, "belga", suspendWhenHidden = FALSE)
  
  output$finvex <- renderImage({
    return(list(
      src = "images/finvex.jpg",
      filetype = "image/jpeg"
    ))
  }, deleteFile = FALSE)
  outputOptions(output, "finvex", suspendWhenHidden = FALSE)
  
  output$nDocs <- renderUI({
    validate(
      need(validator(input), "")
    )
    getData <- computeData() # run computeData() once and assign it to selData for later use
    selData$data <- getData$data
    selData$valid <- getData$valid
    validate(
      need(selData$valid == TRUE,
           paste0(""))
    )
    data <- selData$data
    docs <- data[, .(n = sum(documents, na.rm = TRUE)), by = name]
    HTML(paste0("<b> Number of documents: </b>",
                "<ul>",
                paste0("<li>",
                       paste0(unique(docs$n), " ",
                                     strsplit(paste0("(", input$language, collapse = " ", ")"), split = " ")[[1]],
                              collapse = "</li><li>"),
                       "</li"),
                "</ul>"))
  })
  outputOptions(output, "nDocs", suspendWhenHidden = FALSE)
  
  output$nWords <- renderUI({
    validate(
      need(validator(input), "")
    )
    data <- selData$data
    validate(
      need(selData$valid == TRUE,
           paste0(""))
    )
    words <- data[, .(n = round(mean(words / documents, na.rm = TRUE), 0)), by = name]
    HTML(paste0("<b> Average words per document: </b>",
                "<ul>",
                paste0("<li>",
                       paste0(unique(words$n), " ",
                              strsplit(paste0("(", input$language, collapse = " ", ")"), split = " ")[[1]],
                              collapse = "</li><li>"),
                       "</li"),
                "</ul>"))
  })
  outputOptions(output, "nWords", suspendWhenHidden = FALSE)
  
  output$nDocsFull <- renderUI({
    getData <- computeDataFull() # run computeDataFull() once and assign it to selDataFull for later use
    selDataFull$data <- getData$data
    selDataFull$valid <- getData$valid
    selDataFull$docs <- getData$docs
    selDataFull$words <- getData$words
    validate(
      need(all(c(validator2(input), selDataFull$valid == TRUE)), "")
    )
    docs <- selDataFull$docs
    HTML(paste0("<b> Number of documents: </b>",
                "<ul>",
                paste0("<li>",
                       paste0(unique(docs$n), " ",
                              strsplit(paste0("(", c("fr", "nl"), collapse = " ", ")"), split = " ")[[1]],
                              collapse = "</li><li>"),
                       "</li"),
                "</ul>"))
  })
  outputOptions(output, "nDocsFull", suspendWhenHidden = FALSE)
  
  output$nWordsFull <- renderUI({
    validate(
      need(all(c(validator2(input), selDataFull$valid == TRUE)), "")
    )
    words <- selDataFull$words
    HTML(paste0("<b> Average words per document: </b>",
                "<ul>",
                paste0("<li>",
                       paste0(unique(words$n), " ",
                              strsplit(paste0("(", c("fr", "nl"), collapse = " ", ")"), split = " ")[[1]],
                              collapse = "</li><li>"),
                       "</li"),
                "</ul>"))
  })
  outputOptions(output, "nWordsFull", suspendWhenHidden = FALSE)
  
  output$stats <- renderTable({
    validate(
      need(all(c(validator(input), selData$valid == TRUE)), "Select proper inputs.")
    )
    data <- selData$data
    if (!as.logical(input$normalised)) {
      stats <- data[, .(sentiment = mean(net_sent)), by = name]
      names(stats) <- c("Selection", "Mean Sentiment")
      return(stats)
    }
  })
  outputOptions(output, "stats", suspendWhenHidden = FALSE)
  
  output$meanSent <- renderUI({
    validate(
      need(all(c(validator2(input), selDataFull$valid == TRUE)), paste0("Select proper inputs.", "\n \n"))
    )
    data <- selDataFull$data
    if (!as.logical(input$normalised2)) {
      HTML(paste0("<b> Mean Sentiment: </b> ",
                round(mean(data$net_sent_full, na.rm= TRUE), 4)),
           "<br> <br>")
    }
  })
  outputOptions(output, "meanSent", suspendWhenHidden = FALSE)
  
  output$plot <- renderPlot({
    validate(
      need(all(c(validator(input), selData$valid == TRUE)),
           paste0("You probably made one or more wrong parameter choices. Potential problems are:", "\n",
                  "- select at least one input language and one input lexicon", "\n",
                  "- the minimum number of words and documents need to be above zero", "\n", 
                  "- the moving average period needs to be above zero", "\n",
                  "- there might be too few data points (less than 10 available days) for at least one of the indices", "\n",
                  "\n",
                  "Try out a different combination of parameters and make those time series pop up!"))
    )
    data <- selData$data
    p1 <- plot_sent(data, timeGap = input$dates[2] - input$dates[1])
    data$documents[is.na(data$documents)] <- 0
    dataCol <- rbind(data[language == "nl" & lexicon == unique(lexicon)[1], ],
                     data[language == "fr" & lexicon == unique(lexicon)[1], ])
    p2 <- plot_docs(dataCol)
    make_grid(top = p2, bottom = p1)
  })
  outputOptions(output, "plot", suspendWhenHidden = FALSE)
  
  output$plotFull <- renderPlot({
    validate(
      need(all(c(validator2(input), selDataFull$valid == TRUE)),
           paste0("You probably made one or more wrong parameter choices. Potential problems are:", "\n",
                  "- the minimum number of words and documents need to be above zero", "\n", 
                  "- the moving average period needs to be above zero", "\n",
                  "- there might be too few data points (less than 10 available days) for at least one of the indices", "\n",
                  "\n",
                  "Try out a different combination of parameters and make that global time series pop up!"))
    )
    data <- selDataFull$data
    p1 <- plot_sent(data, timeGap = input$dates2[2] - input$dates2[1], type = "full")
    data$documents[is.na(data$documents)] <- 0
    data$col <- "col"
    p2 <- plot_docs(data, color = "col")
    make_grid(top = p2, bottom = p1)
  })
  outputOptions(output, "plotFull", suspendWhenHidden = FALSE)
  
  output$export <- downloadHandler(
   filename = paste('text-sent-multiple-', Sys.Date(), '.csv', sep = ''),
   content = function(file) {
     data <- selData$data
     data$sma <- data$name <- data$words <- NULL
     write.csv2(data, file)
   },
   contentType = "text/csv"
  )

  output$exportFull <- downloadHandler(
    filename = paste('text-sent-single-', Sys.Date(), '.csv', sep = ''),
    content = function(file) {
      dataFull <- selDataFull$data
      dataFull$sma <- NULL
      write.csv2(dataFull, file)
    },
    contentType = "text/csv"
  )
  
  output$GenFR <- renderTable({
    get_leads(out, input, "fr", "General")
  })
  outputOptions(output, "GenFR", suspendWhenHidden = FALSE)
  
  output$FinFR <- renderTable({
    get_leads(out, input, "fr", "Financial")
  })
  outputOptions(output, "FinFR", suspendWhenHidden = FALSE)
  
  output$GenNL <- renderTable({
    get_leads(out, input, "nl", "General")
  })
  outputOptions(output, "GenNL", suspendWhenHidden = FALSE)
  
  output$FinNL <- renderTable({
    get_leads(out, input, "nl", "Financial")
  })
  outputOptions(output, "FinNL", suspendWhenHidden = FALSE)
}

