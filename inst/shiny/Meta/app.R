library(shiny);library(DT);library(shinycustomloader);library(ggplot2)
source("global.R")
options(shiny.sanitize.errors = F)


ui <- navbarPage("Ranitidine",
                 tabPanel("Table 1", icon = icon("percentage"),
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("target_tb1", "Target", choices = list.idinfo$exposure[1], selected =  list.idinfo$exposure[1], inline = T),
                              radioButtons("comparator_tb1", "Comparator", choices = list.idinfo$exposure[-1],  selected = tail(list.idinfo$exposure, 1), inline = T),
                              selectInput("outcome_tb1", "Outcome", choices = list.idinfo$outcome, selected = list.idinfo$outcome[1]),
                              selectInput("analysis_tb1", "Analysis ID", choices = list.idinfo$analysis, selected = 4)
                            ),
                            mainPanel(
                              radioButtons("database_tb1", "Database", c("All", names.study), "All", inline = T),
                              withLoader(DTOutput("table1"), type="html", loader="loader6")
                            )
                          )
                          
                 ),
                 navbarMenu("Plot", icon = icon("bar-chart-o"),
                            tabPanel("Meta analysis",
                                     sidebarLayout(
                                       sidebarPanel(
                                         
                                       ),
                                       mainPanel(
                                   
                                       )
                                     )
                            )
                 )
)




server <- function(input, output, session) {
  
  ## getBalance
  getbalance <- reactive({
    getBalance_csv(list.balance = data.balance, list.covariate = data.covariate,  
                               targetId = 9991, comparatorId = 9998, analysisId = 4, outcomeId = 1)
    })%>% bindCache(input$target_tb1, input$comparator_tb1, input$input$analysis_tb1, input$outcome_tb1)
  
  

  ## N info
  nn <- reactive({
    lapply(list.result, function(x){
    x[target_id == as.numeric(input$target_tb1) & comparator_id == as.numeric(input$comparator_tb1) & analysis_id == as.numeric(input$analysis_tb1) & outcome_id == as.numeric(input$outcome_tb1), c("target_subjects", "comparator_subjects")]
  }) %>% Reduce(rbind, .)
  })
  
  nn.original <- reactive({
    lapply(list.result, function(x){
      x[target_id == as.numeric(input$target_tb1) & comparator_id == as.numeric(input$comparator_tb1) & analysis_id == analysis.originalN[as.character(input$analysis_tb1)] & outcome_id == as.numeric(input$outcome_tb1), c("target_subjects", "comparator_subjects")]
    }) %>% Reduce(rbind, .)
  })
  
  ## aggregate table 1
  tb1.aggre <- reactive({
    covid.common <- split(getbalance()$covariateId, getbalance()$databaseId) %>% Reduce(intersect, .)
    getbalance.common <- getbalance()[covariateId %in% covid.common]
    
    split.common <- split(getbalance.common, getbalance.common$databaseId)
    tb1.agg <-split.common[[1]]
    
    for (v in c("beforeMatchingMeanTreated", "beforeMatchingMeanComparator")){
      if (grepl(v, "Treated")){
        tb1.agg[[v]]  <- apply(sapply(split.common, `[[`, v), 1, function(x) weighted.mean(x, nn.original()$target_subjects)) 
      } else{
        tb1.agg[[v]]  <- apply(sapply(split.common, `[[`, v), 1, function(x) weighted.mean(x, nn.original()$comparator_subjects)) 
      }
    }
    
    for (v in c("afterMatchingMeanTreated", "afterMatchingMeanComparator")){
      if (grepl(v, "Treated")){
        tb1.agg[[v]]  <- apply(sapply(split.common, `[[`, v), 1, function(x) weighted.mean(x, nn()$target_subjects)) 
      } else{
        tb1.agg[[v]]  <- apply(sapply(split.common, `[[`, v), 1, function(x) weighted.mean(x, nn()$comparator_subjects)) 
      }
    }
    tb1.agg[, `:=`(beforeMatchingStdDiff = (beforeMatchingMeanTreated - beforeMatchingMeanComparator)/sqrt((beforeMatchingMeanTreated * (1 - beforeMatchingMeanTreated) + beforeMatchingMeanComparator * (1 - beforeMatchingMeanComparator))/2),
                   afterMatchingStdDiff = (afterMatchingMeanTreated - afterMatchingMeanComparator)/sqrt((afterMatchingMeanTreated * (1 - afterMatchingMeanTreated) + afterMatchingMeanComparator * (1 - afterMatchingMeanComparator))/2))]
    
    tb1.agg[, `:=`(absBeforeMatchingStdDiff = abs(beforeMatchingStdDiff),  absAfterMatchingStdDiff = abs(afterMatchingStdDiff))]
    return(tb1.agg)
  })
  
  
  output$table1 <- renderDT({
    out.tb1 <- NULL
    if (input$analysis_tb1 %in% unique(analysis.originalN)) return(NULL)
    
    names.tc <- c(names(which(list.idinfo$exposure == input$target_tb1)), names(which(list.idinfo$exposure == input$comparator_tb1)))
    if (input$database_tb1 == "All"){
      out.tb1 <- prepareTable1(balance = tb1.aggre())[-c(1:2), ]
      colnames(out.tb1) <- c("", paste0(names.tc, "(n = ", colSums(nn.original()),")"), "Standardized difference", paste0(names.tc, "(n = ", colSums(nn()),")"), "Standardized difference")
      
    } else{
      out.tb1 <- prepareTable1(balance = getbalance()[databaseId == input$database_tb1])[-c(1:2), ]
      colnames(out.tb1) <- c("", paste0(names.tc, "(n = ", colSums(nn.original()[which(names.study == input$database_tb1)]),")"), "Standardized difference", paste0(names.tc, "(n = ", colSums(nn()[which(names.study == input$database_tb1)]),")"), "Standardized difference")
    }
    fname <- "Table1"
    datatable(out.tb1, rownames = F, extensions = 'Buttons', 
              options = c(list(dom = 'tB', pageLength = -1,
                               buttons = list('copy', 
                                              'print', 
                                              list(extend = 'collection', 
                                                   buttons = list(list(extend = 'csv', filename= fname),
                                                                  list(extend = 'excel', filename= fname), 
                                                                  list(extend = 'pdf', filename= fname)
                                                   ), 
                                                   text = 'Download')), list(scrollX = TRUE))), 
      container = withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, ""),
          th(colspan = 3, "Before"),
          th(colspan = 3, "After")
        ),
        tr(
          lapply(colnames(out.tb1)[-1], th)
        )
      )
    )))
  })
  
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
}

shinyApp(ui, server)
