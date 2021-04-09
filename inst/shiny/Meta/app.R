library(shiny);library(DT);library(shinycustomloader);library(ggplot2);library(meta)
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
                              tabsetPanel(type = "pills",
                                          tabPanel("Table 1", radioButtons("database_tb1", "Database", c("All", names.study), "All", inline = T),
                                                   withLoader(DTOutput("table1"), type="html", loader="loader6")),
                                          tabPanel("Meta analysis", 
                                                   withLoader(plotOutput("meta", width = "100%"), type="html", loader="loader6"),
                                                   h3("Download options"),
                                                   wellPanel(
                                                     uiOutput("downloadControls_forest"),
                                                     downloadButton("downloadButton_forest", label = "Download the plot")
                                                   ))
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
  
  
  obj.meta <- reactive({
    DM <- lapply(list.result, function(x){x[target_id == as.numeric(input$target_tb1) & comparator_id == as.numeric(input$comparator_tb1) & analysis_id == as.numeric(input$analysis_tb1) & outcome_id == as.numeric(input$outcome_tb1)]}) %>% rbindlist
  
    out.meta <- metagen(TE = DM$log_rr, seTE = DM$se_log_rr, studlab = DM$database_id, sm = "HR", hakn = F, comb.fixed = TRUE,comb.random = TRUE)
    out.meta$n.e <- DM$target_subjects
    out.meta$event.e <- DM$target_outcomes
    out.meta$event.rate.t <- round(with(DM, target_outcomes/(target_days/365))*1000,1)
    out.meta$person.year.t <- with(DM, round((target_days/365),0))
    
    out.meta$n.c <- DM$comparator_subjects
    out.meta$event.c <- DM$comparator_outcomes
    out.meta$event.rate.c <- round(with(DM, comparator_outcomes/(comparator_days/365))*1000,1)
    out.meta$person.year.c<-with(DM, round((comparator_days/365),0))
    out.meta$vname <- gsub(" user", "", c(names(which(list.idinfo$exposure == input$target_tb1)), names(which(list.idinfo$exposure == input$comparator_tb1))))
    return(out.meta)
    
  })
  
  xlim <- reactive({
    ceiling(max (1/exp(obj.meta()$lower.random),exp(obj.meta()$upper.random)))
  })
  
   
  
  output$meta <- renderPlot({
    forest(obj.meta(),  leftcols = c("studlab", "n.e","event.e", "n.c","event.c", "effect","ci"), 
           leftlabs = c("Source", "Total","Event","Total","Event","HR","95% CI"), pooled.total= T, pooled.events = T, 
           rightcols = F, lab.e = paste0("             ", obj.meta()$vname[1]), lab.c = paste0("             ", obj.meta()$vname[2]), lab.e.attach.to.col = "n.e", lab.c.attach.to.col = "n.c",
           fontsize=12, comb.fixed = FALSE, comb.random = TRUE, text.random = "Overall", col.diamond.random = "royalblue", col.diamond.lines = "black",
           digits = 2, digits.pval =3, digits.I2 = 1, just.studlab="left", just.addcols.left= "right", just = "center", xlim = c(round(1/xlim(), 2),xlim()),
           plotwidth ="8cm", spacing =1, addrow.overall=TRUE, print.I2 = TRUE, print.pval.I2=F, print.tau2 = F, print.pval.Q = F,
           label.left = paste0("Flavor\n", obj.meta()$vname[1]), label.right = paste0("Flavor\n", obj.meta()$vname[2]), scientific.pval = F, big.mark =","
           )
  })
  
  output$downloadControls_forest <- renderUI({
    fluidRow(
      column(4,
             selectizeInput("forest_file_ext", "File extension (dpi = 300)", 
                            choices = c("jpg","pdf", "tiff", "svg", "emf"), multiple = F, 
                            selected = "emf"
             )
      ),
      column(4,
             sliderInput("fig_width_forest", "Width (in):",
                         min = 5, max = 20, value = 10
             )
      ),
      column(4,
             sliderInput("fig_height_forest", "Height (in):",
                         min = 5, max = 20, value = 10
             )
      )
    )
  })
  
  output$downloadButton_forest <- downloadHandler(
    filename =  function() {
      paste("forestplot.", input$forest_file_ext ,sep="")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      withProgress(message = 'Download in progress',
                   detail = 'This may take a while...', value = 0, {
                     for (i in 1:15) {
                       incProgress(1/15)
                       Sys.sleep(0.01)
                     }
                     
                     if (input$forest_file_ext == "emf"){
                       devEMF::emf(file, width = input$fig_width_forest, height =input$fig_height_forest, coordDPI = 100, emfPlus = F)
                       forest(obj.meta(),  leftcols = c("studlab", "n.e","event.e", "n.c","event.c", "effect","ci"), 
                              leftlabs = c("Source", "Total","Event","Total","Event","HR","95% CI"), pooled.total= T, pooled.events = T, 
                              rightcols = F, lab.e = paste0("             ", obj.meta()$vname[1]), lab.c = paste0("             ", obj.meta()$vname[2]), lab.e.attach.to.col = "n.e", lab.c.attach.to.col = "n.c",
                              fontsize=12, comb.fixed = FALSE, comb.random = TRUE, text.random = "Overall", col.diamond.random = "royalblue", col.diamond.lines = "black",
                              digits = 2, digits.pval =3, digits.I2 = 1, just.studlab="left", just.addcols.left= "right", just = "center", xlim = c(round(1/xlim(), 2),xlim()),
                              plotwidth ="8cm", spacing =1, addrow.overall=TRUE, print.I2 = TRUE, print.pval.I2=F, print.tau2 = F, print.pval.Q = F,
                              label.left = paste0("Flavor\n", obj.meta()$vname[1]), label.right = paste0("Flavor\n", obj.meta()$vname[2]), scientific.pval = F, big.mark =","
                       )
                       dev.off()
                       
                     } else if (input$forest_file_ext == "jpg"){
                       jpeg(file, width = input$fig_width_forest, height =input$fig_height_forest, units = "in", res = 320)
                       forest(obj.meta(),  leftcols = c("studlab", "n.e","event.e", "n.c","event.c", "effect","ci"), 
                              leftlabs = c("Source", "Total","Event","Total","Event","HR","95% CI"), pooled.total= T, pooled.events = T, 
                              rightcols = F, lab.e = paste0("             ", obj.meta()$vname[1]), lab.c = paste0("             ", obj.meta()$vname[2]), lab.e.attach.to.col = "n.e", lab.c.attach.to.col = "n.c",
                              fontsize=12, comb.fixed = FALSE, comb.random = TRUE, text.random = "Overall", col.diamond.random = "royalblue", col.diamond.lines = "black",
                              digits = 2, digits.pval =3, digits.I2 = 1, just.studlab="left", just.addcols.left= "right", just = "center", xlim = c(round(1/xlim(), 2),xlim()),
                              plotwidth ="8cm", spacing =1, addrow.overall=TRUE, print.I2 = TRUE, print.pval.I2=F, print.tau2 = F, print.pval.Q = F,
                              label.left = paste0("Flavor\n", obj.meta()$vname[1]), label.right = paste0("Flavor\n", obj.meta()$vname[2]), scientific.pval = F, big.mark =","
                       )
                       dev.off()
                     } else if (input$forest_file_ext == "tiff"){
                       tiff(file, width = input$fig_width_forest, height =input$fig_height_forest, units = "in", res = 320, compression = "zip")
                       forest(obj.meta(),  leftcols = c("studlab", "n.e","event.e", "n.c","event.c", "effect","ci"), 
                              leftlabs = c("Source", "Total","Event","Total","Event","HR","95% CI"), pooled.total= T, pooled.events = T, 
                              rightcols = F, lab.e = paste0("             ", obj.meta()$vname[1]), lab.c = paste0("             ", obj.meta()$vname[2]), lab.e.attach.to.col = "n.e", lab.c.attach.to.col = "n.c",
                              fontsize=12, comb.fixed = FALSE, comb.random = TRUE, text.random = "Overall", col.diamond.random = "royalblue", col.diamond.lines = "black",
                              digits = 2, digits.pval =3, digits.I2 = 1, just.studlab="left", just.addcols.left= "right", just = "center", xlim = c(round(1/xlim(), 2),xlim()),
                              plotwidth ="8cm", spacing =1, addrow.overall=TRUE, print.I2 = TRUE, print.pval.I2=F, print.tau2 = F, print.pval.Q = F,
                              label.left = paste0("Flavor\n", obj.meta()$vname[1]), label.right = paste0("Flavor\n", obj.meta()$vname[2]), scientific.pval = F, big.mark =","
                       )
                       dev.off()
                       
                     } else if (input$forest_file_ext == "pdf"){
                       pdf(file, width = input$fig_width_forest, height =input$fig_height_forest)
                       forest(obj.meta(),  leftcols = c("studlab", "n.e","event.e", "n.c","event.c", "effect","ci"), 
                              leftlabs = c("Source", "Total","Event","Total","Event","HR","95% CI"), pooled.total= T, pooled.events = T, 
                              rightcols = F, lab.e = paste0("             ", obj.meta()$vname[1]), lab.c = paste0("             ", obj.meta()$vname[2]), lab.e.attach.to.col = "n.e", lab.c.attach.to.col = "n.c",
                              fontsize=12, comb.fixed = FALSE, comb.random = TRUE, text.random = "Overall", col.diamond.random = "royalblue", col.diamond.lines = "black",
                              digits = 2, digits.pval =3, digits.I2 = 1, just.studlab="left", just.addcols.left= "right", just = "center", xlim = c(round(1/xlim(), 2),xlim()),
                              plotwidth ="8cm", spacing =1, addrow.overall=TRUE, print.I2 = TRUE, print.pval.I2=F, print.tau2 = F, print.pval.Q = F,
                              label.left = paste0("Flavor\n", obj.meta()$vname[1]), label.right = paste0("Flavor\n", obj.meta()$vname[2]), scientific.pval = F, big.mark =","
                       )
                       dev.off()
                       
                     } else if (input$forest_file_ext == "svg"){
                       svglite::svglite(file, width = input$fig_width_forest, height =input$fig_height_forest)
                       forest(obj.meta(),  leftcols = c("studlab", "n.e","event.e", "n.c","event.c", "effect","ci"), 
                              leftlabs = c("Source", "Total","Event","Total","Event","HR","95% CI"), pooled.total= T, pooled.events = T, 
                              rightcols = F, lab.e = paste0("             ", obj.meta()$vname[1]), lab.c = paste0("             ", obj.meta()$vname[2]), lab.e.attach.to.col = "n.e", lab.c.attach.to.col = "n.c",
                              fontsize=12, comb.fixed = FALSE, comb.random = TRUE, text.random = "Overall", col.diamond.random = "royalblue", col.diamond.lines = "black",
                              digits = 2, digits.pval =3, digits.I2 = 1, just.studlab="left", just.addcols.left= "right", just = "center", xlim = c(round(1/xlim(), 2),xlim()),
                              plotwidth ="8cm", spacing =1, addrow.overall=TRUE, print.I2 = TRUE, print.pval.I2=F, print.tau2 = F, print.pval.Q = F,
                              label.left = paste0("Flavor\n", obj.meta()$vname[1]), label.right = paste0("Flavor\n", obj.meta()$vname[2]), scientific.pval = F, big.mark =","
                       )
                       dev.off()
                       
                     } 
                     
                   })
      
      
    })
  
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  
  
  
}

shinyApp(ui, server)
