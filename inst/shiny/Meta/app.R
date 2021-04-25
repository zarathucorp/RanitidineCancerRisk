library(shiny);library(DT);library(shinycustomloader);library(ggplot2);library(meta);library(forestplot);library(scales)
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
                                                   selectInput("database_meta", "Database", names.study, names.study, multiple = T),
                                                   withLoader(plotOutput("meta", width = "100%"), type="html", loader="loader6"),
                                                   h3("Download options"),
                                                   wellPanel(
                                                     uiOutput("downloadControls_forest"),
                                                     downloadButton("downloadButton_forest", label = "Download the plot")
                                                   )),
                                          tabPanel("KM plot", 
                                                   radioButtons("database_kap", "Database", names.study, names.study[1], inline = T),
                                                   withLoader(plotOutput("kaplanMeierPlot"), type="html", loader="loader6"),
                                                   uiOutput("kaplanMeierPlotPlotCaption"),
                                                   fluidRow(
                                                     column(4, checkboxInput("ci_km", "Show 95% CI", value = F)),
                                                     column(4, checkboxInput("cumulative_km", "Show cumulative incidence", value = T)),
                                                     column(4, checkboxInput("percent_km", "% Y-scale", value = F)),
                                                     column(4, checkboxInput("yearx_km", "Year X-scale", value = F))
                                                   ),
                                                   fluidRow(
                                                     column(6, uiOutput("x_km")),
                                                     column(6, sliderInput("ymin_km", "Y axis range", min = 0, max = 1, value = c(0, 0.1), step = 0.05))
                                                   ),
                                                   fluidRow(
                                                     column(6, sliderInput("width_km","Plot width", min = 5, max = 20, value = 10)),
                                                     column(6, sliderInput("height_km","Plot height", min = 2, max = 15, value = 5))
                                                   ),
                                                   div(style = "display: inline-block;vertical-align:top;",
                                                       downloadButton("downloadKaplanMeierPlotPng", label = "Download plot as PNG"),
                                                       downloadButton("downloadKaplanMeierPlotEmf", label = "Download plot as EMF")
                                                   )),
                                          tabPanel("Propensity scores",
                                                   radioButtons("database_ps", "Database", names.study, names.study[1], inline = T),
                                                   plotOutput("psDistPlot"),
                                                   div(strong("Figure 2."),"Preference score distribution. The preference score is a transformation of the propensity score
                                                                                                         that adjusts for differences in the sizes of the two treatment groups. A higher overlap indicates subjects in the
                                                                                                         two groups were more similar in terms of their predicted probability of receiving one treatment over the other."),
                                                   fluidRow(
                                                     column(6, sliderInput("width_ps","Plot width", min = 2, max = 20, value = 5)),
                                                     column(6, sliderInput("height_ps","Plot height", min = 2, max = 15, value = 3.5, step = 0.5))
                                                   ),
                                                   div(style = "display: inline-block;vertical-align:top;",
                                                       downloadButton("downloadPsDistPlotPng", label = "Download plot as PNG"),
                                                       downloadButton("downloadPsDistPlotEmf", label = "Download plot as EMF")
                                                   )),
                                          tabPanel("Covariate balance",
                                                   radioButtons("database_bal", "Database", names.study, names.study[1], inline = T),
                                                   uiOutput("hoverInfoBalanceScatter"),
                                                   plotOutput("balancePlot",
                                                              hover = hoverOpts("plotHoverBalanceScatter", delay = 100, delayType = "debounce")),
                                                   uiOutput("balancePlotCaption"),
                                                   fluidRow(
                                                     column(6, sliderInput("width_bal","Plot width", min = 2, max = 20, value = 4)),
                                                     column(6, sliderInput("height_bal","Plot height", min = 2, max = 15, value = 4))
                                                   ),
                                                   div(style = "display: inline-block;vertical-align:top;",
                                                       downloadButton("downloadBalancePlotPng", label = "Download plot as PNG"),
                                                       downloadButton("downloadBalancePlotEmf", label = "Download plot as EMF")
                                                   )),
                                          tabPanel("Power",
                                                   radioButtons("database_power", "Database", names.study, names.study[1], inline = T),
                                                   uiOutput("powerTableCaption"),
                                                   tableOutput("powerTable"),
                                                   uiOutput("timeAtRiskTableCaption"),
                                                   tableOutput("timeAtRiskTable")
                                          ),
                                          tabPanel("Systematic error",
                                                   radioButtons("database_sys", "Database", names.study, names.study[1], inline = T),
                                                   plotOutput("systematicErrorPlot"),
                                                   div(strong("Figure 4."),"Systematic error. Effect size estimates for the negative controls (true hazard ratio = 1)
                                                                                    and positive controls (true hazard ratio > 1), before and after calibration. Estimates below the diagonal dashed
                                                                                    lines are statistically significant (alpha = 0.05) different from the true effect size. A well-calibrated
                                                                                    estimator should have the true effect size within the 95 percent confidence interval 95 percent of times."),
                                                   fluidRow(
                                                     column(6, sliderInput("width_sys","Plot width", min = 2, max = 20, value = 12)),
                                                     column(6, sliderInput("height_sys","Plot height", min = 2, max = 15, value = 5.5, step = 0.5))
                                                   ),
                                                   div(style = "display: inline-block;vertical-align:top;",
                                                       downloadButton("downloadSystematicErrorPlotPng", label = "Download plot as PNG"),
                                                       downloadButton("downloadSystematicErrorPlotEmf", label = "Download plot as EMF")
                                                   )),
                                          tabPanel("Various outcome",
                                                   radioButtons("database_vout", "Database", names.study, names.study[1], inline = T),
                                                   plotOutput("forest_vout"),
                                                   fluidRow(
                                                     column(6, sliderInput("width_vout","Plot width", min = 2, max = 20, value = 12)),
                                                     column(6, sliderInput("height_vout","Plot height", min = 2, max = 15, value = 5.5, step = 0.5))
                                                   ),
                                                   div(style = "display: inline-block;vertical-align:top;",
                                                       downloadButton("downloadvoutPlotPng", label = "Download plot as PNG"),
                                                       downloadButton("downloadvoutPlotEmf", label = "Download plot as EMF")
                                                   )),
                                          tabPanel("HR distribution",
                                                   plotOutput("RrDistr"),
                                                   fluidRow(
                                                     column(6, sliderInput("width_rrdistr","Plot width", min = 2, max = 20, value = 12)),
                                                     column(6, sliderInput("height_rrdistr","Plot height", min = 2, max = 15, value = 5.5, step = 0.5))
                                                   ),
                                                   div(style = "display: inline-block;vertical-align:top;",
                                                       downloadButton("downloadrrdistrPlotPng", label = "Download plot as PNG"),
                                                       downloadButton("downloadrrdistrPlotEmf", label = "Download plot as EMF")
                                                   )),
                                          tabPanel("Sensitivity analysis",
                                                   radioButtons("database_sens", "Database", c("Meta", names.study), names.study[1], inline = T),
                                                   conditionalPanel('input.database_sens == "Meta"',
                                                                    selectInput("database_sens_meta", "Database to include", names.study, names.study, multiple = T)),
                                                   selectInput("outcomegroup_sens", "Outcome Group", names(type.cancer), names(type.cancer)[1]),
                                                   plotOutput("Sensitivity"),
                                                   sliderInput("xmax_sen", "max HR in plot", min = 1.2, max = 3, value = 2, step = 0.1),
                                                   fluidRow(
                                                     column(6, sliderInput("width_sens","Plot width", min = 2, max = 20, value = 12)),
                                                     column(6, sliderInput("height_sens","Plot height", min = 2, max = 15, value = 12, step = 0.5))
                                                   ),
                                                   div(style = "display: inline-block;vertical-align:top;",
                                                       downloadButton("downloadsensPlotPng", label = "Download plot as PNG"),
                                                       downloadButton("downloadsensPlotEmf", label = "Download plot as EMF")
                                                   )),
                                          tabPanel("Other drug",
                                                   radioButtons("database_othdrug", "Database", c("Meta", names.study), names.study[1], inline = T),
                                                   plotOutput("Otherdrug"),
                                                   fluidRow(
                                                     column(6, sliderInput("width_othdrug","Plot width", min = 2, max = 20, value = 12)),
                                                     column(6, sliderInput("height_othdrug","Plot height", min = 2, max = 15, value = 5.5, step = 0.5))
                                                   ),
                                                   div(style = "display: inline-block;vertical-align:top;",
                                                       downloadButton("downloadothdrugPlotPng", label = "Download plot as PNG"),
                                                       downloadButton("downloadothdrugPlotEmf", label = "Download plot as EMF")
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
                               targetId = as.numeric(input$target_tb1), comparatorId = as.numeric(input$comparator_tb1), analysisId = as.numeric(input$analysis_tb1), outcomeId = as.numeric(input$outcome_tb1))
    })%>% bindCache(input$target_tb1, input$comparator_tb1, input$input$analysis_tb1, input$outcome_tb1)
  
  

  ## N info
  nn <- reactive({
    data.result[target_id == as.numeric(input$target_tb1) & comparator_id == as.numeric(input$comparator_tb1) & analysis_id == as.numeric(input$analysis_tb1) & outcome_id == as.numeric(input$outcome_tb1), c("target_subjects", "comparator_subjects")]
  })
  
  nn.original <- reactive({
    data.result[target_id == as.numeric(input$target_tb1) & comparator_id == as.numeric(input$comparator_tb1) & analysis_id == analysis.originalN[as.character(input$analysis_tb1)] & outcome_id == as.numeric(input$outcome_tb1), c("target_subjects", "comparator_subjects")]
    
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
    fname <- paste0(names(which(list.idinfo$exposure == input$target_tb1)), "_", names(which(list.idinfo$exposure == input$comparator_tb1)), "_",
                    names(which(list.idinfo$outcome == input$outcome_tb1)), "_", names(which(list.idinfo$analysis == input$analysis_tb1)))
    if (input$analysis_tb1 %in% unique(analysis.originalN)) return(NULL)
    
    names.tc <- c(names(which(list.idinfo$exposure == input$target_tb1)), names(which(list.idinfo$exposure == input$comparator_tb1)))
    if (input$database_tb1 == "All"){
      out.tb1 <- prepareTable1(balance = tb1.aggre())[-c(1:2), ]
      colnames(out.tb1) <- c("", paste0(names.tc, "(n = ", colSums(nn.original()),")"), "Standardized difference", paste0(names.tc, "(n = ", colSums(nn()),")"), "Standardized difference")
      fname <- paste0("tb1_All_", fname)
    } else{
      out.tb1 <- prepareTable1(balance = getbalance()[databaseId == input$database_tb1])[-c(1:2), ]
      colnames(out.tb1) <- c("", paste0(names.tc, "(n = ", colSums(nn.original()[which(names.study == input$database_tb1)]),")"), "Standardized difference", paste0(names.tc, "(n = ", colSums(nn()[which(names.study == input$database_tb1)]),")"), "Standardized difference")
      fname <- paste0("tb1_", input$database_tb1, "_", fname)
    }
    
    datatable(out.tb1, rownames = F, extensions = 'Buttons', class="compact",
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
    DM <- data.result[database_id %in% input$database_meta & target_id == as.numeric(input$target_tb1) & comparator_id == as.numeric(input$comparator_tb1) & analysis_id == as.numeric(input$analysis_tb1) & outcome_id == as.numeric(input$outcome_tb1)]
    out.meta <- metagen(TE = DM$log_rr, seTE = DM$se_log_rr, studlab = DM$database_id, sm = "HR", hakn = F, comb.fixed = TRUE,comb.random = TRUE)
    if (grepl("interaction", names(list.idinfo$analysis)[as.numeric(input$analysis_tb1)])){
      DM <- data.interaction[database_id %in% input$database_meta & target_id == as.numeric(input$target_tb1) & comparator_id == as.numeric(input$comparator_tb1) & analysis_id == as.numeric(input$analysis_tb1) & outcome_id == as.numeric(input$outcome_tb1)]
      out.meta <- metagen(TE = DM$log_rrr, seTE = DM$se_log_rrr, studlab = DM$database_id, sm = "HR", hakn = F, comb.fixed = TRUE,comb.random = TRUE)
    }
    
  
    
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
           label.left = paste0("Favor\n", obj.meta()$vname[1]), label.right = paste0("Favor\n", obj.meta()$vname[2]), scientific.pval = F, big.mark =","
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
      fname <- paste0("forestplot_", names(which(list.idinfo$exposure == input$target_tb1)), "_", names(which(list.idinfo$exposure == input$comparator_tb1)), "_",
                      names(which(list.idinfo$outcome == input$outcome_tb1)), "_", names(which(list.idinfo$analysis == input$analysis_tb1)), ".", input$forest_file_ext)

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
                              label.left = paste0("Favor\n", obj.meta()$vname[1]), label.right = paste0("Favor\n", obj.meta()$vname[2]), scientific.pval = F, big.mark =","
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
                              label.left = paste0("Favor\n", obj.meta()$vname[1]), label.right = paste0("Favor\n", obj.meta()$vname[2]), scientific.pval = F, big.mark =","
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
                              label.left = paste0("Favor\n", obj.meta()$vname[1]), label.right = paste0("Favor\n", obj.meta()$vname[2]), scientific.pval = F, big.mark =","
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
                              label.left = paste0("Favor\n", obj.meta()$vname[1]), label.right = paste0("Favor\n", obj.meta()$vname[2]), scientific.pval = F, big.mark =","
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
                              label.left = paste0("Favor\n", obj.meta()$vname[1]), label.right = paste0("Favor\n", obj.meta()$vname[2]), scientific.pval = F, big.mark =","
                       )
                       dev.off()
                       
                     } 
                     
                   })
      
      
    })
  
  ## KM
  
  getkm <- reactive({
    km <- data.km[database_id == input$database_kap & target_id == as.numeric(input$target_tb1) & comparator_id == as.numeric(input$comparator_tb1) & analysis_id == as.numeric(input$analysis_tb1) & outcome_id == as.numeric(input$outcome_tb1)]
    return(km)
  })
  
  
  output$x_km <- renderUI({
    sliderInput("xmax_km", "X axis range", min = 0, max = max(getkm()$time), value = c(0, max(getkm()$time)), step = 5)
    
  })
  
  kaplanMeierPlot <- reactive({
    km <- getkm()
    plot <- plotKaplanMeier(kaplanMeier = subset(km, time >= input$xmax_km[1] & time <= input$xmax_km[2]),
                            targetName = names(which(list.idinfo$exposure == input$target_tb1)),
                            comparatorName = names(which(list.idinfo$exposure == input$comparator_tb1)), ymin = input$ymin_km, ci = input$ci_km, cum_inc = input$cumulative_km, percent = input$percent_km, year = input$yearx_km)
    return(plot)
  })
  
  output$kaplanMeierPlot <- renderPlot({
    return(kaplanMeierPlot())
  }, res = 100)
  
  
  output$downloadKaplanMeierPlotPng <- downloadHandler(filename = paste0("km_", input$database_kap, "_", names(which(list.idinfo$exposure == input$target_tb1)), "_", names(which(list.idinfo$exposure == input$comparator_tb1)), "_",
                                                                         names(which(list.idinfo$outcome == input$outcome_tb1)), "_", names(which(list.idinfo$analysis == input$analysis_tb1)), ".png"), 
                                                       contentType = "image/png", 
                                                       content = function(file) {
                                                         ggplot2::ggsave(file, plot = kaplanMeierPlot(), width = input$width_km, height = input$height_km, dpi = 600)
                                                       })
  
  output$downloadKaplanMeierPlotEmf <- downloadHandler(filename = paste0("km_", input$database_kap, "_", names(which(list.idinfo$exposure == input$target_tb1)), "_", names(which(list.idinfo$exposure == input$comparator_tb1)), "_",
                                                                         names(which(list.idinfo$outcome == input$outcome_tb1)), "_", names(which(list.idinfo$analysis == input$analysis_tb1)), ".emf"), 
                                                       contentType = "application/emf", 
                                                       content = function(file) {
                                                         devEMF::emf(file, width = input$width_km, height = input$height_km, emfPlus = F, coordDPI = 600)
                                                         plot(kaplanMeierPlot())
                                                         dev.off()
                                                       })
  
  
  output$kaplanMeierPlotPlotCaption <- renderUI({
    text <- "<strong>Figure 5.</strong> Kaplan Meier plot, showing survival as a function of time. This plot
      is adjusted using the propensity score: The target curve (<em>%s</em>) shows the actual observed survival. The
      comparator curve (<em>%s</em>) applies reweighting to approximate the counterfactual of what the target survival
      would look like had the target cohort been exposed to the comparator instead. The shaded area denotes
      the 95 percent confidence interval."
    return(HTML(sprintf(text, input$target, input$comparator)))
  })
  
  
  
  psDistPlot <- reactive({
    if (input$analysis_tb1 %in% unique(analysis.originalN)) return(NULL)
    ps <- data.ps[database_id == input$database_ps & target_id == as.numeric(input$target_tb1) & comparator_id == as.numeric(input$comparator_tb1) & analysis_id == as.numeric(input$analysis_tb1)]
    plot <- plotPs(ps, names(which(list.idinfo$exposure == input$target_tb1)), names(which(list.idinfo$exposure == input$comparator_tb1)))
    return(plot)
  })
  
  output$psDistPlot <- renderPlot({
    return(psDistPlot())
  })
  
  output$downloadPsDistPlotPng <- downloadHandler(filename = paste0("ps_", input$database_ps, "_", names(which(list.idinfo$exposure == input$target_tb1)), "_", names(which(list.idinfo$exposure == input$comparator_tb1)), "_",
                                                                     "_", names(which(list.idinfo$analysis == input$analysis_tb1)), ".png"), 
                                                  contentType = "image/png", 
                                                  content = function(file) {
                                                    ggplot2::ggsave(file, plot = psDistPlot(), width = input$width_ps, height = input$height_ps, dpi = 600)
                                                  })
  
  output$downloadPsDistPlotEmf <- downloadHandler(filename = paste0("ps_", input$database_ps, "_", names(which(list.idinfo$exposure == input$target_tb1)), "_", names(which(list.idinfo$exposure == input$comparator_tb1)), "_",
                                                                    "_", names(which(list.idinfo$analysis == input$analysis_tb1)), ".emf"), 
                                                  contentType = "application/emf", 
                                                  content = function(file) {
                                                    devEMF::emf(file, width = input$width_ps, height = input$height_ps, emfPlus = T, coordDPI = 600)
                                                    plot(psDistPlot())
                                                    dev.off()
                                                  })
  

  
  balancePlot <- reactive({
    writeLines("Plotting covariate balance")
    plot <- plotCovariateBalanceScatterPlot(balance = getbalance()[databaseId == input$database_bal],
                                            beforeLabel = "Before propensity score adjustment",
                                            afterLabel = "After propensity score adjustment")
    return(plot)
  })
  
  output$balancePlot <- renderPlot({
    return(balancePlot())
  })
  
  output$downloadBalancePlotPng <- downloadHandler(filename = paste0("balanceplot_", input$database_bal, "_", names(which(list.idinfo$exposure == input$target_tb1)), "_", names(which(list.idinfo$exposure == input$comparator_tb1)), "_",
                                                                     names(which(list.idinfo$outcome == input$outcome_tb1)), "_", names(which(list.idinfo$analysis == input$analysis_tb1)), ".png"), 
                                                   contentType = "image/png", 
                                                   content = function(file) {
                                                     ggplot2::ggsave(file, plot = balancePlot(),  width = input$width_bal, height = input$height_bal, dpi = 600)
                                                   })
  
  output$downloadBalancePlotEmf <- downloadHandler(filename = paste0("balanceplot_", input$database_bal, "_", names(which(list.idinfo$exposure == input$target_tb1)), "_", names(which(list.idinfo$exposure == input$comparator_tb1)), "_",
                                                                     names(which(list.idinfo$outcome == input$outcome_tb1)), "_", names(which(list.idinfo$analysis == input$analysis_tb1)), ".emf"), 
                                                   contentType = "application/emf", 
                                                   content = function(file) {
                                                     devEMF::emf(file, width = input$width_bal, height = input$height_bal, emfPlus = F, coordDPI = 600)
                                                     plot(balancePlot())
                                                     dev.off()
                                                     })
  
  output$balancePlotCaption <- renderUI({
    text <- "<strong>Figure 3.</strong> Covariate balance before and after propensity score adjustment. Each dot represents
      the standardizes difference of means for a single covariate before and after propensity score adjustment on the propensity
      score. Move the mouse arrow over a dot for more details."
    return(HTML(sprintf(text)))
  })
  
  output$hoverInfoBalanceScatter <- renderUI({
    
    hover <- input$plotHoverBalanceScatter
    point <- nearPoints(getbalance()[databaseId == input$database_bal], hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) {
      return(NULL)
    }
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:",
                    left_px - 251,
                    "px; top:",
                    top_px - 150,
                    "px; width:500px;")
    beforeMatchingStdDiff <- formatC(point$beforeMatchingStdDiff, digits = 2, format = "f")
    afterMatchingStdDiff <- formatC(point$afterMatchingStdDiff, digits = 2, format = "f")
    div(
      style = "position: relative; width: 0; height: 0",
      wellPanel(
        style = style,
        p(HTML(paste0("<b> Covariate: </b>", point$covariateName, "<br/>",
                      "<b> Std. diff before ",tolower(row$psStrategy),": </b>", beforeMatchingStdDiff, "<br/>",
                      "<b> Std. diff after ",tolower(row$psStrategy),": </b>", afterMatchingStdDiff)))
      )
    )
  })
  
  
  output$powerTableCaption <- renderUI({
    text <- "<strong>Table 1a.</strong> Number of subjects, follow-up time (in years), number of outcome
      events, and event incidence rate (IR) per 1,000 patient years (PY) in the target (<em>%s</em>) and
      comparator (<em>%s</em>) group after propensity score adjustment, as  well as the minimum detectable  relative risk (MDRR).
      Note that the IR does not account for any stratification."
    return(HTML(sprintf(text, names(which(list.idinfo$exposure == input$target_tb1)), names(which(list.idinfo$exposure == input$comparator_tb1)))))
  })
  
  
  ## Power
  output$powerTable <- renderTable({
    res <- data.result[database_id == input$database_power & target_id == as.numeric(input$target_tb1) & comparator_id == as.numeric(input$comparator_tb1) & analysis_id == as.numeric(input$analysis_tb1) & outcome_id == as.numeric(input$outcome_tb1)]
    
    table <- preparePowerTable(res)
    table$description <- NULL
    colnames(table) <- c("Target subjects",
                         "Comparator subjects",
                         "Target years",
                         "Comparator years",
                         "Target events",
                         "Comparator events",
                         "Target IR (per 1,000 PY)",
                         "Comparator IR (per 1,000 PY)",
                         "MDRR")
    return(table[, 1:9])
  })
  
  output$timeAtRiskTableCaption <- renderUI({
    text <- "<strong>Table 1b.</strong> Time (days) at risk distribution expressed as
      minimum (min), 25th percentile (P25), median, 75th percentile (P75), and maximum (max) in the target
     (<em>%s</em>) and comparator (<em>%s</em>) cohort after propensity score adjustment."
    return(HTML(sprintf(text, names(which(list.idinfo$exposure == input$target_tb1)), names(which(list.idinfo$exposure == input$comparator_tb1)))))
  })
  
  output$timeAtRiskTable <- renderTable({
    followUpDist <- data.fudist[database_id == input$database_kap & target_id == as.numeric(input$target_tb1) & comparator_id == as.numeric(input$comparator_tb1) & analysis_id == as.numeric(input$analysis_tb1) & outcome_id == as.numeric(input$outcome_tb1)]
    table <- prepareFollowUpDistTable(followUpDist)
    return(table)
  })
  
  ## Systematic error
  
  systematicErrorPlot <- reactive({
    results <- data.result[database_id == input$database_sys & target_id == as.numeric(input$target_tb1) & comparator_id == as.numeric(input$comparator_tb1) & analysis_id == as.numeric(input$analysis_tb1)]
    negativeControlOutcome <- data.negres[database_id == input$database_sys]
    
    results$effectSize <- NA
    idx <- results$outcome_id %in% negativeControlOutcome$outcome_id
    results$effectSize[idx] <- 1
    #if (!is.null(positiveControlOutcome)) {
    #  idx <- results$outcomeId %in% positiveControlOutcome$outcomeId
    #  results$effectSize[idx] <- positiveControlOutcome$effectSize[match(results$outcomeId[idx],
    #                                                                     positiveControlOutcome$outcomeId)]
    #}
    results <- results[!is.na(results$effectSize), ]
    
    plot <- plotScatter(results)
    return(plot)
  })
  
  output$systematicErrorPlot <- renderPlot({
    return(systematicErrorPlot())
  })
  
  output$downloadSystematicErrorPlotPng <- downloadHandler(filename = paste0("systematicerror", input$database_sys, "_", names(which(list.idinfo$exposure == input$target_tb1)), "_", names(which(list.idinfo$exposure == input$comparator_tb1)), "_",
                                                                              names(which(list.idinfo$analysis == input$analysis_tb1)), ".png"), 
                                                           contentType = "image/png", 
                                                           content = function(file) {
                                                             ggplot2::ggsave(file, plot = systematicErrorPlot(), width = input$width_sys, height = input$height_sys, dpi = 600)
                                                           })
  
  output$downloadSystematicErrorPlotEmf <- downloadHandler(filename = paste0("systematicerror", input$database_sys, "_", names(which(list.idinfo$exposure == input$target_tb1)), "_", names(which(list.idinfo$exposure == input$comparator_tb1)), "_",
                                                                              names(which(list.idinfo$analysis == input$analysis_tb1)), ".emf"), 
                                                           contentType = "application/emf", 
                                                           content = function(file) {
                                                             devEMF::emf(file, width = input$width_sys, height = input$height_sys, emfPlus = F, coordDPI = 600)
                                                             plot(systematicErrorPlot())
                                                             dev.off()
                                                           })
  
  obj.vout <- reactive({
    res <- data.result[database_id == input$database_vout & target_id == as.numeric(input$target_tb1) & comparator_id == as.numeric(input$comparator_tb1) & (outcome_id %in% list.idinfo$outcome) & analysis_id == as.numeric(input$analysis_tb1)][!is.na(rr) & !is.na(ci_95_lb) & !is.na(ci_95_ub)][order(outcome_id)]
    
    tabletext <- cbind(c("Outcome","\n", unlist(sapply(res$outcome_id, function(x){names(list.idinfo$outcome)[which(list.idinfo$outcome == x)]}))),
                       c("HR(95% CI)", "\n", paste0(round(res$rr, 2), "(", round(res$ci_95_lb, 2), "-", round(res$ci_95_ub), ")")),
                       c("P Value","\n",ifelse(res$p >= 0.001, round(res$p, 3), "< 0.001")))
    
    tabletext <- tabletext[, c(1,2, 3)]
    return(list(res = res, tabletext = tabletext))
  })
  
  voutPlot <- reactive({
    res <- obj.vout()$res
    tabletext <- obj.vout()$tabletext
    ## Save as tiff 
    forestplot::forestplot(labeltext=tabletext, graph.pos=2, xticks = c(0.1, 0.5, 1, 2, 10), xlog= T, align = c("r", rep("c", ncol(tabletext) - 1)),                          ## graph.pos- column number
                           mean=c(NA,NA,as.numeric(res$rr)), 
                           lower=c(NA,NA,as.numeric(res$ci_95_lb)), upper=c(NA,NA,as.numeric(res$ci_95_ub)),
                           title="Hazard Ratio",
                           xlab="<---Favor Ranitidine ---    ---Favor Others --->",    ## You cas modify this.
                           hrzl_lines=list("3" = gpar(lwd=1, col="#99999922")
                           ),
                           
                           txt_gp=fpTxtGp(label=gpar(cex=1.25),
                                          ticks=gpar(cex=1.1),
                                          xlab=gpar(cex = 1.2),
                                          title=gpar(cex = 1.2)),
                           col=fpColors(box="black", lines="black", zero = "gray50"),
                           zero=1, cex=0.9, lineheight = "auto", boxsize=0.2, colgap=unit(6,"mm"),
                           lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.16) 
  
  })
  
  
  output$forest_vout <- renderPlot({
    return(voutPlot())
  })
  
  output$downloadvoutPlotPng <- downloadHandler(filename = paste0("forestoutcomes", input$database_vout, ".png"), 
                                                           contentType = "image/png", 
                                                           content = function(file) {
                                                             grDevices::png(file, width = input$width_vout, height = input$width_vout, units = "in")
                                                             res <- obj.vout()$res
                                                             tabletext <- obj.vout()$tabletext
                                                             ## Save as tiff 
                                                             forestplot::forestplot(labeltext=tabletext, graph.pos=2, xticks = c(0.1, 0.5, 1, 2, 10), xlog= T, align = c("r", rep("c", ncol(tabletext) - 1)),                          ## graph.pos- column number
                                                                                    mean=c(NA,NA,as.numeric(res$rr)), 
                                                                                    lower=c(NA,NA,as.numeric(res$ci_95_lb)), upper=c(NA,NA,as.numeric(res$ci_95_ub)),
                                                                                    title="Hazard Ratio",
                                                                                    xlab="<---Favor Ranitidine ---    ---Favor Others --->",    ## You cas modify this.
                                                                                    hrzl_lines=list("3" = gpar(lwd=1, col="#99999922")
                                                                                    ),
                                                                                    
                                                                                    txt_gp=fpTxtGp(label=gpar(cex=1.25),
                                                                                                   ticks=gpar(cex=1.1),
                                                                                                   xlab=gpar(cex = 1.2),
                                                                                                   title=gpar(cex = 1.2)),
                                                                                    col=fpColors(box="black", lines="black", zero = "gray50"),
                                                                                    zero=1, cex=0.9, lineheight = "auto", boxsize=0.2, colgap=unit(6,"mm"),
                                                                                    lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.16) 
                                                             dev.off()
                                                           })
  
  output$downloadvoutPlotEmf <- downloadHandler(filename = paste0("forestoutcomes", input$database_vout, ".emf"), 
                                                           contentType = "application/emf", 
                                                           content = function(file) {
                                                             devEMF::emf(file, width = input$width_vout, height = input$width_vout, emfPlus = F, coordDPI = 600)
                                                             res <- obj.vout()$res
                                                             tabletext <- obj.vout()$tabletext
                                                             ## Save as tiff 
                                                             forestplot::forestplot(labeltext=tabletext, graph.pos=2, xticks = c(0.1, 0.5, 1, 2, 10), xlog= T, align = c("r", rep("c", ncol(tabletext) - 1)),                          ## graph.pos- column number
                                                                                    mean=c(NA,NA,as.numeric(res$rr)), 
                                                                                    lower=c(NA,NA,as.numeric(res$ci_95_lb)), upper=c(NA,NA,as.numeric(res$ci_95_ub)),
                                                                                    title="Hazard Ratio",
                                                                                    xlab="<---Favor Ranitidine ---    ---Favor Others --->",    ## You cas modify this.
                                                                                    hrzl_lines=list("3" = gpar(lwd=1, col="#99999922")
                                                                                    ),
                                                                                    
                                                                                    txt_gp=fpTxtGp(label=gpar(cex=1.25),
                                                                                                   ticks=gpar(cex=1.1),
                                                                                                   xlab=gpar(cex = 1.2),
                                                                                                   title=gpar(cex = 1.2)),
                                                                                    col=fpColors(box="black", lines="black", zero = "gray50"),
                                                                                    zero=1, cex=0.9, lineheight = "auto", boxsize=0.2, colgap=unit(6,"mm"),
                                                                                    lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.16) 
                                                             dev.off()
                                                           })
  
  
  obj.RrDistr <- reactive({
    res <- data.result[target_id == as.numeric(input$target_tb1) & comparator_id == as.numeric(input$comparator_tb1) & (outcome_id == input$outcome_tb1) & !(analysis_id %in% c(1998,2998, 3998, 4998, 5998))]
    
    #nace$databaseId <- factor(nace$databaseId, level = c("OptumPanTher","IQVIA - Hospital", "HIRA", "Meta-analysis"))
    resBeforeCal <- res[, .SD, .SDcols = c("target_id", "comparator_id", "outcome_id", "analysis_id", "rr", "ci_95_lb", "ci_95_ub", "p", "database_id")][, Calibration := "Before calibration"][]
    resAfterCal <- res[, .SD, .SDcols = c("target_id", "comparator_id", "outcome_id", "analysis_id", "calibrated_rr", "calibrated_ci_95_lb", "calibrated_ci_95_ub", "calibrated_p", "database_id")][, Calibration := "After calibration"][]
    names(resAfterCal)[5:8] <- c("rr", "ci_95_lb", "ci_95_ub", "p")
      
    resCal <- rbind(resBeforeCal, resAfterCal)
    resCal$Calibration <- factor(resCal$Calibration, level = c("Before calibration", "After calibration"))
    
    primaryRr<- resCal[analysis_id == 42]
    
    customLimit = c(0.60,1.7)
    customBreaks = c(0.60,0.75,0.9,1.0,1.1, 1.3,1.7)
    
    RrDistr<-ggplot(resCal, aes(x=rr,fill = Calibration, color = Calibration)) +
      geom_histogram(#fill="white",
        alpha = 0.3, position="identity", bins=50) +
      geom_vline(data = primaryRr, aes(xintercept=rr, color = Calibration)) +
      geom_vline(aes(xintercept=1.0), linetype="dashed") +
      facet_grid(database_id~.)+
      ggplot2::theme_bw()+
      scale_x_continuous(trans=log10_trans(), limits= customLimit,breaks =customBreaks
      )+
      xlab('Hazard ratio')+ ylab("Count")
    return(RrDistr)
  })
  
  output$RrDistr <- renderPlot({
    return(obj.RrDistr())
  })
  
  output$downloadrrdistrPlotPng <- downloadHandler(filename = paste0("obj.RrDistr_", names(which(list.idinfo$exposure == input$target_tb1)), "_", names(which(list.idinfo$exposure == input$comparator_tb1)), "_",
                                                                             names(which(list.idinfo$outcome == input$outcome)), ".png"), 
                                                           contentType = "image/png", 
                                                           content = function(file) {
                                                             ggplot2::ggsave(file, plot = obj.RrDistr(), width = input$width_rrdistr, height = input$height_rrdistr, dpi = 600)
                                                           })
  
  output$downloadrrdistrPlotEmf <- downloadHandler(filename = paste0("obj.RrDistr_", names(which(list.idinfo$exposure == input$target_tb1)), "_", names(which(list.idinfo$exposure == input$comparator_tb1)), "_",
                                                                             names(which(list.idinfo$outcome == input$outcome)), ".emf"), 
                                                           contentType = "application/emf", 
                                                           content = function(file) {
                                                             devEMF::emf(file, width = input$width_rrdistr, height = input$height_rrdistr, emfPlus = F, coordDPI = 600)
                                                             plot(obj.RrDistr())
                                                             dev.off()
                                                           })
  
  
  obj.sens <- reactive({
    limits <- c(1/input$xmax_sen, input$xmax_sen)
    res0 <- data.result[!is.na(rr) & target_id == as.numeric(input$target_tb1) & comparator_id == as.numeric(input$comparator_tb1) & (outcome_id %in% type.cancer[[input$outcomegroup_sens]]) & !(analysis_id %in% c(1998, 2998, 3998, 4998, 5998))]
    if (input$database_sens != "Meta"){
      res <- res0[database_id == input$database_sens] %>% merge(type.analysis, by = "analysis_id", all.x = T)
    } else{
      res0 <- res0[database_id %in% input$database_sens_meta]
      pairs <- res0[, .N, keyby = c("outcome_id", "analysis_id")]
      res <- mapply(function(x, y){
        obj.metagen <-metagen(TE = log_rr, seTE = se_log_rr, data = res0[outcome_id == x & analysis_id == y])
        return(exp(c("rr" = obj.metagen$TE.random, "ci_95_lb" = obj.metagen$lower.random, "ci_95_ub" = obj.metagen$upper.random, "p" = log(obj.metagen$pval.fixed))))
      }, pairs$outcome_id, pairs$analysis_id) %>% t %>% cbind(pairs[, 1:2]) %>% merge(type.analysis, by = "analysis_id", all.x = T)
      
    }
    
    res$outcomeName <- factor(sapply(res$outcome_id, function(x){names(list.idinfo$outcome)[which(list.idinfo$outcome == x)]}))
    res$Significance<-factor(ifelse(res$p<0.05,"P<.05","Not significant"), levels = c("P<.05","Not significant"))
    res[, `:=`(ci95LbOut = ifelse(ci_95_lb < limits[1], rr - limits[1], NA), 
               ci95UbOut = ifelse(ci_95_ub > limits[2], rr - limits[2], NA))]
    
    return(gridForest(res, xLimits = limits))
    
  })
  
  output$Sensitivity <- renderPlot({
    return(obj.sens())
  })
  
  output$downloadsensPlotPng <- downloadHandler(filename = paste0("Sensitivity", input$database_sens, "_", names(which(list.idinfo$exposure == input$target_tb1)), "_", names(which(list.idinfo$exposure == input$comparator_tb1)), "_",
                                                                             input$outcomegroup_sens, ".png"), 
                                                           contentType = "image/png", 
                                                           content = function(file) {
                                                             ggplot2::ggsave(file, plot = obj.sens(), width = input$width_sens, height = input$height_sens, dpi = 600)
                                                           })
  
  output$downloadsensPlotEmf <- downloadHandler(filename = paste0("Sensitivity", input$database_sens, "_", names(which(list.idinfo$exposure == input$target_tb1)), "_", names(which(list.idinfo$exposure == input$comparator_tb1)), "_",
                                                                  input$outcomegroup_sens, ".emf"), 
                                                           contentType = "application/emf", 
                                                           content = function(file) {
                                                             devEMF::emf(file, width = input$width_sens, height = input$height_sens, emfPlus = F, coordDPI = 600)
                                                             plot(obj.sens())
                                                             dev.off()
                                                           })
  
  
  obj.othdrug <- reactive({
    
  })
  
  output$Otherdrug <- renderPlot({
    return(obj.othdrug())
  })
  
  output$downloadothdrugPlotPng <- downloadHandler(filename = paste0("Otherdrug", input$database_sys, "_", names(which(list.idinfo$exposure == input$target_tb1)), "_", names(which(list.idinfo$exposure == input$comparator_tb1)), "_",
                                                                             names(which(list.idinfo$outcome == input$outcome_tb1)), ".png"), 
                                                           contentType = "image/png", 
                                                           content = function(file) {
                                                             ggplot2::ggsave(file, plot = obj.othdrug(), width = input$width_othdrug, height = input$height_othdrug, dpi = 600)
                                                           })
  
  output$downloadothdrugPlotEmf <- downloadHandler(filename = paste0("Otherdrug", input$database_sys, "_", names(which(list.idinfo$exposure == input$target_tb1)), "_", names(which(list.idinfo$exposure == input$comparator_tb1)), "_",
                                                                             names(which(list.idinfo$outcome == input$outcome_tb1)), ".emf"), 
                                                           contentType = "application/emf", 
                                                           content = function(file) {
                                                             devEMF::emf(file, width = input$width_othdrug, height = input$height_othdrug, emfPlus = F, coordDPI = 600)
                                                             plot(obj.othdrug())
                                                             dev.off()
                                                           })
  
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  
}

shinyApp(ui, server)
