

plotPs <- function(ps, targetName, comparatorName,
                   showEquiposeLabel = TRUE, equipoiseBounds = c(0.3,0.7)) {
  psOrigin <- ps
  ps <- rbind(data.frame(x = ps$preference_score, y = ps$target_density, group = targetName),
              data.frame(x = ps$preference_score, y = ps$comparator_density, group = comparatorName))
  ps$group <- factor(ps$group, levels = c(as.character(targetName), as.character(comparatorName)))
  theme <- ggplot2::element_text(colour = "#000000", size = 12, margin = ggplot2::margin(0, 0.5, 0, 0.1, "cm"))
  plot <- ggplot2::ggplot(ps,
                          ggplot2::aes(x = x, y = y, color = group, group = group, fill = group)) + ggplot2::theme_classic() +
    ggplot2::geom_density(stat = "identity") +
    ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                          rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                           rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_x_continuous("Preference score", limits = c(0, 1)) +
    ggplot2::scale_y_continuous("Density") +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "top",
                   legend.text = theme,
                   axis.text = theme,
                   axis.title = theme)
  if (showEquiposeLabel) {
    labelsLeft <- c()
    labelsRight <- c()
    if (showEquiposeLabel) {
      equiIndex <- psOrigin$preference_score>=equipoiseBounds[1] & psOrigin$preference_score<=equipoiseBounds[2]
      equipoise <- mean (sum(psOrigin$comparator_density[equiIndex]), sum(psOrigin$comparator_density[equiIndex]))/100
      labelsRight <- c(labelsRight, sprintf("%2.1f%% is in equipoise", 
                                            equipoise * 100))
    }
    if (length(labelsLeft) > 0) {
      dummy <- data.frame(text = paste(labelsLeft, collapse = "\n"))
      plot <- plot + ggplot2::geom_label(x = 0, #y = max(d$y) * 1.24, 
                                         hjust = "left", vjust = "top", alpha = 0.8, 
                                         ggplot2::aes(label = text), data = dummy, size = 3.5)
    }
    if (length(labelsRight) > 0) {
      dummy <- data.frame(text = paste(labelsRight, collapse = "\n"))
      plot <- plot + ggplot2::annotate("label", x = 1, y = max(ps$y) * 1, 
                                       hjust = "right", vjust = "top", 
                                       alpha = 0.8, 
                                       label = labelsRight,
                                       #ggplot2::aes(label = labelsRight),
                                       #ggplot2::aes(label = text), data = dummy, 
                                       size = 3.5)
      # plot <- plot + ggplot2::geom_label(x = 1, y = max(ps$y) * 1.24, 
      #                                    hjust = "right", vjust = "top", 
      #                                    alpha = 0.8, 
      #                                    ggplot2::aes(label = labelsRight),
      #                                    ggplot2::aes(label = text), data = dummy, 
      #                                    size = 3.5)
    }
  }
  return(plot)
}

plotCovariateBalanceScatterPlot <- function(balance, beforeLabel = "Before stratification", afterLabel = "After stratification") {
  limits <- c(min(c(balance$absBeforeMatchingStdDiff, balance$absAfterMatchingStdDiff),
                  na.rm = TRUE),
              max(c(balance$absBeforeMatchingStdDiff, balance$absAfterMatchingStdDiff),
                  na.rm = TRUE))
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  labels <- aggregate(covariateId ~ databaseId, balance, length)
  labelMaxs <- aggregate(absAfterMatchingStdDiff ~ databaseId, balance, max)
  labels$text <- sprintf("Number of covariates: %s\nmax(absolute):%.2f", format(labels$covariateId, big.mark = ",", scientific = FALSE),labelMaxs$absAfterMatchingStdDiff)
  plot <- ggplot2::ggplot(balance, ggplot2::aes(x = absBeforeMatchingStdDiff, y = absAfterMatchingStdDiff)) + ggplot2::theme_classic() +
    ggplot2::geom_point(color = rgb(0, 0, 0.8, alpha = 0.3), shape = 16, size = 2) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_hline(yintercept = 0.1, color="red", linetype="dashed") +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::scale_x_continuous(beforeLabel, limits = limits) +
    ggplot2::scale_y_continuous(afterLabel, limits = limits) +
    ggplot2::theme(text = theme) + 
    ggplot2::geom_text(x = limits[1] + 0.01,
                        y = limits[2],
                        hjust = "left",
                        vjust = "top",
                        alpha = 0.8,
                       ggplot2::aes(label = text),
                       data = labels,
                        size = 3.5)
  
  return(plot)
}

plotKaplanMeier <- function(kaplanMeier, targetName, comparatorName, ci = F, ymin, cum_inc = F, percent = F, year = F) {
  data <- rbind(data.frame(time = kaplanMeier$time,
                           s = kaplanMeier$target_survival_lb,
                           lower = kaplanMeier$target_survival_lb,
                           upper = kaplanMeier$target_survival_ub,
                           strata = paste0(" ", targetName, "    ")),
                data.frame(time = kaplanMeier$time,
                           s = kaplanMeier$comparator_survival,
                           lower = kaplanMeier$comparator_survival_lb,
                           upper = kaplanMeier$comparator_survival_ub,
                           strata = paste0(" ", comparatorName)))
  
  xlims <- c(-max(data$time)/40, max(data$time))
  ylims <- ymin
  xLabel <- "Time in days"
  yLabel <- "Survival probability"
  
  if (cum_inc == T){
    data$s <- 1 - data$s
    lower2 <- 1 - data$upper
    upper2 <- 1 - data$lower
    data$upper <- upper2
    data$lower <- lower2
    yLabel <- "Cumulative incidence"
  }
  
  
  xBreaks <- kaplanMeier$time[!is.na(kaplanMeier$target_at_risk)]
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = time,
                                             y = s,
                                             color = strata,
                                             fill = strata,
                                             ymin = lower,
                                             ymax = upper)) + 
    ggplot2::theme_classic() +
    #ggplot2::geom_ribbon(color = rgb(0, 0, 0, alpha = 0)) +
    ggplot2::geom_step(size = 1) +
    ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.8),
                                           rgb(0, 0, 0.8, alpha = 0.8))) +
    ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.3),
                                          rgb(0, 0, 0.8, alpha = 0.3))) +
    ggplot2::scale_x_continuous(xLabel, limits = xlims, breaks = xBreaks) +
    ggplot2::scale_y_continuous(yLabel, limits = ylims) +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   legend.position = "top",
                   legend.key.size = ggplot2::unit(1, "lines"),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::theme(axis.title.y = ggplot2::element_text(vjust = -10))
  
  if (ci){
    plot <- plot + ggplot2::geom_ribbon(color = rgb(0, 0, 0, alpha = 0))
  }
  
  if (percent){
    plot <- plot + ggplot2::scale_y_continuous(name = paste(yLabel, "(%)"), limits = ylims, labels = function(x) paste0(x*100, "%")) 
  }
  
  if (year){
    plot <- plot + ggplot2::scale_x_continuous(name = "Year", limits = xlims, breaks = xBreaks, labels = function(x) round(x/365, 1))
  }
  
  targetAtRisk <- kaplanMeier$target_at_risk[!is.na(kaplanMeier$target_at_risk)]
  comparatorAtRisk <- kaplanMeier$comparator_at_risk[!is.na(kaplanMeier$comparator_at_risk)]
  labels <- data.frame(x = c(0, xBreaks, xBreaks),
                       y = as.factor(c("Number at risk",
                                       rep(targetName, length(xBreaks)),
                                       rep(comparatorName, length(xBreaks)))),
                       label = c("",
                                 formatC(targetAtRisk, big.mark = ",", mode = "integer"),
                                 formatC(comparatorAtRisk, big.mark = ",", mode = "integer")))
  labels$y <- factor(labels$y, levels = c(comparatorName, targetName, "Number at risk"))
  dataTable <- ggplot2::ggplot(labels, ggplot2::aes(x = x, y = y, label = label)) + ggplot2::geom_text(size = 3.5, vjust = 0.5)  + ggplot2::scale_x_continuous(xLabel,
                                                                                                                                                              limits = xlims,
                                                                                                                                                              breaks = xBreaks) + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                                                                                                                                                     axis.text.x = ggplot2::element_text(color = "white"),
                                                                                                                                                                                                 axis.title.x = ggplot2::element_text(color = "white"),
                                                                                                                                                                                                 axis.title.y = ggplot2::element_blank(),
                                                                                                                                                                                                 axis.ticks = ggplot2::element_line(color = "white"))
  plots <- list(plot, dataTable)
  grobs <- widths <- list()
  for (i in 1:length(plots)) {
    grobs[[i]] <- ggplot2::ggplotGrob(plots[[i]])
    widths[[i]] <- grobs[[i]]$widths[2:5]
  }
  maxwidth <- do.call(grid::unit.pmax, widths)
  for (i in 1:length(grobs)) {
    grobs[[i]]$widths[2:5] <- as.list(maxwidth)
  }
  plot <- gridExtra::grid.arrange(grobs[[1]], grobs[[2]], heights = c(400, 100))
  return(plot)
}


## Power
preparePowerTable <- function(mainResults) {
  table <- data.frame(mainResults)
  alpha <- 0.05
  power <- 0.8
  z1MinAlpha <- qnorm(1 - alpha/2)
  zBeta <- -qnorm(1 - power)
  pA <- table$target_subjects/(table$target_subjects + table$comparator_subjects)
  pB <- 1 - pA
  totalEvents <- abs(table$target_outcomes) + (table$comparator_outcomes)
  table$mdrr <- exp(sqrt((zBeta + z1MinAlpha)^2/(totalEvents * pA * pB)))
  table$targetYears <- table$target_days/365.25
  table$comparatorYears <- table$comparator_days/365.25
  table$targetIr <- 1000 * table$target_outcomes/table$targetYears
  table$comparatorIr <- 1000 * table$comparator_outcomes/table$comparatorYears
  table <- table[, c("target_subjects",
                     "comparator_subjects",
                     "targetYears",
                     "comparatorYears",
                     "target_outcomes",
                     "comparator_outcomes",
                     "targetIr",
                     "comparatorIr",
                     "mdrr")]
  table$targetSubjects <- formatC(table$target_subjects, big.mark = ",", format = "d")
  table$comparatorSubjects <- formatC(table$comparator_subjects, big.mark = ",", format = "d")
  table$targetYears <- formatC(table$targetYears, big.mark = ",", format = "d")
  table$comparatorYears <- formatC(table$comparatorYears, big.mark = ",", format = "d")
  table$targetOutcomes <- formatC(table$target_outcomes, big.mark = ",", format = "d")
  table$comparatorOutcomes <- formatC(table$comparator_outcomes, big.mark = ",", format = "d")
  table$targetIr <- sprintf("%.2f", table$targetIr)
  table$comparatorIr <- sprintf("%.2f", table$comparatorIr)
  table$mdrr <- sprintf("%.2f", table$mdrr)
  table$targetSubjects <- gsub("^-", "<", table$targetSubjects)
  table$comparatorSubjects <- gsub("^-", "<", table$comparatorSubjects)
  table$targetOutcomes <- gsub("^-", "<", table$targetOutcomes)
  table$comparatorOutcomes <- gsub("^-", "<", table$comparatorOutcomes)
  table$targetIr <- gsub("^-", "<", table$targetIr)
  table$comparatorIr <- gsub("^-", "<", table$comparatorIr)
  idx <- (table$targetOutcomes < 0 | table$comparatorOutcomes < 0)
  table$mdrr[idx] <- paste0(">", table$mdrr[idx])
  return(table)
}


prepareFollowUpDistTable <- function(followUpDist) {
  targetRow <- data.frame(Cohort = "Target",
                          Min = followUpDist$target_min_days,
                          P10 = followUpDist$target_p10_days,
                          P25 = followUpDist$target_p25_days,
                          Median = followUpDist$target_median_days,
                          P75 = followUpDist$target_p75_days,
                          P90 = followUpDist$target_p90_days,
                          Max = followUpDist$target_max_days)
  comparatorRow <- data.frame(Cohort = "Comparator",
                              Min = followUpDist$comparator_min_days,
                              P10 = followUpDist$comparator_p10_days,
                              P25 = followUpDist$comparator_p25_days,
                              Median = followUpDist$comparator_median_days,
                              P75 = followUpDist$comparator_p75_days,
                              P90 = followUpDist$comparator_p90_days,
                              Max = followUpDist$comparator_max_days)
  table <- rbind(targetRow, comparatorRow)
  table$Min <- formatC(table$Min, big.mark = ",", format = "d")
  table$P10 <- formatC(table$P10, big.mark = ",", format = "d")
  table$P25 <- formatC(table$P25, big.mark = ",", format = "d")
  table$Median <- formatC(table$Median, big.mark = ",", format = "d")
  table$P75 <- formatC(table$P75, big.mark = ",", format = "d")
  table$P90 <- formatC(table$P90, big.mark = ",", format = "d")
  table$Max <- formatC(table$Max, big.mark = ",", format = "d")
  return(table)
}


## Systematic error

plotScatter <- function(controlResults) {
  size <- 2
  labelY <- 0.7
  d <- rbind(data.frame(yGroup = "Uncalibrated",
                        logRr = controlResults$log_rr,
                        seLogRr = controlResults$se_log_rr,
                        ci95Lb = controlResults$ci_95_lb,
                        ci95Ub = controlResults$ci_95_ub,
                        trueRr = controlResults$effectSize),
             data.frame(yGroup = "Calibrated",
                        logRr = controlResults$calibrated_log_rr,
                        seLogRr = controlResults$calibrated_se_log_rr,
                        ci95Lb = controlResults$calibrated_ci_95_lb,
                        ci95Ub = controlResults$calibrated_ci_95_ub,
                        trueRr = controlResults$effectSize))
  d <- d[!is.na(d$logRr), ]
  d <- d[!is.na(d$ci95Lb), ]
  d <- d[!is.na(d$ci95Ub), ]
  if (nrow(d) == 0) {
    return(NULL)
  }
  d$Group <- as.factor(d$trueRr)
  d$Significant <- d$ci95Lb > d$trueRr | d$ci95Ub < d$trueRr
  temp1 <- aggregate(Significant ~ Group + yGroup, data = d, length)
  temp2 <- aggregate(Significant ~ Group + yGroup, data = d, mean)
  temp1$nLabel <- paste0(formatC(temp1$Significant, big.mark = ","), " estimates")
  temp1$Significant <- NULL
  
  temp2$meanLabel <- paste0(formatC(100 * (1 - temp2$Significant), digits = 1, format = "f"),
                            "% of CIs include ",
                            temp2$Group)
  temp2$Significant <- NULL
  dd <- merge(temp1, temp2)
  dd$tes <- as.numeric(as.character(dd$Group))
  
  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  themeLA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 0)
  
  d$Group <- paste("True hazard ratio =", d$Group)
  dd$Group <- paste("True hazard ratio =", dd$Group)
  alpha <- 1 - min(0.95 * (nrow(d)/nrow(dd)/50000)^0.1, 0.95)
  plot <- ggplot2::ggplot(d, ggplot2::aes(x = logRr, y = seLogRr), environment = environment()) +
    ggplot2::geom_vline(xintercept = log(breaks), colour = "#AAAAAA", lty = 1, size = 0.5) +
    ggplot2::geom_abline(ggplot2::aes(intercept = (-log(tes))/qnorm(0.025), slope = 1/qnorm(0.025)),
                         colour = rgb(0.8, 0, 0),
                         linetype = "dashed",
                         size = 1,
                         alpha = 0.5,
                         data = dd) +
    ggplot2::geom_abline(ggplot2::aes(intercept = (-log(tes))/qnorm(0.975), slope = 1/qnorm(0.975)),
                         colour = rgb(0.8, 0, 0),
                         linetype = "dashed",
                         size = 1,
                         alpha = 0.5,
                         data = dd) +
    ggplot2::geom_point(size = size,
                        color = rgb(0, 0, 0, alpha = 0.05),
                        alpha = alpha,
                        shape = 16) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_label(x = log(0.15),
                        y = 0.9,
                        alpha = 1,
                        hjust = "left",
                        ggplot2::aes(label = nLabel),
                        size = 5,
                        data = dd) +
    ggplot2::geom_label(x = log(0.15),
                        y = labelY,
                        alpha = 1,
                        hjust = "left",
                        ggplot2::aes(label = meanLabel),
                        size = 5,
                        data = dd) +
    ggplot2::scale_x_continuous("Hazard ratio",
                                limits = log(c(0.1, 10)),
                                breaks = log(breaks),
                                labels = breaks) +
    ggplot2::scale_y_continuous("Standard Error", limits = c(0, 1)) +
    ggplot2::facet_grid(yGroup ~ Group) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = themeRA,
                   axis.text.x = theme,
                   axis.title = theme,
                   legend.key = ggplot2::element_blank(),
                   strip.text.x = theme,
                   strip.text.y = theme,
                   strip.background = ggplot2::element_blank(),
                   legend.position = "none")
  
  return(plot)
}

plotLargeScatter <- function(d, xLabel) {
  d$Significant <- d$ci95Lb > 1 | d$ci95Ub < 1
  
  oneRow <- data.frame(nLabel = paste0(formatC(nrow(d), big.mark = ","), " estimates"),
                       meanLabel = paste0(formatC(100 *
                                                    mean(!d$Significant, na.rm = TRUE), digits = 1, format = "f"), "% of CIs includes 1"))
  
  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  themeLA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 0)
  
  alpha <- 1 - min(0.95 * (nrow(d)/50000)^0.1, 0.95)
  plot <- ggplot2::ggplot(d, ggplot2::aes(x = logRr, y = seLogRr)) +
    ggplot2::geom_vline(xintercept = log(breaks), colour = "#AAAAAA", lty = 1, size = 0.5) +
    ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1/qnorm(0.025)),
                         colour = rgb(0.8, 0, 0),
                         linetype = "dashed",
                         size = 1,
                         alpha = 0.5) +
    ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1/qnorm(0.975)),
                         colour = rgb(0.8, 0, 0),
                         linetype = "dashed",
                         size = 1,
                         alpha = 0.5) +
    ggplot2::geom_point(size = 2, color = rgb(0, 0, 0, alpha = 0.05), alpha = alpha, shape = 16) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_label(x = log(0.11),
                        y = 1,
                        alpha = 1,
                        hjust = "left",
                        ggplot2::aes(label = nLabel),
                        size = 5,
                        data = oneRow) +
    ggplot2::geom_label(x = log(0.11),
                        y = 0.935,
                        alpha = 1,
                        hjust = "left",
                        ggplot2::aes(label = meanLabel),
                        size = 5,
                        data = oneRow) +
    ggplot2::scale_x_continuous(xLabel, limits = log(c(0.1,
                                                       10)), breaks = log(breaks), labels = breaks) +
    ggplot2::scale_y_continuous("Standard Error", limits = c(0, 1)) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = themeRA,
                   axis.text.x = theme,
                   axis.title = theme,
                   legend.key = ggplot2::element_blank(),
                   strip.text.x = theme,
                   strip.background = ggplot2::element_blank(),
                   legend.position = "none")
  return(plot)
}


