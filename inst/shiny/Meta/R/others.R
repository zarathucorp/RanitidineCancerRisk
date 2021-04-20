

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
  dataTable <- ggplot2::ggplot(labels, ggplot2::aes(x = x, y = y, label = label)) + ggplot2::geom_text(size = 3.5, vjust = 0.5) + ggplot2::scale_x_continuous(xLabel,
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