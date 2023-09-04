library(dplyr)

plotKaplanMeier <- function(population,
                            censorMarks = FALSE,
                            confidenceIntervals = F,
                            includeZero = FALSE,
                            dataTable = TRUE,
                            cumulativeIncidence = TRUE,
                            dataCutoff = 0.90,
                            targetLabel = "Treated",
                            comparatorLabel = "Comparator",
                            title,
                            fileName = NULL) {
  population$y <- 0
  population$y[population$outcomeCount != 0] <- 1
  if (is.null(population$stratumId) || length(unique(population$stratumId)) == nrow(population)/2) {
    sv <- survival::survfit(survival::Surv(survivalTime, y) ~ treatment, population, conf.int = TRUE)
    data <- data.frame(time = sv$time,
                       n.censor = sv$n.censor,
                       s = sv$surv,
                       strata = summary(sv, censored = T)$strata,
                       upper = sv$upper,
                       lower = sv$lower)
    levels(data$strata)[levels(data$strata) == "treatment=0"] <- comparatorLabel
    levels(data$strata)[levels(data$strata) == "treatment=1"] <- targetLabel
  } else {
    ParallelLogger::logInfo("Variable size strata detected so using adjusted KM for stratified data")
    population$stratumSizeT <- 1
    strataSizesT <- aggregate(stratumSizeT ~ stratumId, population[population$treatment == 1,], sum)
    if (max(strataSizesT$stratumSizeT) == 1) {
      # variable ratio matching: use propensity score to compute IPTW
      if (is.null(population$propensityScore)) {
        stop("Variable ratio matching detected, but no propensity score found")
      }
      weights <- aggregate(propensityScore ~ stratumId, population, mean)
      weights$weight <- weights$propensityScore / (1 - weights$propensityScore)
    } else {
      # stratification: infer probability of treatment from subject counts
      strataSizesC <- aggregate(stratumSizeT ~ stratumId, population[population$treatment == 0,], sum)
      colnames(strataSizesC)[2] <- "stratumSizeC"
      weights <- merge(strataSizesT, strataSizesC)
      weights$weight <- weights$stratumSizeT / weights$stratumSizeC
    }
    strataSizesC <- aggregate(stratumSizeT ~ stratumId, population[population$treatment == 0,], sum)
    colnames(strataSizesC)[2] <- "stratumSizeC"
    weights <- merge(strataSizesT, strataSizesC)
    weights$weight <- weights$stratumSizeT / weights$stratumSizeC
    population <- merge(population, weights[, c("stratumId", "weight")])
    population$weight[population$treatment == 1] <- 1
    idx <- population$treatment == 1
    survTarget <- adjustedKm(weight = population$weight[idx],
                             time = population$survivalTime[idx],
                             y = population$y[idx])
    survTarget$strata <- targetLabel
    idx <- population$treatment == 0
    survComparator <- adjustedKm(weight = population$weight[idx],
                                 time = population$survivalTime[idx],
                                 y = population$y[idx])
    survComparator$strata <- comparatorLabel
    if (censorMarks) {
      addCensorData <- function(surv, treatment) {
        censorData <- aggregate(rowId ~ survivalTime, population[population$treatment == treatment, ], length)
        colnames(censorData) <- c("time", "censored")
        eventData <- aggregate(y ~ survivalTime, population, sum)
        colnames(eventData) <- c("time", "events")
        surv <- merge(surv, censorData)
        surv <- merge(surv, eventData, all.x = TRUE)
        surv$n.censor = surv$censored - surv$events
        return(surv)
      }
      survTarget <- addCensorData(survTarget, 1)
      survComparator <- addCensorData(survComparator, 0)
    }
    
    data <- rbind(survTarget, survComparator)
    data$upper <- data$s^exp(qnorm(1 - 0.025)/log(data$s)*sqrt(data$var)/data$s)
    data$lower <- data$s^exp(qnorm(0.025)/log(data$s)*sqrt(data$var)/data$s)
    data$lower[data$s > 0.9999] <- data$s[data$s > 0.9999]
  }
  data$strata <- factor(data$strata, levels = c(targetLabel, comparatorLabel))
  cutoff <- quantile(population$survivalTime, dataCutoff)
  xLabel <- "Time in days"
  yLabel <- "Survival probability"
  xlims <- c(-cutoff/40, cutoff)
  logrank <- survival::survdiff(survival::Surv(survivalTime, y) ~ treatment, data = population)
  pvalue <- round(pchisq(logrank$chisq, df = 1, lower.tail = F),3)
  
  if (cutoff <= 300) {
    xBreaks <- seq(0, cutoff, by = 50)
  } else if (cutoff <= 600) {
    xBreaks <- seq(0, cutoff, by = 100)
  } else {
    xBreaks <- seq(0, cutoff, by = 250)
  }
  
  data <- data[data$time <= cutoff, ]
  if (includeZero) {
    ylims <- c(0, 1)
  } else if (confidenceIntervals) {
    ylims <- c(min(data$lower), 1)
  } else {
    ylims <- c(min(data$lower), 1)
  }
  
  if(cumulativeIncidence == TRUE){
    ylims <- c(0, 1-ylims[1])*100
    xLabel <- "Follow-up duration in days"
    yLabel <- "Cumulative incidence (%)"
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data$time,
                                               y = (1-.data$s)*100,
                                               color = .data$strata,
                                               fill = .data$strata,
                                               linetype = .data$strata,
                                               ymin = (1-.data$lower)*100,
                                               ymax = (1-.data$upper)*100))  
  }else{
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data$time,
                                               y = .data$s,
                                               color = .data$strata,
                                               fill = .data$strata,
                                               linetype = .data$strata,
                                               ymin = .data$lower,
                                               ymax = .data$upper))
  }
  
  if (confidenceIntervals) {plot <- plot + ggplot2::geom_ribbon(color = rgb(0, 0, 0, alpha = 0))}
  
  plot <- plot +
    ggplot2::theme_classic() +
    ggplot2::geom_step(size = 1) +
    ggplot2::scale_color_manual(values = c(rgb(0.0, 0.0, 0.0, alpha = 0.8),
                                           rgb(0.5, 0.5, 0.5, alpha = 0.8))) +
    ggplot2::scale_fill_manual(values = c(rgb(0.0, 0.0, 0.0, alpha = 0.1),
                                          rgb(0.5, 0.5, 0.5, alpha = 0.1))) +
    ggplot2::scale_linetype_manual(values = c("solid", "dashed")) +
    ggplot2::scale_x_continuous(xLabel, limits = xlims, breaks = xBreaks) +
    ggplot2::scale_y_continuous(yLabel, limits = ylims) +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   legend.position = "top",
                   plot.title = ggplot2::element_text(hjust = 0.5))
  
  if(cumulativeIncidence == T){
    plot <- plot + ggplot2::annotate("text", x = xlims[2]-50, y =  ylims[2], 
                                     label = ifelse(pvalue < 0.001, "p-value < 0.001", ifelse(pvalue < 0.01 , "p-value < 0.01", paste0("p-value = ", pvalue))))
  }else{
    plot <- plot + ggplot2::annotate("text", x = 1, y =  min((data$s)), 
                                     label = ifelse(pvalue < 0.001, "p-value < 0.001", ifelse(pvalue < 0.01 , "p-value < 0.01", paste0("p-value = ", pvalue))))
  }
  
  if (censorMarks == TRUE) {
    plot <- plot + ggplot2::geom_point(data = subset(data, .data$n.censor >= 1),
                                       ggplot2::aes(x = .data$time, y = .data$s),
                                       shape = "|",
                                       size = 3)
  }
  if (!missing(title) && !is.null(title)) {
    plot <- plot + ggplot2::ggtitle(title)
  }
  if (dataTable) {
    targetAtRisk <- c()
    comparatorAtRisk <- c()
    for (xBreak in xBreaks) {
      targetAtRisk <- c(targetAtRisk, sum(population$treatment == 1 & population$survivalTime >= xBreak))
      comparatorAtRisk <- c(comparatorAtRisk, sum(population$treatment == 0 & population$survivalTime >= xBreak))
    }
    labels <- data.frame(x = c(0, xBreaks, xBreaks),
                         y = as.factor(c("Number at risk", rep(targetLabel, length(xBreaks)), rep(comparatorLabel, length(xBreaks)))),
                         label = c("", formatC(targetAtRisk, big.mark = ","), formatC(comparatorAtRisk, big.mark = ",")))
    labels$y <- factor(labels$y, levels = c(comparatorLabel, targetLabel, "Number at risk"))
    dataTable <- ggplot2::ggplot(labels, ggplot2::aes(x = .data$x, y = .data$y, label = .data$label)) +
      ggplot2::geom_text(size = 3.5, vjust = 0.5) +
      ggplot2::scale_x_continuous(xLabel, limits = xlims, breaks = xBreaks) +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     legend.position = "none",
                     panel.border = ggplot2::element_blank(),
                     panel.background = ggplot2::element_blank(),
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
    plot <- gridExtra::grid.arrange(grobs[[1]], grobs[[2]], heights = c(400,100))
  }
  
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 7, height = 5, dpi = 400)
  return(plot)
}

adjustedKm <- function(weight, time, y) {
  .Call('_CohortMethod_adjustedKm', PACKAGE = 'CohortMethod', weight, time, y)
}

# 2581 Severe asthma exacerbation
plotKaplanMeier(readRDS("~/PLE/asthma01261/output/cmOutput/StratPop_l1_s3_p1_t2002_c2003_s1_o2581.rds"))
# 1967 Asthma exacerbation
plotKaplanMeier(readRDS("~/PLE/asthma01261/output/cmOutput/StratPop_l1_s3_p1_t2002_c2003_s1_o1967.rds"))
# 2580 asthma all cause hospitalization
plotKaplanMeier(readRDS("~/PLE/asthma01261/output/cmOutput/StratPop_l1_s3_p1_t2002_c2003_s1_o2580.rds"))
# 2642 T2DM outcome (New onset)
plotKaplanMeier(readRDS("~/PLE/asthma01261/output/cmOutput/StratPop_l1_s8_p1_t2002_c2003_s1_o2642.rds"))
# 2685 HTN outcome (New onset)
plotKaplanMeier(readRDS("~/PLE/asthma01261/output/cmOutput/StratPop_l1_s8_p1_t2002_c2003_s1_o2685.rds"))

View(outcomeModelReference %>% filter(outcomeId == 1967) %>% select(strataFile, outcomeModelFile))
readRDS("~/PLE/asthma01261/output/cmOutput/StratPop_l1_s3_p1_t2002_c2003_s1_o1967.rds")

readRDS("~/PLE/asthma01261/output/cmOutput/Analysis_3/om_t2002_c2003_o1967.rds")$attrition
