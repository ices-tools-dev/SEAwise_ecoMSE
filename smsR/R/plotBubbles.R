#' Make a combined bubble plot of survey and catch fits
#'
#' @param sas fitted smsR model
#' @param CVscale TRUE/FALSE whether to scale residuals with the estimated CV
#'
#' @return
#' returns a bubble plot
#' @export
#'
#' @examples
#'
#' plotBubbles(sas)
#' @importFrom ggplot2 ggplot aes geom_bar geom_line geom_hline geom_ribbon element_blank
#' @importFrom ggplot2 alpha scale_x_continuous scale_y_continuous coord_cartesian coord_flip
#' @importFrom ggplot2 facet_grid facet_wrap xlab ylab unit theme theme_classic theme_bw
plotBubbles <- function(sas, CVscale = TRUE) {
  df.tmb <- sas$dat

  # Catch residuals
  CR <- getResidCatch(df.tmb, sas)
  CR$col <- "Positive"
  CR$col[CR$ResidCatch < 0] <- "Negative"


  # Scale CR with cv
  catchCV <- getCatchCV(df.tmb, sas)
  CR$CV <- NA
  for (i in 1:df.tmb$nseason) {
    cvtmp <- catchCV[catchCV$season == i, ]
    yr.tmp <- unique(CR$years[CR$season == i])
    for (j in 1:length(yr.tmp)) {
      CR$CV[CR$season == i & CR$years == yr.tmp[j]] <- cvtmp$catchCV[cvtmp$ages %in% CR$ages[CR$season == i & CR$years == yr.tmp[j]]]
    }
  }


  ss <- c(round(range(abs(CR$ResidCatch))[1]), max(abs(CR$ResidCatch)) * 1.2)



  p7 <- ggplot(CR, ggplot2::aes(x = years, y = as.character(ages), color = factor(col))) +
    ggplot2::geom_point(ggplot2::aes(size = abs(ResidCatch) / CV), alpha = .3) +
    facet_wrap(~season, nrow = df.tmb$nseason) +
    theme_classic() +
    ggplot2::scale_size(range = ss) +
    ggplot2::scale_color_manual(values = c("red", "blue")) +
    ggplot2::scale_y_discrete("age")



  SR <- getResidSurvey(df.tmb, sas)
  SR$col <- "Positive"
  SR$col[SR$ResidSurvey < 0] <- "Negative"


  surveyCV <- getSurveyCV(df.tmb, sas)
  SR$CV <- NA

  if (df.tmb$nsurvey == 1) {
    SR$survey <- dimnames(df.tmb$Surveyobs)[[3]]
    surveyCV$survey <- dimnames(df.tmb$Surveyobs)[[3]]
  }



  snames <- dimnames(df.tmb$Surveyobs)[[3]]

  for (i in 1:df.tmb$nsurvey) {
    svtmp <- surveyCV[surveyCV$survey == snames[i], ]
    yr.tmp <- unique(SR$years[SR$survey == snames[i]])
    for (j in 1:length(yr.tmp)) {
      SR$CV[SR$survey == snames[i] & SR$years == yr.tmp[j]] <-
        svtmp$surveyCV[which(svtmp$ages %in% (SR$ages[SR$survey == snames[i] & SR$years == yr.tmp[j]]))]
    }
  }


  # Bind the two together for a big plot
  SR.b <- SR %>%
    dplyr::select(-SE) %>%
    dplyr::mutate(fleet = paste("survey:", survey)) %>%
    dplyr::rename(residual = ResidSurvey) %>%
    dplyr::select(-survey)
  CR.b <- CR %>%
    dplyr::mutate(fleet = paste("catch season:", season)) %>%
    dplyr::rename(residual = ResidCatch) %>%
    dplyr::select(-season)
  dat.plot <- dplyr::bind_rows(SR.b %>% dplyr::mutate(source = "survey"), CR.b %>% dplyr::mutate(source = "catch"))

  ss <- c(round(range(abs(dat.plot$residual))[1]), max(abs(dat.plot$residual)) * 1.5)

  if (CVscale == TRUE) {
    p <- ggplot(dat.plot, ggplot2::aes(x = years, y = as.character(ages), color = factor(col))) +
      ggplot2::geom_point(ggplot2::aes(size = abs(residual) / CV), alpha = .3) +
      facet_wrap(~fleet) +
      theme_classic() +
      ggplot2::scale_size(range = ss) +
      ggplot2::scale_color_manual(values = c("red", "blue")) +
      scale_x_continuous("Year") +
      ggplot2::scale_y_discrete("Age") +
      theme(legend.title = element_blank(), legend.position = "top") +
      ggplot2::labs(title = "Residuals", subtitle = "(log(obs) - log(estimated)")
  } else {
    p <- ggplot(dat.plot, ggplot2::aes(x = years, y = as.character(ages), color = factor(col))) +
      ggplot2::geom_point(ggplot2::aes(size = abs(residual)), alpha = .3) +
      facet_wrap(~fleet) +
      theme_classic() + # ggplot2::scale_size(range = ss)+
      ggplot2::scale_color_manual(values = c("red", "blue")) +
      scale_x_continuous("Year") +
      ggplot2::scale_y_discrete("Age") +
      theme(legend.title = element_blank(), legend.position = "top") +
      ggplot2::labs(title = "Residuals", subtitle = "(log(obs) - log(estimated)")
  }

  print(p)
  return(p)
}
