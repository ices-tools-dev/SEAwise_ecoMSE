#' Plot all diagnostics of fitted stock assesment model
#'
#' @param df.tmb List of smsR input data
#' @param sas Fitted smsR model
#'
#' @return
#' returns a list of plots
#' @export
#'
#' @examples
#' p <- plotDiagnostics(df.tmb, sas)
#'
#' print(p$SR) # Print the stock recruitment relationship
#'
#' @importFrom reshape2 melt
#'
plotDiagnostics <- function(df.tmb, sas, mr = NULL) {

  nseason <- df.tmb$nseason
  years <- df.tmb$years


  surv <- df.tmb$Surveyobs

  if (is.null(dimnames(surv))) {
    dimnames(surv) <- list(age = df.tmb$age, year= df.tmb$years,survey = 1:df.tmb$nsurvey)
  }

  surv.fit <- getSurvey(df.tmb, sas) %>% dplyr::rename(
    "age" = ages,
    "cpue" = surveyest
  )

  for (i in 1:df.tmb$nsurvey) {
    s.out <- as.data.frame(t(surv[, , i]))
    s.out[s.out == -1] <- NA
    s.out$year <- df.tmb$years
    names(s.out)[1:df.tmb$nage] <- df.tmb$age

    s.out <- s.out %>%
      tidyr::pivot_longer(as.character(df.tmb$age), values_to = "cpue", names_to = "age") %>%
      tidyr::drop_na(cpue)

    survey_name <- dimnames(surv)$survey[i]
    if(is.null(survey_name)){
      survey_name <- i
    }

    s.out$survey <- as.character(survey_name)

    if (i == 1) {
      s.exp <- s.out
    } else {
      s.exp <- rbind(s.out, s.exp)
    }

    if (df.tmb$nsurvey == 1) {
      s.exp$survey <- dimnames(surv)[[3]]
    }
  }

  surv.fit$age <- as.character(surv.fit$age)


  p1 <- ggplot2::ggplot(s.exp, ggplot2::aes(x = year, y = cpue, color = age)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~survey, scales = "free") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top") +
    ggplot2::scale_y_log10("survey index") +
    ggplot2::geom_line(linewidth = .7, linetype = 2, alpha = .2) +
    ggplot2::geom_line(data = surv.fit, ggplot2::aes(x = years)) +
    ggplot2::geom_ribbon(
      data = surv.fit, ggplot2::aes(x = years, ymin = low, ymax = high, fill = age),
      alpha = .1, linetype = 0
    )




  catch <- df.tmb$Catchobs
  catch.fit <- getCatchN(df.tmb, sas) %>% dplyr::rename("age" = ages)

  for (i in 1:df.tmb$nseason) {
    c.out <- as.data.frame(t(catch[, , i]))
    names(c.out) <- as.character(df.tmb$age)
    c.out$year <- df.tmb$years


    c.out <- c.out %>%
      tidyr::pivot_longer(as.character(df.tmb$age), values_to = "CatchN", names_to = "age")
    c.out$season <- i

    if (i == 1) {
      c.exp <- c.out
    } else {
      c.exp <- rbind(c.out, c.exp)
    }
  }

  catch.fit$age <- as.character(catch.fit$age)
  c.exp$CatchN[c.exp$CatchN == 0] <- NA
  catch.fit$CatchN[catch.fit$CatchN == 0] <- NA

  p2 <- ggplot2::ggplot(c.exp, ggplot2::aes(x = year, y = CatchN, color = age)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(season ~ age, scales = "free") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top") +
    ggplot2::scale_y_log10("catch (numbers)") +
    ggplot2::geom_line(data = catch.fit, ggplot2::aes(x = years)) +
    ggplot2::geom_ribbon(
      data = catch.fit, ggplot2::aes(x = years, ymin = low, ymax = high, fill = age),
      alpha = .1, linetype = 0
    )



  # Do the internal dredge survey thing
  survey <- reshape2::melt(surv) %>% dplyr::filter(value > 0)

  snames <- unique(survey$survey)

  if(is.null(snames)){
    snames <- 1:df.tmb$nsurvey
  }

  p3 <- list()
  idx <- 1

  for (i in 1:length(snames)) {

    if(df.tmb$Qminage[i] < df.tmb$Qmaxage[i]){


    s.tmp <- survey %>% dplyr::filter(survey == snames[i])

    ages <- unique(s.tmp$age)

    for (j in 1:(length(ages) - 1)) {
      a1 <- s.tmp %>% dplyr::filter(age == ages[j])
      names(a1)[4] <- "Age1"

      a2 <- s.tmp %>% dplyr::filter(age == ages[j + 1])
      a2$year <- a2$year - 1
      names(a2)[4] <- "Age2"

      ff <- dplyr::left_join(a1, a2, by = c("year", "survey"))
      xmod <- lm(log(Age2) ~ log(Age1), data = ff)
      sum.lm <- summary(xmod)

      p3[[idx]] <- ggplot2::ggplot(ff, ggplot2::aes(x = log(Age1), y = log(Age2), color = year)) +
        ggplot2::geom_point() +
        ggplot2::geom_smooth(method = "lm", color = "black", se = FALSE) +
        ggplot2::scale_color_viridis_c() +
        theme(legend.position = "top") +
        theme_classic() +
        ggplot2::annotate("text",
          x = mean(log(ff$Age1), na.rm = TRUE), y = log(max(ff$Age1, na.rm = TRUE)),
          label = paste(snames[i], " Rsquare =", round(sum.lm$r.squared, 2))
        ) +
        scale_y_continuous(paste("Age", ages[j + 1], "(year - 1)", sep = "")) +
        scale_x_continuous(paste("Age", ages[j], "(year)", sep = "")) +
        theme(legend.title = ggplot2::element_blank())

      idx <- idx + 1
    }
    }
  }

  #
  #   snames <- unique(s.exp$survey)[grep('Dredge', unique(s.exp$survey))]
  #   splot <- s.exp %>% filter(survey %in% snames)
  #   # minimum ages
  #   minnames <- as.character(unique(splot$age)[1:2])
  #
  #
  #   splot$year[splot$age == 1] <- splot$year[splot$age == 1]-1 # Fix 0 and 1 to same cohort
  #   dredge <- splot %>% arrange(year) %>% tidyr::pivot_wider(values_from = cpue, names_from = age) %>%
  #     dplyr::rename('Age0' = minnames[1],
  #            'Age1' = minnames[2])
  #


  # xmod <- lm(log(Age1) ~ log(Age0), data = dredge)
  #
  # sum.lm <- summary(xmod)
  #
  # p3 <- ggplot2::ggplot(dredge, ggplot2::aes(x = log(Age0), y = log(Age1), color = year))+ggplot2::geom_point()+
  #   ggplot2::geom_smooth(method = 'lm', color = 'black', se = FALSE)+ggplot2::scale_color_viridis_c()+
  #   theme(legend.position = 'top')+theme_classic()+
  #   ggplot2::annotate('text', x = mean(log(dredge$Age0), na.rm =TRUE), y = log(max(dredge$Age0, na.rm = TRUE)),
  #            label = paste('Rsquare =',round(sum.lm$r.squared,2)))+
  #   scale_y_continuous('Age 1 (year - 1)')+
  #   scale_x_continuous('Age 0 (year)')+
  #   theme(legend.title = ggplot2::element_blank())

  # Proportion in catch
  catchdf <- as.data.frame(t(df.tmb$Catchobs[, , 1]))

  names(catchdf) <- df.tmb$age
  catchdf$season <- 1
  catchdf$year <- df.tmb$years
  catchdf[, 1:df.tmb$nage] <- catchdf[, 1:df.tmb$nage] / rowSums(catchdf[, 1:df.tmb$nage], na.rm = TRUE)

  if (nseason > 1) {
    for (i in 2:df.tmb$nseason) {
      tmp <- as.data.frame(t(df.tmb$Catchobs[, , i]))
      names(tmp) <- df.tmb$age
      tmp$season <- i
      tmp$year <- df.tmb$years
      tmp[, 1:df.tmb$nage] <- tmp[, 1:df.tmb$nage] / rowSums(tmp[, 1:df.tmb$nage])


      catchdf <- rbind(catchdf, tmp)
    }
  }
  # `take the average for the plot

  catchdf.plot <- catchdf %>%
    tidyr::pivot_longer(paste(df.tmb$age), names_to = "Age", values_to = "canum") %>%
    dplyr::group_by(Age, year) %>%
    dplyr::summarise(catch = mean(canum, na.rm = TRUE))

  p4 <- ggplot(catchdf.plot, ggplot2::aes(x = year, y = catch, fill = Age)) +
    geom_bar(position = "fill", stat = "identity") +
    scale_y_continuous("proportion at age \n in catch") +
    theme_classic()


  # Plot mean weight at age

  wdf <- as.data.frame(t(df.tmb$weca[, , 1]))

  names(wdf) <- df.tmb$age
  wdf$season <- 1
  wdf$year <- c(df.tmb$years, max(years) + 1)

  nseason <- df.tmb$nseason

  if (nseason > 1) {
    for (i in 2:df.tmb$nseason) {
      tmp <- as.data.frame(t(df.tmb$weca[, , i]))
      names(tmp) <- df.tmb$age
      tmp$season <- i
      tmp$year <- c(df.tmb$years, max(years) + 1)

      wdf <- rbind(wdf, tmp)
    }
  }
  wdf.p <- wdf %>% tidyr::pivot_longer(paste(df.tmb$age), names_to = "Age", values_to = "weca")

  p5 <- ggplot(wdf.p, ggplot2::aes(x = year, y = weca * 1000, col = Age)) +
    ggplot2::geom_line() +
    facet_wrap(~season, nrow = df.tmb$nseason) +
    theme_classic() +
    scale_y_continuous("weight at age (g)") +
    theme(legend.position = "top")

  Mdf <- as.data.frame(t(df.tmb$M[, , 1]))

  names(Mdf) <- df.tmb$age
  Mdf$season <- 1
  years <- df.tmb$years

  Mdf$year <- c(df.tmb$years, max(years) + 1)

  if (nseason > 1) {
    for (i in 2:df.tmb$nseason) {
      tmp <- as.data.frame(t(df.tmb$M[, , i]))
      names(tmp) <- df.tmb$age
      tmp$season <- i
      tmp$year <- c(df.tmb$years, max(years) + 1)

      Mdf <- rbind(Mdf, tmp)
    }
  }
  Mdf.p <- Mdf %>% tidyr::pivot_longer(paste(df.tmb$age), names_to = "Age", values_to = "M")

  pM <- ggplot(Mdf.p, ggplot2::aes(x = year, y = M, col = Age)) +
    ggplot2::geom_line() +
    facet_wrap(~season, nrow = df.tmb$nseason) +
    theme_classic() +
    scale_y_continuous("Natural mortality \n(per year)") +
    theme(legend.position = "top")

  # print(p5)
  Matdf <- as.data.frame(t(df.tmb$Mat[, , df.tmb$recseason]))

  names(Matdf) <- df.tmb$age
  Matdf$season <- 1
  Matdf$year <- c(df.tmb$years, max(years) + 1)

  Mdf.p <- Matdf %>% tidyr::pivot_longer(paste(df.tmb$age), names_to = "Age", values_to = "M")

  pMat <- ggplot(Mdf.p, ggplot2::aes(x = year, y = M, col = Age)) +
    ggplot2::geom_line() +
    facet_wrap(~season, nrow = df.tmb$nseason) +
    theme_classic() +
    scale_y_continuous("Maturity ogive") +
    theme(legend.position = "top")


  # Catch residuals
  CR <- getResidCatch(df.tmb, sas)
  CR$col <- "Positive"
  CR$col[CR$ResidCatch < 0] <- "Negative"

  ss <- c(round(range(abs(CR$ResidCatch))[1]), max(abs(CR$ResidCatch)) * 1.5)


  p6 <- ggplot(CR, ggplot2::aes(x = years, y = as.character(ages), color = factor(col))) +
    ggplot2::geom_point(ggplot2::aes(size = abs(ResidCatch)), alpha = .3) +
    facet_wrap(~season, nrow = df.tmb$nseason) +
    theme_classic() +
    ggplot2::scale_size(range = ss) +
    ggplot2::scale_color_manual(values = c("red", "blue")) +
    ggplot2::labs(title = "catch residuals") +
    ggplot2::scale_y_discrete("age")
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
    ggplot2::labs(title = "scaled catch residuals") +
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


  ss <- c(round(range(abs(SR$ResidSurvey))[1]), max(abs(SR$ResidSurvey)) * 3)

  p8 <- ggplot(SR, ggplot2::aes(x = years, y = as.character(ages), color = factor(col))) +
    ggplot2::geom_point(ggplot2::aes(size = abs(ResidSurvey)), alpha = .3) +
    facet_wrap(~survey, nrow = df.tmb$nsurvey) +
    theme_classic() +
    ggplot2::scale_size(range = ss) +
    ggplot2::scale_color_manual(values = c("red", "blue")) +
    ggplot2::labs(title = "survey residuals") +
    ggplot2::scale_y_discrete("age")

  p9 <- ggplot(SR, ggplot2::aes(x = years, y = as.character(ages), color = factor(col))) +
    ggplot2::geom_point(ggplot2::aes(size = abs(ResidSurvey) / CV), alpha = .3) +
    facet_wrap(~survey, nrow = df.tmb$nsurvey) +
    theme_classic() +
    ggplot2::scale_size(range = ss) +
    ggplot2::scale_color_manual(values = c("red", "blue")) +
    ggplot2::labs(title = "scaled survey residuals") +
    ggplot2::scale_y_discrete("age")

  # stock recrurtment


  SR_pred <- getSR(df.tmb, sas)
  R <- getR(df.tmb, sas)
  SSB <- getSSB(df.tmb, sas)
  R$SSB <- SSB$SSB[1:(nrow(SSB) - 1)]

  lims <- c(0, max(R$R) * 2)


  p10 <- ggplot(SR_pred, ggplot2::aes(x = SSB, y = SR / 1e8)) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = minSR / 1e8, ymax = maxSR / 1e8), fill = "red", alpha = .2) +
    theme_classic() +
    ggplot2::scale_color_viridis_c() +
    ggplot2::geom_point(data = R, ggplot2::aes(x = SSB, y = R / 1e8, col = years)) +
    scale_y_continuous("Recruitment (1e8)") +
    coord_cartesian(ylim = lims / 1e8)


  # Retro fits


  # mr <- readRDS(file.path(wd, 'mr.RDS'))
  if (is.null(mr)) {
    # parms <- getParms(df.tmb)
    # mr <- smsR::mohns_rho(df.tmb,parms, peels = 5,plotfigure = FALSE)

    p11 <- NULL
  } else {
    p11 <- mr$p1()
  }


  # Create a list of figures
  ls.out <- list(
    survey = p1,
    catch = p2,
    cohort = p3,
    agecomp = p4,
    mwa = p5,
    m2 = pM,
    mat = pMat,
    cresids = p6,
    cresids_scaled = p7,
    sresids = p8,
    sresids_scaled = p9,
    SR = p10,
    mohnsrho = p11
  )
  return(ls.out)
}
