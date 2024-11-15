#' Plot a comparison of stock assessments when removing surveys
#'
#' @param sas fitted smsR model
#' @param parms optional parameters
#'
#' @return returns a figure
#' @export
#'
#' @examples
#' p1 <-  removeSurvey(sas) #
#' print(p1) # Plot the figure
#' @importFrom ggplot2 ggplot aes geom_bar geom_line geom_hline geom_ribbon
#' @importFrom ggplot2 alpha scale_x_continuous scale_y_continuous coord_cartesian coord_flip
#' @importFrom ggplot2 facet_grid facet_wrap xlab ylab unit theme theme_classic theme_bw
#' @importFrom patchwork plot_layout
#'
removeSurvey <- function(sas, parms = NULL){

  df.new <- sas$dat
  dat.sum <- getSummary(sas$dat, sas) %>% dplyr::mutate(surveys = 'all')

  for(i in 1:df.new$nsurvey){

    df.tmp <- df.new
    df.tmp$nsurvey <- df.tmp$nsurvey-1 # Remove one survey

    surveyout <- rep(1, df.new$nsurvey)
    surveyout[i] <- 0

    powers <- list()
    for(k in 1:df.new$nsurvey){
      if(sum(df.new$powers) == 0){
        powers[[k]] <- NA
      }else{
        powers[[k]] <- which.max(df.tmb$powers[,i] == 1)
      }

    }

    df.tmp <- get_TMB_parameters(
      mtrx = list(weca = df.new$weca, west = df.new$west, M = df.new$M, mat = df.new$Mat),
      Surveyobs = df.new$Surveyobs,
      Catchobs = df.new$Catchobs,
      propM = df.new$propM,
      propF = df.new$propF,
      years = df.new$years,
      startYear = min(df.new$years),
      endYear = max(df.new$years),
      nseason = df.new$nseason,
      nsurvey = sum(surveyout),
      ages = df.new$age,
      Fbarage = df.new$Fbarage,
      recseason = df.new$recseason,
      Fminage = df.new$Fminage,
      Fmaxage = df.new$Fmaxage,
      Qminage = df.new$Qminage,
      Qmaxage = df.new$Qmaxage,
      Qlastage = df.new$Qlastage,
      isFseason = df.new$isFseason,
      CminageSeason = df.new$CminageSeason,
      CmaxageSeason = max(df.new$age), # Fix this later
      endFseason = df.new$nseason,
      nocatch = df.new$nocatch,
      useEffort = df.new$useEffort,
      estimateCreep = df.new$estimateCreep,
      effort = df.new$effort,
      blocks = df.new$blocks,
      surveyStart = df.new$surveyStart,
      surveyEnd = df.new$surveyEnd,
      surveySeason = df.new$surveySeason,
      leavesurveyout = surveyout,
      minSDsurvey = df.new$minSDsurvey,
      minSDcatch = df.new$minSDcatch,
      peneps = df.new$peneps,
      penepsC = df.new$penepsC,
      powers = powers,
      scv = df.new$scv,
      surveyCV = df.new$surveyCV,
      catchCV = df.new$catchCVin,
      recmodel = df.new$recmodel,
      estCV = df.new$estCV,
      CVmin = df.new$CVmin,
      betaSR = df.new$betaSR,
      nllfactor = df.new$nllfactor,
      randomF = df.new$randomF
    )


    parms.tmp <- getParms(df.tmp)
    sas.tmp <- runAssessment(df.tmp, parms.tmp)

    sum.tmp <- getSummary(sas.tmp$dat, sas.tmp) %>% dplyr::mutate(surveys = paste('without',dimnames(df.new$Surveyobs)[[3]][i]))

    dat.sum <- rbind(dat.sum, sum.tmp)


  }


  # Prepare plotting


  #dat.plot <- dat.sum %>% pivot_longer(c(R, SSB, Fbar)) #%>% pivot_longer(c(Rlow, Rhigh, SSBlow, SSBhigh, Fbarhigh, Fbarlow))

  p1 <- ggplot(dat.sum, aes(x = years, y = SSB/1000, color = surveys))+geom_line()+theme_classic()+
    geom_ribbon(data = dat.sum %>% dplyr::filter(surveys == 'all'),aes(ymin = SSBlow/1000, ymax = SSBhigh/1000),
                show.legend = FALSE, fill = alpha('gray', alpha = .25), linetype = 0)+
    theme(legend.position = 'top', legend.title = element_blank())+scale_y_continuous('SSB\n (1000 t)')+
    scale_x_continuous('Year')

  p2 <- ggplot(dat.sum %>% dplyr::filter(years < max(years)), aes(x = years, y = R/1e6, color = surveys))+geom_line()+theme_classic()+
    geom_ribbon(data = dat.sum %>% dplyr::filter(surveys == 'all'),aes(ymin = Rlow/1e6, ymax = Rhigh/1e6),
                show.legend = FALSE, fill = alpha('gray', alpha = .25), linetype = 0)+
    theme(legend.position = 'none', legend.title = element_blank())+scale_y_continuous('R\n (millions)')+
    scale_x_continuous('')

  p3 <- ggplot(dat.sum, aes(x = years, y = Fbar, color = surveys))+geom_line()+theme_classic()+
    geom_ribbon(data = dat.sum %>% dplyr::filter(surveys == 'all'),aes(ymin = Fbarlow, ymax = Fbarhigh),
                show.legend = FALSE, fill = alpha('gray', alpha = .25), linetype = 0)+
    theme(legend.position = 'none', legend.title = element_blank())+scale_y_continuous('Fbar')+
    scale_x_continuous('Year')

  p1/p2/p3




  return(p1)
}
