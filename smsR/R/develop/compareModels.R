#' Compare input data and output from two assessments
#'
#' @param df.tmb input data from first model
#' @param sas smsR fit from first model
#' @param df.tmb_2 input data from second model
#' @param sas_2 smsR fit from second model
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom package function
compareModels <- function(df.tmb, sas, df.tmb_2, sas_2) {
  # Weight at age
  waa_1 <- getWeight(df.tmb) %>% dplyr::mutate(model = "model_1")
  waa_2 <- getWeight(df.tmb_2) %>% dplyr::mutate(model = "model_2")

  waa <- rbind(waa_1, waa_2)

  p1 <- ggplot(waa, aes(x = years, y = value, color = model, group = season)) +
    geom_line() +
    facet_wrap(~age) +
    theme_classic()

  # mortality at age
  waa_1 <- getM(df.tmb) %>% dplyr::mutate(model = "model_1")
  waa_2 <- getM(df.tmb_2) %>% dplyr::mutate(model = "model_2")

  waa <- rbind(waa_1, waa_2)

  p2 <- ggplot(waa, aes(x = years, y = value, color = model)) +
    geom_line() +
    facet_grid(season ~ age) +
    theme_classic()

  # Effort
  eff_1 <- as.data.frame(df.tmb$effort) %>% dplyr::mutate(model = "model_1", years = df.tmb$years)
  eff_2 <- as.data.frame(df.tmb_2$effort) %>% dplyr::mutate(model = "model_2", years = df.tmb$years)

  eff <- rbind(eff_1, eff_2)
  names(eff)[1:df.tmb$nseason] <- 1:df.tmb$nseason
  eff <- eff %>% tidyr::pivot_longer(1:df.tmb$nseason, values_to = "effort", names_to = "season")

  p3 <- ggplot(eff, aes(x = years, y = effort, color = model)) +
    geom_line() +
    facet_wrap(~season) +
    theme_classic()

  # Catch at age

  getCatchN()
}
