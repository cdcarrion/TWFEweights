
# data --------------------------------------------------------------------

tb2 <- tibble(
  X = rnorm(10),
  Y = 9*rnorm(10),
  G = rbinom(10, size=1, p=0.7),
  U = rnorm(10),
  T = 2000:2009,
  D = 5*rnorm(10),
  D0 = 10*rnorm(10),
  CRT1 = rbinom(10, size=1, p=0.7),
  CRT2 = rbinom(10, size=1, p=0.7),
  TREAT1 = residuals(lm(Y ~ X)),
  TREAT2 = 10*residuals(lm(Y ~ X)),
  rand_wei = 25*rnorm(10)#,
  #weights = abs(rnorm(10))
)

tb2_2 = structure(list(X = c(0.512025410572104, 1.34199356465929, -1.25270649900694, 
                             -0.733000712714794, 0.45465371837226, -0.127917924970955, 0.31527125548264, 
                             NA, 1.48657524744542, -0.19959359633143), 
                       Y = c(-0.661273939978519, -12.7527853536461, -1.17638144073283, NA, 8.47888404561908, 
                             8.57745418700866, 9.31590763962404, NA, 4.25920544787551, 16.1806108716548), 
                       G = c(1, NA, 1, 1, 0, 1, 0, NA, 1, 1), 
                       U = c(-0.333914863990124, -1.12935646288322, -1.69358562462423, -0.395929701341583, 
                             0.539754714374008, -1.04544664173391, 1.22480232242127, NA, 0.444278569991649, 
                             0.0596820740502588), 
                       T = c(2000, 2001, 2002, 2003, 2004, 2005, 2006, NA, 2008, 2009), 
                       D = c(5.53807382142459, -4.45890044023787, -4.02776194571743, -2.40946010272286, 
                             0.517463695461427, NA, 1.44204693390199, NA, -8.22747683712939, -2.98560166016223), 
                       D0 = c(NA, -11.1320556669676, -3.56453610182438, 4.01976245984647, -5.99808700582389, 
                              9.96620396574901, 5.80036617369695, NA, 12.7934259732021, -9.79730042991476), 
                       CRT1 = c(-9.33852764928105, 29.1198381594211, 0.965478696195844, -23.8140405650042, 
                                -11.9346599938887, -7.18523176736665, 5.4838459604535, NA, -17.3985872962915, NA), 
                       CRT2 = c(`1` = 2.93893603185069, `2` = 1.228446714169, `3` = 6.57588892581666, `4` = 5.50482229342548, 
                                `5` = 3.05717390177606, `6` = 4.25780138105421, `7` = 3.34442855446459, `8` = NA, 
                                `9` = NA, `10` = 4.40551845862642), 
                       TREAT1 = c(`1` = -3.60020997182921, `2` = -13.9812320678151, `3` = -7.75227036654949, `4` = -6.01945591669384, 
                                  `5` = 5.42171014384302, `6` = 4.31965280595445, `7` = 5.97147908515944, `8` = NA, 
                                  `9` = 3.3287285084747, `10` = 11.7750924130284), 
                       TREAT2 = c(`1` = -36.0020997182921, `2` = -139.812320678151, `3` = -77.5227036654949, `4` = -60.1945591669384, 
                                  `5` = 54.2171014384302, `6` = 43.1965280595445, `7` = 59.7147908515944, `8` = NA, 
                                  `9` = 33.287285084747, `10` = 117.750924130284), 
                       rand_wei = c(-17.1969296658761, 22.9057780481208, -33.4966252613491, 57.3810802699894, 4.33294142856405, 
                                    -12.579733621978, 20.3965087501415, NA, 57.0577414340694, 
                                    39.6584493366088)), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -10L))



# libraries ---------------------------------------------------------------

library(rlang)
library(dplyr)
library(R.utils)
library(tidyselect)
library(data.table)
library(fixest)


# 1st block ------------------------------------------------------------------


fn_ctrl_rename <- function(x) { paste("ctrl", x, sep="_") }
get_controls_rename <- function(controls) { unlist(lapply(controls, fn_ctrl_rename)) }
fn_treatment_rename <- function(x) { paste("OT", x, sep="_")}
get_treatments_rename <- function(treatments) { unlist(lapply(treatments, fn_treatment_rename)) }
fn_treatment_weight_rename <- function(x) { paste("weight_", x, sep = "") }
fn_random_weight_rename <- function(x) { paste("RW", x, sep="_")}
get_random_weight_rename <- function(ws) { unlist(lapply(ws, fn_random_weight_rename)) }

# start TWFEweights function ----------------------------------------------

## 1st function twowayfeweights_rename_var ---------------------------------

twowayfeweights_rename_var <- function(df, Y, G, T, D, D0, controls, treatments, random_weights) {
  controls_rename <- get_controls_rename(controls)
  treatments_rename <- get_treatments_rename(treatments)
  
  
  if (length(random_weights) > 0) {
    random_weight_rename <- get_random_weight_rename(random_weights)
    random_weight_df <- df[random_weights]
    colnames(random_weight_df) <- random_weight_rename
  }
  
  original_names = c(Y, G, T, D, controls, treatments)
  new_names = c("Y", "G", "T", "D", controls_rename, treatments_rename)
  
  if (!is.null(D0)) {
    original_names = c(original_names, D0)
    new_names = c(new_names, "D0")
  }
  
  df <- data.frame(df) %>% select_at(vars(original_names))
  colnames(df) <- new_names
  
  if (length(random_weights) > 0) {
    df <- cbind(df, random_weight_df)
  }
  
  df
}

#TODO: It works
twowayfeweights_rename_var(df = tb2, Y="Y", G = "G", T = "T", D = "D", controls = c("CRT1", "CRT2"),
                           D0 = "D0", treatments = c("TREAT1", "TREAT2"), random_weights = "rand_wei")


## 2nd function twowayfeweights_normalize_var  -----------------------------------------------------------

twowayfeweights_normalize_var2 = function(data, varname){
  #varname = enexpr(varname)
  sdf <- data %>%
    dplyr::group_by(G, T) %>%
    summarise(tmp_mean_gt = mean({{varname}}), 
              tmp_sd_gt = sd({{varname}}), .groups = "drop" #added
    ) 
  tmp_sd_gt_sum = sum(sdf$tmp_sd_gt, na.rm=TRUE)
  
  list(retcode = (tmp_sd_gt_sum > 0), df = data)
  
}

twowayfeweights_normalize_var2(tb2, varname = D)
twowayfeweights_normalize_var2(tb2, varname = c(CRT1,CRT2))

## 3th function  twowayfeweights_transform ------------------------------------------------------------

twowayfeweights_transform2 = function(data, controls, treatments, weights = NULL){
  
  cols_cntrl = data %>% select({{controls}})
  cols_nms_cntrl = names(cols_cntrl)
  
  cols_treatments = data %>% select({{treatments}})
  cols_nms_treatments = names(cols_treatments)
  
  weights = enexpr(weights)
  
  ret = twowayfeweights_normalize_var2(data, D)
  if (ret$retcode) {
    df <- ret$df
    printf("The treatment variable 'D' in the regression varies within some group * period cells.
The results in de Chaisemartin, C. and D'Haultfoeuille, X. (2020) apply to two-way fixed effects regressions with a group * period level treatment.
The command will replace the treatment 'D' by its average value in each group * period.
The results below apply to the two-way fixed effects regression with that treatment variable.\n\n")
  }
  
  for (i in cols_nms_cntrl) {
    ret = twowayfeweights_normalize_var2(data, {{controls}})
    if (ret$retcode) {
      data <- ret$df
      printf("The control variable %s in the regression varies within some group * period cells.
The results in de Chaisemartin, C. and D'Haultfoeuille, X. (2020) apply to two-way fixed effects regressions with controls apply to group * period level controls.
The command will replace replace control variable %s by its average value in each group * period.
The results below apply to the regression with control variable %s averaged at the group * period level.\n\n", i, i, i)
    }
  }
  
  for (i in cols_nms_treatments) {
    ret = twowayfeweights_normalize_var2(data, {{treatments}})
    if (ret$retcode) {
      data <- ret$df
      printf("The other treatment variable %s in the regression varies within some group * period cells.
The results in de Chaisemartin, C. and D'Haultfoeuille, X. (2020) apply to two-way fixed effects regressions with several treatments apply to group * period level controls.
The command will replace replace other treatment variable %s by its average value in each group * period.
The results below apply to the regression with other treatment variable %s averaged at the group * period level.\n\n", i, i, i)
    }
  }
  
  if (is.null(weights)) {
    data$weights = 1
  } else{
    data = data %>% rename(weights = {{weights}})
  }
  
  data$Tfactor = factor(data$T)
  data = data %>% 
    mutate(TfactorNum = as.numeric(factor(data$Tfactor,
                                          labels = seq(1:length(unique(Tfactor))))))
  
  return(data)
  
}

res1 = twowayfeweights_transform2(tb2, c(CRT1, CRT2), treatments = c(TREAT1, TREAT2), weights = rand_wei
)

## 4th function  twowayfeweights_filter ---------------------------------


twowayfeweights_filter2 <- function(data, cmd_type, controls, treatments){
  #Remove NA -- add message
  
  data = data.table(data)
  
  controls <- map_chr(rlang::enexpr(controls), ~ rlang::as_string(.x))[-1]
  treatments = rlang::as_string(rlang::ensym(treatments))
  
  if (cmd_type != "fdTR") {
    
    data <- data %>% 
      na.omit(cols = c("Y", "G", "T", "D", controls, treatments))
    
  } else{
    
    data = data %>% 
      na.omit(cols = c("Y",      "T", "D", "D0", controls))
    
  }
  data
}


twowayfeweights_filter2(tb2_2, cmd_type = "fdTR", controls = c("CRT1", "CRT2"), treatments = "TREAT1")
twowayfeweights_filter2(tb2_2, cmd_type = "fdTR", controls = c(CRT1, CRT2), treatments = TREAT1) #without data.table


## 5th function twowayfeweights_calculate_fetr ------------------------------------------------------------


twowayfeweights_calculate_fetr2 <- function(data, controls = NULL){
  
  controls <- enexpr(controls)
  
  mean_D <- weighted.mean(data$D, data$weights, na.rm = TRUE)
  obs <- sum(data$weights)
  data <- data %>% 
    group_by(G, T) %>% 
    mutate(P_gt = sum(weights, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(P_gt = P_gt / obs,
           nat_weight = P_gt * D / mean_D)
  
  if (length(controls) == 0) {
    formula = "D ~ 1"
  } 
  else {
    
    if (length(controls) > 1) controls <- vapply(as.list(controls[-1]), rlang::as_string, FUN.VALUE = character(1))
    
    formula = paste(controls, collapse = " + ")
    formula = paste("D ~ ", formula, sep = "")
  }
  
  formula = paste(formula, " | G + Tfactor", sep = "")
  denom.lm <- feols(as.formula(formula), data = data, weights = abs(data$weights)
                    )
  data$eps_1 <- residuals(denom.lm)
  data$eps_1_E_D_gt <- data$eps_1 * data$D
  denom_W <- weighted.mean(data$eps_1_E_D_gt, data$weights, na.rm = TRUE)
  return(denom.lm)
}


twowayfeweights_calculate_fetr2(res1)
t=twowayfeweights_calculate_fetr2(res1,c(CRT1,CRT2))

t