#' Poisson dist for count data
#' @export

State <- sample(c("District of Columbia"), size=230, replace=TRUE)
Covid <- sample(c(NA,17, 43,107,231,308,239,226,0,1173,0,NA,NA,22,45,68,39,23,0,204
                  ,NA,13,30,91,193,279,219,212,0,1040,0,0,NA,NA,NA,NA,NA,NA,0,38,0,0,0,0
                  ,0,0,0,0,0,0,0,NA,NA,NA,NA,NA,NA,NA,0,25,0,NA,16,69,190,289,224,215,0
                  ,1008,0,0,NA,NA,29,64,45,33,0,180,0,0,0,NA,NA,0,NA,0,0,NA,0,0,NA
                  ,NA,NA,NA,21,21,0,64,0,0,NA,NA,14,28,12,15,0,76,NA,0,0,NA,NA,15,15,10,0,56,0
                  ,0,NA,NA,17,23,21,19,0,89,0,0,0,NA,NA,NA,10,NA,0,23,0,0,NA
                  ,NA,23,49,45,28,0,153,NA,NA,16,57,152,213,122,90,0,657,NA,NA,18,31
                  ,50,34,10,NA,0,156,0,0,0,0,0,NA,NA,12,0,19,0,0,0,0,NA,17,48,69
                  ,0,140,0,0,NA,12,14,32,17,17,0,96,0,0,NA,NA,12,NA,NA,NA,0,46,NA,NA,18,46,96,114
                  ,87,73,0,446,NA,19,54,143,305,416,316,287,0,1546))
Condition <- rep(c("Influenza and pneumonia","Chronic lower respiratory diseases","Adult respiratory distress syndrome",
                   "Respiratory failure","Respiratory arrest","Other diseases of the respiratory system",
                   "Hypertensive diseases","Ischemic heart disease","Cardiac arrest","Cardiac arrhythmia",
                   "Heart failure","Cerebrovascular diseases","Other diseases of the circulatory system",
                   "Sepsis","Malignant neoplasms","Diabetes","Obesity","Alzheimer disease","Vascular and unspecified dementia",
                   "Renal failure","Intentional and unintentional injury, poisoning, and other adverse events",
                   "All other conditions and causes","COVID-19"), times=10)
Age.Group <- rep(c("0-24","25-34","35-44","45-54","55-64","65-74","75-84","85+",
                   "Not Stated","All Ages"), times=23)



cvd <- data.frame(State, Covid, Condition, Age.Group)
rm(State, Covid, Condition, Age.Group) #remove sex and age from the global environment
head(cvd)

to_analyze <- function(Covid, Condition, Age.Group, data){
  fit <- lm(formula = Covid ~ Condition + Age.Group, family=poisson, data=cvd)
  fit_summary <- summary(fit)
  return(fit_summary)
}

to_analyze(10,"Influenza and pneumonia", "35-44")
to_analyze(15,"COVID-19", "65-74")
