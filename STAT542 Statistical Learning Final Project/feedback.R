rm(list = ls())
wd <- "/Users/semyon/Library/Mobile Documents/com~apple~CloudDocs/UIUC/Sem2_sp23/STAT 542/stat542_group4_final_project"
setwd(wd)
feedback <- read.csv('feedback.csv')[,-1]
feedback <- as.matrix(feedback)
feedback <- as.vector(feedback)

feedback_orig <- read.csv('Feedback_original.csv')[,-1]
feedback_orig <- as.matrix(feedback_orig)
feedback_orig <- as.vector(feedback_orig)

feedback_our <- read.csv('feedback_predictions.csv')[,-1]
feedback_our <- as.matrix(feedback_our)
feedback_our <- as.vector(feedback_our)

feedback_naive <- feedback
nas <- which(is.na(feedback))
feedback_naive[nas] <- mean(feedback, na.rm = TRUE)
feedback_naive <- round(feedback_naive)

mse_naive <- sum((feedback_orig-feedback_naive)^2)/(length(feedback))
mse_our <- sum((feedback_orig-feedback_our)^2)/(length(feedback))

paste(round(100*(1-(mse_our/mse_naive)),2),'%',sep='')
