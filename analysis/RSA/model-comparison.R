setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ### set wd to path of the script

source("ambiguity-model.R") ### load the speaker and listener functions

library(ggplot2) ### for plotting
library(RColorBrewer)

output <- read.csv("output-everynot.csv")[,c(2:ncol(read.csv("output-everynot.csv")))] ### load the results from the exps

### matrix with qud priors for each item (exp 1)
exp1 <- sapply(1:nrow(output), function(x) c(output$many.qud[x], output$all.qud[x], output$none.qud[x]))
exp1 <- prop.table(exp1, 2) ### normalisation

### matrix with world priors for each item (exp 2)
exp2 <- sapply(1:nrow(output), function(x) c(output$none.prior[x], output$sbna.prior[x], output$all.prior[x]))
exp2 <- prop.table(exp2, 2) ### normalisation

### matrix with results from exp 3
exp3 <- sapply(1:nrow(output), function(x) c(output$none.accept[x], output$sbna.accept[x], output$all.accept[x]))
exp3 <- prop.table(exp3, 2) ### normalisation

### generate predictions based on results from exps 1 and 2
preds.full <- sapply(1:ncol(exp1), function(x) NewListener("every-not", # full model
                                                      exp2[,x], # state priors
                                                      c(0.5,0.5), # scope priors
                                                      exp1[,x], # qud priors
                                                      1)) #alpha

preds.noqud <- sapply(1:ncol(exp1), function(x) NoQUDNewListener("every-not", # model without qud
                                                      exp2[,x], # qud priors
                                                      c(0.5,0.5), # scope priors
                                                      1)) # alpha

preds.noprior <- sapply(1:ncol(exp1), function(x) NoPriorNewListener("every-not", # model without priors
                                                      c(0.5,0.5), # scope prior
                                                      exp1[,x], # qud priors
                                                      1)) # alpha

preds.inverse <- sapply(1:ncol(exp1), function(x) NewListener("every-not", # model with only inverse scope
                                                      exp2[,x], # state priors
                                                      c(0,1), # scope priors
                                                      exp1[,x], # qud priors
                                                      1)) # alpha

preds.surface <- sapply(1:ncol(exp1), function(x) NewListener("every-not", # model with only surface scope
                                                      exp2[,x], # state priors
                                                      c(1,0), # scope priors
                                                      exp1[,x], # qud priors
                                                      1)) # alpha

# get correlations between model predictions and data
# per item
for(i in list(preds.full, preds.noqud, preds.noprior, preds.inverse, preds.surface)) {
  print(mean(sapply(1:ncol(i), function(x) cor(i[,x], exp3[,x]))))
}

# per state
for(i in list(preds.full, preds.noqud, preds.noprior, preds.inverse, preds.surface)) {
  print(mean(sapply(1:nrow(i), function(x) cor(i[,x], exp3[,x]))))
}

# overall
for(i in list(preds.full, preds.noqud, preds.noprior, preds.inverse, preds.surface)) {
  print(cor(as.vector(i), as.vector(exp3)))
}

### optimization procedure to determine the optimal scope value

optimisation_all <- function(par) {

  surface <- par

  pred.together <- sapply(1:ncol(exp1), function(x) NewListener("every-not",
                                                             exp2[,x],
                                                             c(surface, 1-surface),
                                                             exp1[,x],
                                                             1))

  c1 <- mean(sapply(1:ncol(pred.together), function(x) cor(pred.together[,x], exp3[,x]))) # per item correlation
  c2 <- mean(sapply(1:nrow(pred.together), function(x) cor(pred.together[x,], exp3[x,]))) # per state correlation
  c3 <- cor(as.vector(pred.together), as.vector(exp3)) # overall correlation
  c <- mean(c(c1, c2, c3)) # mean of those correlations, which is going to be optimized

  show(c)
  return(-c)
}

### optimize: the surface scope prior can range from 0 to 1
vals <- optim(par = c(0.5), fn = optimisation_all, lower = c(0), upper = c(1), method = "L-BFGS-B")
# vals should show that $par is 0, i.e., that the optimal model makes surface scope impossible

### plot predictions vs data

df <- data.frame(data = rep(as.vector(exp3), 5), 
                 pred = c(as.vector(preds.full), as.vector(preds.noqud), as.vector(preds.noprior), as.vector(preds.surface), as.vector(preds.inverse)),
                 con = c(rep("full", length(as.vector(exp3))),
                         rep("noQUD", length(as.vector(exp3))),
                         rep("noPrior", length(as.vector(exp3))),
                         rep("surface", length(as.vector(exp3))),
                         rep("inverse", length(as.vector(exp3)))
                 ),
                 State = rep(c("None", "Some but not all", "All"), length(as.vector(exp3))*5))

df$con <- factor(df$con, levels = c("full", "noPrior", "noQUD", "surface", "inverse"))
df$State <- factor(df$State, levels = c("None", "Some but not all", "All"))

pdf("predictions.pdf", height = 3.7, width = 5)
ggplot(data = df, aes(x = data, y = pred, colour = State, fill = State)) +
  geom_point() +
  theme_classic() +
  scale_x_continuous(name = "Ratings (Exp. 3)",
                     limits = c(0, 1), 
                     breaks = c(0, 0.25, 0.5, 0.75, 1), 
                     labels = c("0", "0.25", "0.5", "0.75", "1")) +
  scale_y_continuous(name = "Predictions",
                     breaks = c(0, 0.25, 0.5, 0.75, 1), 
                     labels = c("0", "0.25", "0.5", "0.75", "1")) +
  facet_wrap( ~ con, nrow = 2, scales = "free_x") +
  scale_color_brewer(palette = "Set1") +
  theme(
    axis.text = element_text(color = "black"),
    strip.background = element_rect(color = "white"),
    legend.position = c(0.85, 0.25),
    legend.title = element_text(size = 9)
  )
dev.off()