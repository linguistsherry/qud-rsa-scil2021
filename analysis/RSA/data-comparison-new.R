setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ### set wd to path of the script

library(ggplot2) ### for plotting
library(gridExtra)

source("savinelli-new.R") ### load the speaker and listener functions

output <- read.csv("output.csv")[,c(2:ncol(read.csv("output.csv")))] ### load the results from the exps

### specify scopes and set scope priors
### easier to change if specified here
Scopes <- c("surface", "inverse")
ScopePriors <- c(0.5, 0.5)
ScopePriors <- ScopePriors / sum(ScopePriors)
names(ScopePriors) <- Scopes

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
preds <- sapply(1:ncol(exp1), function(x) NewListener("every-not",
                                                      exp2[,x],
                                                      ScopePriors,
                                                      exp1[,x],
                                                      1))

### plot data vs predictions
pdf("comparison.pdf", height = 5, width = 5)
ggplot(data = data.frame(preds = as.vector(preds), exp3 = as.vector(exp3)), aes(x = preds, y = exp3)) +
  geom_point(shape = 1) +
  theme_classic() +
  theme(axis.text = element_text(colour = "black"))
dev.off()

### optimisation function over alpha and scope priors
optimisation <- function(par) {

  alpha <- par[1]
  surface <- par[2]
  upriors <- c(par[3], rep(par[4], 3))
  
  pred.together <- sapply(1:ncol(exp1), function(x) NewListener("every-not",
                                                             exp2[,x],
                                                             c(surface, 1-surface),
                                                             exp1[,x],
                                                             alpha))
  
  c <- cor(as.vector(pred.together), as.vector(exp3))
  show(c)
  return(-c)
}

### optimise --- alpha is constrained between 1 and 10; surface between 0.1 and 0.9
vals <- optim(par = c(1, 0.5), fn = optimisation, lower = c(0.1, 0.01), upper = c(3, 0.99), method="L-BFGS-B")

### generate optimal model predictions
optim.preds <- sapply(1:ncol(exp1), function(x) NewListener("every-not",
                                                            exp2[,x],
                                                            c(vals$par[2],1-vals$par[2]),
                                                            exp1[,x],
                                                            vals$par[1]))

### plot optimal predictions vs data
pdf("optimal-comparison.pdf", height = 5, width = 5)
ggplot(data = data.frame(preds = as.vector(optim.preds), exp3 = as.vector(exp3)), aes(x = preds, y = exp3)) +
  geom_point(shape = 1) +
  theme_classic() +
  theme(axis.text = element_text(colour = "black"))
dev.off()

### optimisation function over alpha and scope priors
optimisation_noamb <- function(par) {

  alpha <- par
  
  pred.together <- sapply(1:ncol(exp1), function(x) NewListener("every-not",
                                                             exp2[,x],
                                                             c(0,1),
                                                             exp1[,x],
                                                             alpha))
  
  c <- cor(as.vector(pred.together), as.vector(exp3))
  show(c)
  return(-c)
}

### optimise --- alpha is constrained between 1 and 10; surface between 0.1 and 0.9
vals.noamb <- optim(par = c(1), fn = optimisation_noamb, lower = c(0.1), upper = c(3), method="L-BFGS-B")

### generate optimal model predictions
optim.preds.noamb <- sapply(1:ncol(exp1), function(x) NewListener("every-not",
                                                            exp2[,x],
                                                            c(0,1),
                                                            exp1[,x],
                                                            vals.noamb$par[1]))

### plot optimal predictions vs data
df <- data.frame(data = as.vector(exp3),
                 amb = as.vector(optim.preds),
                 noamb = as.vector(optim.preds.noamb))

a <- ggplot(data = df, aes(x = data, y = amb)) +
  geom_point(shape = 1) +
  theme_classic() +
  ggtitle("Ambiguity") +
  xlab("Data") +
  scale_x_continuous(limits = c(0, 1)) +
  ylab("Model predictions") +
  theme(axis.text = element_text(colour = "black"),
        plot.title = element_text(face = "bold"))

b <- ggplot(data = df, aes(x = data, y = noamb)) +
  geom_point(shape = 1) +
  ggtitle("No ambiguity") +
  xlab("Data") +
  scale_x_continuous(limits = c(0, 1)) +
  theme_classic() +
  theme(axis.text = element_text(colour = "black"),
        plot.title = element_text(face = "bold"),
        axis.title.y = element_blank())

pdf("comparison.pdf", height = 2.1, width = 4.2)
grid.arrange(a, b, nrow = 1, ncol = 2, widths = c(1, 0.95))
dev.off()

# install.packages("rjags")
# install.packages("coda")

require(rjags)
require(coda)
