setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library("qdapRegex")
library("tidyr")
library("Rmisc")
library("ggplot2")
library("reshape2")

### uncomment to load the data (takes a while)
# res <- read.xlsx("results_all.xlsx", 2)
res <- read.csv("results_all.csv")
res <- res[res$rating != -1,]
res <- res[,c("Time", "quantifier", "prior", "qudbias", "sentence", "item", "rating", "RT")]
colnames(res)[1] <- "subject"
res <- res[!is.na(res$rating),]

### get the question types
res$question <- sapply(rm_between(res$sentence, "<b>", "</b>", extract = TRUE), function(x) (x[[1]]))
res$question <- ifelse(res$question %in% c("all of", "Did all", "Have all", "Were all"), "all",
                       ifelse(res$question %in% c("any of", "Did any", "Have any", "Were any"), "any", "many"))

### fill our response times
res <- res %>% fill(RT, .direction = "up")

### remove instances where participants clicked multiple times
res$temp <- c(sapply(c(1:(nrow(res)-1)), function(x) res$item[x] == res$item[x+1] & 
                       res$subject[x] == res$subject[x+1] &
                       res$sentence[x] == res$sentence[x+1]), FALSE)
res <- res[!(res$temp),]
res$temp <- NULL

### fill in value 50 for non-answered questions

subjects <- c()
items <- c()
questions <- c()
priors <- c()
qudbiases <- c()
rts <- c()

for(i in unique(res$subject)) {
  for(j in unique(res$item)) {
    if(nrow(res[res$subject == i & res$item == j,]) < 3) {
      subjects <- append(subjects, rep(i, 3 - nrow(res[res$subject == i & res$item == j,])))
      items <- append(items, rep(j, 3 - nrow(res[res$subject == i & res$item == j,])))
      priors <- append(priors, rep(paste(res[res$subject == i & res$item == j,]$prior[1]), 3 - nrow(res[res$subject == i & res$item == j,])))
      qudbiases <- append(qudbiases, rep(paste(res[res$subject == i & res$item == j,]$qudbias[1]), 3 - nrow(res[res$subject == i & res$item == j,])))
      rts <- append(rts, rep(as.numeric(paste(res[res$subject == i & res$item == j,]$RT[1])), 3 - nrow(res[res$subject == i & res$item == j,])))
      questions <- append(questions, (setdiff(c("all", "any", "many"), res[res$subject == i & res$item == j,]$question)))
    }
  }
}

temp <- data.frame(subject = subjects, quantifier = rep("EveryNot", length(subjects)), prior = priors,
                   qudbias = qudbiases, sentence = rep("na", length(subjects)), item = items,
                   rating = rep(50, length(subjects)), RT = rts, question = questions)

res <- rbind(res, temp)

### normalize ratings per item
res$norm <- sapply(c(1:nrow(res)), function(x) (res$rating[x] - min(res[res$item == res$item[x] & res$subject == res$subject[x],]$rating)) /
         (max(res[res$item == res$item[x] & res$subject == res$subject[x],]$rating) -
            min(res[res$item == res$item[x] & res$subject == res$subject[x],]$rating)))

### output rating per item
output <- data.frame(item = rep(unique(res$item), 3),
                     qudbias = c(rep("NoneBias", length(unique(res$item))),
                                 rep("AllBias", length(unique(res$item))),
                                 rep("HowManyBias", length(unique(res$item)))),
                     prior = rep(sapply(unique(res$item), function(x) res[res$item == x,]$prior[1]), 3),
                     none = c(sapply(unique(res$item), function(x) mean(res[res$item == x & 
                                                                              res$qudbias == "NoneBias" & res$question == "any",]$norm, na.rm = TRUE)),
                              sapply(unique(res$item), function(x) mean(res[res$item == x & 
                                                                              res$qudbias == "AllBias" & res$question == "any",]$norm, na.rm = TRUE)),
                              sapply(unique(res$item), function(x) mean(res[res$item == x & 
                                                                              res$qudbias == "HowManyBias" & res$question == "any",]$norm, na.rm = TRUE))),
                     all = c(sapply(unique(res$item), function(x) mean(res[res$item == x & 
                                                                             res$qudbias == "NoneBias" & res$question == "all",]$norm, na.rm = TRUE)),
                             sapply(unique(res$item), function(x) mean(res[res$item == x & 
                                                                             res$qudbias == "AllBias" & res$question == "all",]$norm, na.rm = TRUE)),
                             sapply(unique(res$item), function(x) mean(res[res$item == x & 
                                                                             res$qudbias == "HowManyBias" & res$question == "all",]$norm, na.rm = TRUE))),
                     many = c(sapply(unique(res$item), function(x) mean(res[res$item == x & 
                                                                              res$qudbias == "NoneBias" & res$question == "many",]$norm, na.rm = TRUE)),
                             sapply(unique(res$item), function(x) mean(res[res$item == x & 
                                                                             res$qudbias == "AllBias" & res$question == "many",]$norm, na.rm = TRUE)),
                             sapply(unique(res$item), function(x) mean(res[res$item == x & 
                                                                             res$qudbias == "HowManyBias" & res$question == "many",]$norm, na.rm = TRUE))))
                                                                        
write.table(output, "output-qudexp.csv")

### plot preference per a priori categorization for qud

summary <- summarySEwithin(data = res, measurevar = "rating",
                           withinvars = c("qudbias", "question"),
                           idvar = "subject", conf.interval = 0.95)

summary$qudbias <- c(rep(c("All?-QUD"), 3), rep("Many?-QUD", 3), rep("Any?-QUD", 3))
summary$qudbias <- factor(summary$qudbias, levels = c("Any?-QUD", "Many?-QUD", "All?-QUD"))
summary$question <- rep(c("All?", "Any?", "Many?"), 3)
summary$question <- factor(summary$question, levels = c("Any?", "Many?", "All?"))

pdf("Exp2-qudbias.pdf", height = 1.8, width = 5)
ggplot(summary, aes(x = question, y = rating, fill = question)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = rating - ci, ymax = rating + ci),
                width = 0.2, position = position_dodge(0.9)) +
  facet_grid(. ~ qudbias) +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(name = "Rating", expand = c(0, 0), limits = c(0, 100)) +
  theme_linedraw() +
  xlab("QUD") +
  theme(strip.background = element_rect(size = 0, fill = "white", colour = "white"),
        strip.text = element_text(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.line = element_line(colour = "black")
  )
dev.off()

### plot preference per a priori categorization for priors

summary <- summarySEwithin(data = res, measurevar = "rating",
                           withinvars = c("prior", "question"),
                           idvar = "subject", conf.interval = 0.95)

summary$prior <- ifelse(summary$prior == "All", "All-prior", ifelse(summary$prior == "None", "None-prior", "Some-prior"))
summary$prior <- factor(summary$prior, levels = c("None-prior", "Some-prior", "All-prior"))
summary$question <- ifelse(summary$question == "all", "All?", ifelse(summary$question == "any", "Any?", "Many?"))
summary$question <- factor(summary$question, levels = c("Any?", "Many?", "All?"))

pdf("Exp2-prior.pdf", height = 1.8, width = 5)
ggplot(summary, aes(x = question, y = rating, fill = question)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = rating - ci, ymax = rating + ci),
                width = 0.2, position = position_dodge(0.9)) +
  facet_grid(. ~ prior) +
  scale_fill_brewer(palette="Set1") +
  xlab("QUD") +
  scale_y_continuous(name = "Rating", expand = c(0, 0), limits = c(0, 100)) +
  theme_linedraw() +
  theme(strip.background = element_rect(size = 0, fill = "white", colour = "white"),
        strip.text = element_text(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.line = element_line(colour = "black")
  )
dev.off()

### plot qud preference for a priori categorization for both qud and priors

summary <- summarySEwithin(data = res, measurevar = "rating",
                           withinvars = c("prior", "question", "qudbias"),
                           idvar = "subject", conf.interval = 0.95)


summary$prior <- ifelse(summary$prior == "All", "All-prior", ifelse(summary$prior == "None", "None-prior", "Some-prior"))
summary$prior <- factor(summary$prior, levels = c("None-prior", "Some-prior", "All-prior"))
summary$question <- ifelse(summary$question == "all", "All?", ifelse(summary$question == "any", "Any?", "Many?"))
summary$question <- factor(summary$question, levels = c("Any?", "Many?", "All?"))
summary$qudbias <- ifelse(summary$qudbias == "AllBias", "All?-QUD", ifelse(summary$qudbias == "HowManyBias", "Many?-QUD", "Any?-QUD"))
summary$qudbias <- factor(summary$qudbias, levels = c("Any?-QUD", "Many?-QUD", "All?-QUD"))

pdf("Exp2-both.pdf", height = 4, width = 5)
ggplot(summary, aes(x = question, y = rating, fill = question)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = rating - ci, ymax = rating + ci),
                width = 0.2, position = position_dodge(0.9)) +
  facet_grid(qudbias ~ prior) +
  scale_fill_brewer(palette="Set1") +
  xlab("QUD") +
  scale_y_continuous(name = "Rating", expand = c(0, 0), limits = c(0, 102), breaks = c(0, 25, 50, 75, 100)) +
  theme_linedraw() +
  theme(strip.background = element_rect(size = 0, fill = "white", colour = "white"),
        strip.text = element_text(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.line = element_line(colour = "black")
  )
dev.off()