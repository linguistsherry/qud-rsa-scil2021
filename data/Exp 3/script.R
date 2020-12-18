setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# options(java.parameters = "- Xmx1024m")

# library("xlsx")
library("qdapRegex")
library("tidyr")
library("Rmisc")
library("ggplot2")
library("ggrepel")

### uncomment to load the data (takes a while)
res <- read.csv("results_all.csv")
res$RT <- NA
res <- res[,c("Time", "quantifier", "prior", "qudbias", "sentence", "item", "rating", "RT")]
colnames(res)[1] <- "subject"
res <- res[!is.na(res$rating),]
res$rating <- as.numeric(paste(res$rating))

### bad subject
bad.subjects <- c(1568370750, 1568816552, 1568821156, 1568370750)
res <- res[!(res$subject %in% bad.subjects),]

### get the question types
res$question <- sapply(rm_between(res$sentence, "<b>", "</b>", extract = TRUE), function(x) (x[[1]]))
res$question <- ifelse(res$question %in% c("all", "All"), "all",
                       ifelse(res$question %in% c("any", "None"), "none", "some"))

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

for(i in unique(res$subject)) {
  for(j in unique(res$item)) {
    if(nrow(res[res$subject == i & res$item == j,]) < 3) {
      subjects <- append(subjects, rep(i, 3 - nrow(res[res$subject == i & res$item == j,])))
      items <- append(items, rep(j, 3 - nrow(res[res$subject == i & res$item == j,])))
      priors <- append(priors, rep(paste(res[res$subject == i & res$item == j,]$prior[1]), 3 - nrow(res[res$subject == i & res$item == j,])))
      qudbiases <- append(qudbiases, rep(paste(res[res$subject == i & res$item == j,]$qudbias[1]), 3 - nrow(res[res$subject == i & res$item == j,])))
      questions <- append(questions, (setdiff(c("all", "none", "some"), res[res$subject == i & res$item == j,]$question)))
    }
  }
}

temp <- data.frame(subject = subjects, quantifier = rep("EveryNot", length(subjects)), prior = priors,
                   qudbias = qudbiases, sentence = rep("na", length(subjects)), item = items,
                   rating = rep(50, length(subjects)), RT = rep(NA, length(subjects)), question = questions)

res <- rbind(res, temp)

### normalize ratings per item
res$norm <- sapply(c(1:nrow(res)), function(x) (res$rating[x] - min(res[res$item == res$item[x] & res$subject == res$subject[x],]$rating)) /
                     (max(res[res$item == res$item[x] & res$subject == res$subject[x],]$rating) -
                        min(res[res$item == res$item[x] & res$subject == res$subject[x],]$rating)))

# res$prior <- ifelse(res$item == 23, "All", paste(res$prior))

### output rating per item
output <- data.frame(item = rep(unique(res$item), 3),
                     qudbias = c(rep("NoneBias", length(unique(res$item))),
                                 rep("AllBias", length(unique(res$item))),
                                 rep("HowManyBias", length(unique(res$item)))),
                     prior = rep(sapply(unique(res$item), function(x) res[res$item == x,]$prior[1]), 3),
                     none = c(sapply(unique(res$item), function(x) mean(res[res$item == x & res$qudbias == "NoneBias" & res$question == "none",]$norm)),
                              sapply(unique(res$item), function(x) mean(res[res$item == x & res$qudbias == "AllBias" & res$question == "none",]$norm)),
                              sapply(unique(res$item), function(x) mean(res[res$item == x & res$qudbias == "HowManyBias" & res$question == "none",]$norm))),
                     all = c(sapply(unique(res$item), function(x) mean(res[res$item == x & res$qudbias == "NoneBias" & res$question == "all",]$norm)),
                             sapply(unique(res$item), function(x) mean(res[res$item == x & res$qudbias == "AllBias" & res$question == "all",]$norm)),
                             sapply(unique(res$item), function(x) mean(res[res$item == x & res$qudbias == "HowManyBias" & res$question == "all",]$norm))),
                     some = c(sapply(unique(res$item), function(x) mean(res[res$item == x & res$qudbias == "NoneBias" & res$question == "some",]$norm)),
                              sapply(unique(res$item), function(x) mean(res[res$item == x & res$qudbias == "AllBias" & res$question == "some",]$norm)),
                              sapply(unique(res$item), function(x) mean(res[res$item == x & res$qudbias == "HowManyBias" & res$question == "some",]$norm))))


### load in results from previous experiments
priorexp <- read.table("output-priorexp.csv")
qudexp <- read.table("output-qudexp.csv")

colnames(output)[4:6] <- c("none.accept", "all.accept", "sbna.accept")
colnames(priorexp)[4:6] <- c("none.prior", "all.prior", "sbna.prior")
colnames(qudexp)[4:6] <- c("none.qud", "all.qud", "many.qud")

temp <- merge(priorexp, qudexp, by = c("item", "qudbias", "prior"))
output <- merge(output, temp, by = c("item", "qudbias", "prior"))

### for convenience: convert values to the 0:100 interval
output$none.accept <- output$none.accept * 100
output$all.accept <- output$all.accept * 100
output$sbna.accept <- output$sbna.accept * 100

output$none.prior <- output$none.prior * 100
output$all.prior <- output$all.prior * 100
output$sbna.prior <- output$sbna.prior * 100

output$none.qud <- output$none.qud * 100
output$all.qud <- output$all.qud * 100
output$many.qud <- output$many.qud * 100

res$norm <- res$norm * 100

output <- output[!(output$item == 23),]
output <- write.csv(output, "output-everynot.csv") 

### separate prior effects
temp <- gather(output, condition, rating, c("none.prior", "all.prior", "sbna.prior"), factor_key = TRUE)
temp$condition <- ifelse(temp$condition == "none.prior", "None-prior",
                         ifelse(temp$condition == "all.prior", "All-prior", "Some-prior"))
temp$condition <- factor(temp$condition, levels = c("None-prior", "Some-prior", "All-prior"))

pdf("Exp3-prior.pdf", height = 2, width = 4.5)
ggplot(data = temp, aes(x = rating, y = sbna.accept)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm, se = FALSE) +
  facet_grid( ~ condition) +
  xlab("Prior probability rating (Exp. 1)") +
  ylab("'Some but not all' rating") +
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

### separate qud effects
temp <- gather(output, condition, rating, c("none.qud", "all.qud", "many.qud"), factor_key = TRUE)
temp$condition <- ifelse(temp$condition == "none.qud", "Any?-QUD",
                         ifelse(temp$condition == "all.qud", "All?-QUD", "Many?-QUD"))
temp$condition <- factor(temp$condition, levels = c("Any?-QUD", "Many?-QUD", "All?-QUD"))

pdf("Exp3-qudbias.pdf", height = 2.0, width = 4.5)
ggplot(data = temp, aes(x = rating, y = sbna.accept)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm, se = FALSE) +
  facet_grid( ~ condition) +
  xlab("QUD salience rating (Exp. 2)") +
  ylab("'Some but not all' rating") +
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