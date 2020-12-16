setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# options(java.parameters = "- Xmx1024m")

# library("xlsx")
library("qdapRegex")
library("tidyr")
library("Rmisc")
library("ggplot2")
library("ggrepel")

### uncomment to load the data (takes a while)
# res <- read.xlsx("results_all.xlsx", 4)
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

### fill our response times (no rts were measured)
### res <- res %>% fill(RT, .direction = "up")

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
# rts <- c()

for(i in unique(res$subject)) {
  for(j in unique(res$item)) {
    if(nrow(res[res$subject == i & res$item == j,]) < 3) {
      subjects <- append(subjects, rep(i, 3 - nrow(res[res$subject == i & res$item == j,])))
      items <- append(items, rep(j, 3 - nrow(res[res$subject == i & res$item == j,])))
      priors <- append(priors, rep(paste(res[res$subject == i & res$item == j,]$prior[1]), 3 - nrow(res[res$subject == i & res$item == j,])))
      qudbiases <- append(qudbiases, rep(paste(res[res$subject == i & res$item == j,]$qudbias[1]), 3 - nrow(res[res$subject == i & res$item == j,])))
      # rts <- append(rts, rep(as.numeric(paste(res[res$subject == i & res$item == j,]$RT[1])), 3 - nrow(res[res$subject == i & res$item == j,])))
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

### new prior measure 
### get a single value by adding 0 * rating for none situation + 0.5 * rating for some but not all situation + 1 * rating for all situation
new.prior <- sapply(unique(output$item), function(x) 
    weighted.mean(x = c(0, 50, 100), w = c(mean(output[output$item == x,]$none.prior), 
                                           mean(output[output$item == x,]$sbna.prior), 
                                           mean(output[output$item == x,]$all.prior))))

new.accept <- sapply(unique(output$item), function(x) mean(output[output$item == x,]$sbna.accept))

prior <- data.frame(item = unique(output$item), new.prior = new.prior, new.accept = new.accept)
output <- merge(output, prior, by = c("item"))

### many people accepted the "all" situation for item 23, so remove that item
output <- output[!(output$item == 23),]

pdf("sbna-prior-global.pdf", height = 2.0, width = 2.7)
ggplot(data = output, aes(x = new.prior, y = sbna.accept)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm, se = FALSE) +
  #facet_grid( ~ condition) +
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

### global prior effect (none)
output$prior.global <- 0 * output$none.prior + 0.5 * output$sbna.prior + 1 * output$all.prior

pdf("none-prior-global.pdf", height = 2.0, width = 2.7)
ggplot(data = output, aes(x = prior.global, y = none.accept)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm, se = FALSE) +
  #facet_grid( ~ condition) +
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

### separate prior effects (none)
temp <- gather(output, condition, rating, c("none.prior", "all.prior", "sbna.prior"), factor_key = TRUE)

pdf("none-prior-separate.pdf", height = 2.0, width = 3.7)
ggplot(data = temp, aes(x = rating, y = none.accept)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm, se = FALSE) +
  facet_grid( ~ condition) +
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

### separate qud effects (none)
temp <- gather(output, condition, rating, c("none.qud", "all.qud", "many.qud"), factor_key = TRUE)

pdf("none-qud-separate.pdf", height = 2.0, width = 3.7)
ggplot(data = temp, aes(x = rating, y = none.accept)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm, se = FALSE) +
  facet_grid( ~ condition) +
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

### global prior effect (all)
output$prior.global <- 0 * output$none.prior + 0.5 * output$sbna.prior + 1 * output$all.prior

pdf("all-prior-global.pdf", height = 2.0, width = 2.7)
ggplot(data = output, aes(x = prior.global, y = all.accept)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm, se = FALSE) +
  #facet_grid( ~ condition) +
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

### separate prior effects (all)
temp <- gather(output, condition, rating, c("none.prior", "all.prior", "sbna.prior"), factor_key = TRUE)

pdf("all-prior-separate.pdf", height = 2.0, width = 3.7)
ggplot(data = temp, aes(x = rating, y = all.accept)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm, se = FALSE) +
  facet_grid( ~ condition) +
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

### separate qud effects (all)
temp <- gather(output, condition, rating, c("none.qud", "all.qud", "many.qud"), factor_key = TRUE)

pdf("all-qud-separate.pdf", height = 2.0, width = 3.7)
ggplot(data = temp, aes(x = rating, y = all.accept)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm, se = FALSE) +
  facet_grid( ~ condition) +
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


### plot results in the style of Exp1 and Exp2
res <- res[!is.na(res$norm),]

### plot preference per prior
summary <- summarySEwithin(data = res, measurevar = "norm",
                           withinvars = c("prior", "question", "qudbias"),
                           idvar = "subject", conf.interval = 0.95)

pdf("norm-both.pdf", height = 4, width = 4)
ggplot(summary, aes(x = question, y = norm, fill = question)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = norm - ci, ymax = norm + ci),
                width = 0.2, position = position_dodge(0.9)) +
  facet_grid(qudbias ~ prior) +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.05), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
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

summary <- summarySEwithin(data = res, measurevar = "rating",
                           withinvars = c("prior", "question", "qudbias"),
                           idvar = "subject", conf.interval = 0.95)

summary$question <- rep(c(rep("All", 3), rep("None", 3), rep("Some", 3)), 3)
summary$qudbias <- c(rep(c("All?", "Many?", "None?"), 9))
summary$prior <- factor(summary$prior, levels = c("None", "Some", "All"))
summary$qudbias <- factor(summary$qudbias, levels = c("None?", "Many?", "All?"))
  
pdf("raw-both.pdf", height = 4, width = 5)
ggplot(summary, aes(x = question, y = rating, fill = question)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = rating - ci, ymax = rating + ci),
                width = 0.2, position = position_dodge(0.9)) +
  facet_grid(qudbias ~ prior) +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 102), breaks = c(0, 25, 50, 75, 100)) +
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

