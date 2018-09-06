## -----------------------------------------------------------------------------------------------------
## clean_data.r
## Author: Amelia Bertozzi-Villa
## Date: August 9th 2018
## Description: Analyze pre-post test from MFF game pilot in Chennai

## School info: 
# 1.30th July sunshine Academy 28 boys 24 girls. -> 52
# 2. Aug.1st .kendriya vidyala Tambaram 105 boys 125 girls.  -> 130
# 3. Aug.4th. St.Vincent 78 boys 69 girls. -> 147

## Hours: 
## 9/2 4:30-6
## 9/3 7-
## -----------------------------------------------------------------------------------------------------

library(data.table)
library(ggplot2)

rm(list=ls())

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

main_dir <- "/Users/bertozzivill/Dropbox/main/Collaborations/my_future_family/chennai_2018/data_090818/clean"

data <- fread(file.path(main_dir, "prepost.csv"))
data[, question.count:= as.integer(gsub("answer","", key))]
data[, event.name:=tolower(gsub(" Quiz", "", event.name))]

# todo: standardize column naming
# todo: check male/female #s in dataset compared to reported above 
data <- dcast(data, user.id + question.count + question + correct_answer ~ event.name, value.var=c("value", "timestamp"))

# remove questions greater than 14
data <- data[question.count<=14]

# check numbers: compare reported with dataset
# total students in dataset:
length(unique(data$user.id))

# count by date
data[, date:= ifelse(is.na(timestamp_pre), format(as.Date(timestamp_post), "%d-%m"), format(as.Date(timestamp_pre), "%d-%m"))]

counts <- unique(data[, list(user.id, date)])
counts[, count:=.N, by="date"]

# only keep data from three reported dates: July 30, August 1, August 4
# data <- data[date %in% c("30-07", "01-08", "04-08")]
## don't add up-- does it matter? 

complete <- data[!is.na(value_post) & !is.na(value_pre)]

# time elapsed between pre and post
complete[, elapsed_minutes:= as.numeric(difftime(timestamp_post, timestamp_pre, units="mins"))]
ggplot(complete[elapsed_minutes<500], aes(x=elapsed_minutes)) + geom_density() + geom_vline(xintercept=4, color="red")

# based on this, drop everyone with less than 4 or more than 50 minutes between tests
invalid_users <- unique(complete[elapsed_minutes<4 | elapsed_minutes>50]$user.id)
complete <- complete[!(user.id %in% invalid_users)]

# adjust "Testicles" to "Testicle" and "Fallopian Tube" to "Fallopian Tubes"
complete[value_pre=="Testicles", value_pre:="Testicle"]
complete[value_post=="Testicles", value_post:="Testicle"]
complete[value_pre=="Fallopian Tube", value_pre:="Fallopian Tubes"]
complete[value_post=="Fallopian Tube", value_post:="Fallopian Tubes"]

# calculate scores
complete[, c("elapsed_minutes"):=NULL]
complete[, score_pre:=100*sum(value_pre==correct_answer)/14, by="user.id"]
complete[, score_post:=100*sum(value_post==correct_answer)/14, by="user.id"]
complete[, score_gain:=score_post-score_pre]

scores <- unique(complete[, list(user.id, score_pre, score_post, score_gain)])

ggplot(scores, aes(x=score_pre, y=score_post))+ 
  geom_jitter(alpha=0.5) +
  geom_abline()

ggplot(scores, aes(x=score_gain)) +
  geom_density() + 
  geom_vline(xintercept=0, color="red")

## todo on these: assess prob and cumulative prob of getting an answer right by random chance, given the format of the quiz

# question-by-question analysis of responses
complete <- complete[value_post!=""] # drop one buggy line for user 544
complete_long <- melt(complete, id.vars = c("user.id", "question.count", "question", "correct_answer"), 
                      measure.vars=c("value_pre", "value_post"))
complete_long[, variable:=factor(variable, levels=c("value_pre", "value_post"), labels=c("Pre", "Post"))]
complete_long[, value:=factor(value, levels=rev(c("Anus Female", "Anus Male", "Bladder Male", "Fallopian Tubes", 
                                              "Ovary", "Penis", "Testicle", "Urethra Female", "Uterus", "Vagina", "not sure")))]

colors <- c("#808080", gg_color_hue(10))

ggplot(complete_long, aes(x=variable)) +
  geom_bar(aes(fill=value)) + 
  scale_fill_manual(values=colors) + 
  # coord_flip() +
  facet_wrap(~question) +
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  labs(x="", y="")

threecolor <- gg_color_hue(3)
complete_long[, short_answer:= ifelse(value==correct_answer, "Correct", 
                                      ifelse(value=="not sure", "Not Sure", "Incorrect"))]
complete_long[, short_answer:=factor(short_answer, levels=c("Incorrect", "Not Sure", "Correct"))]
ggplot(complete_long, aes(x=variable)) +
  geom_bar(aes(fill=short_answer)) + 
  scale_fill_manual(values=c(threecolor[1], threecolor[3], threecolor[2])) + 
  # coord_flip() +
  facet_wrap(~question) +
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  labs(x="", y="")


