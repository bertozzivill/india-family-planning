## -----------------------------------------------------------------------------------------------------
## clean_data.r
## Author: Amelia Bertozzi-Villa
## Date: August 9th 2018
## Description: Analyze pre-post test from MFF game
## -----------------------------------------------------------------------------------------------------

library(data.table)
library(ggplot2)

rm(list=ls())

main_dir <- "/Users/bertozzivill/Dropbox/main/Collaborations/my_future_family/chennai_2018/data_090818/clean"

data <- fread(file.path(main_dir, "prepost.csv"))
data[, question.count:= as.integer(gsub("answer","", key))]
data[, event.name:=tolower(gsub(" Quiz", "", event.name))]

# todo: standardize column naming
data <- dcast(data, user.id + question.count + question + correct_answer ~ event.name, value.var=c("value", "timestamp"))

complete <- data[!is.na(value_post) & !is.na(value_pre)]

# time elapsed between pre and post
complete[, elapsed_seconds:= as.numeric(difftime(timestamp_post, timestamp_pre))]
complete[, elapsed_minutes:= elapsed_seconds/60]
ggplot(complete[elapsed_minutes<500], aes(x=elapsed_minutes)) + geom_density() + geom_vline(xintercept=4, color="red")

# based on this, drop everyone with less than 4 or more than 50 minutes between tests
invalid_users <- unique(complete[elapsed_minutes<4 | elapsed_minutes>50]$user.id)
complete <- complete[!(user.id %in% invalid_users)]

complete[, c("timestamp_pre", "timestamp_post", "elapsed_seconds", "elapsed_minutes"):=NULL]
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
