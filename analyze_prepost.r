## -----------------------------------------------------------------------------------------------------
## clean_data.r
## Author: Amelia Bertozzi-Villa
## Date: August 9th 2018
## Description: Analyze pre-post test from MFF game pilot in Chennai

## School info: 
# 1.30th July sunshine Academy 28 boys 24 girls. -> 52
# 2. Aug.1st .kendriya vidyala Tambaram 105 boys 125 girls.  -> 230
# 3. Aug.4th. St.Vincent 78 boys 69 girls. -> 147

## Hours: 
## 9/2 4:30-6
## 9/3 7-7:30
## 9/13 5:45-6:45
## 9/15 8-9:15
## 9/22 12:15-2:34

## -----------------------------------------------------------------------------------------------------

library(Hmisc)
library(plyr)
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
data <- dcast(data, user.id + question.count + question + correct.answer ~ event.name, value.var=c("value", "timestamp"))

setnames(data, c("value_pre", "value_post", "timestamp_pre", "timestamp_post"),
         c("value.pre", "value.post", "timestamp.pre", "timestamp.post"))

# remove questions greater than 14
data <- data[question.count<=14]

# count by date
data[, date:= ifelse(is.na(timestamp.pre), format(as.Date(timestamp.post), "%d-%m"), format(as.Date(timestamp.pre), "%d-%m"))]

# only keep data from three reported dates: July 30, August 1, August 4
data <- data[date %in% c("30-07", "01-08", "04-08")]
data[, school:= mapvalues(date, c("30-07", "01-08", "04-08"), c("Sunshine Academy", "Kendriya Vidyala Tambaram", "St Vincent"))]

# question: how many people don't have post tests? are they mostly from one date/school?
no_post <- data[is.na(value.post)]
no_post[, .N/14, by="school"]


###---- Assessment of pre-test alone, by gender -----------------------------------
# adjust "Testicles" to "Testicle" and "Fallopian Tube" to "Fallopian Tubes"
data[value.pre=="Testicles", value.pre:="Testicle"]
data[value.post=="Testicles", value.post:="Testicle"]
data[value.pre=="Fallopian Tube", value.pre:="Fallopian Tubes"]
data[value.post=="Fallopian Tube", value.post:="Fallopian Tubes"]

# demographics data
demog <- fread(file.path(main_dir, "demog.csv"))

data <- merge(data, demog[, list(user.id, sex=capitalize(self.report.sex))], by="user.id", all.x=T)

pre <- copy(data)
pre[, c("value.post", "timestamp.post"):=NULL]

# 8 questions refer to female organs, 6 to male
pre[, organ.sex:= ifelse(correct.answer %in% c("Ovary", "Uterus", "Vagina", "Fallopian Tubes", "Urethra Female", "Anus Female"),
                            "Female", "Male")]

# scores-- overall and by sex of anatomy referenced
scores_pre <- pre[, list(score.total=100*sum(value.pre==correct.answer)/14), by=list(user.id, sex, school)]

pre[, total:=100*sum(value.pre==correct.answer)/14, by=user.id]
pre[, female.organs:=100*sum(value.pre==correct.answer & organ.sex=="Female")/8, by=user.id]
pre[, male.organs:=100*sum(value.pre==correct.answer & organ.sex=="Male")/6, by=user.id]

scores_pre <- unique(pre[, list(user.id, sex, school, total, female.organs, male.organs)])
scores_pre <- melt(scores_pre, id.vars = c("user.id", "sex", "school"), value.name="score")

ggplot(scores_pre, aes(x=variable, y=score)) +
  geom_boxplot(aes(fill=sex), alpha=0.5) +
  facet_grid(school~.) +
  labs(title="Scores: Overall and by Sex of Question",
       x="",
       y="Score")

### ------ Assessment of pre and post test in comparison to each other -----------------------------

# 'complete' here meaning "having responses to both pre and post test" 
complete <- data[!is.na(value.post) & !is.na(value.pre)]

# time elapsed between pre and post
complete[, elapsed.minutes:= as.numeric(difftime(timestamp.post, timestamp.pre, units="mins"))]
ggplot(complete[elapsed.minutes<500], aes(x=elapsed.minutes)) + geom_density() + geom_vline(xintercept=4, color="red")

# based on this, drop everyone with less than 4 or more than 50 minutes between tests
invalid_users <- unique(complete[elapsed.minutes<4 | elapsed.minutes>50]$user.id)
complete <- complete[!(user.id %in% invalid_users)]


# question: what is the pre/post time gap for those who answer entirely "not sure" to post-tests? is there a difference 
# between schools? 
notsure_post <- complete[value.post=="not sure"]
notsure_post[, count:=.N, by=user.id]
notsure_post[question.count==1 & count==14, .N, by="school"]
notsure_post <- notsure_post[count==14]

ggplot(notsure_post, aes(x=question)) +
  geom_bar(aes(fill=value.pre))


# calculate scores
complete[, c("elapsed.minutes"):=NULL]
complete[, score.pre:=100*sum(value.pre==correct.answer)/14, by="user.id"]
complete[, score.post:=100*sum(value.post==correct.answer)/14, by="user.id"]
complete[, score.gain:=score.post-score.pre]

scores <- unique(complete[, list(user.id, score.pre, score.post, score.gain)])

ggplot(scores, aes(x=score.pre, y=score.post))+ 
  geom_jitter(alpha=0.5) +
  geom_abline()

ggplot(scores, aes(x=score.gain)) +
  geom_density() + 
  geom_vline(xintercept=0, color="red")

## todo on these: assess prob and cumulative prob of getting an answer right by random chance, given the format of the quiz

# question-by-question analysis of responses
complete <- complete[value.post!=""] # drop one buggy line for user 544
complete_long <- melt(complete, id.vars = c("user.id", "question.count", "question", "correct.answer"), 
                      measure.vars=c("value.pre", "value.post"))
complete_long[, variable:=factor(variable, levels=c("value.pre", "value.post"), labels=c("Pre", "Post"))]
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
complete_long[, short.answer:= ifelse(value==correct.answer, "Correct", 
                                      ifelse(value=="not sure", "Not Sure", "Incorrect"))]
complete_long[, short.answer:=factor(short.answer, levels=c("Incorrect", "Not Sure", "Correct"))]
ggplot(complete_long, aes(x=variable)) +
  geom_bar(aes(fill=short.answer)) + 
  scale_fill_manual(values=c(threecolor[1], threecolor[3], threecolor[2])) + 
  # coord_flip() +
  facet_wrap(~question) +
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  labs(x="", y="")


