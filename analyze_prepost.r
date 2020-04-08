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
library(RColorBrewer)
library(ggplot2)

rm(list=ls())

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

main_dir <- "/Users/bertozzivill/Dropbox (Personal)/main/Collaborations/games/my_future_family/chennai_2018/data_090818/clean"
plot_dir <- file.path(main_dir, "plots/prepost")

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
data[, school:= mapvalues(date, c("30-07", "01-08", "04-08"), c("Sunshine Academy", "Kendriya Vidyala \n Tambaram", "St Vincent"))]

# question: how many people don't have post tests? are they mostly from one date/school?
no_post <- data[is.na(value.post)]
no_post[, .N/14, by="school"]

print(paste(length(unique(no_post$user.id)), "of", length(unique(data$user.id)), "students do not have post-tests" )) 

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
scores_pre <- melt(scores_pre, id.vars = c("user.id", "sex", "school"), value.name="score", variable.name = "question_type")

scores_pre[, question_type:=mapvalues(question_type, c("total", "male.organs", "female.organs"), 
                                 c("All Questions", "Male Anatomy \n Questions", "Female Anatomy \n Questions"))]

scores_pre[, sex:=mapvalues(sex, c("Female", "Male"), 
                            c("Female Respondent", "Male Respondent"))]

scores_pre_agg <- scores_pre[, list(mean=mean(score), se=sd(score)/sqrt(.N)), by=list(sex, school, question_type)]

school_sex_barplot <- ggplot(scores_pre_agg[question_type=="All Questions"], aes(x=school, y=mean, ymin=mean-se, ymax=mean+se)) +
geom_bar(aes(color=sex, fill=sex), alpha=0.75, position=position_dodge(), stat="identity") +
geom_errorbar(aes(group=sex), position=position_dodge(0.9), width=0.2) + 
theme_minimal() + 
theme(legend.title = element_blank()) +
labs(title="Pre-Test Scores, by School and Sex of Respondent",
     x="",
     y="Score")

t.test(scores_pre[question_type%like% "All" & school %like% "Vidyala"]$score,
       scores_pre[question_type%like% "All" & !school %like% "Vidyala"]$score)

t.test(scores_pre[question_type%like% "All" & school %like% "Vincent" & sex %like% "Female"]$score,
       scores_pre[question_type%like% "All" & school %like% "Vincent" & sex %like% "Male"]$score)


pdf(file.path(plot_dir, "school_sex_barplot.pdf"), height=7, width=7)
  print(school_sex_barplot)
graphics.off()

scores_pre_agg_over_school <- scores_pre[, list(mean=mean(score), se=sd(score)/sqrt(.N)), by=list(sex, question_type)]

t.test(scores_pre[question_type%like% "Female" & sex %like% "Female"]$score,
       scores_pre[question_type%like% "Female" & sex %like% "Male"]$score)
t.test(scores_pre[question_type%like% "Male" & sex %like% "Female"]$score,
       scores_pre[question_type%like% "Male" & sex %like% "Male"]$score)




question_sex_barplot <- ggplot(scores_pre_agg_over_school, aes(x=question_type, y=mean, ymin=mean-se, ymax=mean+se)) +
  geom_bar(aes(color=sex, fill=sex), alpha=0.75, position=position_dodge(), stat="identity") +
  geom_errorbar(aes(group=sex), position=position_dodge(0.9), width=0.2) + 
  theme_minimal() + 
  theme(legend.title = element_blank()) +
  labs(title="Pre-Test Scores, by Type of Question and Sex of Respondent",
       x="",
       y="Score")

pdf(file.path(plot_dir, "question_sex_barplot.pdf"), height=7, width=7)
  print(question_sex_barplot)
graphics.off()


pre[, value.pre.factor:=factor(value.pre, levels=c("not sure", "Anus Female", "Anus Male", "Bladder Male", "Urethra Female", "Fallopian Tubes", 
                                                  "Ovary", "Penis", "Testicle", "Uterus", "Vagina"))]

# save table of correct answers
correct_key <- unique(data[, list(question, correct.answer)])

# convert answer counts to proportions
pre_props <- pre[, list(count=.N), by=list(question, value.pre.factor)]
pre_props[, tot_count:=sum(count), by=list(question)]
pre_props[, prop:=count/tot_count]
pre_props <- merge(pre_props, correct_key, by="question", all=T)
pre_props[, is.correct:= ifelse(value.pre.factor==correct.answer, "Correct Answer", "Wrong Answer")]

# find which answers have a plurality of correct responses
prop_correct <- pre_props[is.correct %like% "Correct", list(question, prop.correct=prop)]
pre_props <- merge(pre_props, prop_correct)
pre_props[, diff.correct:= prop.correct-max(prop), by="question"]
unique(pre_props[diff.correct!=0]$question)

init_colors <- brewer.pal(9, "YlGnBu")
colors <- c("#808080", init_colors[5:8], init_colors[1:4], init_colors[9:10])
colors <- c("#808080","#fffff3", init_colors )

pre_answer_bar <- ggplot(pre_props, aes(x=question, y=prop)) +
  geom_bar(stat="identity", aes(fill=value.pre.factor, color=is.correct, linetype=is.correct), size=1) + 
  scale_fill_manual(values=colors) + 
  scale_color_manual(values = c("black", "white")) +
  scale_linetype_manual(values=c("solid", "blank")) + 
  coord_flip() + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.title = element_blank(),
  ) +
  labs(x="", y="Proportion",
       title="Proportional Responses to Pre-Test")

pdf(file.path(plot_dir, "pretest_answers_barplot.pdf"), height=8, width=11)
  print(pre_answer_bar)
graphics.off()


### ------ Assessment of pre and post test in comparison to each other -----------------------------

# 'complete' here meaning "having responses to both pre and post test" 
complete <- data[!is.na(value.post) & !is.na(value.pre)]

# time elapsed between pre and post
complete[, elapsed.minutes:= as.numeric(difftime(timestamp.post, timestamp.pre, units="mins"))]
ggplot(complete[elapsed.minutes<500], aes(x=elapsed.minutes)) + geom_density() + geom_vline(xintercept=4, color="red")

# based on this, drop everyone with less than 4 or more than 50 minutes between tests
invalid_users <- unique(complete[elapsed.minutes<4 | elapsed.minutes>50]$user.id)
complete <- complete[!(user.id %in% invalid_users)]


# find people who answer entirely "not sure"
notsure_post <- complete[value.post=="not sure"]
notsure_post[, count:=.N, by=user.id]
notsure_post[question.count==1 & count==14, .N, by="school"]
notsure_post <- notsure_post[count==14]
notsure_ids <- unique(notsure_post$user.id)
complete[, post.status:=ifelse(user.id %in% notsure_ids, "Null Post Test", "Attempted Post Test") ]

# calculate scores
complete[, score.pre:=100*sum(value.pre==correct.answer)/14, by="user.id"]
complete[, score.post:=100*sum(value.post==correct.answer)/14, by="user.id"]
complete[, score.gain:=score.post-score.pre]

scores <- unique(complete[, list(user.id,post.status, score.pre, score.post, score.gain)])
non_null <- scores[!post.status %like% "Null"]
t.test(non_null$score.gain)

null <- scores[post.status %like% "Null"]
t.test(null$score.gain)

prepost_scatter <- ggplot(scores, aes(x=score.pre, y=score.post))+ 
  geom_jitter(aes(color=post.status)) +
  geom_abline() +
  theme_minimal() +
  theme(legend.title = element_blank()) + 
  labs(x="Pregame Score",
       y="Postgame Score",
       title="Pre- vs Post-Test Scores, by Type of Post-Test")

pdf(file.path(plot_dir, "prepost_scatter.pdf"), height=7, width=7)
  print(prepost_scatter)
graphics.off()

score_gain_dist <- ggplot(non_null, aes(x=score.gain)) +
                        geom_density(aes(fill=post.status, color=post.status), alpha=0.5) + 
                        geom_vline(xintercept=0, color="black") +
                        theme_minimal() +
                        theme(legend.position = "none") + 
                        labs(x="Score Gain",
                             y="Density",
                             title="Distribution of Score Gain Among Students Who Attempted Post-Test")
pdf(file.path(plot_dir, "score_gain_dist.pdf"), height=7, width=8)
  print(score_gain_dist)
graphics.off()

non_null_summary <- melt(non_null, id.vars=c("post.status", "user.id"), variable.name = "test_type", value.name="score")
non_null_summary <- non_null_summary[!test_type %like% "gain"]
non_null_summary[, test_type:=factor(test_type, levels=c("score.pre", "score.post"),
                                     labels=c("Pre-Test", "Post-Test"))]
prepost_summary <- ggplot(non_null_summary, aes(x=test_type, y=score, fill=test_type, color=test_type)) + 
                            geom_violin(alpha=0.5) +
                            stat_summary(fun.data = "mean_sdl",  fun.args = list(mult = 1), 
                                        geom = "pointrange") + 
                            theme(legend.position = "none") +
                            labs(title="Score Comparison Among\nThose Who Attempted Post-Test",
                                 x="",
                                 y="Score")

pdf(file.path(plot_dir, "prepost_summary_violin.pdf"), height=5, width=4)
  print(prepost_summary)
graphics.off()


## todo on these: assess prob and cumulative prob of getting an answer right by random chance, given the format of the quiz

# question-by-question analysis of responses
complete <- complete[value.post!=""] # drop one buggy line for user 544
complete_long <- melt(complete, id.vars = c("user.id", "post.status", "question.count", "question", "correct.answer"), 
                      measure.vars=c("value.pre", "value.post"))
complete_long[, variable:=factor(variable, levels=c("value.pre", "value.post"), labels=c("Pre", "Post"))]
complete_long[, value:=factor(value, levels=c("not sure", "Anus Female", "Anus Male", "Bladder Male", "Urethra Female", "Fallopian Tubes", 
                                              "Ovary", "Penis", "Testicle", "Uterus", "Vagina"))]

threecolor <- gg_color_hue(3)
complete_long[, short.answer:= ifelse(value==correct.answer, "Correct", 
                                      ifelse(value=="not sure", "Not Sure", "Incorrect"))]
complete_long[, short.answer:=factor(short.answer, levels=c("Incorrect", "Not Sure", "Correct"))]

correct_agg <- complete_long[, list(count=.N), by=list(variable, question, short.answer)]
correct_agg[, prop:= count/sum(count), by=list(variable, question)]

correct_answer_percents <- ggplot(correct_agg, aes(x=variable)) +
  geom_bar(aes(fill=short.answer, y=prop), stat="identity", alpha=0.9) + 
  scale_fill_manual(values=c(threecolor[1], "#808080", threecolor[2])) + 
  # coord_flip() +
  facet_wrap(~question, labeller = label_wrap_gen()) +
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  labs(x="", 
       y="Proportion",
       title="Pre- vs Post-Test Responses, Including Those with Null Post Tests")

pdf(file.path(plot_dir, "correct_answer_percents.pdf"), height=11, width=10)
  print(correct_answer_percents)
graphics.off()

# drop null post
correct_agg_nonull <- complete_long[!post.status %like% "Null", list(count=.N), by=list(variable, question, short.answer)]
correct_agg_nonull[, prop:= count/sum(count), by=list(variable, question)]

correct_answer_percents_nonull <- ggplot(correct_agg_nonull, aes(x=variable)) +
  geom_bar(aes(fill=short.answer, y=prop), stat="identity", alpha=0.9) + 
  scale_fill_manual(values=c(threecolor[1], "#808080", threecolor[2])) + 
  # coord_flip() +
  facet_wrap(~question, labeller = label_wrap_gen()) +
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  labs(x="", 
       y="Proportion",
       title="Pre- vs Post-Test Responses, Excluding Those with Null Post Tests")

pdf(file.path(plot_dir, "correct_answer_percents_nonull.pdf"), height=11, width=10)
  print(correct_answer_percents_nonull)
graphics.off()






