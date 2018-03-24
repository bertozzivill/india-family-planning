## analyze and plot age an approval from post-game questionnaire

library(data.table)
library(ggplot2)
library(stringr)
library(Hmisc)
library(scales)

rm(list=ls())

## set ggtheme
theme_set(theme_minimal())

main_dir <- "/Users/bertozzivill/Dropbox/main/Collaborations/family_planning_game/pilot_data/"
if (!dir.exists(main_dir)){
  main_dir <- "C:/Users/abertozzivilla/personal/india_game/"
}

raw <- fread(paste0(main_dir, "questionnaire.csv"))

newlabels <- c("user.id",
               "school.code",
               "eval.date",
               "educ.level",
               "age",
               "school.name",
               "sex",
               "residence",
               "education.medium",
               "fav.subj.science",
               "fav.subj.social.science",
               "fav.subj.language",
               "fav.subj.other",
               "hometown",
               "family.type",
               "brothers.older",
               "brothers.younger",
               "sisters.older",
               "sisters.younger",
               "occ.father",
               "educ.father",
               "occ.mother",
               "educ.mother",
               "religion",
               "future.career",
               "male.child.count",
               "female.child.count",
               "child.1.age",
               "child.2.age",
               "child.3.age",
               "child.4.age",
               "liked.game",
               "recommend.friends",
               "improved.knowledge",
               "want.more.family.planning.games",
               "helped.plan.future",
               "want.more.health.games",
               "prior.awareness",
               "other.health.topics",
               "other.comments",
               "complete")

names(raw) <- newlabels
raw <- raw[sex!=""]

## plot age and sex
demog <- raw[, list(user.id, age, sex)]
demog_plot_quest  <- ggplot(demog[!is.na(age)], aes(x=age)) +
                    geom_bar(aes(fill=sex), alpha=0.75) +
                    facet_grid(~sex) +
                    theme(legend.position = "none")+
                    labs(x="",
                         y="Count",
                         title="Post-Game Questionnaire: \n Self-Reported Age and Sex")


## plot rating of game
rating <- raw[, list(user.id, sex, liked.game, recommend.friends, improved.knowledge)]
rating <- melt(rating, id.vars = c("user.id", "sex"))
rating[, value:=capitalize(value)]
rating[, value:=factor(value, levels=c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))]
rating[, variable:= factor(variable, labels=c("Liked Game", "Recommend \n to Friends", "Game Improved \n Knowledge"))]

rate_plot_quest <- ggplot(rating[!is.na(value)], aes(x=value)) +
            geom_bar(aes(fill=sex), alpha=0.75) +
            facet_grid(variable~sex) +
            theme(axis.text.x = element_text(angle=45, hjust=1,),
                  legend.position="none",
                  text=element_text(size=14)) +
            labs(x="",
                 y="Count",
                 title="Post-Game Questionnaire: \n Ratings and Knowledge Gain")

rating_sum <- rating[!is.na(value), list(count=.N), by=list(variable, value)]
rating_sum[, tot.count:= sum(count), by="variable"]
rating_sum[, perc:= count/tot.count*100]

## prior knowledge 
prior <- raw[, list(user.id, sex, prior.awareness, improved.knowledge)]
prior[prior.awareness=="Everything was new", prior.awareness:="<25%"]
prior[, knowledge.sum:= ifelse(improved.knowledge %in% c("Agree", "Strongly agree"), "Knowledge Gained: \n Agree or Strongly Agree",
                               "Knowledge Gained: \n Disagree or Neutral")]
prior[, improved.knowledge:=factor(improved.knowledge, levels=c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))]
prior[, prior.awareness:=factor(prior.awareness, 
                                levels=c("<25%",
                                         "25-50%",
                                         "51-75%",
                                         "76-100%"))]
prior_plot_quest <- ggplot(prior[!is.na(prior.awareness) & !is.na(improved.knowledge)], aes(x=prior.awareness)) +
                      geom_bar(aes(fill=sex), alpha=0.75) +
                      facet_grid(knowledge.sum~sex) +
                      theme(axis.text.x = element_text(angle=45, hjust=1),
                            legend.position = "none",
                            text=element_text(size=14)) +
                      labs(x="",
                           y="Count",
                           title="Post-Game Questionnaire: \n Prior Knowledge of Material, by Knowledge Gained in Game")

prior_sum <- prior[!is.na(prior.awareness) & !is.na(improved.knowledge), list(count=.N), by=list(knowledge.sum, prior.awareness, sex)]
prior_sum[, list(count=sum(count)), by="prior.awareness"]
sex_prior_sum <- prior_sum[knowledge.sum %like% "Agree", list(count=sum(count)), by=list(sex, prior.awareness)]
both_sex_sum <-  prior_sum[knowledge.sum %like% "Agree", list(count=sum(count)), by=list(prior.awareness)]
both_sex_sum[, sex:= "Both"]
sex_prior_sum <- rbind(sex_prior_sum, both_sex_sum)
sex_prior_sum[, tot.sex:= sum(count), by="sex"]
sex_prior_sum <- sex_prior_sum[order(sex, prior.awareness)]
sex_prior_sum[, perc:=count/tot.sex*100]

## validation: compare age at first child in questionnaire and game
load(paste0(main_dir, "marriage_child_age.rdata"))
child_age_game <- marriage_child_age[event.name=="First Child" & event.age>0, list(user.id, type="In-Game", sex=confirmed.gender, event.age)]

child_age_quest <- raw[!is.na(child.1.age), list(type="Questionnaire", user.id, sex, event.age=child.1.age)]
child_compare <- rbind(child_age_game, child_age_quest)

child_age_plot_quest <- ggplot(child_compare, aes(x=event.age)) +
                        geom_bar(aes(fill=sex, color=sex), alpha=0.75) +
                        facet_grid(type~sex) +
                        theme(legend.position="none",
                              text=element_text(size=14)) + 
                        labs(title="Age at First Child, Game vs Questionnaire, by Sex",
                             x="Age",
                             y="Count")


## plotting -----------------------

plots <- Filter( function(x) 'ggplot' %in% class( get(x) ), ls() )


pdf(paste0(main_dir, "quest_plots.pdf"), width=11, height=8.5)

for (plot in plots){
  print(get(plot))
}

graphics.off()


for (plot in plots){
  png(paste0(main_dir, "prelim_plots/", plot, ".png"), height=900, width=900, units = "px", res=140)
  print(get(plot))
  graphics.off()
}






