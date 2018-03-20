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
rating[, value:=factor(value, levels=c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))]
rating[, variable:= factor(variable, labels=c("Liked Game", "Recommend \n to Friends", "Game Improved \n Knowledge"))]

rate_plot_quest <- ggplot(rating, aes(x=value)) +
            geom_bar(aes(fill=sex), alpha=0.75) +
            facet_grid(variable~sex) +
            theme(axis.text.x = element_text(angle=45, hjust=1,),
                  legend.position="none") +
            labs(x="",
                 y="Count",
                 title="Post-Game Questionnaire: \n Ratings and Knowledge Gain")

## prior knowledge 
prior <- raw[, list(user.id, sex, prior.awareness)]
prior[, prior.awareness:=factor(prior.awareness, 
                                levels=c("Everything was new", 
                                         "<25%",
                                         "25-50%",
                                         "51-75%",
                                         "76-100%"))]
prior_plot_quest <- ggplot(prior, aes(x=prior.awareness)) +
                      geom_bar(aes(fill=sex), alpha=0.75) +
                      facet_grid(~sex) +
                      theme(axis.text.x = element_text(angle=45, hjust=1),
                            legend.position = "none") +
                      labs(x="",
                           y="Count",
                           title="Post-Game Questionnaire: \n Prior Knowledge")

## family
fam <- raw[, list(user.id, age, sex, 
                  family.type, brothers.older, brothers.younger,
                  sisters.older, sisters.younger,
                  male.child.count, female.child.count)]
fam <- melt(fam, id.vars=c("user.id", "age", "sex", "family.type"),
            measure.vars = list(c("brothers.older", "brothers.younger", "sisters.older", "sisters.younger"),
                                c("male.child.count", "female.child.count")))
fam[is.na(count), count:=0]
fam[, tot.sibs:=sum(count), by=c("user.id", "family.type")]

plots <- Filter( function(x) 'ggplot' %in% class( get(x) ), ls() )


pdf(paste0(main_dir, "quest_plots.pdf"), width=11, height=8.5)

for (plot in plots){
  print(get(plot))
}

graphics.off()


for (plot in plots){
  png(paste0(main_dir, "prelim_plots/", plot, ".png"), height=900, width=1200, units = "px", res=140)
  print(get(plot))
  graphics.off()
}






