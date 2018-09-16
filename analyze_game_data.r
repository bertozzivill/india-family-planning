## -----------------------------------------------------------------------------------------------------
## analyze_game_data.r
## Author: Amelia Bertozzi-Villa
## Date: April 15th 2018
## Description: Make descriptive plots and tables included in game analysis.  
## -----------------------------------------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(stringr)
library(Hmisc)
library(scales)

rm(list=ls())

## set ggtheme
theme_set(theme_minimal())

## load data 
in_dir <- "/Users/bertozzivill/Dropbox/main/Collaborations/my_future_family/chennai_2018/data_090818/clean/"

all_data <- fread(file.path(in_dir, "all_game_data.csv"))
milestones <- fread(file.path(in_dir, "milestones.csv"))
demog <- fread(file.path(in_dir, "demog.csv")) 

## remove some values where they were still playing the old game
demog <- demog[self.report.sex %in% c("male", "female")]
demog <- demog[, list(user.id, event.id, age, self.report.sex)]

## Age distribution by sex
age_dist <- ggplot(demog, aes(x=age)) +
            geom_bar(aes(fill=self.report.sex, color=self.report.sex), alpha=0.7) +
            facet_grid(~self.report.sex) +
            theme(legend.title = element_blank()) +
            labs(title="Distributions of Reported Age, by Sex",
                 x="Reported Age",
                 y="Count")


##  Milestone analysis ---------------------------------------------------------------------------------------------

milestones <- merge(milestones, demog[, list(user.id, age, self.report.sex)], 
                    by="user.id", all.y=T)


# format data about age and number of marriage and children
marriage_age <- milestones[event.name=="marriage" & key=="age"]
marriage_age <- marriage_age[, list(user.id, self.report.sex, age, wants.marriage=T, marriage.age=as.integer(value))]

no_marriage_age <- milestones[event.name=="marriage" & key=="skip" & value=="yes",
                              list(user.id, self.report.sex, age, wants.marriage=F, marriage.age=NA)]
marriage_data <- rbind(marriage_age, no_marriage_age)

children_age <- milestones[event.name =="children" & key=="baby"] 
children_age[, child.sex:= gsub(" .*", "", value)]
children_age[, age.at.child:=as.integer(gsub(".* ", "", value))]
children_age[, child.idx:= rowid(user.id)]
children_age[, child.count:= .N, by="user.id"]
children_age <- children_age[, list(user.id, self.report.sex, age, wants.children=T, 
                                    child.count, child.sex, age.at.child, child.idx)]

no_children_age <- milestones[event.name=="children" & key=="skip" & value=="yes",
                              list(user.id, self.report.sex, age, wants.children=F, 
                                   child.count=NA, child.sex=NA, age.at.child=NA, child.idx=NA)]

children_data <- rbind(children_age, no_children_age)

family_data <- merge(marriage_data, children_data, by=c("user.id", "self.report.sex", "age"), all=T)


## plot distribution of reported age vs age at events (before imposing age restrictions)

age_comparison <- melt(family_data[(wants.marriage==T | wants.children==T) & (is.na(child.idx) | child.idx==1)], 
                       id.vars=c("user.id", "self.report.sex", "age"),
                       measure.vars=c("marriage.age", "age.at.child"),
                       variable.name = "event", value.name="age.at.event")
age_comparison[, event := ifelse(event=="age.at.child", "First Child", "Marriage")]
age_comparison <- age_comparison[, list(count=.N), by=list(event, self.report.sex, 
                                                           age, age.at.event)]
age_comparison[, event:=factor(event, levels=c("Marriage", "First Child"))]

age_milestone_compare <- ggplot(age_comparison[!is.na(age.at.event)], aes(x=age, y=age.at.event)) +
                          geom_point(aes(color=self.report.sex, size=count), alpha=0.75) +
                          scale_color_discrete(guide=FALSE) + 
                          facet_grid(event ~ self.report.sex) +
                          geom_vline(xintercept=19) +
                          geom_abline(intercept=0, slope=1) +
                          theme(legend.title = element_blank(),
                                text=element_text(size=14)) +
                          labs(x="Reported Age",
                               y="Desired Age at Event",
                               title="Reported Age vs Desired Age At Milestone")


## Drop ages not within 13-19 
milestones <- milestones[age>=13 & age<=19]
family_data <- family_data[age>=13 & age <=19]
age_comparison <- age_comparison[age>=13 & age<=19]

## Plot: What age do people want to get married/have their first child?
age_plot <- ggplot(age_comparison, aes(x=age.at.event)) +
            geom_bar(aes(fill=self.report.sex, color=self.report.sex), alpha=0.75) +
            facet_grid(event~self.report.sex) +
            theme(legend.position="none",
                  text=element_text(size=14)) + 
            labs(title="Desired Age of Milestones, by Sex",
                 x="Age",
                 y="Count")

write.csv(age_comparison, file=paste0(in_dir, "marriage_child_age.csv"), row.names=F)

## Plot: how many kids do people want? What Sex?
child_count_plot <- ggplot(family_data[child.count>0], aes(x=child.count)) +
                    geom_bar(aes(fill=self.report.sex, color=self.report.sex), alpha=0.75) + 
                    facet_grid(~self.report.sex) +
                    theme(legend.position="none",
                          text=element_text(size=14)) + 
                    labs(title="Desired Number of Children, by Sex",
                         x="# of Children",
                         y="Count")

children_age[, self.report.sex:= paste(self.report.sex, "Respondent")]
children_age[, child.sex:= paste(child.sex, "Child")]

child_sex_plot <- ggplot(children_age, aes(x=child.idx)) +
                    geom_bar(aes(fill=child.sex, color=child.sex), alpha=0.75) +
                    facet_grid(~self.report.sex) +
                    theme(legend.title = element_blank(),
                          text=element_text(size=14)) + 
                    labs(title="Desired Sex of Each Child, by Respondent Sex",
                         x="Child",
                         y="Count")


# Plot: Who influences life choices?
influencers <- milestones[key %like% "influencer", list(user.id, influencer=value, self.report.sex,
                                                  milestone=capitalize(event.name))]
influencer_order <- influencers[, list(count=sum(.N)), by=influencer][order(count)]$influencer
influencers[, influencer:= factor(influencer, levels=influencer_order)]
influencers[, milestone:=factor(milestone, levels=c("Graduation", "Lifepartner", "Marriage", "Children"))]
colors = brewer_pal(type="div", palette="Spectral")(7)

influence_plot <- ggplot(influencers, aes(x=milestone)) +
                  geom_bar(aes(fill=influencer, color=influencer), alpha=0.8) +
                  facet_grid(~self.report.sex) +
                  theme(legend.title = element_blank(),
                        axis.text.x = element_text(angle=45, hjust=1),
                        text=element_text(size=14)) + 
                  scale_color_manual(values=colors) +
                  scale_fill_manual(values=colors) +
                  labs(title="Milestone Influencers, by Sex",
                       x="Milestone",
                       y="Count")


## table for paper: influencer percent
influence_perc <- influencers[, list(count=.N), by=list(self.report.sex, milestone, influencer)]
influence_perc[, perc:=count/sum(count)*100, by=list(self.report.sex, milestone)]
influence_perc <- dcast(influence_perc, self.report.sex + milestone ~ influencer, value.var = "perc")

## --------------------------------------------------------------------------------------------------------------
## save plots

plots <- Filter( function(x) 'ggplot' %in% class( get(x) ), ls() )

pdf(paste0(in_dir, "plots/game_plots.pdf"), width=11, height=8.5)

for (plot in plots){
  print(get(plot))
}
graphics.off()

for (plot in plots){
  png(paste0(in_dir, "plots/", plot, ".png"), height=900, width=1200, units = "px", res=140)
  print(get(plot))
  graphics.off()
}


