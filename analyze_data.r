## -----------------------------------------------------------------------------------------------------
## analyze_data.r
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
in_dir <- "/Users/bertozzivill/Dropbox/main/Collaborations/family_planning_game/data_for_open_access/"

milestones <- fread(paste0(in_dir, "milestones.csv"))
demog <- fread(paste0(in_dir, "demog.csv")) 

## Age distribution by gender
age_dist <- ggplot(demog, aes(x=age)) +
            geom_bar(aes(fill=confirmed.gender, color=confirmed.gender), alpha=0.7) +
            facet_grid(~confirmed.gender) +
            theme(legend.title = element_blank()) +
            labs(title="Distributions of Reported Age, by Confirmed Gender",
                 x="Reported Age",
                 y="Count")


##  Milestone analysis ---------------------------------------------------------------------------------------------

milestones <- merge(milestones, demog[, list(user.id, age, confirmed.gender, self.report.gender, school.code)], 
                    by="user.id", all.x=T)

# user 66 has no demographic data; drop
milestones <- milestones[!is.na(age)]

# plot before dropping older ages: what age do people want marriage/children, compared to the age they stated?
age_comparison <- milestones[event.name %in% c("children", "marriage") & key=="age"]
age_comparison[, age.at.event:=as.integer(value)]
age_comparison[, event.name := ifelse(event.name=="children", "First Child", "Marriage")]
age_comparison <- age_comparison[, list(count=.N), by=list(event.name, confirmed.gender, 
                                                           age, age.at.event)]
age_comparison[, event.name:=factor(event.name, levels=c("Marriage", "First Child"))]

age_milestone_compare <- ggplot(age_comparison[age.at.event>0], aes(x=age, y=age.at.event)) +
                          geom_point(aes(color=confirmed.gender, size=count), alpha=0.75) +
                          scale_color_discrete(guide=FALSE) + 
                          facet_grid(event.name ~ confirmed.gender) +
                          geom_vline(xintercept=19) +
                          geom_abline(intercept=0, slope=1) +
                          theme(legend.title = element_blank(),
                                text=element_text(size=14)) +
                          labs(x="Reported Age",
                               y="Desired Age at Event",
                               title="Reported Age vs Desired Age At Milestone")

## Drop ages not within 13-19 and gender mismatches
milestones <- milestones[age>=13 & age<=19]
milestones <- milestones[self.report.gender==confirmed.gender]

# summary dataset on desire for marriage, children, number of children, and respective ages of events
child_data <- milestones[key %like% "Baby"]
child_data[, child.idx:= as.integer(str_extract(key, "[0-9]+")) +1]
child_data[, child.sex:= str_extract(value, "Male|Female")]
child_data[, age.at.child:= as.integer(str_extract(value, "[0-9]+"))]
child_data[, c("key", "value", "event.name", "school.code", "event.id"):=NULL]
child_data[, child.count:= .N, by="user.id"]

## for paper: summary stats on desired sex of first two children, by sex of respondent 
summary_child_sex <- child_data[child.idx<3, list(count=.N), by=list(child.idx, child.sex, confirmed.gender)]
summary_child_sex[, tot:=sum(count), by=list(confirmed.gender, child.idx)]
summary_child_sex[, perc:= count/tot*100]

child_age <- milestones[event.name=="children" & key=="age", list(user.id, child.age=as.integer(value))]

## Marriage/family size descriptions 
marriage_data <- milestones[event.name=="marriage" & key=="age", list(user.id, confirmed.gender, age, marriage.age=as.integer(value))]
marriage_data[, wants.marriage:= marriage.age>0]

marriage_data[wants.marriage==T, list(mean(marriage.age)), by=confirmed.gender]
nomarriage_perc = marriage_data[, list(count=.N), by=list(confirmed.gender, wants.marriage)]
nomarriage_perc[, tot:= sum(count), by=confirmed.gender]

family_summary <- merge(marriage_data, unique(child_data[, list(user.id, child.count)]), by="user.id", all=T)
family_summary[is.na(child.count), child.count:=0]
family_summary <- merge(family_summary, child_age, by="user.id", all=T)
family_summary <- merge(family_summary, child_data[child.idx==1, list(user.id, age.at.first.child=age.at.child)], by="user.id", all=T)
family_summary[is.na(age.at.first.child), age.at.first.child:=0]
family_summary[,wants.children:= child.age>0]

## for paper: means and standard errors for age at marriage/children
family_summary[, list(mean.marriage.age =mean(marriage.age)), by=list(confirmed.gender, wants.marriage)]
family_summary[, list(mean.child.age =mean(child.age)), by=list(confirmed.gender, wants.children)]
family_summary[, list(sd.marriage.age =sd(marriage.age)), by=list(confirmed.gender, wants.marriage)]
family_summary[, list(sd.child.age =sd(child.age)), by=list(confirmed.gender, wants.children)]

## for paper: summary stats on age at marriage and age at first child
family_summary[wants.marriage==T, list(mean(marriage.age)), by=confirmed.gender]
nomarriage_perc = family_summary[, list(count=.N), by=list(confirmed.gender, wants.marriage)]
nomarriage_perc[, tot:= sum(count), by=confirmed.gender]

family_summary[wants.children==T, list(mean(child.age)), by=confirmed.gender]
nobaby_perc = family_summary[, list(count=.N), by=list(confirmed.gender, wants.children)]
nobaby_perc[, tot:= sum(count), by=confirmed.gender]

## Question: What age do people want to get married/have their first child?
marriage_child_age <- melt(family_summary, id.vars = c("user.id", "confirmed.gender", "age"),
                           measure.vars = c("marriage.age", "child.age"),
                           value.name = "event.age")
marriage_child_age[, event.name:=ifelse(variable %like% "marriage", "Marriage", "First Child")]
marriage_child_age[, event.name:=factor(event.name, levels=c("Marriage", "First Child"))]

age_plot <- ggplot(marriage_child_age, aes(x=event.age)) +
            geom_bar(aes(fill=confirmed.gender, color=confirmed.gender), alpha=0.75) +
            facet_grid(event.name~confirmed.gender) +
            theme(legend.position="none",
                  text=element_text(size=14)) + 
            labs(title="Desired Age of Milestones, by Sex",
                 x="Age",
                 y="Count")

write.csv(marriage_child_age, file=paste0(in_dir, "marriage_child_age.csv"), row.names=F)

## Question: how many kids do people want? What Gender?
child_count_plot <- ggplot(family_summary, aes(x=child.count)) +
                    geom_bar(aes(fill=confirmed.gender, color=confirmed.gender), alpha=0.75) + 
                    facet_grid(~confirmed.gender) +
                    theme(legend.position="none",
                          text=element_text(size=14)) + 
                    labs(title="Desired Number of Children, by Sex",
                         x="# of Children",
                         y="Count")

child_data[, confirmed.gender:= paste(confirmed.gender, "Respondent")]
child_data[, child.sex:= paste(child.sex, "Child")]

child_gender_plot <- ggplot(child_data, aes(x=child.idx)) +
                    geom_bar(aes(fill=child.sex, color=child.sex), alpha=0.75) +
                    facet_grid(~confirmed.gender) +
                    theme(legend.title = element_blank(),
                          text=element_text(size=14)) + 
                    labs(title="Desired Sex of Each Child, by Respondent Sex",
                         x="Child",
                         y="Count")


# Question: Who influences life choices
influencers <- milestones[key=="influencer", list(user.id, influencer=value, confirmed.gender,
                                                  milestone=capitalize(event.name))]
influencer_order <- influencers[, list(count=sum(.N)), by=influencer][order(count)]$influencer
influencers[, influencer:= factor(influencer, levels=influencer_order)]
influencers[, milestone:=factor(milestone, levels=c("Graduation", "Lifepartner", "Marriage", "Children"))]
colors = brewer_pal(type="div", palette="Spectral")(5)

influence_plot <- ggplot(influencers, aes(x=milestone)) +
                  geom_bar(aes(fill=influencer, color=influencer), alpha=0.75) +
                  facet_grid(~confirmed.gender) +
                  theme(legend.title = element_blank(),
                        axis.text.x = element_text(angle=45, hjust=1),
                        text=element_text(size=14)) + 
                  scale_color_manual(values=colors) +
                  scale_fill_manual(values=colors) +
                  labs(title="Milestone Influencers, by Sex",
                       x="Milestone",
                       y="Count")


## table for paper: influencer percent
influence_perc <- influencers[, list(count=.N), by=list(confirmed.gender, milestone, influencer)]
influence_perc[, perc:=count/sum(count)*100, by=list(confirmed.gender, milestone)]
influence_perc <- dcast(influence_perc, confirmed.gender + milestone ~ influencer, value.var = "perc")

## --------------------------------------------------------------------------------------------------------------
## plotting

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


