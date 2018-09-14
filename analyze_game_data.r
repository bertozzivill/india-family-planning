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


# plot before dropping older ages: what age do people want marriage/children, compared to their self-reported age?

marriage_age <- milestones[event.name=="marriage" & key=="age"]
marriage_age[, age.at.event:=as.integer(value)]
marriage_age <- marriage_age[, list(user.id, event.name, self.report.sex, age, age.at.event)]

children_age <- milestones[event.name =="children" & key=="baby"] 
children_age[, child.sex:= gsub(" .*", "", value)]
children_age[, child.age:=as.integer(gsub(".* ", "", value))]

first_child_age <- children_age[, list(age.at.event=min(child.age)), 
                                by=list(user.id, event.name, self.report.sex, age)]

age_comparison <- rbind(marriage_age, first_child_age)

age_comparison[, event.name := ifelse(event.name=="children", "First Child", "Marriage")]
age_comparison <- age_comparison[, list(count=.N), by=list(event.name, self.report.sex, 
                                                           age, age.at.event)]
age_comparison[, event.name:=factor(event.name, levels=c("Marriage", "First Child"))]

age_milestone_compare <- ggplot(age_comparison[age.at.event>0], aes(x=age, y=age.at.event)) +
                          geom_point(aes(color=self.report.sex, size=count), alpha=0.75) +
                          scale_color_discrete(guide=FALSE) + 
                          facet_grid(event.name ~ self.report.sex) +
                          geom_vline(xintercept=19) +
                          geom_abline(intercept=0, slope=1) +
                          theme(legend.title = element_blank(),
                                text=element_text(size=14)) +
                          labs(x="Reported Age",
                               y="Desired Age at Event",
                               title="Reported Age vs Desired Age At Milestone")



## Drop ages not within 13-19 
milestones <- milestones[age>=13 & age<=19]

# summary dataset on desire for marriage, children, number of children, and respective ages of events
child_data <- milestones[key %like% "Baby"]
child_data[, child.idx:= as.integer(str_extract(key, "[0-9]+")) +1]
child_data[, child.sex:= str_extract(value, "Male|Female")]
child_data[, age.at.child:= as.integer(str_extract(value, "[0-9]+"))]
child_data[, c("key", "value", "event.name", "school.code", "event.id"):=NULL]
child_data[, child.count:= .N, by="user.id"]

## for paper: summary stats on desired sex of first two children, by sex of respondent 
summary_child_sex <- child_data[child.idx<3, list(count=.N), by=list(child.idx, child.sex, confirmed.sex)]
summary_child_sex[, tot:=sum(count), by=list(confirmed.sex, child.idx)]
summary_child_sex[, perc:= count/tot*100]

child_age <- milestones[event.name=="children" & key=="age", list(user.id, child.age=as.integer(value))]

## Important note: There are two separate metrics for "age at first child" in the data. Players were asked explicitly at what age they wanted to 
##                 have children (here denoted by 'child.age'), but also recorded their own age when they described the characteristics of 
##                 their first child (here, 'age.at.first.child'). Both are preserved in the dataset, but 'child.age' was used for plotting 
##                 and analysis. Future versions of the game will remove this ambiguity. 

## Marriage/family size descriptions 
marriage_data <- milestones[event.name=="marriage" & key=="age", list(user.id, confirmed.sex, age, marriage.age=as.integer(value))]
marriage_data[, wants.marriage:= marriage.age>0]
marriage_data[wants.marriage==T, list(mean(marriage.age)), by=confirmed.sex]
nomarriage_perc = marriage_data[, list(count=.N), by=list(confirmed.sex, wants.marriage)]
nomarriage_perc[, tot:= sum(count), by=confirmed.sex]

family_summary <- merge(marriage_data, unique(child_data[, list(user.id, child.count)]), by="user.id", all=T)
family_summary[is.na(child.count), child.count:=0]
family_summary <- merge(family_summary, child_age, by="user.id", all=T)
family_summary <- merge(family_summary, child_data[child.idx==1, list(user.id, age.at.first.child=age.at.child)], by="user.id", all=T)
family_summary[is.na(age.at.first.child), age.at.first.child:=0]
family_summary[,wants.children:= child.age>0]

## for paper: means and standard errors for age at marriage/children
family_summary[, list(mean.marriage.age =mean(marriage.age)), by=list(confirmed.sex, wants.marriage)]
family_summary[, list(mean.child.age =mean(child.age)), by=list(confirmed.sex, wants.children)]
family_summary[, list(sd.marriage.age =sd(marriage.age)), by=list(confirmed.sex, wants.marriage)]
family_summary[, list(sd.child.age =sd(child.age)), by=list(confirmed.sex, wants.children)]

## for paper: summary stats on age at marriage and age at first child
family_summary[wants.marriage==T, list(mean(marriage.age)), by=confirmed.sex]
nomarriage_perc = family_summary[, list(count=.N), by=list(confirmed.sex, wants.marriage)]
nomarriage_perc[, tot:= sum(count), by=confirmed.sex]

family_summary[wants.children==T, list(mean(child.age)), by=confirmed.sex]
nobaby_perc = family_summary[, list(count=.N), by=list(confirmed.sex, wants.children)]
nobaby_perc[, tot:= sum(count), by=confirmed.sex]

## Plot: What age do people want to get married/have their first child?
marriage_child_age <- melt(family_summary, id.vars = c("user.id", "confirmed.sex", "age"),
                           measure.vars = c("marriage.age", "child.age"),
                           value.name = "event.age")
marriage_child_age[, event.name:=ifelse(variable %like% "marriage", "Marriage", "First Child")]
marriage_child_age[, event.name:=factor(event.name, levels=c("Marriage", "First Child"))]

age_plot <- ggplot(marriage_child_age, aes(x=event.age)) +
            geom_bar(aes(fill=confirmed.sex, color=confirmed.sex), alpha=0.75) +
            facet_grid(event.name~confirmed.sex) +
            theme(legend.position="none",
                  text=element_text(size=14)) + 
            labs(title="Desired Age of Milestones, by Sex",
                 x="Age",
                 y="Count")

write.csv(marriage_child_age, file=paste0(in_dir, "marriage_child_age.csv"), row.names=F)

## Plot: how many kids do people want? What Sex?
child_count_plot <- ggplot(family_summary[child.count>0], aes(x=child.count)) +
                    geom_bar(aes(fill=confirmed.sex, color=confirmed.sex), alpha=0.75) + 
                    facet_grid(~confirmed.sex) +
                    theme(legend.position="none",
                          text=element_text(size=14)) + 
                    labs(title="Desired Number of Children, by Sex",
                         x="# of Children",
                         y="Count")

child_data[, confirmed.sex:= paste(confirmed.sex, "Respondent")]
child_data[, child.sex:= paste(child.sex, "Child")]

## for paper: respondent count by number of children
parity_counts <- unique(child_data[, list(user.id, confirmed.sex, child.count)])
parity_counts <- parity_counts[, list(count=.N), by=list(confirmed.sex, child.count)]
parity_counts <- parity_counts[order(confirmed.sex, child.count)]
parity_counts[, sex.count:=sum(count), by="confirmed.sex"]
parity_counts[, sex.perc:=count/sex.count*100]



child_sex_plot <- ggplot(child_data, aes(x=child.idx)) +
                    geom_bar(aes(fill=child.sex, color=child.sex), alpha=0.75) +
                    facet_grid(~confirmed.sex) +
                    theme(legend.title = element_blank(),
                          text=element_text(size=14)) + 
                    labs(title="Desired Sex of Each Child, by Respondent Sex",
                         x="Child",
                         y="Count")


# Plot: Who influences life choices?
influencers <- milestones[key=="influencer", list(user.id, influencer=value, confirmed.sex,
                                                  milestone=capitalize(event.name))]
influencer_order <- influencers[, list(count=sum(.N)), by=influencer][order(count)]$influencer
influencers[, influencer:= factor(influencer, levels=influencer_order)]
influencers[, milestone:=factor(milestone, levels=c("Graduation", "Lifepartner", "Marriage", "Children"))]
colors = brewer_pal(type="div", palette="Spectral")(5)

influence_plot <- ggplot(influencers, aes(x=milestone)) +
                  geom_bar(aes(fill=influencer, color=influencer), alpha=0.75) +
                  facet_grid(~confirmed.sex) +
                  theme(legend.title = element_blank(),
                        axis.text.x = element_text(angle=45, hjust=1),
                        text=element_text(size=14)) + 
                  scale_color_manual(values=colors) +
                  scale_fill_manual(values=colors) +
                  labs(title="Milestone Influencers, by Sex",
                       x="Milestone",
                       y="Count")


## table for paper: influencer percent
influence_perc <- influencers[, list(count=.N), by=list(confirmed.sex, milestone, influencer)]
influence_perc[, perc:=count/sum(count)*100, by=list(confirmed.sex, milestone)]
influence_perc <- dcast(influence_perc, confirmed.sex + milestone ~ influencer, value.var = "perc")

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


