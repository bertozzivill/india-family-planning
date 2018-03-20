## explore pilot data from family planning game, make a plot or two
## possibly: test significance of group differences? (chi-squared)

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

events <- fread(paste0(main_dir, "mff_study_events.csv"))
event_data <- fread(paste0(main_dir, "mff_study_eventdata.csv"))

setnames(events, c("event.id", "old.user.id", "event.name", "event.type", "device.id", "timestamp"))
setnames(event_data, c("event.id", "key", "value"))

# make more usable user ids 
users <- unique(events[, list(old.user.id)])
users$user.id <- as.integer(rownames(users))
events <- merge(events, users, by="old.user.id", all=T)
event_data <- merge(event_data, unique(events[, list(event.id, user.id)]), by="event.id", all.x=T)

# split events dataset by type
split_events <- split(events, by="event.type")


## sanity check #1: milestones. Does each play instance have all five milestones?
milestone_events <- split_events$MI
milestone_events[, event.count:=.N, by="user.id"]
incorrect_mi <- milestone_events[event.count!=5]
print(paste(length(unique(incorrect_mi[event.count<5]$user.id)), "players have fewer than 5 milestones recorded, and",
            length(unique(incorrect_mi[event.count>5]$user.id)), "players have more than 5 milestones recorded--dropping."))

milestone_events <- milestone_events[event.count==5]
valid_users <- unique(milestone_events$user.id)


## --------------------------------------------------------------------------------------------------------------
# generate a demographics dataset of participants
demog_events <- split_events$DE[user.id %in% valid_users]
demog <- event_data[event.id %in% demog_events$event.id]
demog <- dcast(demog, event.id + user.id ~ key, value.var = "value")
demog[, age:=as.integer(age)]
setnames(demog, c("confirmed gender", "school code"), c("confirmed.gender", "school.code"))
demog[, school.code:=as.integer(school.code)]
demog[, times.played:= .N, by="user.id"]


## sanity check #2: How often is there a gender mismatch, by school?
demog[, gender.agreement:= gender==confirmed.gender]

gender_check <- ggplot(demog, aes(x=school.code, fill=gender.agreement)) +
                geom_bar(position= "fill") +
                labs(title="Gender disagreement, by school",
                     x="School Code",
                     y="Gender (dis)agreement Proportions")

## TODO: see if cases with sex mismatch are different from the others

# compare English vs Kannada by school

language <- ggplot(demog, aes(x=school.code, fill=language)) +
            geom_bar(position= "fill") +
            labs(title="Game Language, by School",
                 x="School Code",
                 y="Language Proportions")

## TODO: ask if schools 1,2, and 10 had different language conventions
## TODO: religion?

# age distribution by (confirmed) gender:
age_dist <- ggplot(demog, aes(x=age)) +
            geom_bar(aes(fill=confirmed.gender, color=confirmed.gender), alpha=0.7) +
            facet_grid(~confirmed.gender) +
            theme(legend.title = element_blank()) +
            labs(title="Distributions of Reported Age, by Confirmed Gender",
                 x="Reported Age",
                 y="Count")


# age distribution by (confirmed) gender and school code:
age_dist_school <- ggplot(demog, aes(x=age)) +
                    geom_density(aes(fill=confirmed.gender, color=confirmed.gender), alpha=0.7) +
                    facet_grid(school.code~confirmed.gender, scales="free_y") +
                    theme(legend.title = element_blank()) +
                    labs(title="Distributions of Reported Age, by Confirmed Gender and School",
                         x="Reported Age",
                         y="Density")


## --------------------------------------------------------------------------------------------------------------

## Deeper dive into milestones

milestones <- event_data[event.id %in% milestone_events$event.id]
milestones <- merge(milestones, demog[, list(user.id, age, confirmed.gender, school.code)], by="user.id", all.x=T)

# user 66 has no demographic data; drop
milestones <- milestones[!is.na(age)]

# add event name 
milestones <- merge(milestones, events[, list(event.id, event.name)], by="event.id")

# plot before dropping older ages: what age do people want marriage/children, compared to the age they stated?
age_comparison <- milestones[event.name %in% c("children", "marriage") & key=="age"]
age_comparison[, age.at.event:=as.integer(value)]
age_comparison[, event.name := ifelse(event.name=="children", "First Child", "Marriage")]
age_comparison <- age_comparison[, list(count=.N), by=list(event.name, confirmed.gender, 
                                                           age, age.at.event)]

age_milestone_compare <- ggplot(age_comparison[age.at.event>0], aes(x=age, y=age.at.event)) +
                          geom_point(aes(color=confirmed.gender, size=count), alpha=0.75) +
                          scale_color_discrete(guide=FALSE) + 
                          facet_grid(confirmed.gender ~ event.name) +
                          geom_vline(xintercept=19) +
                          geom_abline(intercept=0, slope=1) +
                          theme(legend.title = element_blank(),
                                text=element_text(size=14)) +
                          labs(x="Reported Age",
                               y="Desired Age at Event",
                               title="Reported Age vs Desired Age At Milestone")

## Drop older ages
milestones <- milestones[age<=19]


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


## for paper: summary stats on age at marriage and age at first child
family_summary[wants.marriage==T, list(mean(marriage.age)), by=confirmed.gender]
nomarriage_perc = family_summary[, list(count=.N), by=list(confirmed.gender, wants.marriage)]
nomarriage_perc[, tot:= sum(count), by=confirmed.gender]

family_summary[wants.children==T, list(mean(child.age)), by=confirmed.gender]
nobaby_perc = family_summary[, list(count=.N), by=list(confirmed.gender, wants.children)]
nobaby_perc[, tot:= sum(count), by=confirmed.gender]

## Question: What age do people want to get married?
marriage_age_plot <- ggplot(family_summary, aes(x=marriage.age)) +
                      geom_bar(aes(fill=confirmed.gender, color=confirmed.gender), alpha=0.5) +
                      facet_grid(~confirmed.gender) +
                      theme(legend.title = element_blank()) + 
                      labs(title="Desired Age of Marriage, by Sex",
                           x="Age",
                           y="Count")

## Question: How does current age compare to desired age of marriage?
years_to_marriage <- ggplot(family_summary[wants.marriage==T], aes(x=marriage.age-age)) +
                            geom_bar(aes(fill=confirmed.gender, color=confirmed.gender), alpha=0.5) +
                            geom_vline(xintercept=0) +
                            facet_grid(~confirmed.gender) +
                            theme(legend.title = element_blank()) + 
                            labs(title="Desired Years Until Marriage, by Gender",
                                 x="Marriage Age Minus Current Age",
                                 y="Count")

## Question: At what age do people want their first child?
child_age_plot <- ggplot(family_summary, aes(x=child.age)) +
                  geom_bar(aes(fill=confirmed.gender, color=confirmed.gender), alpha=0.5) +
                  facet_grid(~confirmed.gender) +
                  theme(legend.title = element_blank()) + 
                  labs(title="Desired Age at First Child, by Sex",
                       x="Age",
                       y="Count")

child_age_plot_alt <- ggplot(family_summary, aes(x=age.at.first.child)) +
                      geom_bar(aes(fill=confirmed.gender, color=confirmed.gender), alpha=0.5) +
                      facet_grid(~confirmed.gender) +
                      theme(legend.title = element_blank()) + 
                      labs(title="Desired Age at First Child, by Sex (Using Timeline)",
                           x="Age",
                           y="Count")


# todo: does anyone want kids but not marriage?


## Question: how many kids do people want? What Gender?
child_count_plot <- ggplot(family_summary, aes(x=child.count)) +
                    geom_bar(aes(fill=confirmed.gender, color=confirmed.gender), alpha=0.5) + 
                    facet_grid(~confirmed.gender) +
                    theme(legend.title = element_blank()) + 
                    labs(title="Desired Number of Children, by Sex",
                         x="# of Children",
                         y="Count")

child_data[, confirmed.gender:= paste(confirmed.gender, "Respondent")]
child_data[, child.sex:= paste(child.sex, "Child")]

child_gender_plot <- ggplot(child_data, aes(x=child.idx)) +
                      geom_bar(aes(fill=child.sex, color=child.sex), alpha=0.5) +
                      facet_grid(~confirmed.gender) +
                      theme(legend.title = element_blank()) + 
                      labs(title="Desired Sex of Each Child, by Respondent Sex",
                           x="Child",
                           y="Count")


# Question: Who influences life choices
influencers <- milestones[key=="influencer", list(user.id, influencer=value, confirmed.gender,
                                                  milestone=capitalize(event.name))]
influencer_order <- influencers[, list(count=sum(.N)), by=influencer][order(count)]$influencer
influencers[, influencer:= factor(influencer, levels=influencer_order)]
influencers[, milestone:=factor(milestone, levels=c("Graduation", "Lifepartner", "Marriage", "Children"))]

influence_plot <- ggplot(influencers, aes(x=milestone)) +
                  geom_bar(aes(fill=influencer, color=influencer), alpha=0.75) +
                  facet_grid(~confirmed.gender) +
                  theme(legend.title = element_blank(),
                        axis.text.x = element_text(angle=45, hjust=1)) + 
                  scale_color_manual(values=rev(hue_pal()(5))) +
                 scale_fill_manual(values=rev(hue_pal()(5))) +
                  labs(title="Milestone Influencers, by Sex",
                       x="Milestone",
                       y="Count")


## table for paper: influencer percent
influence_perc <- influencers[, list(count=.N), by=list(confirmed.gender, milestone, influencer)]
influence_perc[, perc:=count/sum(count)*100, by=list(confirmed.gender, milestone)]
influence_perc <- dcast(influence_perc, confirmed.gender + milestone ~ influencer, value.var = "perc")

## --------------------------------------------------------------------------------------------------------------


# Question: What's the average time spent on each minigame?
games <- events[event.type=="MG" & !event.name %like% "win"]
games[, timestamp:= strptime(timestamp, format="%Y-%m-%d %H:%M:%S")]

games[, game.type:= gsub(" start| end", "", event.name)]
games[, game.point:= ifelse(event.name %like% "start", "start", "end")]

games <- games[order(user.id, game.type, timestamp, -game.point)]
games[, game.count:= seq_len(.N), by=list(user.id, game.type, game.point)]

games <- dcast(games, user.id + game.type + game.count ~ game.point, value.var="timestamp")
games[, duration:= as.numeric(end-start)/60]

#drop nas and outlier
games <- games[!is.na(duration) & duration<50]

# sum time individuals spent on same game
games <- games[, list(duration=sum(duration)), by=list(user.id, game.type)]

mean_games <- games[, list(mean.duration=mean(duration)), by="game.type"]
mean_games[, game.type:= gsub("game","Game", game.type)]

ggplot(mean_games, aes(x=game.type, y=mean.duration)) +
                  geom_bar(aes(color=game.type, fill=game.type), stat="identity") +
                  theme(legend.position = "none",
                        axis.text.x = element_text(angle=45, hjust=1)) +
                  labs(title="Mean Minigame Duration",
                       x="Minigame",
                       y="Mean Duration (minutes)")

# time spent on all minigames
games[, tot.duration:= sum(duration), by=list(user.id)]
tot_duration <- unique(games[,list(user.id, tot.duration)])

ggplot(tot_duration, aes(x=tot.duration)) +
                geom_density(fill="black", alpha=0.5) +
                geom_vline(xintercept = mean(tot_duration$tot.duration), color="red") + 
                # geom_label(x=mean(tot_duration$tot.duration), y=0.14, label=mean(tot_duration$tot.duration))+
                labs(title="Time Spent on All Minigames",
                     x="Total Duration (minutes)",
                     y="Count")

## --------------------------------------------------------------------------------------------------------------
## plotting

plots <- Filter( function(x) 'ggplot' %in% class( get(x) ), ls() )


pdf(paste0(main_dir, "prelim_plots.pdf"), width=11, height=8.5)

for (plot in plots){
  print(get(plot))
}

graphics.off()


for (plot in plots){
  png(paste0(main_dir, "prelim_plots/", plot, ".png"), height=900, width=1200, units = "px", res=140)
  print(get(plot))
  graphics.off()
}

