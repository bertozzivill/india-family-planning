## explore pilot data from family planning game, make a plot or two
## possibly: test significance of group differences? (chi-squared)

library(data.table)
library(ggplot2)
library(stringr)

rm(list=ls())

main_dir <- "/Users/bertozzivill/Dropbox/main/Collaborations/family_planning_game/pilot_data/"
if (!dir.exists(main_dir)){
  main_dir <- "C:/Users/abertozzivilla/personal/india_game/"
}

events <- fread(paste0(main_dir, "mff_study_events.csv"))
event_data <- fread(paste0(main_dir, "mff_study_eventdata.csv")) # todo: replace this with 'events.csv' with full timestamp

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

ggplot(demog, aes(x=school.code, fill=gender.agreement)) +
      geom_bar(position= "fill") 

## TODO: see if cases with sex mismatch are different from the others

# compare English vs Kannada by school
ggplot(demog, aes(x=school.code, fill=language)) +
        geom_bar(position= "fill") 
## TODO: ask if schools 1,2, and 10 had different language conventions
## TODO: religion?

# age distribution by (confirmed) gender:
age_dist <- ggplot(demog, aes(x=age)) +
            geom_bar(aes(fill=confirmed.gender, color=confirmed.gender), alpha=0.7) +
            facet_grid(~confirmed.gender) +
            theme(legend.title = element_blank()) +
            labs(title="Distributions of Reported Age, by Confirmed Gender",
                 x="Reported Age",
                 y="Density")


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
milestones <- merge(milestones, events[, list(event.id, event.name)], by="event.id", all.x=T)

# summary dataset on desire for marriage, children, number of children, and respective ages of events
child_data <- milestones[key %like% "Baby"]
child_data[, child.idx:= as.integer(str_extract(key, "[0-9]+"))]
child_data[, child.sex:= str_extract(value, "Male|Female")]
child_data[, age.at.child:= as.integer(str_extract(value, "[0-9]+"))]
child_data[, c("key", "value", "event.name", "school.code", "event.id"):=NULL]
child_data[, child.count:= .N, by="user.id"]

child_age <- milestones[event.name=="children" & key=="age", list(user.id, child.age=as.integer(value))]

marriage_data <- milestones[event.name=="marriage" & key=="age", list(user.id, confirmed.gender, age, marriage.age=as.integer(value))]
marriage_data[, wants.marriage:= marriage.age>0]

family_summary <- merge(marriage_data, unique(child_data[, list(user.id, child.count)]), by="user.id", all=T)
family_summary[is.na(child.count), child.count:=0]
family_summary[,wants.children:= child.count>0]
family_summary <- merge(family_summary, child_age, by="user.id", all=T)

## Question 1: what age do people want to get married?
marriage_age_plot <- ggplot(marriage_age[value>0], aes(x=as.integer(value))) +
                      geom_bar(aes(fill=confirmed.gender, color=confirmed.gender), alpha=0.5) +
                      facet_grid(~confirmed.gender) +
                      theme(legend.title = element_blank()) + 
                      labs(title="Desired Age of Marriage, by Gender",
                           x="Age",
                           y="Count")

## current age vs age of marriage
ggplot(marriage_age, aes(x=age, y=value)) +
  geom_jitter(aes(color=confirmed.gender))

## age dists of those who don't want to marry
ggplot(marriage_age[value==0], aes(x=age)) +
  geom_bar(aes(fill=confirmed.gender, color=confirmed.gender), alpha=0.5) +
  facet_grid(~confirmed.gender) +
  theme(legend.title = element_blank()) + 
  labs(title="Age Distribution of Students Who Don't Want Marriage, by Gender",
       x="Age",
       y="Count")


## Question 2: at what age do people want their first child?
children_age <- milestones[event.name=="children" & key=="age"]
children_age[, value:= as.integer(value)]
child_age_plot <- ggplot(children_age[value>0], aes(x=value)) +
                  geom_bar(aes(fill=confirmed.gender, color=confirmed.gender), alpha=0.5) +
                  facet_grid(~confirmed.gender) +
                  theme(legend.title = element_blank()) + 
                  labs(title="Desired Age at First Child, by Gender",
                       x="Age",
                       y="Count")

## Sanity check: do people want "children" at the age at which they say they want their first child?
child_data <- milestones[key %like% "Baby"]
child_data[, child.idx:= as.integer(str_extract(key, "[0-9]+"))]
child_data[, child.sex:= str_extract(value, "Male|Female")]
child_data[, age.at.child:= as.integer(str_extract(value, "[0-9]+"))]

first_data <- child_data[child.idx==0]
first_data <- merge(first_data, children_age[, list(user.id, age.at.children=value)], by="user.id", all.x=T)
first_data[, diff:= age.at.child-age.at.children]

ggplot(first_data[age>20], aes(x=age.at.children, y=age.at.child)) +
  geom_point(aes(color=age), alpha=0.75) +
  facet_grid(~confirmed.gender)

# todo: does anyone want kids but not marriage?


## Question 3: how many kids do people want?
children <- milestones[event.name=="children" & key %like% "Baby"]
children[, count:= .N, by="event.id"]

child_count <- unique(children[, list(user.id, game.age, confirmed.gender, count)])

child_count_plot <- ggplot(child_count, aes(x=count)) +
                    geom_bar(aes(fill=confirmed.gender, color=confirmed.gender), alpha=0.5) + 
                    facet_grid(~confirmed.gender) +
                    theme(legend.title = element_blank()) + 
                    labs(title="Desired Number of Children, by Gender",
                         x="# of Children",
                         y="Count")

# Question 4: Who influences marriage choices?
marriage_influence <- milestones[event.name=="marriage" & key=="influencer"]

marriage_influence_plot <- ggplot(marriage_influence, aes(x=value)) +
                              geom_bar(aes(fill=value, color=value)) +
                              facet_grid(~confirmed.gender)+
                              theme(legend.position = "none",
                                    axis.text.x=element_text(angle=45, hjust=1)) + 
                              labs(title="Influencers of Marriage Choices",
                                   x="Influencer",
                                   y="Count")

# Question 5: Who influences child choices?
children_influence <- milestones[event.name=="children" & key=="influencer"]

child_influence_plot <- ggplot(children_influence, aes(x=value)) +
                          geom_bar(aes(fill=value, color=value)) +
                          facet_grid(~confirmed.gender)+
                          theme(legend.position = "none",
                                axis.text.x=element_text(angle=45, hjust=1)) + 
                          labs(title="Influencers of Child Choices",
                               x="Influencer",
                               y="Count")


# Question 6: What's the average time spent on each minigame?
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

minigame_plot <- ggplot(mean_games, aes(x=game.type, y=mean.duration)) +
                  geom_bar(aes(color=game.type, fill=game.type), stat="identity") +
                  theme(legend.position = "none",
                        axis.text.x = element_text(angle=45, hjust=1)) +
                  labs(title="Mean Minigame Duration",
                       x="Minigame",
                       y="Mean Duration (minutes)")

# time spent on all minigames
games[, tot.duration:= sum(duration), by=list(user.id)]
tot_duration <- unique(games[,list(user.id, tot.duration)])

tot_time_plot <- ggplot(tot_duration, aes(x=tot.duration)) +
                geom_density(fill="black", alpha=0.5) +
                geom_vline(xintercept = mean(tot_duration$tot.duration), color="red") + 
                # geom_label(x=mean(tot_duration$tot.duration), y=0.14, label=mean(tot_duration$tot.duration))+
                labs(title="Time Spent on All Minigames",
                     x="Total Duration (minutes)",
                     y="Count")


pdf(paste0(main_dir, "prelim_plots.pdf"), width=11, height=8.5)

print(age_dist)
print(age_dist_school)
print(marriage_age_plot)
print(child_age_plot)
print(child_count_plot)
print(marriage_influence_plot)
print(child_influence_plot)
print(minigame_plot)

graphics.off()

png(paste0(main_dir, "prelim_plots/age_dist.png"), height=900, width=900, units = "px", res=140)
print(age_dist)
graphics.off()

png(paste0(main_dir, "prelim_plots/age_dist_school.png"), height=1200, width=900, units = "px", res=140)
print(age_dist_school)
graphics.off()

png(paste0(main_dir, "prelim_plots/marriage_age_plot.png"), height=900, width=900, units = "px", res=140)
print(marriage_age_plot)
graphics.off()

png(paste0(main_dir, "prelim_plots/child_age_plot.png"), height=900, width=900, units = "px", res=140)
print(child_age_plot)
graphics.off()

png(paste0(main_dir, "prelim_plots/child_count_plot.png"), height=900, width=900, units = "px", res=140)
print(child_count_plot)
graphics.off()

png(paste0(main_dir, "prelim_plots/marriage_influence_plot.png"), height=900, width=900, units = "px", res=140)
print(marriage_influence_plot)
graphics.off()

png(paste0(main_dir, "prelim_plots/child_influence_plot.png"), height=900, width=900, units = "px", res=140)
print(child_influence_plot)
graphics.off()

png(paste0(main_dir, "prelim_plots/minigame_plot.png"), height=900, width=900, units = "px", res=140)
print(minigame_plot)
graphics.off()

png(paste0(main_dir, "prelim_plots/tot_time_plot.png"), height=900, width=900, units = "px", res=140)
print(tot_time_plot)
graphics.off()




