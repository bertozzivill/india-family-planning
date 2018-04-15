## -----------------------------------------------------------------------------------------------------
## clean_data.r
## Author: Amelia Bertozzi-Villa
## Description: Clean pilot data from family planning game for public availability. This includes:
##            -- increasing clarity of column names and ID variables
##            -- merging event names and data
##            -- saving a separate, cleaned dataset for demographics, milestones, and minigame events.
## -----------------------------------------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(stringr)
library(Hmisc)
library(scales)

rm(list=ls())

in_dir <- "/Users/bertozzivill/Dropbox/main/Collaborations/family_planning_game/pilot_data/"
out_dir <- "/Users/bertozzivill/Dropbox/main/Collaborations/family_planning_game/data_for_open_access/"

# 'events': time, device, and description of each event recorded in the game. One line per event. 
events <- fread(paste0(in_dir, "mff_study_events.csv"))
setnames(events, c("event.id", "old.user.id", "event.name", "event.type", "device.id", "timestamp"))

# 'event_data': records the actual outcome of each event. 
# e.g.: the first event id is "Puberty game Female End". This has 8 associated lines in "event_data" 
#       describing completion statistics and timings of components within that event.
event_data <- fread(paste0(in_dir, "mff_study_eventdata.csv"))
setnames(event_data, c("event.id", "key", "value"))

# make more usable user ids 
users <- unique(events[, list(old.user.id)])
users$user.id <- as.integer(rownames(users))
events <- merge(events, users, by="old.user.id", all=T)
events[, old.user.id:=NULL]

# Merge two datasets; split by event type in next section. 
all_data <- merge(event_data, events, by="event.id", all=T)
write.csv(all_data, file=paste0(out_dir, "all_game_data.csv"), row.names = F)

## --------------------------------------------------------------------------------------------------------------
# Demographics Datatset (note: includes one student who started game multiple times)
demog <- dcast(all_data[event.type=="DE"], user.id + event.id ~ key, value.var = "value")
demog[, age:=as.integer(age)]
setnames(demog, c("confirmed gender", "gender", "school code"), c("confirmed.gender", "self.report.gender", "school.code"))
demog[, school.code:=as.integer(school.code)]
write.csv(demog, file=paste0(out_dir, "demog.csv"), row.names = F)

## --------------------------------------------------------------------------------------------------------------
# Milestone Dataset
milestones <- all_data[event.type=="MI", list(user.id, event.id, event.name, key, value)]
write.csv(milestones, file=paste0(out_dir, "milestones.csv"), row.names = F)

## --------------------------------------------------------------------------------------------------------------
# Minigame Dataset
minigames <- all_data[event.type=="MG", list(user.id, device.id, event.id, event.name, key, value, timestamp)]
write.csv(minigames, file=paste0(out_dir, "minigames.csv"), row.names = F)

