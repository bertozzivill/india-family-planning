## -----------------------------------------------------------------------------------------------------
## clean_data.r
## Author: Amelia Bertozzi-Villa
## Date: April 15th 2018
## Description: Clean pilot data from family planning game for public availability. This includes:
##            -- increasing clarity of column names and ID variables
##            -- merging event names and data
##            -- saving a separate, cleaned dataset for demographics, milestones, and minigame events.
##
##            Also clean post-game questinnaire data by renaming columns and keeping only the variables
##            used in the analysis. 
## -----------------------------------------------------------------------------------------------------

library(data.table)

rm(list=ls())

in_dir <- "/Users/bertozzivill/Dropbox/main/Collaborations/my_future_family/chennai_2018/data_090818"
out_dir <- file.path(in_dir, "clean")

##NOTES: 
## - need keys for what pre-post game q's and correct a's are
## - in future, ask to have different event.ids for pre- and post game quiz

## 1: In-game data (from tablets)----------------------------------------------------------------------------------

# 'events': time, device, and description of each event recorded in the game. One line per event. 
events <- fread(file.path(in_dir, "events.csv"))
setnames(events, c("event.id", "old.user.id", "event.name", "event.type", "device.id", "timestamp"))

# 'event_data': records the actual outcome of each event. 
# e.g.: the first event id is "Puberty game Female End". This has 8 associated lines in "event_data" 
#       describing completion statistics and timings of components within that event.
event_data <- fread(file.path(in_dir, "eventdata.csv"))
setnames(event_data, c("event.id", "key", "value"))

# make more usable user ids 
users <- unique(events[, list(old.user.id)])
users$user.id <- as.integer(rownames(users))
events <- merge(events, users, by="old.user.id", all=T)
events[, old.user.id:=NULL]

# drop anyone who doesn't have the proper number (5) of milestones completed
milestone_events <- events[event.type=="MI"]
milestone_events[, event.count:=.N, by="user.id"]
incorrect_mi <- milestone_events[event.count!=5]
print(paste(length(unique(incorrect_mi[event.count<5]$user.id)), "players have fewer than 5 milestones recorded, and",
            length(unique(incorrect_mi[event.count>5]$user.id)), "players have more than 5 milestones recorded--dropping."))
milestone_events <- milestone_events[event.count==5]
valid_users <- unique(milestone_events$user.id)

# Merge two datasets 
all_data <- merge(event_data, events, by="event.id", all=T)
all_data <- all_data[user.id %in% valid_users]
setcolorder(all_data, c("device.id", "user.id", "event.id", "event.type", "event.name", "key", "value", "timestamp"))
all_data[event.name=="Pre Quiz", event.type:="PRQZ"]
all_data[event.name=="Post Quiz", event.type:="POQZ"]
write.csv(all_data, file=file.path(out_dir, "all_game_data.csv"), row.names = F)

# Demographics Datatset
demog <- dcast(all_data[event.type=="DE"], user.id + event.id ~ key, value.var = "value")
demog[, age:=as.integer(age)]
# note: lots of missingness in confirmed sex, school code
setnames(demog, c("confirmed gender", "gender", "school code"), c("confirmed.sex", "self.report.sex", "school.code"))
demog[, school.code:=as.integer(school.code)]
write.csv(demog, file=file.path(out_dir, "demog.csv"), row.names = F)

# Milestone Dataset
milestones <- all_data[event.type=="MI", list(user.id, event.id, event.name, key, value)]
write.csv(milestones, file=file.path(out_dir, "milestones.csv"), row.names = F)

# Minigame Dataset
minigames <- all_data[event.type=="MG"]
write.csv(minigames, file=file.path(out_dir, "minigames.csv"), row.names = F)

# Pre-post dataset
answers <- fread(file.path(in_dir, "quiz_key.csv"))
prepost <- all_data[event.type %like% "QZ"]
prepost <- merge(prepost, answers, by="key", all=T)
write.csv(prepost, file=file.path(out_dir, "prepost.csv"), row.names = F)


## 2: Post-Game Questionnaire (paper) -------------------------------------------------------------------

quest_fname <- file.path(in_dir, "questionnaire.csv")

if (file.exists(quest_fname)){
  questionnaire <- fread(quest_fname)
  
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
  
  names(questionnaire) <- newlabels
  
  questionnaire <- questionnaire[, list(user.id, age, sex,
                                        male.child.count, female.child.count, child.1.age, child.2.age, child.3.age, child.4.age,
                                        liked.game, recommend.friends, improved.knowledge, want.more.family.planning.games,
                                        helped.plan.future, want.more.health.games, prior.awareness, other.health.topics, other.comments)]
  questionnaire[questionnaire==""] <- NA
  write.csv(questionnaire, file=paste0(out_dir, "questionnaire.csv"), row.names = F)
  
  
}


