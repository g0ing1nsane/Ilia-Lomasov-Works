
# # ----------------------DATA PREPARATION------------------------

rm(list=ls())
library(skedastic)
library(sandwich)
library(DescTools)
library(lmtest)
library(data.table); library(ggplot2); library(urca); library(tictoc); library(dplyr); library(stringr)

# ---------- Data collection and preparation (deleting non-250/500/1000/GS tournaments)
datafile1 <- "https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2021.csv"
dt1 <- as.data.table(read.csv(datafile1))
datafile2 <- "https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2022.csv"
dt2 <- as.data.table(read.csv(datafile2))
dt <- as.data.table(rbind(dt1,dt2))
rm(datafile1); rm(datafile2); rm(dt1); rm(dt2)
dt <- dt[!grepl("D", dt$tourney_level),]
dt <- dt[!tourney_id=="2021-7696",]
dt <- dt[!tourney_name=="Tokyo Olympics",]
dt <- dt[!tourney_name=="Atp Cup",]
dt <- dt[order(tourney_date,tourney_name,match_num),]

# ---------- Deleting main NA's (ranks, points, names, W/O's)
dt <- dt[!is.na(dt$loser_rank_points)==TRUE,]
dt <- dt[!is.na(dt$loser_rank)==TRUE,]
dt <- dt[!which(dt$score=="W/O"),]
no_min <- which(is.na(dt$minutes))
dt$minutes[1864] <- 79; dt$minutes[1868] <- 104; dt$minutes[3031] <- 129
rm(no_min)

# ---------- Adding tourney date as a number
y <- substr(dt[,tourney_date],1,4); m <- substr(dt[,tourney_date],5,6); d <- substr(dt[,tourney_date],7,8)
date <- paste(y,m,d,sep = "-",collapse = NULL)
  rm(y); rm(m); rm(d)
dt$tourney_date_num <- as.numeric(as.POSIXct(date,format = "%Y-%m-%d"))/86400
dt$tourney_date_num <- dt$tourney_date_num-dt$tourney_date_num[1]
  rm(date)

# ---------- Adding "wins/losses in the last 14/28 days" to the table
dt_winloss <- as.data.table(cbind(dt[,tourney_date_num],dt[,winner_id],dt[,loser_id]))
colnames(dt_winloss) <- c("tdt","wid","lid")
dt_winloss$tdt <- as.numeric(dt_winloss$tdt); dt_winloss$wid <- as.numeric(dt_winloss$wid); dt_winloss$lid <- as.numeric(dt_winloss$lid)
w_w_56d <- vector(mode = "numeric",length = nrow(dt_winloss))
w_l_56d <- vector(mode = "numeric",length = nrow(dt_winloss))
l_w_56d <- vector(mode = "numeric",length = nrow(dt_winloss))
l_l_56d <- vector(mode = "numeric",length = nrow(dt_winloss))
w_w_28d <- vector(mode = "numeric",length = nrow(dt_winloss))
w_l_28d <- vector(mode = "numeric",length = nrow(dt_winloss))
l_w_28d <- vector(mode = "numeric",length = nrow(dt_winloss))
l_l_28d <- vector(mode = "numeric",length = nrow(dt_winloss))
tic()
for (i in 2:nrow(dt_winloss)) {
    a56WW <- 0; a56WL <- 0; a56LW <- 0; a56LL <- 0; a28WW <- 0; a28WL <- 0; a28LW <- 0; a28LL <- 0
    for (j in 1:(i-1)){
        if (dt_winloss$tdt[i]-dt_winloss$tdt[j]<=56) {
            if(dt_winloss$wid[i]==dt_winloss$wid[j]){a56WW <- a56WW+1}
            if(dt_winloss$wid[i]==dt_winloss$lid[j]){a56WL <- a56WL+1}
            if(dt_winloss$lid[i]==dt_winloss$wid[j]){a56LW <- a56LW+1}
            if(dt_winloss$lid[i]==dt_winloss$lid[j]){a56LL <- a56LL+1}
        }
        if (dt_winloss$tdt[i]-dt_winloss$tdt[j]<=28) {
            if(dt_winloss$wid[i]==dt_winloss$wid[j]){a28WW <- a28WW+1}
            if(dt_winloss$wid[i]==dt_winloss$lid[j]){a28WL <- a28WL+1}
            if(dt_winloss$lid[i]==dt_winloss$wid[j]){a28LW <- a28LW+1}
            if(dt_winloss$lid[i]==dt_winloss$lid[j]){a28LL <- a28LL+1}
        }
    }
    w_w_56d[i] <- a56WW; w_l_56d[i] <- a56WL; l_w_56d[i] <- a56LW; l_l_56d[i] <- a56LL
    w_w_28d[i] <- a28WW; w_l_28d[i] <- a28WL; l_w_28d[i] <- a28LW; l_l_28d[i] <- a28LL
}
toc()
  dt$w_w_56d <- w_w_56d; dt$w_l_56d <- w_l_56d; dt$l_w_56d <- l_w_56d; dt$l_l_56d <- l_l_56d
  dt$w_w_28d <- w_w_28d; dt$w_l_28d <- w_l_28d; dt$l_w_28d <- l_w_28d; dt$l_l_28d <- l_l_28d
  rm(i); rm(j); rm(a56LL); rm(a56WL); rm(a56LW); rm(a56WW); rm(a28LL); rm(a28WL); rm(a28LW); rm(a28WW)
  rm(w_w_56d); rm(w_l_56d); rm(l_w_56d); rm(l_l_56d); rm(w_w_28d); rm(w_l_28d); rm(l_w_28d); rm(l_l_28d); rm(dt_winloss)

# ---------- Adding minutes played prior in this tournament to the table
dt_minutes <- as.data.table(cbind(dt[,tourney_id],dt[,winner_id],dt[,loser_id],as.numeric(dt[,minutes])))
colnames(dt_minutes) <- c("tid","wid","lid",'min')
dt_minutes$wid <- as.numeric(dt_minutes$wid); dt_minutes$lid <- as.numeric(dt_minutes$lid); dt_minutes$min <- as.numeric(dt_minutes$min)
mins_so_far <- vector(mode = "numeric",length = nrow(dt_minutes))
mins_so_far_l <- vector(mode = "numeric",length = nrow(dt_minutes))

tic()
for (i in 2:nrow(dt_minutes)) {
  for (j in 1:(i-1)){
    if (dt_minutes$tid[i]==dt_minutes$tid[j]){
      if(dt_minutes$wid[i]==dt_minutes$wid[j]) {mins_so_far[i] <- mins_so_far[i]+dt_minutes$min[j]}
      if(dt_minutes$lid[i]==dt_minutes$wid[j]) {mins_so_far_l[i] <- mins_so_far_l[i]+dt_minutes$min[j]}
    }
  }
}
toc()
dt[,mins_so_far_w:=mins_so_far]
dt[,mins_so_far_l:=mins_so_far_l]
rm(i); rm(j); rm(dt_minutes); rm(mins_so_far); rm(mins_so_far_l)
dt$mins_difference <- dt$mins_so_far_w-dt$mins_so_far_l

# ---------- Adding RANK (& POINTS) difference and AGE difference
dt[,rank_difference:=loser_rank - winner_rank]
dt[,rank_points_difference:=winner_rank_points - loser_rank_points]
dt[,age_difference:=winner_age - loser_age]
dt[,age_mean_difference:=abs(winner_age-mean(c(winner_age,loser_age))) - abs(loser_age-mean(c(winner_age,loser_age)))]

# ---------- Specifying level (250 or 500)
tnm <- as.data.table(cbind(dt$tourney_name,dt$tourney_level))
colnames(tnm) <- c("name","level")
tnm$country <- vector(mode="character",length = nrow(tnm))
# un_250500 <- as.data.frame(unique(tnm$name[which(tnm$level=="A")]))
# colnames(un_250500) <- "name"
# write.csv(un_250500,"/Users/semyon/Library/Mobile Documents/com~apple~CloudDocs/ICEF/y4_2122/Diploma/intermediate_data/atp250500level.csv", row.names = FALSE)
for(i in 1:nrow(tnm)){
  if(tnm$name[i]=="Rotterdam"){tnm$level[i] <- "A500"; tnm$country==""}
  if(tnm$name[i]=="Rio de Janeiro"){tnm$level[i] <- "A500"}
  if(tnm$name[i]=="Acapulco"){tnm$level[i] <- "A500"}
  if(tnm$name[i]=="Dubai"){tnm$level[i] <- "A500"}
  if(tnm$name[i]=="Barcelona"){tnm$level[i] <- "A500"}
  if(tnm$name[i]=="Halle"){tnm$level[i] <- "A500"}
  if(tnm$name[i]=="Queen's Club"){tnm$level[i] <- "A500"}
  if(tnm$name[i]=="Hamburg"){tnm$level[i] <- "A500"}
  if(tnm$name[i]=="Washington"){tnm$level[i] <- "A500"}
  if(tnm$name[i]=="Vienna"){tnm$level[i] <- "A500"}
}
dt$tourney_level <- tnm$level
rm(un_250500); rm(i); rm(j)

# ---------- Adding tourney country
un <- as.data.frame(unique(dt$tourney_name))
colnames(un) <- "name"
un_country <- as.data.frame(unique(c(dt$winner_ioc,dt$loser_ioc)))
colnames(un_country) <- "country"
write.csv(un,"/Users/semyon/Library/Mobile Documents/com~apple~CloudDocs/ICEF/y4_2122/Diploma/intermediate_data/tourneys.csv", row.names = FALSE)
write.csv(un_country,"/Users/semyon/Library/Mobile Documents/com~apple~CloudDocs/ICEF/y4_2122/Diploma/intermediate_data/player countries.csv", row.names = FALSE)
tourneys_countries <- as.data.table(read.csv("/Users/semyon/Library/Mobile Documents/com~apple~CloudDocs/ICEF/y4_2122/Diploma/intermediate_data/tourneys_countries.csv"))
for(i in 1:nrow(tourneys_countries)){
  for(j in 1:nrow(tnm)){
    if(tnm$name[j]==tourneys_countries$name[i]){tnm$country[j]=tourneys_countries$tourney_country[i]}  
  }
}
dt$tourney_ioc <- tnm$country
require(dplyr)
dt <- dt %>% relocate(tourney_ioc, .before = surface)
rm(tnm); rm(tourneys_countries); rm(un); rm(un_country); rm(i); rm(j)
dt$tourney_ioc[dt$tourney_name=="Dubai"] <- "UAE"
dt$tourney_ioc[dt$tourney_name=="Acapulco"] <- "MEX"

# ---------- Adding home advantage dummy
ha <- as.data.table(cbind(dt$tourney_ioc,dt$winner_ioc,dt$loser_ioc))
colnames(ha) <- c("tourney","winner","loser")
ha$winner_dummy <- as.numeric(ha$tourney==ha$winner)
ha$loser_dummy <- as.numeric(ha$tourney==ha$loser)
ha$winner_diff <- ha$winner_dummy-ha$loser_dummy
dt$winner_ioc_home_adv <- as.numeric(ha$winner_diff==1)
dt$winner_ioc_home_disadv <- as.numeric(ha$winner_diff==-1)
dt$winner_ioc_adv <- dt$winner_ioc_home_adv-dt$winner_ioc_home_disadv
rm(ha)

# ---------- Adding odds
file <- as.data.frame(cbind(dt$tourney_id,dt$tourney_name,dt$tourney_ioc,dt$winner_name,dt$loser_name))
write.csv(file,"/Users/semyon/Library/Mobile Documents/com~apple~CloudDocs/ICEF/y4_2122/Diploma/intermediate_data/odds.csv", row.names = FALSE)
rm(file)
dt_odds <- read.csv("/Users/semyon/Library/Mobile Documents/com~apple~CloudDocs/ICEF/y4_2122/Diploma/intermediate_data/odds_final.csv")
dt_odds$result <- NULL
dt_odds$tourney_name <- NULL
dt_odds$tourney_ioc <- NULL
a <- str_split(dt_odds$names,"[:blank:]-[:blank:]")
a <- transpose(as.data.table(a))
a$V1 <- gsub("Cerundolo F.", "francisco cerundolo",a$V1,fixed = TRUE); a$V2 <- gsub("Cerundolo F.", "francisco cerundolo",a$V2,fixed = TRUE)
a$V1 <- gsub("Cerundolo J. M.", "juan manuel cerundolo",a$V1,fixed = TRUE); a$V2 <- gsub("Cerundolo J. M.", "juan manuel cerundolo",a$V2,fixed = TRUE)
a$V1 <- str_remove(a$V1,"[:alpha:]\\.$"); a$V2 <- str_remove(a$V2,"[:alpha:]\\.$")
a$V1 <- str_remove(a$V1,"[:alpha:]\\.[:blank:]$"); a$V2 <- str_remove(a$V2,"[:alpha:]\\.[:blank:]$")
a$V1 <- str_remove(a$V1,"[:alpha:]\\-$"); a$V2 <- str_remove(a$V2,"[:alpha:]\\-$")
a$V1 <- str_remove(a$V1,"'"); a$V2 <- str_remove(a$V2,"'")
a$V1 <- str_trim(a$V1); a$V2 <- str_trim(a$V2)
a$V1 <- str_remove(a$V1,"[:blank:][:alpha:]$"); a$V2 <- str_remove(a$V2,"[:blank:][:alpha:]$")
a$V1 <- str_trim(a$V1); a$V2 <- str_trim(a$V2)
a$V1 <- str_remove(a$V1,"[:blank:][:alpha:]\\.$"); a$V2 <- str_remove(a$V2,"[:blank:][:alpha:]\\.$")
a$V1 <- str_trim(a$V1); a$V2 <- str_trim(a$V2)
a$V1 <- gsub("Andujar-Alba","Andujar",a$V1); a$V2 <- gsub("Andujar-Alba","Andujar",a$V2)
a$V1 <- gsub("Nam Ji","Nam",a$V1); a$V2 <- gsub("Nam Ji","Nam",a$V2)
a$V1 <- gsub("Galan Riveros","Galan",a$V1); a$V2 <- gsub("Galan Riveros","Galan",a$V2)
a$V1 <- gsub("-"," ",a$V1,fixed = TRUE); a$V2 <- gsub("-"," ",a$V2,fixed = TRUE)
a$V1 <- str_to_lower(a$V1); a$V2 <- str_to_lower(a$V2)
dt_odds$names <- NULL; dt_odds$name1 <- a$V1; dt_odds$name2 <- a$V2; rm(a)
# a <- str_split(arg_ba_22$result,"\\:")
# a <- transpose(as.data.table(a))
# a$V2 <- str_sub(a$V2,2,2)

dt_odds_sub <- as.data.table(cbind(dt$tourney_id,dt$winner_name,dt$loser_name))
colnames(dt_odds_sub) <- c("tourney_id","winner_name","loser_name")
dt_odds_sub$winner_odd <- vector(mode = "numeric", length = nrow(dt_odds_sub))
dt_odds_sub$loser_odd <- vector(mode = "numeric", length = nrow(dt_odds_sub))
dt_odds_sub$winner_name <- str_to_lower(dt_odds_sub$winner_name)
dt_odds_sub$loser_name <- str_to_lower(dt_odds_sub$loser_name)

tic()
for(i in 1:nrow(dt_odds)){
  for(j in 1:nrow(dt_odds_sub)){
    if(dt_odds$tourney_id[i]==dt_odds_sub$tourney_id[j]){
      if(str_detect(dt_odds_sub$winner_name[j], dt_odds$name1[i], negate = FALSE)==TRUE & str_detect(dt_odds_sub$loser_name[j], dt_odds$name2[i], negate = FALSE)==TRUE){
        dt_odds_sub$winner_odd[j] <- dt_odds$odd1[i]
        dt_odds_sub$loser_odd[j] <- dt_odds$odd2[i]
      }
      if(str_detect(dt_odds_sub$winner_name[j], dt_odds$name2[i], negate = FALSE)==TRUE & str_detect(dt_odds_sub$loser_name[j], dt_odds$name1[i], negate = FALSE)==TRUE){
        dt_odds_sub$winner_odd[j] <- dt_odds$odd2[i]
        dt_odds_sub$loser_odd[j] <- dt_odds$odd1[i]
      }
    }
  }
}
toc()

dt$winner_odds <- dt_odds_sub$winner_odd; dt$loser_odds <- dt_odds_sub$loser_odd
dt$winner_odds <- gsub(",",".",dt$winner_odds)
dt$loser_odds <- gsub(",",".",dt$loser_odds)
typeof(dt$winner_odds)
sum(dt$loser_odds==0); sum(dt$loser_odds=="-")
dt$winner_odds[dt$loser_odds=="-"] <- 1.8; dt$loser_odds[dt$loser_odds=="-"] <- 2
dt$winner_odds <- as.numeric(dt$winner_odds); dt$loser_odds <- as.numeric(dt$loser_odds)

# dt$winner_odds_prob <- vector(mode = "numeric", length = nrow(dt))
# dt$loser_odds_prob <- vector(mode = "numeric", length = nrow(dt))
dt <- as.data.table(read.csv("/Users/semyon/Library/Mobile Documents/com~apple~CloudDocs/ICEF/y4_2122/Diploma/database.csv"))
dt <- dt[winner_odds!=0,]
dt$winner_odds_prob <- dt$loser_odds/(dt$winner_odds+dt$loser_odds)
dt$loser_odds_prob <- dt$winner_odds/(dt$winner_odds+dt$loser_odds)
dt$odds_prob_ratio <- fifelse(dt$winner_odds==0,0,dt$winner_odds_prob/dt$loser_odds_prob)
dt$odds_prob_difference <- dt$winner_odds_prob-dt$loser_odds_prob
dt$odds_ratio <- fifelse(dt$winner_odds==0,0,(dt$winner_odds/dt$loser_odds)^(-1))
dt$odds_difference <- -dt$winner_odds+dt$loser_odds

rm(dt_odds); rm(dt_odds_sub); rm(i); rm(j)

# ---------- Adding "form"
a <- 1.1; b <- 1
dt$winner_form_28 <- a*dt$w_w_28d-b*dt$w_l_28d
dt$winner_form_56 <- a*dt$w_w_56d-b*dt$w_l_56d
dt$loser_form_28 <- a*dt$l_w_28d-b*dt$l_l_28d
dt$loser_form_56 <- a*dt$l_w_56d-b*dt$l_l_56d
dt$form_difference_28 <- dt$winner_form_28-dt$loser_form_28
dt$form_difference_56 <- dt$winner_form_56-dt$loser_form_56

# ---------- Adding all the other variables
dt$round_num <- vector(mode = "numeric", length = nrow(dt))
dt$round_num[dt$round=="F"] <- 7; dt$round_num[dt$round=="SF"] <- 6
dt$round_num[dt$round=="RR"] <- 5.5; dt$round_num[dt$round=="QF"] <- 5
dt$round_num[dt$round=="R16"] <- 4; dt$round_num[dt$round=="R32"] <- 3
dt$round_num[dt$round=="R64"] <- 2; dt$round_num[dt$round=="R128"] <- 1

dt$tourney_level_num <- vector(mode = "numeric", length = nrow(dt))
dt$tourney_level_num[dt$tourney_level=="A"] <- 250; dt$tourney_level_num[dt$tourney_level=="A500"] <- 500
dt$tourney_level_num[dt$tourney_level=="M"] <- 1000; dt$tourney_level_num[dt$tourney_level=="F"] <- 1300
dt$tourney_level_num[dt$tourney_level=="G"] <- 2000

dt$winner_seed[which(is.na(dt$winner_seed))] <- 0
dt$loser_seed[which(is.na(dt$loser_seed))] <- 0
dt$entry_dummy <- vector(mode="numeric",length = nrow(dt))
dt$entry_dummy[dt$winner_seed!=0 & dt$winner_entry==""] <- 4 
dt$entry_dummy[dt$winner_seed==0 & dt$winner_entry==""] <- 3 
dt$entry_dummy[dt$winner_seed==0 & dt$winner_entry=="Q"] <- 2
dt$entry_dummy[dt$winner_seed==0 & dt$winner_entry!="Q" & dt$winner_entry!=""] <- 1
dt$entry_dummy_l <- vector(mode="numeric",length = nrow(dt))
dt$entry_dummy_l[dt$loser_seed!=0 & dt$loser_entry==""] <- 4 
dt$entry_dummy_l[dt$loser_seed==0 & dt$loser_entry==""] <- 3 
dt$entry_dummy_l[dt$loser_seed==0 & dt$loser_entry=="Q"] <- 2
dt$entry_dummy_l[dt$loser_seed==0 & dt$loser_entry!="Q" & dt$loser_entry!=""] <- 1
dt$entry_diff <- dt$entry_dummy-dt$entry_dummy_l

goat_w <- dt$winner_name=="Novak Djokovic" | dt$winner_name=="Rafael Nadal" # | dt$winner_name=="Roger Federer"
goat_l <- dt$loser_name=="Novak Djokovic" | dt$loser_name=="Rafael Nadal" # | dt$loser_name=="Roger Federer"
goat_d <- as.numeric(goat_w)-as.numeric(goat_l)
goat_gs_w <- goat_w & dt$tourney_level_num==2000
goat_gs_l <- goat_l & dt$tourney_level_num==2000
goat_gs_d <- as.numeric(goat_gs_w)-as.numeric(goat_gs_l)
goat_m_w <- goat_w & dt$tourney_level_num>999
goat_m_l <- goat_l & dt$tourney_level_num>999
goat_m_d <- as.numeric(goat_m_w)-as.numeric(goat_m_l)
dt$goat_d <- goat_d
dt$goat_gs_d <- goat_gs_d
dt$goat_m_d <- goat_m_d
rm(goat_w); rm(goat_l); rm(goat_gs_w); rm(goat_gs_l); rm(goat_m_w); rm(goat_m_l); rm(goat_d); rm(goat_gs_d); rm(goat_m_d)

# file <- as.data.frame(dt1)
# write.csv(file,"/Users/semyon/Library/Mobile Documents/com~apple~CloudDocs/ICEF/y4_2122/Diploma/database_final.csv", row.names = FALSE)
# rm(file)

# # ----------------------PART 1------------------------

# y - dependent variable, x - expl. variable, d - dummy, c - categorical quantifiable
dt1 <- rename(dt,ypr=odds_prob_ratio) #probability ratio
dt1 <- rename(dt1,ypd=odds_prob_difference) #probability difference 
dt1 <- rename(dt1,yor=odds_ratio) #odds ratio
dt1 <- rename(dt1,yod=odds_difference) #odds difference

dt1 <- rename(dt1,xrpd=rank_points_difference) #rank points difference
dt1 <- rename(dt1,xrd=rank_difference) #rank difference
dt1 <- rename(dt1,crnd=round_num) #round - 8
dt1 <- rename(dt1,ctnm=tourney_level_num) #tourney level - 5
dt1 <- rename(dt1,xfrm28d=form_difference_28) #form 28
dt1 <- rename(dt1,xfrm56d=form_difference_56) #form 56
dt1 <- rename(dt1,dsf=surface) #surface dummy - 3
dt1 <- rename(dt1,cha=winner_ioc_adv) #home advantage dummy - 3
dt1 <- rename(dt1,xmind=mins_difference) #mins difference
dt1 <- rename(dt1,xagemd=age_mean_difference) #age mean difference
dt1 <- rename(dt1,cent=entry_diff) #entry difference
dt1 <- rename(dt1,dgoat=goat_d) #goat dummy
dt1 <- rename(dt1,dgoatgs=goat_gs_d) #goat grand slam dummy
dt1 <- rename(dt1,dgoatm=goat_m_d) #goat masters and grand slam dummy
dt1$ctnmcrnd <- dt1$ctnm*dt1$crnd
c <- which(str_detect(dt1$score,"RET",negate = FALSE))
dt1 <- dt1[-c,]; rm(c)


lm0pd <- lm(ypd ~ xrpd, data = dt1) # rpd
summary(lm0pd)
lm01pd <- lm(ypd ~ xrpd+xrd, data = dt1) # rpd and rd
summary(lm01pd)
lm1pd <- lm(ypd ~ xrpd+xrd+xfrm28d, data = dt1) #add xfrm28
summary(lm1pd)
lm2pd <- lm(ypd ~ xrpd+xrd+xfrm28d+xfrm56d, data = dt1) #add xfrm56
summary(lm2pd)
lm3pd <- lm(ypd ~ xrpd+xrd+xfrm28d+xfrm56d+ctnm, data = dt1) #add ctnm
summary(lm3pd)
lm4pd <- lm(ypd ~ xrpd+xrd+xfrm28d+xfrm56d+ctnm+crnd, data = dt1) #add crnd
summary(lm4pd)
lm5pd <- lm(ypd ~ xrpd+xrd+xfrm28d+xfrm56d+xmind+ctnm+crnd, data = dt1) #add xmind
summary(lm5pd)
lm6pd <- lm(ypd ~ xrpd+xrd+xfrm28d+xfrm56d+xmind+ctnm+crnd+cha, data = dt1) #add cha
summary(lm6pd)
lm7pd <- lm(ypd ~ xrpd+xrd+xfrm28d+xfrm56d+xmind+ctnm+crnd+cha+dsf, data = dt1) #add dsf
summary(lm7pd)
lm8pd <- lm(ypd ~ xrpd+xrd+xfrm28d+xfrm56d+xmind+xagemd+ctnm+crnd+cha+dsf, data = dt1) #add xagemd
summary(lm8pd)
lm9pd <- lm(ypd ~ xrpd+xrd+xfrm28d+xfrm56d+xmind+xagemd+ctnm+crnd+ctnmcrnd+cha+dsf, data = dt1) #add ctnm*crnd
summary(lm9pd)
lm10pd <- lm(ypd ~ xrpd+xrd+xfrm56d+xmind+xagemd+ctnm+crnd+ctnmcrnd+cha+cent+dsf, data = dt1) #add cent
summary(lm10pd)
lm11pd <- lm(ypd ~ xrpd+xrd+xfrm56d+xmind+xagemd+ctnm+crnd+ctnmcrnd+cha+cent+dsf+dgoatgs, data = dt1) #add goat
summary(lm11pd)
bp <- bptest(lm11pd)
wh <- white_lm(lm11pd)
ct <- coeftest(lm11pd, vcov = vcovHC(lm11pd, type = "HC0"))
lm11pd_r <- lm(ypd ~ xrpd, data = dt1)
wt <- waldtest(lm11pd,lm11pd_r, vcov = vcovHC(lm11pd, type = "HC0"))
lm11pdc <- lm(ypd ~ xrpd+xrd+xfrm56d+xmind+xagemd+ctnm+cha+cent, data = dt1) #add goat
summary(lm11pdc)
bpc <- bptest(lm11pdc)
whc <- white_lm(lm11pdc)
ctc <- coeftest(lm11pdc, vcov = vcovHC(lm11pdc, type = "HC0"))
wtc <- waldtest(lm11pdc,lm11pd_r, vcov = vcovHC(lm11pd, type = "HC0"))
# ----------------------------
rm(lm0pd); rm(lm01pd); rm(lm1pd);rm(lm2pd); rm(lm3pd); rm(lm4pd); rm(lm5pd);rm(lm6pd); rm(lm7pd); rm(lm8pd); rm(lm9pd); rm(lm10pd)
# ----------------------------

numexpl <- c("xrpd","xrd","xfrm56d","xmind","xagemd","ctnm","crnd","ctnmcrnd","cha","cent","dgoatgs")
covmatrix <- cor(dt1[,numexpl])
View(covmatrix)

# # ----------------------PART 2------------------------

# ---------- Training the model
n_train <- round(nrow(dt1)/2,0)
pvalue <- vector(mode="numeric",length = 2000)
rmse <- vector(mode="numeric",length = 2000)
mae <- vector(mode="numeric",length = 2000)


for(i in 1:1000){
set.seed(i)
ind_train <- sample(1:n_train,n_train,replace = TRUE)
dt_train <- dt1[ind_train,]
dt_test <- dt1[!ind_train,]

tlmpd1 <- lm(ypd ~ xrpd+xrd+xfrm56d+xmind+xagemd+ctnm+crnd+ctnmcrnd+cha+cent+dsf+dgoatgs, data = dt_train)
b1 <- tlmpd1$coefficients

fypd1 <- b1[1]+b1[2]*dt_test$xrpd+b1[3]*dt_test$xrd+b1[4]*dt_test$xfrm56d+b1[5]*dt_test$xmind+b1[6]*dt_test$xagemd+
    b1[7]*dt_test$ctnm+b1[8]*dt_test$crnd+b1[9]*dt_test$ctnmcrnd+b1[10]*dt_test$cha+b1[11]*dt_test$cent+
    b1[12]*as.numeric(dt_test$dsf=="Grass")+b1[13]*as.numeric(dt_test$dsf=="Hard")+b1[14]*dt_test$dgoatgs

tlmpd2 <- lm(ypd ~ xrpd+xrd+xfrm56d+xmind+xagemd+ctnm+crnd+ctnmcrnd+cha+cent+dsf+dgoatgs, data = dt_test)
b2 <- tlmpd2$coefficients
  
fypd2 <- b2[1]+b2[2]*dt_train$xrpd+b2[3]*dt_train$xrd+b2[4]*dt_train$xfrm56d+b2[5]*dt_train$xmind+b2[6]*dt_train$xagemd+
    b2[7]*dt_train$ctnm+b2[8]*dt_train$crnd+b2[9]*dt_train$ctnmcrnd+b2[10]*dt_train$cha+b2[11]*dt_train$cent+
    b2[12]*as.numeric(dt_train$dsf=="Grass")+b2[13]*as.numeric(dt_train$dsf=="Hard")+b2[14]*dt_train$dgoatgs

ypd_diff1 <- dt_test$ypd-fypd1
ypd_diff2 <- dt_train$ypd-fypd2

pvalue[i] <- 1-pnorm(mean(ypd_diff1)/(sd(ypd_diff1)/sqrt(length(ypd_diff1))))
pvalue[i+1000] <- 1-pnorm(mean(ypd_diff2)/(sd(ypd_diff2)/sqrt(length(ypd_diff2))))

rmse[i] <- sqrt(mean(ypd_diff1^2))
rmse[i+1000] <- sqrt(mean(ypd_diff2^2))

mae[i] <- mean(abs(ypd_diff1))
mae[i+1000] <- mean(abs(ypd_diff2))

rm(ind_train); rm(dt_train); rm(dt_test)
}
rm(tlmpd1); rm(tlmpd2);
rm(ypd_diff1); rm(ypd_diff2); rm(n_train); rm(i); rm(fypd1); rm(fypd2); rm(b1); rm(b2)

zst <- (mean(pvalue)-0.1)/(sd(pvalue)/sqrt(length(pvalue)))
p_zst <- 1-pnorm(zst)

lm11pd$coefficients





# --Line----of----"----code----for----the----section----that----has----not----been----started-----yet----"--


ggplot(dt1)+geom_point(aes(x=xagemd,y=ypd))








dt_hard <- dt1[dt$surface=="Hard",]
hlm02pd <- lm(ypd ~ xrpd+xrd+xfrm28d+xfrm56d+xmind+xagemd+ctnm+ctnmcrnd+cha, data = dt_hard)
summary(hlm02pd)
dt_train_hard <- dt_hard[dt_hard$tourney_date<=20220101,]
thlm10pd <- lm(ypd ~ xrpd+xrd+xfrm56d+xmind+xagemd+ctnm+ctnmcrnd+cha, data = dt_train_hard)
summary(thlm10pd)

dt_test_hard <- dt_hard[dt_hard$tourney_date>20220101,]
bh <- thlm10pd$coefficients

dt_test_hard$fypd <- bh[1]+bh[2]*dt_test_hard$xrpd+bh[3]*dt_test_hard$xrd+bh[4]*dt_test_hard$xfrm56d+bh[5]*dt_test_hard$xmind+
  bh[6]*dt_test_hard$xagemd+bh[7]*dt_test_hard$ctnm+bh[8]*dt_test_hard$ctnmcrnd+bh[9]*dt_test_hard$cha
ypd_diff_h <- dt_test_hard$ypd-dt_test_hard$fypd

pvalue_h <- 2*(1-pnorm(mean(ypd_diff_h)/(sd(ypd_diff_h)/sqrt(length(ypd_diff_h)))))

# save the file
file <- as.data.frame(dt1)
write.csv(file,"/Users/semyon/Library/Mobile Documents/com~apple~CloudDocs/ICEF/y4_2122/Diploma/database_2805.csv", row.names = FALSE)
rm(file)
