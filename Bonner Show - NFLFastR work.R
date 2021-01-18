#Reading in Packages
library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)

data2020 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

#names(data2020) shows all variable names

#How to find shotgun pass plays that were 75+ yards
data2020 %>% 
  select(posteam, defteam, desc, rush, pass, yards_gained, shotgun) %>% 
  filter(yards_gained >= 75 & pass == 1 & shotgun == 1)

#How to find the top 10 highest plays by EPA
data2020 %>% 
  select(posteam, defteam, desc, rush, pass, yards_gained, epa) %>% 
  filter(rush == 1 | pass == 1 & epa >5) %>% 
  top_n(10) %>% 
  arrange(-epa)


#Creating a dataframe of only rushing and passing plays that have EPA involved
pbp_rp <- data2020 %>%
  filter(rush == 1 | pass == 1, !is.na(epa))

#Looking at epa per pass play for 49ers receivers
pbp_rp %>%
  filter(posteam == "SF", down <=4, play_type == 'pass') %>%
  group_by(receiver) %>%
  summarize(
    mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()
  ) %>%
  arrange(-mean_epa) %>%
  filter(plays > 10)

#Looking at leaguewide rushing stats sorted by epa
pbp_rp %>%
  filter(down <=4, play_type == 'run', rusher != 'NA') %>%
  group_by(rusher) %>%
  summarize(
    mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()
  ) %>%
  arrange(-mean_epa) %>%
  filter(plays > 30)

#Looking at home/away average epa per play
pbp_rp %>%
  mutate(
    home = if_else(posteam == home_team, 1, 0)
  ) %>%
  group_by(home) %>%
  summarize(epa = mean(epa))



#Looking at completion percentage for different areas of the field for each quarterback
QB_AirYards <- pbp_rp %>%
  filter(!is.na(cp)) %>%
  mutate(
    depth = case_when(
      air_yards < 0 ~ "Negative",
      air_yards >= 0 & air_yards < 10 ~ "Short",
      air_yards >= 10 & air_yards < 20 ~ "Medium",
      air_yards >= 20 ~ "Deep"
    )
  ) %>%
  group_by(passer, depth) %>%
  summarize(cp = mean(cp), plays = n(), mean_epa = mean(epa), cpoe = mean(cpoe), team = last(posteam)) %>% 
  filter(plays > 15) %>% 
  arrange(-plays)

#Doing the same as above but for receiver
WR_AirYards <- pbp_rp %>%
  filter(!is.na(cp)) %>%
  mutate(
    depth = case_when(
      air_yards < 0 ~ "Negative",
      air_yards >= 0 & air_yards < 10 ~ "Short",
      air_yards >= 10 & air_yards < 20 ~ "Medium",
      air_yards >= 20 ~ "Deep"
    )
  ) %>%
  group_by(receiver, depth) %>%
  summarize(cp = mean(cp), plays = n(), mean_epa = mean(epa), cpoe = mean(cpoe), team = last(posteam)) %>% 
  filter(plays > 10) %>% 
  arrange(-plays)

#Querying which receivers have the highest cp on deep ball throws
WR_AirYards %>%
  filter(depth == 'Deep', plays > 25) %>% 
  arrange(-cp)

#Querying QBs with the highest CPOE on medium throws
QB_AirYards %>%
  filter(depth == 'Medium', plays > 25) %>% 
  arrange(-cpoe)




#QB EPA/CPOE graph section
qbs2020 <- pbp_rp %>%
  filter(week <= 17, !is.na(epa)) %>%
  group_by(id, name, passer) %>%
  summarize(
    epa = mean(qb_epa),
    cpoe = mean(cpoe, na.rm = T),
    n_dropbacks = sum(pass),
    n_plays = n(),
    team = last(posteam)
  ) %>%
  ungroup() %>%
  filter(n_dropbacks > 300 & n_plays > 300)

#Joining team logos to the quarterback
qbs2020 <- qbs2020 %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

#Plotting CPOE vs EPA
qbs2020 %>%
  ggplot(aes(x = cpoe, y = epa)) +
  #horizontal line with mean EPA
  geom_hline(yintercept = mean(qbs2020$epa), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean CPOE
  geom_vline(xintercept =  mean(qbs2020$cpoe), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the logos
  geom_image(aes(image = team_logo_espn), size = qbs2020$n_plays / 15000, asp = 16 / 9) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=name)) +
  #add a smooth line fitting cpoe + epa
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "Completion % above expected (CPOE)",
       y = "EPA per play (passes, rushes, and penalties)",
       title = "Quarterback Efficiency, 2020",
       caption = "Data: @nflfastR, Logo Size a Function of the Number of Snaps") +
  theme_bw() +
  #center title
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  #make ticks look nice
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

#QB's on Third Down
qbs2020down3 <- pbp_rp %>%
  filter(week <= 17, !is.na(epa), down == 3) %>%
  group_by(id, name) %>%
  summarize(
    epa = mean(qb_epa),
    cpoe = mean(cpoe, na.rm = T),
    n_dropbacks = sum(pass),
    n_plays = n(),
    team = last(posteam)
  ) %>%
  ungroup() %>%
  filter(n_dropbacks > 70 & n_plays > 70)

qbs2020down3 <- qbs2020down3 %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

qbs2020down3 %>%
  ggplot(aes(x = cpoe, y = epa)) +
  #horizontal line with mean EPA
  geom_hline(yintercept = mean(qbs2020down3$epa), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean CPOE
  geom_vline(xintercept =  mean(qbs2020down3$cpoe), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the logos
  geom_image(aes(image = team_logo_espn), size = qbs2020down3$n_plays / 5000, asp = 16 / 9) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=name)) +
  #add a smooth line fitting cpoe + epa
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "Completion % above expected (CPOE)",
       y = "EPA per play (passes, rushes, and penalties)",
       title = "3rd Down Quarterback Efficiency, 2020",
       caption = "Data: @nflfastR, Logo Size a Function of the Number of Snaps") +
  theme_bw() +
  #center title
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  #make ticks look nice
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))


#Sum EPA for QB's
sumqbs2020 <- pbp_rp %>%
  filter(week <= 17, !is.na(epa)) %>%
  group_by(id, name) %>%
  summarize(
    epa = sum(qb_epa),
    cpoe = mean(cpoe, na.rm = T),
    n_dropbacks = sum(pass),
    n_plays = n(),
    team = last(posteam)
  ) %>%
  ungroup() %>%
  filter(n_dropbacks > 300 & n_plays > 300)

sumqbs2020 <- sumqbs2020 %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

sumqbs2020 %>%
  ggplot(aes(x = n_dropbacks, y = epa)) +
  #horizontal line with sum EPA
  geom_hline(yintercept = mean(sumqbs2020$epa), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean dropbacks
  geom_vline(xintercept =  mean(sumqbs2020$n_dropbacks), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the logos
  geom_image(aes(image = team_logo_espn), size = sumqbs2020$n_plays / 15000, asp = 16 / 9) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=name)) +
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "Number of Dropbacks",
       y = "Total EPA (passes, rushes, and penalties)",
       title = "Total EPA by QB for the Season, 2020",
       caption = "Data: @nflfastR, Logo Size a Function of the Number of Snaps") +
  theme_bw() +
  #center title
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  #make ticks look nice
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))


#Looking at offensive vs defensive Team EPA's
offense <- pbp_rp %>%
  group_by(posteam, season, pass) %>% 
  summarize(epa = mean(epa)) %>%
  pivot_wider(names_from = pass, values_from = epa) %>%
  rename(off_pass_epa = `1`, off_rush_epa = `0`)
#Adding team colors for plotting sake
offense$team_abbr <- offense$posteam
offense <- offense %>%
  left_join(teams_colors_logos, by = 'team_abbr')

defense <- pbp_rp %>%
  group_by(defteam, season, pass) %>% 
  summarize(epa = mean(epa)) %>%
  pivot_wider(names_from = pass, values_from = epa) %>%
  rename(def_pass_epa = `1`, def_rush_epa = `0`)
#Adding team colors for plotting sake
defense$team_abbr <- defense$defteam
defense <- defense %>%
  left_join(teams_colors_logos, by = 'team_abbr')


offense %>% 
  ggplot(aes(off_rush_epa, off_pass_epa)) +
  geom_hline(yintercept = mean(offense$off_pass_epa), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(offense$off_rush_epa), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the logos
  geom_image(aes(image = team_logo_espn)) +
  #geom_text_repel(aes(label=team_abbr)) +
  #stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm') +
  labs(x = "Offensive Rush EPA per play",
       y = "Offensive Passing EPA per play",
       title = "Team Offensive Rush EPA vs. Pass EPA (Per Play), 2020",
       caption = "Data: @nflfastR") + 
  theme_light() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  )

defense %>% 
  ggplot(aes(def_rush_epa, def_pass_epa)) +
  geom_hline(yintercept = mean(defense$def_pass_epa), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(defense$def_rush_epa), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the logos
  geom_image(aes(image = team_logo_espn)) +
  #geom_text_repel(aes(label=team_abbr)) +
  #stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm') +
  labs(x = "Defensive Rush EPA (per play) Allowed",
       y = "Defensive Passing EPA (per play) Allowed",
       title = "Team Defensive Rush vs. Pass EPA Allowed, 2020",
       caption = "Data: @nflfastR") + 
  theme_light() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  )

#Quarterbacks
quarterbacks <- pbp_rp %>%
  filter(down <=4, play_type == 'pass', air_yards != 'NA', cpoe != 'NA', yards_gained != 'NA') %>%
  group_by(passer) %>%
  summarize(
    mean_epa = mean(epa),
    total_epa = sum(epa),
    cpoe = mean(cpoe), 
    avg_air_yards = mean(air_yards), 
    yards_per_att = mean(yards_gained),
    total_yards = sum(yards_gained),
    Passing_TDs = sum(pass_touchdown),
    plays=n(),
    team = last(posteam)
  ) %>%
  arrange(-mean_epa) %>%
  filter(plays > 200)
quarterbacks <- quarterbacks %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

quarterbacks %>%
  ggplot(aes(x = avg_air_yards, y = yards_per_att)) +
  #horizontal line with mean YPA
  geom_hline(yintercept = mean(quarterbacks$yards_per_att), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean Air Yards
  geom_vline(xintercept =  mean(quarterbacks$avg_air_yards), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the logos
  geom_image(aes(image = team_logo_espn), size = .03, asp = 16 / 9) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=passer)) +
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "Average Air Yards per Pass Attempt",
       y = "Passing Yards per Attempt",
       title = "Quarterback Air Yards Travelled vs. Average Yards per Attempt, 2020",
       caption = "Data: @nflfastR, QB's above the line benefit slightly from YAC") +
  theme_bw() +
  #center title
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  #make ticks look nice
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

quarterbacks %>%
  ggplot(aes(x = total_yards, y = Passing_TDs)) +
  #horizontal line with mean YPA
  geom_hline(yintercept = mean(quarterbacks$Passing_TDs), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean Air Yards
  geom_vline(xintercept =  mean(quarterbacks$total_yards), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the logos
  geom_image(aes(image = team_logo_espn), size = quarterbacks$plays / 15000, asp = 16 / 9) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=passer)) +
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "Total Yards",
       y = "Passing Touchdowns",
       title = "Quarterback Total Passing Yards vs. Passing Touchdowns, 2020",
       caption = "Data: @nflfastR, Logo Size as a Number of Passing Plays") +
  theme_bw() +
  #center title
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  #make ticks look nice
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))


#Running Backs
rbs <- pbp_rp %>%
  filter(down <=4, play_type == 'run', rusher != 'NA') %>%
  group_by(id, rusher) %>%
  summarize(
    mean_epa = mean(epa),
    success_rate = mean(success), 
    ypc=mean(yards_gained), 
    rush_TDs = sum(rush_touchdown),
    total_yards = sum(yards_gained),
    rushes=n(),
    first_downs = sum(first_down),
    first_down_percentage = first_downs/rushes,
    team = last(posteam)
  ) %>%
  arrange(-mean_epa) %>%
  filter(rushes > 100)
rbs <- rbs %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

rbs %>%
  ggplot(aes(x = total_yards, y = rush_TDs)) +
  #horizontal line with mean YPA
  geom_hline(yintercept = mean(rbs$rush_TDs), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean Air Yards
  geom_vline(xintercept =  mean(rbs$total_yards), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the logos
  geom_image(aes(image = team_logo_espn), size = rbs$rushes / 6000, asp = 16 / 9) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=rusher)) +
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "Total Rushing Yards",
       y = "Total Rushing Touchdowns",
       title = "Total Rushing Yards vs. Rushing Touchdowns, 2020",
       caption = "Data: @nflfastR, Logo Size as a Number of Attempts (min. 100)") +
  theme_bw() +
  #center title
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  #make ticks look nice
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

rbs %>%
  ggplot(aes(x = ypc, y = first_downs)) +
  #horizontal line with mean YPA
  geom_hline(yintercept = mean(rbs$first_downs), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean Air Yards
  geom_vline(xintercept =  mean(rbs$ypc), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the logos
  geom_image(aes(image = team_logo_espn), size = rbs$rushes / 6000, asp = 16 / 9) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=rusher)) +
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "Yards per Carry",
       y = "Total First Downs Earned",
       title = "Yards per Carry vs. Rushing First Downs, 2020",
       caption = "Data: @nflfastR, Logo Size as a Number of Attempts (min. 100)") +
  theme_bw() +
  #center title
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  #make ticks look nice
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))




#Receivers
rec <- pbp_rp %>%
  filter(down <=4, play_type == 'pass', receiver != 'NA', incomplete_pass == 0, yards_after_catch != 'NA', yards_gained != 'NA') %>%
  group_by(id, receiver) %>%
  summarize(
    mean_epa = mean(epa),
    yards_per_rec=mean(yards_gained), 
    total_yards = sum(yards_gained),
    avg_YAC = mean(yards_after_catch),
    total_YAC = sum(yards_after_catch),
    avg_depth_target = mean(air_yards),
    rec_TDs = sum(pass_touchdown),
    receptions=n(),
    first_downs = sum(first_down),
    first_down_percentage = first_downs/receptions,
    team = last(posteam)
  ) %>%
  arrange(-mean_epa) %>%
  filter(receptions > 50)
rec <- rec %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

rec %>%
  ggplot(aes(x = avg_depth_target, y = avg_YAC)) +
  #horizontal line with mean YPA
  geom_hline(yintercept = mean(rbs$avg_YAC), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean Air Yards
  geom_vline(xintercept =  mean(rbs$avg_depth_target), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the logos
  geom_image(aes(image = team_logo_espn), size = rec$receptions / 6000, asp = 16 / 9) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=receiver)) +
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "Average Depth of Target",
       y = "Average Yards after Catch",
       title = "Receivers' Depth of Target vs. YAC, 2020",
       caption = "Data: @nflfastR, Logo Size as a Number of Receptions (min. 50)") +
  theme_bw() +
  #center title
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  #make ticks look nice
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

rec %>%
  ggplot(aes(x = total_yards, y = rec_TDs)) +
  #horizontal line with mean YPA
  geom_hline(yintercept = mean(rec$rec_TDs), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean Air Yards
  geom_vline(xintercept =  mean(rec$total_yards), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the logos
  geom_image(aes(image = team_logo_espn), size = rec$receptions / 6000, asp = 16 / 9) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=receiver)) +
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "Total Receiving Yards",
       y = "Total Receiving Touchdowns",
       title = "Total Receiving Yards vs. Receiving Touchdowns, 2020",
       caption = "Data: @nflfastR, Logo Size as a Number of Receptions (min. 50)") +
  theme_bw() +
  #center title
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  #make ticks look nice
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))


#Quarterbacks in the Red Zone
qbsRZ <- pbp_rp %>%
  filter(down <=4, yardline_100<=20, play_type == 'pass', air_yards != 'NA', cpoe != 'NA', yards_gained != 'NA') %>%
  group_by(passer) %>%
  summarize(
    mean_epa = mean(epa),
    total_epa = sum(epa),
    cpoe = mean(cpoe), 
    avg_air_yards = mean(air_yards), 
    passing_TDs = sum(pass_touchdown),
    ints = sum(interception),
    TD_int_Ratio = if_else(ints == 0, passing_TDs, (passing_TDs/ints)),
    attempts=n(),
    team = last(posteam)
  ) %>%
  arrange(-mean_epa) %>%
  filter(attempts > 25)
qbsRZ <- qbsRZ %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

qbsRZ %>%
  ggplot(aes(y = TD_int_Ratio, x = mean_epa)) +
  #horizontal line with mean YPA
  geom_hline(yintercept = mean(qbsRZ$TD_int_Ratio), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean Air Yards
  geom_vline(xintercept =  mean(qbsRZ$mean_epa), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the logos
  geom_image(aes(image = team_logo_espn), size = qbsRZ$attempts / 1750, asp = 16 / 9) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=passer)) +
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(y = "TD/Interception Ratio (Red Zone Attempts Only)",
       x = "QB EPA per play in the Red Zone",
       title = "QB EPA per play vs. TD/INT Ratio in the Red Zone, 2020",
       caption = "Data: @nflfastR, Logo Size as a Number of Attempts (min. 25)") +
  theme_bw() +
  #center title
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  #make ticks look nice
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

qbsRZ %>%
  ggplot(aes(x = cpoe, y = mean_epa)) +
  #horizontal line with mean EPA
  geom_hline(yintercept = mean(qbsRZ$mean_epa), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean CPOE
  geom_vline(xintercept =  mean(qbsRZ$cpoe), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the logos
  geom_image(aes(image = team_logo_espn), size = qbsRZ$attempts / 1750, asp = 16 / 9) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=passer)) +
  #add a smooth line fitting cpoe + epa
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "Completion % above expected (CPOE)",
       y = "EPA per play (passes, rushes, and penalties)",
       title = "Quarterback Efficiency (Red Zone Only), 2020",
       caption = "Data: @nflfastR, Logo Size a Function of the Number of Snaps") +
  theme_bw() +
  #center title
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  #make ticks look nice
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))


#Create EPA by week Chart for each Passer
weekly_EPA <- pbp_rp %>% 
  select(qb_epa, week, posteam, defteam) %>% 
  filter(qb_epa != 'NA') %>% 
  group_by(week, posteam) %>% 
  summarize(
    mean_epa = mean(qb_epa),
    team = last(defteam)
  )
weekly_EPA <- weekly_EPA %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

#Creating an average weekly epa for the whole league
qb_avg <- weekly_EPA %>% group_by(week) %>% 
  summarize(mean_epa = mean(mean_epa))

#This takes forever to run so be careful
weekly_EPA %>% filter(posteam == 'BAL') %>% 
  ggplot() +
  #add points for the QBs with the logos
  geom_line(aes(x = week, y = mean_epa), size = 1, color = "purple4") +
  geom_image(aes(x = week, y = mean_epa, image = team_logo_espn), asp = 16/9, size = .04) +
  geom_line(data = qb_avg, aes(x = week, y = mean_epa), size = 1, color = 'red', alpha = 0.5) +
  facet_wrap(~posteam) +
  
  #titles and caption
  labs(x = "Week",
       y = "Lamar Jackson EPA per play (passes, rushes, and penalties)",
       title = "QB EPA per play by Week",
       caption = "Data: @nflfastR, Red line represents QB league-average week-to-week") +
  theme_bw() +
  #center title
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  #make ticks look nice
  #scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(limits = c(-.75,.75)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))



#This takes forever to run so be careful - All teams
weekly_EPA %>%
  ggplot() +
  #add points for the QBs with the logos
  geom_line(aes(x = week, y = mean_epa), size = 1, color = "red") +
  geom_image(aes(x = week, y = mean_epa, image = team_logo_espn), asp = 16/9, size = .04) +
  geom_line(data = qb_avg, aes(x = week, y = mean_epa), size = 1, color = 'gray', alpha = 0.5) +
  facet_wrap(~posteam) +
  #titles and caption
  labs(x = "Week",
       y = "QB EPA per play (passes, rushes, and penalties)",
       title = "QB EPA per play by Week",
       caption = "Data: @nflfastR, Red line represents QB league-average week-to-week") +
  theme_bw() +
  #center title
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  #make ticks look nice
  #scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(limits = c(-.75,.75)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))





#Creating a list to create headers for Facet Labels
EPAlist <- c(
  `GB` = 'Packers',
  `KC` = 'Chiefs',
  `BUF` = 'Bills',
  `TEN` = 'Titans',
  `TB` = 'Buccaneers',
  `BAL` = 'Ravens',
  `CLE` = 'Browns',
  `MIN` = 'Vikings',
  `HOU` = 'Texans',
  `IND` = 'Colts',
  `NO` = 'Saints',
  `SEA` = 'Seahawks'
)

#Just looking at top 12 Teams in Passing EPA
top10weeklyEPA <- weekly_EPA %>% group_by(posteam) %>% 
  summarize(mean_epa = mean(mean_epa)) %>%  top_n(n = 12)

#Plotting Top 12 passing teams in EPA per play
weekly_EPA %>%
  filter(posteam == 'GB' | posteam == 'KC' | posteam == 'BUF' | posteam == 'TEN' | posteam == 'TB' |
           posteam == 'BAL' | posteam == 'CLE' | posteam == 'MIN' | posteam == 'HOU' | posteam == 'IND' |
           posteam == 'NO' | posteam == 'SEA') %>% 
  ggplot() +
  #add points for the QBs with the logos
  geom_line(aes(x = week, y = mean_epa), size = 1, color = 'red', alpha = .75) +
  geom_image(aes(x = week, y = mean_epa, image = team_logo_espn), asp = 16/9, size = .05) +
  #geom_text(x = 2, y = -.4, aes(label = mean_epa)) +
  geom_line(data = qb_avg, aes(x = week, y = mean_epa), size = 1, color = 'gray', alpha = 0.5) +
  facet_wrap(~posteam, labeller = as_labeller(EPAlist)) +
  #titles and caption
  labs(x = "Week",
       y = "QB EPA per play (passes, rushes, and penalties)",
       title = "Top 12 Teams in QB EPA per play by Week",
       caption = "Data: @nflfastR, Gray line represents QB league-average week-to-week") +
  theme_bw() +
  #center title
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  #make ticks look nice
  #scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))


library(RColorBrewer)
library(ggsci)
#Boxplot of Weekly EPA's
weekly_EPA %>% 
  ggplot() +
  geom_boxplot(aes(x = week, y = mean_epa, group = week),
               outlier.color = 'red',
               outlier.shape = 8,
               outlier.size = .75,
               color = 'navy',
               ) +
  #titles and caption
  labs(x = "Week",
     y = "EPA per play (passes, rushes, and penalties)",
     title = "Boxplot Distributions of QB EPA per play by Week",
     caption = "Data: @nflfastR") +
  theme_bw() +
  #center title
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold", color = 'navy')
  ) +
  #make ticks look nice
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
  


#Graph quarterbacks completion percentage by depth of target -
#Create a density chart for each QB by depth of target, along with layering completion perception in that depth

QB_depth <- pbp_rp %>% 
  select(passer, posteam, air_yards, epa, cp, play_type, incomplete_pass) %>% 
  filter(play_type == 'pass', air_yards != 'NA') %>% 
  group_by(passer, posteam, air_yards, epa, cp, incomplete_pass) %>% 
  summarize(attempts = n())

QB_depth %>% filter(passer == 'T.Brady') %>% 
  ggplot(aes(x = air_yards), color = 'red') +
  geom_histogram(aes(y = ..density..), colour="red", fill="white") +
  geom_density(alpha = .2, fill = 'red', color = 'black') +
  labs(x = "Depth of Target (yards)",
       y = "Percentage of Attempts",
       title = "Tom Brady Depth of Target Percentage",
       caption = "Data: @nflfastR") +
  theme_bw() +
  #center title
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold", color = 'black')
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

QB_depth %>% filter(passer == 'T.Tagovailoa') %>% 
  ggplot(aes(air_yards), color = 'orange') +
  geom_histogram(stat = 'count', color = 'orange', fill = 'white') +
  labs(x = "Depth of Target (yards)",
       y = "Attempts",
       title = "Tua Tagovailoa Depth of Target per Pass Attempt") +
  theme_bw() +
  theme(
    aspect.ratio = 9 / 16,
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold", color = 'white'),
    plot.background = element_rect(fill = 'darkturquoise'),
    panel.background = element_rect(fill = 'darkturquoise'),
    axis.title.x = element_text(color = 'white', size = 13, vjust = -.5),
    axis.title.y = element_text(color = 'white', size = 13, hjust = .5),
    axis.text = element_text(color = 'white'),
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

#Average yards to go vs depth of throw by QB


#Create fantasy stats for each player





#Trying to assess how models of each variable evaluate QB's correlation wise
qb_vars <- c(data2020$epa, data2020$air_yards, data2020$cpoe, data2020$wpa)
QBs <- data2020 %>% 
  group_by(passer_player_name, season, posteam) %>% 
  summarize(avg_EPA = mean(epa), avg_AY = mean(air_yards), avg_CPOE = mean(cpoe), avg_WPA = mean(wpa))
QBs$std_epa <- data2020 %>% 
  group_by(passer_player_name, season, posteam) %>% 
  summarize(std_EPA = sd(epa))

