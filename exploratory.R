###############
#Block 1: Setup
###############

if (!require(data.table)) { install.packages('data.table'); require(data.table) }
if (!require(ggplot2)) { install.packages('ggplot2'); require(ggplot2) }
if (!require(reshape2)) { install.packages('reshape2'); require(reshape2) }
if (!require(devtools)) { install.packages('devtools'); require(devtools) }
if (!require(ggmap)) { install.packages('ggmap'); require(ggmap) }
if (!require(glmnet)) { install.packages('glmnet'); require(glmnet) }

source("ggplot_utils.R")

crimes.violent = c("ASSAULTS", 'HOMICIDE', 'THREATS, HARASSMENT', 'WEAPONS CALLS')
crimes.serious = c("ARREST", 'AUTO THEFTS', 'BURGLARY', 'CAR PROWL', 'NARCOTICS COMPLAINTS',
                   'PERSON DOWN/INJURY', 'PERSONS - LOST, FOUND, MISSING', 'PROPERTY DAMAGE',
                   'PROSTITUTION', 'PROWLER', 'ROBBERY')

data = fread("data/Seattle_Police_Department_911_Incident_Response.csv", header=T, sep=",")
data[,at_scene_time_ts := as.POSIXct(strptime(`At Scene Time`, "%m/%d/%Y %I:%M:%S %p"))]
data[,at_scene_time_date := as.Date(at_scene_time_ts)]
data[,at_scene_time_week := floor(as.numeric(at_scene_time_date - min(at_scene_time_date, na.rm=T)) / 7) + 1]
data[,event_clearance_ts := as.POSIXct(strptime(`Event Clearance Date`, "%m/%d/%Y %I:%M:%S %p"))]
data[,time_until_event_clear := as.numeric(event_clearance_ts - at_scene_time_ts)]
data[,`Initial Type Group` := factor(`Initial Type Group`)]
data[,`Event Clearance Group` := factor(`Event Clearance Group`)]
data[,`Zone/Beat` := factor(`Zone/Beat`)]
data[,LatitudeBin := round(Latitude, 3)]
data[,LongitudeBin := round(Longitude, 3)]
data[,crime_type := ifelse(`Event Clearance Group` %in% crimes.violent, "Violent", ifelse(`Event Clearance Group` %in% crimes.serious, "Serious", "Minor"))]

#Block 2: Sanity Check
sanityCheck = data.frame(data[,table(weekdays(unique(at_scene_time_date)))]);
sanityCheck$Var1 = factor(sanityCheck$Var1, levels = rev(c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))
ggplot(sanityCheck,
       aes(x = Var1, y = Freq, fill = as.numeric(Var1) %% 2)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  theme_bw(base_size = 25) + 
  xlab ("Day of Week") + ylab("Count") +
  guides(fill=F)

###########################
#Block 3: Types of contacts
###########################
ggplot.freqtable.1d(data[,table(`Event Clearance Group`)]) + 
  xlab("Contact Type") + ylab("Count") +
  theme_bw(base_size = 25) +
  guides(fill=F)

##########################
#Block 4: DoW distribution
##########################

data.dow = rbind(data.frame(data[`Event Clearance Group` %in% crimes.violent,table(weekdays(at_scene_time_date))],
                       type = "Violent"),
            data.frame(data[`Event Clearance Group` %in% crimes.serious,table(weekdays(at_scene_time_date))],
                       type = "Serious"))
data.dow$Var1 = factor(data.dow$Var1, levels = rev(c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))
ggplot(data.dow,
       aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat='identity') +
  facet_grid(type~.)+
  coord_flip()+
  theme_bw(base_size = 25) +
  xlab("Day of Week") + ylab("Count") +
  guides(fill = F)

#########################
#Block 5: multinomial glm
#########################

data.clean = data[!is.na(at_scene_time_date) & !is.na(`Zone/Beat`)]
data.clean[,weekday := factor(weekdays(at_scene_time_date), levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))]
data.clean[,zone := factor(`Zone/Beat`)]
crime.x = model.matrix(~ -1 + weekday*zone, data = data.clean)
crime.y = factor(data.clean$crime_type)
set.seed(100); crime.glmnet = glmnet(crime.x, crime.y, alpha = .8, family='multinomial')
coef(crime.glmnet)
coef(crime.glmnet)$Serious[,ncol(coef(crime.glmnet)$Serious)]
coef(crime.glmnet)$Violent[,ncol(coef(crime.glmnet)$Violent)]
crime.glmnet.grid = expand.grid(weekday = data.clean[,unique(weekday)], zone = data.clean[,unique(zone)])
crime.x2 = model.matrix(~ -1 + weekday*zone, data = crime.glmnet.grid)
crime.probs = predict(crime.glmnet, newx = crime.x2, s = rev(crime.glmnet$lambda)[1], type = 'response')[,,1]
crime.probs.df = data.frame(weekday = crime.glmnet.grid$weekday, zone = crime.glmnet.grid$zone,
                            minor = crime.probs[,1],
                            serious = crime.probs[,2],
                            violent = crime.probs[,3])
crime.probs.melt = melt(crime.probs.df, id.vars = 1:2)
colnames(crime.probs.melt)[3] = "Crime Type"
ggplot(subset(crime.probs.melt, zone %in% c("K1", "K2", "K3", "M2", "M3", "W1", "N3", "D2")),
       aes(x = weekday, y = value, color = `Crime Type`)) +
  geom_point(size = 4) +
  facet_grid(zone ~ .) +
  theme_bw(base_size = 20) +
  xlab("Day of Week") + ylab("Conditional Probability")

#########################
#Block 6: Seattle heatmap
#########################

seattle = get_map(location = c(lon = -122.31, lat = 47.61), zoom = 12, maptype = 'roadmap')
data.filtered = data[!is.na(time_until_event_clear) & time_until_event_clear > 0,
                     list(count = .N),
                     list(LongitudeBin, LatitudeBin)]
ggmap(seattle) +
  geom_point(data = data.filtered[count > 10],
             mapping = aes(x = LongitudeBin, y = LatitudeBin, size = log10(count), alpha = log10(count)), color = 'red') +
  theme_grey(base_size = 25) +
  theme(legend.position="top") +
  xlab("Lon") + ylab("Lat") + labs(title = "Heatmap of Crimes")

############################
#Block 7: crime type by beat
############################

beat.latlon = data[,
                   list(Longitude = mean(Longitude), Latitude = mean(Latitude)),
                   list(`Zone/Beat`)]
beat.crimes = data[,
                   list(count = .N),
                   list(`Zone/Beat`, Type = `Event Clearance SubGroup`)]
setkey(beat.crimes, `Zone/Beat`)
beat.topcrimes = beat.crimes[!Type %in% c("")][count >= 10][order(count)][,tail(.SD,1),list(`Zone/Beat`)]
beat.topcrimes = merge(beat.topcrimes, beat.latlon, by = 'Zone/Beat')
ggmap(seattle) +
  geom_point(data = beat.topcrimes, mapping = aes(x = Longitude, y = Latitude, color = Type), size = 6) +
  theme_grey(base_size = 15) + 
  theme(legend.position="top") +
  guides(colour = guide_legend(nrow = 5)) +
  xlab("Lon") + ylab("Lat") + labs(title = "Most Common Contact Type By Police Beat")

#######################################
#Block 8: heatmap of time until cleared
#######################################

crime = 'PERSON DOWN/INJURY'
data.filtered = data[!is.na(time_until_event_clear) & time_until_event_clear > 0]
data.filtered.contact = data.filtered[`Event Clearance Group` == crime]
time_until_event_clear.q01 = quantile(data.filtered.contact$time_until_event_clear, .01); time_until_event_clear.q99 = quantile(data.filtered.contact$time_until_event_clear, .99)
data.filtered.contact = data.filtered.contact[time_until_event_clear > time_until_event_clear.q01 & time_until_event_clear < time_until_event_clear.q99]
data.filtered.contact2 = data.filtered.contact[,list(Hours = mean(time_until_event_clear / 3600), count = .N), list(Longitude = LongitudeBin, Latitude = LatitudeBin)]

ggmap(seattle) +
  geom_point(data = data.filtered.contact2,
             mapping = aes(x = Longitude, y = Latitude, color = Hours, alpha = Hours, size = count)) +
  scale_colour_gradientn(colours=c("blue", "red")) +
  scale_alpha(range = c(0.2, 0.7)) +
  scale_size_continuous(range = c(5,20)) +
  guides(alpha=F,size=F) +
  theme_grey(base_size = 25) +
  theme(legend.position="top") +
  xlab("Lon") + ylab("Lat") + 
  labs(title = sprintf("Time Until %s Cleared", crime))

##################
#Block 9: variance
##################

#Variance between districts in mean time until cleared
#Variance inside districts in mean time until cleared
data.filtered = data[!is.na(time_until_event_clear) & time_until_event_clear > 0]
time_until_cleared.between = time_until_cleared.within = list()
for (crime in c(crimes.serious, crimes.violent)) {
  data.filtered.contact = data.filtered[`Event Clearance Group` == crime]
  time_until_event_clear.q01 = quantile(data.filtered.contact$time_until_event_clear, .01); time_until_event_clear.q99 = quantile(data.filtered.contact$time_until_event_clear, .99)
  data.filtered.contact = data.filtered.contact[time_until_event_clear > time_until_event_clear.q01 & time_until_event_clear < time_until_event_clear.q99]
  if (nrow(data.filtered.contact) > 0) {
    sample.beat = (data.filtered.contact[sample(1:nrow(data.filtered.contact), min(10000, nrow(data.filtered.contact))),
                                     list(zone = `Zone/Beat`,
                                          Longitude, Latitude, 
                                          hours = time_until_event_clear / 3600)])
    sample.district = (data.filtered.contact[sample(1:nrow(data.filtered.contact), min(10000, nrow(data.filtered.contact))),
                                         list(district = `District/Sector`,
                                              Longitude, Latitude, 
                                              hours = time_until_event_clear / 3600)])
    sample.agg = sample.district[,list(hours = mean(hours), count = .N, sd = sd(hours)),list(district)][order(count)]
    time_until_cleared.between = rbind(time_until_cleared.between,
                                       data.frame(crime = crime,
                                                  variance = sample.beat[,list(hours = mean(hours), count = .N, sd = sd(hours)), list(zone)][,var(hours)]))
    time_until_cleared.within = rbind(time_until_cleared.within,
                                      data.frame(crime = crime,
                                                 sample.agg))
  }
}
time_until_cleared.between = time_until_cleared.between[order(time_until_cleared.between$variance, decreasing=F),]
time_until_cleared.between$crime = factor(time_until_cleared.between$crime, levels = time_until_cleared.between$crime)
ggplot(subset(time_until_cleared.between, !is.na(variance)),
       aes(x = crime, y = sqrt(variance), fill = crime)) +
  geom_bar(stat='identity') +
  geom_text(aes(x = crime, y = sqrt(variance) + .2, label = round(sqrt(variance), 3))) +
  coord_flip() +
  guides(fill=F) +
  theme_bw(base_size = 25) +
  xlab("Contact Type") + ylab("Standard Deviation (hours)") + labs(title = 'Variation across police beats to clear a crime')

time_until_cleared.within = data.table(time_until_cleared.within)
p1 = ggplot(time_until_cleared.within[!district %in% c("", "99") & count > 30,list(crime = crime, district = district, CV = sd/hours)],
            aes(x = district, y = CV, fill = district)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~crime) + 
  theme_bw(base_size = 20) + 
  xlab("District") + ylab("Coefficient of Variation (sd / mean)") + labs(title = "Variation within police districts to clear a crime")
facetAdjust(p1)

