setwd("/Users/nickhedger/Documents/SMELLDAT/BANANA")


# Read in banana data
filevec=list.files(pattern=".txt")

rows1=rep(0,length(filevec))
p1=data.frame()
for (i in 1:length(filevec)){
  rows1[i]=nrow(read.table(file=filevec[i]))
  p1=rbind(read.table(file=filevec[i], header = FALSE,blank.lines.skip=FALSE,sep=",",fill=TRUE),p1)
}

# Read in rose data
setwd("/Users/nickhedger/Documents/SMELLDAT/ROSE")

filevec=list.files(pattern=".txt")
rows2=rep(0,length(filevec))
p2=data.frame()
for (i in 1:length(filevec)){
  rows2[i]=nrow(read.table(file=filevec[i]))
  p2=rbind(read.table(file=filevec[i], header = FALSE,blank.lines.skip=FALSE,sep=",",fill=TRUE),p2)
}


# Assign session labels.
SESS=c(rep(1,nrow(p1)),rep(2,nrow(p2)))

FRAME=cbind(rbind(p1,p2),SESS)

colnames(FRAME)=c("ps","trial","eye","item","resp","time","Session")


# Logical for if rose or banana are pressed.
FRAME$BANANA=as.logical(ifelse(as.numeric(FRAME$resp)==1,1,0))
FRAME$ROSE=as.logical(ifelse(as.numeric(FRAME$resp)==2,1,0))

# Session factor
FRAME$Session=factor(FRAME$Session,levels=c(1,2),labels=c("Banana","Rose"))

FRAME$trackloss=as.logical(ifelse(FRAME$time=="NaN",1,0))

# Convert to milliseconds.
FRAME$time=FRAME$time*1000

# Change trial numbers for the second session, otherwise the timeseries library will through a wobbly.
FRAME[FRAME$Session=="Rose",]$trial=FRAME[FRAME$Session=="Rose",]$trial+32

ET_DATA <- make_eyetrackingr_data(FRAME, 
                                  participant_column = "ps",
                                  trial_column = "trial",
                                  time_column = "time",
                                  aoi_columns = c("ROSE","BANANA"),
                                  treat_non_aoi_looks_as_missing = TRUE,trackloss_column="trackloss"
)


# Overall responding
response_window_agg_by_sub <- make_time_window_data(ET_DATA, aois=c("ROSE","BANANA"),summarize_by = "ps",predictor_columns = c("Session"))

WINDOW_PLOT=plot(response_window_agg_by_sub, predictor_column = "Session")+ylab("Prop response")

WINDOW_PLOT


library(afex)
library(phia)


model_window <- lmer(Prop ~ AOI*Session + (1 | ps), data = response_window_agg_by_sub, REML = FALSE)

model_window_p = mixed(Prop ~ AOI*Session+(1|ps), response_window_agg_by_sub)
model_window_p

# No interaction between response and Session.

# Time window data, analyze in 1000 ms bins
response_time <- make_time_sequence_data(ET_DATA, time_bin_size = 1000,aois = c("ROSE","BANANA"),summarize_by = "ps",predictor_columns = c("Session"))

TS_PLOT=plot(response_time,predictor_column = "Session")+ylab("Prop response")
TS_PLOT


# Show switch proportion as a function of time
onsets <- make_onset_data(ET_DATA, onset_time = 1000, fixation_window_length=100,target_aoi='BANANA',distractor_aoi = 'ROSE')

SWITCH_PLOT=plot(onsets,predictor_columns="Session")

SWITCH_PLOT


onset_switches <- make_switch_data(onsets, predictor_columns = "Session")


# visualize subject's switch times
ONSET_SWITCH=plot(onset_switches, predictor_columns = c("Session"))

ONSET_SWITCH


model_switches <- lmer(FirstSwitch ~ FirstAOI*Session+ (1 | ps), data=onset_switches, REML=FALSE)

model_switches_p = mixed(FirstSwitch ~ FirstAOI*Session+(1|ps), onset_switches)

model_switches_p

plot(allEffects(model_switches))

# Interaction indicates that People were slower to switch from responding "BANANA" in the banana session and vice versa.


# Function for plotting data from a single observer.
plot_observer=function(FRAME,Observer){
FRAME[FRAME$Session=="Rose",]$trial=FRAME[FRAME$Session=="Rose",]$trial-32
TS_PLOT=ggplot(FRAME[FRAME$ps==Observer,],aes(x=time,y=resp))+geom_line(size=1)+
  facet_grid(trial~Session)+theme_bw()+geom_vline(aes(xintercept=time,colour=factor(resp)),alpha=.3)+ scale_colour_manual(values = c("gray90","yellow","pink"))
print(TS_PLOT)
}


OBS_PLOT=plot_observer(FRAME,1)
