##### PARAMETERS FOR PreProcessing pupil walkthrough #####

#### PART 1: LOADING FILES
#(string) the suffix of each folder. e.g. "Sub1"--> sub_folder_suffix="Sub"   | "participant_1" ---> sub_folder_suffix="participant_"
sub_folder_suffix<-"Sub_"   


#### Preprocessing of pupil #####

init_sample_rate<-90 # the initial sampling rate of the HTC VIVE (90 is approximation, in future maybe first interpolate data to be exactly at 90)

## Deblinking parameters (used in "extend_blinks" function)

deblink_fill_forward_val=100 #value in ms
deblink_fill_back_val=100 


## count_missing_pupil
# % NA in trial needed for trial to be excluded
missing_thresh_per_trial=.2 # (numeric), ranges from 0-1. High values mean that you need a lot of NA

###event alignment (done by "onset_pupil" function)
#define here lower& upper bound of time that will be displayed & analyzed
start_time_zero= -100 #typically this will be a negative number because it is after alignment
end_time_zero=4*1000
bin_dur_ms=200 #duration of bin - bins are used for downsampling

#from this we calculate how many bin we have and their points
n_bins<-(abs(end_time_zero)+abs(start_time_zero))/bin_dur_ms
bin_vec=seq(start_time_zero,end_time_zero,by=bin_dur_ms)
