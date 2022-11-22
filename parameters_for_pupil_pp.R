##### PARAMETERS FOR PreProcessing pupil #####

#### PART 1: LOADING FILES (& saving)


## details on how each subject's folder is named. This is relevant for vector selection method (i.e., select_folder_by_gui=FALSE)

# string with path to the  folder that contains all the individual subjects' folders
path_to_all_sub_folder<-"~/Documents/GitHub/WTB_pilot_22/raw_data/data_75_25_biu/" 

#(string) the suffix of each folder. e.g. "Sub1"--> sub_folder_suffix="Sub"   | "participant_1" ---> sub_folder_suffix="participant_"
sub_folder_suffix<-"Sub_"   

# (list of numbers). the numbers of the subjects to pp
sub_num_vec<-c(4:5) 

 # if under the subject's folder there are a number of different sub folders for different experiments then add a string with the name (e.g. "SoA_Training").
#If there are no subfolders then leave like this "\\w"
subfolder_name<-   "Butterfly SOA" #"Butterfly Predictions"  #"//w"

##EXPORTING CSV 
# Currently 2 files are saved. "pup.all_stages_XX_date.csv" (that has all the columns of the different satges of the pp'ing)
# & "pup.slim_XX_date.csv" that is slim and has downsampled pupil data & the behavioral data, so it's easy to run analysis on

#define here the string (replaces XX) and it will be added to the name of csv files saved.
export_fname<-"SoA_BIU_pilot1"

###### Part 2: Parameters for pupil pping #####


# fix_sample_rate=TRUE # (logical) Sets whether the sampling is predetrmined or extracted from time stamp. if false then interopolatioin is performed
init_sample_rate=90 #(integer) in hz

#values for Extending Blinks (a step in deblinking)
deblink_fill_forward_val=100 #value in ms
deblink_fill_back_val=100 

## count_missing_pupil
# % NA in trial needed for trial to be excluded
missing_thresh_per_trial=.2 # (numeric), ranges from 0-1. High values mean that you need a lot of NA for trial to be discarded


###event alignment (done by "onset_pupil" function)
#define here lower& upper bound of time that will be displayed & analyzed
start_time_zero= -100 #typically this will be a negative number because it is after alignment
end_time_zero=4*1000
bin_dur_ms=200 #duration of bin - bins are used for downsampling

#from this we calculate how many bin we have and their points
n_bins<-(abs(end_time_zero)+abs(start_time_zero))/bin_dur_ms
bin_vec=seq(start_time_zero,end_time_zero,by=bin_dur_ms)


#### PART 3: PP'ing Behavioral files #####

##THIS PART IS OPTIONAL. MAYBE MOVE IT OUT OF THE PIPELINE FOR CLARITY

# (string) Type of experiment. Currently supports: "WBF"/ "SoA"
exp_type<-  "SoA"  #"WBF"


#### Parameters for PP'ing WBF task #####
# If pp'ing SoA these parameters can be ignored

# Question cues- this question number cued the BF location. 
#note that this is reversed at different points in the exp
## Question 222
q_ind_2222<-c(rep("right",32),rep("left",32),rep("right",32),rep("left",32),rep("right",32))


## Question 333
q_ind_3333<-c(rep("left",32),rep("right",32),rep("left",32),rep("right",32),rep("left",32))


##BF location
right_BF_loc<-c(60,62) # these are numbers of BF that on the right

left_BF_loc<-c(61,63) # these are numbers of BF that on the right

#trial numbers that are used in tracker output csv used in filter_trial_room function (in functions for EM)
trkr_trial_val<-c(1:159) 
