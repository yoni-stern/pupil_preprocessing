#### functions for pupil_pp #####

###### Functions for loading files

###### Function for choosing folder interactively #######
##IMPORTANT! need to choose a folder that in it (and its subfolders) there is only a single answer, trial and tracker csv. 

get_fnames_by_gui<-function(sub_folder_suffix){
  # choose interactively the subject's folder  
  #(using selectDirectory from package rstudioapi that is compatible on mac and not "choose.dir" that only works on windows)
  
  path_to_sub_folder <- selectDirectory ( caption = "Select Subject's folder")
  
  #getting ANSWER file name
  ans_fnames<-list.files(path_to_sub_folder,recursive = TRUE,full.names = TRUE)%>%
    str_subset(.,regex('zip$',ignore_case=TRUE),negate=TRUE)%>%
    str_subset(.,regex('answers',ignore_case=TRUE))%>%
    str_subset(.,regex('answers',ignore_case=TRUE))%>%
    str_subset(.,'.csv$')
  
  ##make sure there is only 1  
  if (length(ans_fnames)!=1){
    stop(str_glue("Error!! subject has ", length(ans_fnames),"answer files"))
  } 
  
  ##getting TRIAL file name
  trial_fnames<-list.files(path_to_sub_folder,recursive = TRUE,full.names = TRUE)%>%
    str_subset(.,regex('zip$',ignore_case=TRUE),negate=TRUE)%>%
    str_subset(.,regex('trials',ignore_case=TRUE))%>%
    str_subset(.,'.csv$')
  
  #make sure there is only 1  
  if (length(trial_fnames)!=1){
    stop(str_glue("Error!! subject has ", length(ans_fnames),"trial files"))
  } 
  
  ##getting TRACKEROUTPUT file name
  trkr_fnames<-list.files(path_to_sub_folder,recursive = TRUE,full.names = TRUE)%>%
    str_subset(.,regex('zip$',ignore_case=TRUE),negate=TRUE)%>%
    str_subset(.,regex('TrackersOutputData',ignore_case=TRUE))%>%
    str_subset(.,'.csv$')
  
  #make sure there is only 1  
  if (length(trkr_fnames)!=1){
    stop(str_glue("Error!! subject has ", length(trkr_fnames),"TrackerOutput files"))
  }  
  
  ## getting subject's name
  
  sub_name<-str_extract(ans_fnames,str_glue(eval(sub_folder_suffix),"[[:digit:]]+"))[1]
  
  ## saving the file names in a tibble
  
  
  fnames_df<-tibble( sub_name=sub_name,
                     ans_fname=ans_fnames,
                     trial_fname=trial_fnames,
                     trkr_fname=trkr_fnames
  )
  
  return(fnames_df)
}


###### Function for choosing multiple subjects by vector  #######
# INPUT: 
get_fnames_by_vec<-function(path_to_all_sub_folder,sub_folder_suffix,sub_num_vec, subfolder_name="\\w"){
  #creating collector variable for all subjects
  fnames_df<-tibble(sub_name=as.character(),
                    ans_fname=as.character(),
                    trial_fname=as.character(),
                    trkr_fname=as.character())
  
  for (sub_idx in 1:length(sub_num_vec)) {
    
    #getting the current subject's name
    cur_sub_name<-str_glue(sub_folder_suffix,sub_num_vec[sub_idx],"/") ##on windows change "/" --> "\"
    
    #getting ANSWER file
    ans_fnames<-list.files(path_to_all_sub_folder,recursive = TRUE,full.names = TRUE)%>%
      str_subset(.,regex(cur_sub_name,ignore_case=FALSE))%>% ##taking only the current subject
      str_subset(.,regex(subfolder_name,ignore_case=TRUE))%>% # taking only the subfolder 
      str_subset(.,regex('zip$',ignore_case=TRUE),negate=TRUE)%>%
      str_subset(.,regex('answers',ignore_case=TRUE))%>%
      str_subset(.,'.csv$')
    
    ##make sure there is only 1  
    if (length(ans_fnames)!=1){
      stop(str_glue("Error!! subject has ", length(ans_fnames),"answer files"))
    } 
    
    #getting TRIAL file
    trial_fnames<-list.files(path_to_all_sub_folder,recursive = TRUE,full.names = TRUE)%>%
      str_subset(.,regex(cur_sub_name,ignore_case=FALSE))%>% ##taking only the current subject
      str_subset(.,regex(subfolder_name,ignore_case=TRUE))%>% # taking only the subfolder 
      str_subset(.,regex('zip$',ignore_case=TRUE),negate=TRUE)%>%
      str_subset(.,regex('trials',ignore_case=TRUE))%>%
      str_subset(.,'.csv$')
    
    ##make sure there is only 1  
    if (length(trial_fnames)!=1){
      stop(str_glue("Error!! subject has ", length(ans_fnames),"trial files"))
    } 
    
    #getting TRACKER FILES file
    trkr_fnames<-list.files(path_to_all_sub_folder,recursive = TRUE,full.names = TRUE)%>%
      str_subset(.,regex(cur_sub_name,ignore_case=FALSE))%>% ##taking only the current subject
      str_subset(.,regex(subfolder_name,ignore_case=TRUE))%>% # taking only the subfolder 
      str_subset(.,regex('zip$',ignore_case=TRUE),negate=TRUE)%>%
      str_subset(.,regex('TrackersOutputData',ignore_case=TRUE))%>%
      str_subset(.,'.csv$')
    
    ##make sure there is only 1  
    if (length(trkr_fnames)!=1){
      stop(str_glue("Error!! subject has ", length(trkr_fnames),"tracker files"))
    } 
    
    
    #adding to collector variable
    #creating collector variable for specific subject
    cur_sub_fnames_df<-tibble(sub_name=cur_sub_name,
                              ans_fname=ans_fnames,
                              trial_fname=trial_fnames,
                              trkr_fname=trkr_fnames
    )
    
    #concatenating to the df for all subjects
    fnames_df<-bind_rows(fnames_df,cur_sub_fnames_df)
  }
  return(fnames_df)
}

####### FUNCTIONS FOR PP'ing files  ##########

#### function to add message to trkr

add_msg<-function(trkr_df){
  
  
}

## function that fixs problem with scaling in group_by in dplyr
scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}


