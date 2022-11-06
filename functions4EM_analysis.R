## MOther function for processing tracker output
#INPUT
#trkr_output: file of tracker data
# tbt_df: processed files of trial numbers & answers
# trkr_trial_val: index of trials to include in analysis
#exp_type "BF_prediction"/ "SoA" 

process_trkr_output<-function(trkr_output,tbt_df,trkr_trial_val, exp_type){
  library(hms)
  #extract only the Room section  also adds additional fields like relative time
  trkr_df<-filter_trial_room(trkr_output,trkr_trial_val,exp_type)
  
  #adding row wal
  trkr_df<-trkr_df%>%
    rowwise()%>%
    dplyr::mutate(wall_row=get_wall_row(LastObject)[1],wall_col=get_wall_row(LastObject)[2])
  
  # adding z score of wall col (this is across entire frames of room)
  trkr_df<-trkr_df%>%
    ungroup()%>%
    dplyr::mutate(z_wall_col=scale(wall_col), z_wall_row=scale(wall_row))
  trkr_df$z_wall_col<-unlist(trkr_df$z_wall_col) 
  
  #joining w/answers & trial info
  if (exp_type=="BF_prediction"){ #line for BF 
    trkr_df<-left_join(trkr_df,tbt_df, by=c("TrialNumber","BlockNumber"))
  }else if (exp_type =="SoA"){   #line for SoA
    trkr_df<-left_join(trkr_df,tbt_df, by="TrialNumber")
  }
  
  #adding z score of gaze (positive values = congruent with cue). Only relevant for BF 
  if (exp_type=="BF_prediction"){ #line for BF 
    
    #gaze congruent with cue
    trkr_df<-trkr_df%>%
      ungroup()%>%
      rowwise()%>%
      dplyr::mutate(cue_cong_z_gaze=get_cue_cong_zgaze_wcol(z_wall_col,quest_cue))
  #gaze congruent w/sub's prediction (i.e. response)
    trkr_df<-trkr_df%>%
      rowwise()%>%
      dplyr::mutate(pred_cong_z_gaze=get_pred_cong_zgaze_wcol(z_wall_col,QuestionResult))  
  } else if (exp_type =="SoA"){   #line for SoA
  }
  
  trkr_df$cue_cong_z_gaze<-unlist(trkr_df$cue_cong_z_gaze) 
  trkr_df$pred_cong_z_gaze<-unlist(trkr_df$pred_cong_z_gaze) 
  
  ##ensuring that colums are in approiate format
  if (is.character(trkr_df$TimePadClick)){
    trkr_df$TimePadClick<-as.double(trkr_df$TimePadClick)
  }
  
  return(trkr_df)
  
}

####filter_trial_room extracts the trial in the room & adds  QA flags####
filter_trial_room<-function(trkr_df,trial_val,max_frame_duration=20){
  n_trkr_df<-trkr_df%>%
    filter(TrialNumber%in%trial_val)%>%
    filter(SceneName=="Room")%>% #only timestapms in which subject is in room (not question etc. )
    filter(SecondarySceneName=="NoBlockView") # these are trials in which Ss sees the room and not the block
  
  
  #### QA: calculating difference in frame numbers #####
  n_trkr_df<-n_trkr_df%>%
    dplyr::group_by(TrialNumber)%>%
    dplyr::mutate(diff_frame_num=lead(FrameNumber)-FrameNumber)
  #replacing NAs (end of trial's frame with 1)
  n_trkr_df$diff_frame_num[is.na(n_trkr_df$diff_frame_num)]=1
  
  
  ##### Dealing with time & Timestamps ####

  #convert timestamp into hms format with ms resolution
  n_trkr_df<-n_trkr_df%>%
    rowwise()%>%
    dplyr::mutate(TimeStamp_ms=convert_ts_to_hms(TimeStamp))
  
  if(is.character(n_trkr_df$TimeStamp)){
    n_trkr_df$TimeStamp<-as.double( n_trkr_df$TimeStamp)
  }
  n_trkr_df$TimeStamp<- as_hms(n_trkr_df$TimeStamp) 
  
  n_trkr_df2<-n_trkr_df%>%
    dplyr::group_by(TrialNumber)%>%
    dplyr::mutate(dur_msec=as.numeric(lead(TimeStamp_ms)-TimeStamp_ms))
  
  #converting back to numeric for conveince
  n_trkr_df$dur_msec<-as.numeric(n_trkr_df$dur_msec)*1000
  #padding w/ 11 ms TS at end
  n_trkr_df$dur_msec[is.na(n_trkr_df$dur_msec)]<-11
  #### ADDing relative time column
  n_trkr_df<-n_trkr_df%>%
    dplyr::group_by(TrialNumber)%>%
    dplyr::mutate(rel_time=cumsum(dur_msec)-dur_msec[1])
  # rel_time<-cumsum(dur_msec)-dur_msec[1]
  
  return(n_trkr_df)
}


# function that converts "TrackersOutput.csv" Timestamp column into an hms object with ms resolution
convert_ts_to_hms<-function(tms){
  #ensure that R doesn't put the time as scientfic notation
  tms<-format(tms,scientific = FALSE)
  
  if(!is.character(tms)){ #convert tms to charcter
    tms<-as.character( tms)
  }
  if (str_length(tms)!=9){ #checking that has proper length
    print(str_glue("Timestamp has", str_length(tms)," digits!"))
  }
  
  #format so that it is HH:MM:SS.ms
  new_tms<-paste0(str_sub(tms,1,2),":",str_sub(tms,3,4),":", str_sub(tms,5,6),".",str_sub(tms,7,9)) 
  
  #convert to hms object
  new_tms<-as_hms(new_tms)
  
  return(new_tms)
}


#function to get absolute duration per object
get_abs_item_duration<-function(trk_trial_df){
  dur_objct6<-trk_trial_df%>%
    dplyr::group_by(LastObject)%>%
    summarize(sum_dur=sum(dur_msec))
}

#### function that extracts wall location #####
get_wall_row<-function(LastObject){
  if(str_detect(LastObject,"MainWall")){
    regexp <- "[[:digit:]]+"
    wall_loc<-str_extract_all(LastObject, regexp)
    wall_loc<- as.numeric(unlist(wall_loc))
    return(wall_loc)
  }else{
    wall_loc=c(NA,NA)
    return(wall_loc)
  }
  
}

#function that standardizes the view so that positive values are towards the congruent direction of THE CUE negative values incongruent
get_cue_cong_zgaze_wcol<-function(z_wall_col,quest_cue){
  if(is.na(z_wall_col)){
    z_congaze_wcol<-NA
    return( z_congaze_wcol)
    
  }else{
    if (quest_cue=="right"){
      z_congaze_wcol<- z_wall_col
      
    } else if (quest_cue=="left"){
      z_congaze_wcol<- z_wall_col*-1
      
    }
  }
  return (z_congaze_wcol)
}


#function that standardizes the view so that positive values are towards the congruent direction of THEsubject's response (prediction) negative values incongruent
get_pred_cong_zgaze_wcol<-function(z_wall_col,QuestionResult){
  if(is.na(z_wall_col)){
    z_congaze_wcol<-NA
    return( z_congaze_wcol)
    
  }else{
    if (QuestionResult=="right"){
      z_congaze_wcol<- z_wall_col
      
    } else if (QuestionResult=="left"){
      z_congaze_wcol<- z_wall_col*-1
      
    }
  }
  return (z_congaze_wcol)
}

## function for extracting data by timewindow
extract_by_time_window<-function(trkr_df,time_window_val){
  tw_list<-vector("list",length=length(time_window_val))
  for (i in seq_along(time_window_val)) {
    tw_list[[i]]<-trkr_df%>%
      dplyr::ungroup()%>%
      dplyr::filter(rel_time<time_window_val[i])%>%
      mutate(time_window=time_window_val[i])
  }
  tw_df<-dplyr::bind_rows(tw_list)
  return(tw_df)
}

##### MOTHER function that makes a slimmer version (only relevant columns and only first 300 msec)
## of trkr output

make_slim_trkr_df<-function(trkr_df,max_rel_time=300){
  
  #selecting relevant columns
  #making slimmer version of traker df
  slim_trkr_df<-trkr_df%>%
    #selecting relevant columns
    dplyr::select(TrialNumber,BlockNumber,FrameNumber,TimeStamp, 
                  RealHandLocalPositionX:Z...30,
                  dur_msec:z_wall_row, rel_trial_number:tail(names(trkr_df),1))%>%
  
    #keeping only the first time period
    dplyr::filter(rel_time<max_rel_time)%>%
    dplyr::ungroup()%>%
    dplyr::mutate(time_window=max_rel_time)%>% # adding time_window to ensure compability with older scripts
   
    #moving columns around so that is more conveint
     dplyr::relocate(sub_name)%>%
    dplyr::relocate(rel_trial_number,.after=BlockNumber)%>%
    dplyr::relocate(c(cue_cong_z_gaze,pred_cong_z_gaze),.after=z_wall_col)

  return(slim_trkr_df)  
  
}


## function for creating df to compare congruent and incongruent trials- matching the number of trials
extract_cmpr_cong_incong_df<-function(tw_df){
  #getting incongruent trials in df
  incong_df<-tw_df%>%
    filter(prev_cong=="incong")
  #number of trials in incongruent 
  n_trials<-length(unique(incong_df$TrialNumber))
  
  #getting  Trial index of randomly sampled same number of congruent trials
  cong_trial_index<-tw_df%>%
    filter(prev_cong=="cong")%>%
    summarize(trial_ind=unique(TrialNumber))%>%
    sample_n(.,n_trials)
  
  # extracting congruent 
  cong_df<-tw_df%>%
    filter(TrialNumber %in% cong_trial_index$trial_ind)
  
  con_incon_df<-bind_rows(incong_df,cong_df)
  
  return(con_incon_df)
  
}


### Function that extarct distance from 3D cordinates#####
# input is vector of cordinate values over time
get_3d_dist<-function(x,y,z){
  
  prev_x<-lag(x)
  prev_x[1]<-prev_x[2]
  
  prev_y<-lag(y)
  prev_y[1]<-prev_y[2]
  
  prev_z<-lag(z)
  prev_z[1]<-prev_z[2]
  
  cord_df<-data.frame(x=x,y=y,z=z,prev_x=prev_x,prev_y=prev_y,prev_z=prev_z)%>%
    
    mutate(dist=sqrt((x-prev_x)^2+(y-prev_y)^2+(z-prev_z)^2))
  return(as.array(cord_df$dist))
  
}



##### Function for Cohen's D of one samople ttest
one_sample_ttest_cohen_d<-function(value_vec){
  as.vector(value_vec)
  Mean = mean(value_vec,na.rm=TRUE)
  Mu   = 0
  Sd   = sd(value_vec,na.rm=TRUE)
  Diff = Mean - Mu
  CohenD = (Mean - Mu) / Sd
  
  return(CohenD)
}
### Plotting functions#####
## Function to plot 2D distribution
#Input: tw_wc_porp_df: has wall columns per tw- for SINGLE SUBJECT
# mean_wc_ptw_df: is summary statistics of tw_wc_porp_df for SINGLE SUBJECT ## (maybe make this inside function)
plot_2D_heatmap<-function(tw_wc_porp_df,mean_wc_ptw_df){
  
  # Plotting distribution of porportion time per wall column
  plot_count_dur<-ggplot(data=tw_wc_porp_df, aes(x=as.integer(wall_col),y=porp_count, color=quest_cue))+
    geom_line(aes(linetype=as.factor(time_window)))+
    geom_point()+
    scale_x_continuous(name = "wall column")+
    scale_y_continuous(name="% count")+
    theme_minimal()+
    theme(panel.grid.minor = element_blank())
  
  #ensure that a figure is created (caused by subjects that don't have LastObject)
  if(dim(mean_wc_ptw_df)[1]!=0){
  #getting the limits
  x_axis_lim<-ggplot_build(plot_count_dur)$layout$panel_scales_x[[1]]$range$range
  
  
  
  ## Plotting - mean time
  plot_horz_mean<-ggplot(data=mean_wc_ptw_df,aes(x=mean_wall_col,y=factor(time_window),color=quest_cue))+
    geom_pointrange(aes(xmin=lo_ci,xmax=hi_ci),position=position_dodge(width = .1))+
    # coord_fixed(ratio=.1)+
    ylab("time window")+
    xlab("")+
    xlim(x_axis_lim)
  
  ##combining plots
  plot_comb<-plot_grid(plot_horz_mean,plot_count_dur,ncol=1,rel_heights = c(1,3),align="v") 
  
  
  }else{
    plot_comb<-plot.new()
  }
  return(plot_comb)
}
