## functions for ppi'ng SOAL data
#written by YS 23/10/22
#strongly based on "functions_for_pp_SOAL_wbf2.R", but slightly modified


#function that merges trial & answer files
merge_trial_ans_SoA<-function(trial_df,ans_df,sub_name){
  
  #merging answers & trials
  trial_ans_df<- full_join(ans_df,trial_df,by=c("TrialNumber"="#trial number"))%>%
    select(TrialNumber:QuestionResult,ResponseTime:QuestionID,"SensoMotoric Delay")%>%
    mutate(sub_name=sub_name)%>%
    filter(TrialNumber>0)%>%
    mutate(condition="neutral")
  
  
  #pivoting to wide
  trial_ans_df_wide<-pivot_wider(trial_ans_df,names_from=QuestionID,values_from=c(QuestionResult,ResponseTime))
  
  trial_ans_df<-trial_ans_df_wide%>%
    dplyr::rename(IsCong=QuestionResult_88,
                  Conf=QuestionResult_666,
                  RT_IsCong=ResponseTime_88,
                  RT_Conf=ResponseTime_666,
                  delay="SensoMotoric Delay")
  
  # getting accuracy 
  trial_ans_df<-trial_ans_df%>%
    rowwise()%>%
    mutate(is_acc=get_acc(delay,IsCong))
 
   # getting SDT
  trial_ans_df<-trial_ans_df%>%
    rowwise()%>%
    mutate(SDT_cat=find_SDT(delay,is_acc))
  
  
  return(trial_ans_df)
}


# get_acc returns if response is accurate
get_acc<- function(mag,IsCong ){
  if (c(mag==0 && IsCong==1)||c(mag!=0 && IsCong==0)){
    acc=1
  }else if(c(mag==0 && IsCong==0)||c(mag!=0 && IsCong==1)){
    acc=0
  }
  return(acc)
}

find_SDT<- function(mag,acc){
  if (is.na(mag)|| is.na(acc)){
    val<-"NaN"
  } else if (mag==0 && acc==1){
    val<-"hit"
  } else if (mag==0 && acc==0) {
    val<-"miss"
  } else if (mag!=0 && acc==1){
    val<-"cr"
  } else if (mag!=0 && acc==0){
    val<-"fa"
  }
}
