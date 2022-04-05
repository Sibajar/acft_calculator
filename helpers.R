# Helper functions
calc_points<-function(event_val, val, age_val, gender_val, df) {
  
  # Lower is better for these
  if(event_val=='Run' |event_val=='Sprint-Drag-Carry'){
    # Filter out by criteria
    filtered<-df %>% filter(event==event_val & max_age>=age_val & min_age<=age_val & gender==gender_val)
    
    # If value is higher than max then just return 0
    if (val>max(filtered$value)){
      return (0)
    }
    # Otherwise get score using this
    else{
      filtered<-filtered %>% filter(value>=val)
      return (max(filtered$points))
    }
  }
  
  # Higher is better for these
  else{
    
    # Filter out by criteria
    filtered<-df %>% filter(event==event_val & max_age>=age_val & min_age<=age_val & gender==gender_val)
    
    # If value is higher than max then just return 0
    if (val<min(filtered$value)){
      return (0)
    }
    # Otherwise get score using this
    else{
      filtered<-filtered %>% filter(value<=val)
      return (max(filtered$points))
    }
  }
}

get_time<- function(hours, mins, seconds){
  hours<-as.numeric(hours)
  mins<-as.numeric(mins)
  seconds<-as.numeric(seconds)
  return (hours*3600+mins*60+seconds)
}

get_score_list<-function(input, df){
  event<-input[1]
  val<-input[2]
  age<-input[3]
  gender<-input[4]
  return(calc_points(event, val, age, gender, df))
}

get_total_score<-function(input_values, df){
  scores<-lapply(input_values, get_score_list, df=df)
  #score<-sum(as.numeric(scores))
  return(scores)
}

textInputRow<-function (inputId, label, value = "") 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "numeric", value = value,class="input-small"))
}

get_df<-function(input_values, scores){
  col_names<-c("Event", "Value", "Score")
  data<-c()
  events<-c()
  values<-c()
  scores2<-c()
  for (index in 1:length(input_values)){
    event<-input_values[[index]][[1]]
    val<-as.numeric(input_values[[index]][[2]])
    score<-as.numeric(scores[[index]])
    events<-append(events,event)
    values<-append(values,val)
    scores2<-append(scores2,score)
  }
  df<-data.frame("Event"=events,"Value"=as.numeric(values),"Score"=as.numeric(scores2))
  return (df)
}