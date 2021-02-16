#Defining functions for mapping growthcurve fitting to each curve




#Fit the growth curve models using GrowthCurveR for each treatment combination

fit_growth <- function(df, treatments = c(), time_col, od_col = "od", pred_period_length = 0.5, bg_cor = "min"){ 

  
  gc_model <- function(df){
    SummarizeGrowth(df %>% pull(time_col), df %>% pull(od_col),bg_correct = bg_cor)
  }
  
  #Format growth curve data for tidy functions
  get_vals <- function(list){
    as.data.frame(`class<-`(list$vals,"list"))
  }
  
  #Get the growth model
  get_model <- function(list){
    list$model
  }
  
  #Get the model data
  get_model_data <- function(list){
    as.data.frame(list$data)
  }
  
  #Add times_to_predict to predict for to each row in nested data frame
  make_predictions <- function(x){
    times_to_predict
  }
  
  #Make add_predictions generate a data frame instead of the list it was for some reason
  mod_add_predictions <- function(data,model){
    as.data.frame(add_predictions(data,model))
  }
  
    
  #make nested data frame with row for each curve to be fit
  nested_data <- df %>% 
    group_by(across(treatments)) %>% #Columns defining treatment groups to compare
    nest()
  
  #Generate a list of times to make predictions for
  times_to_predict <- data.frame(t = seq(0,max(df %>% pull(time_col)), as.numeric(pred_period_length)))
  
  #Apply the model, store results in the same nested data frame
  nested_data <- nested_data %>% 
    mutate(gc_results = purrr::map(data, gc_model), 
           model = map(gc_results, get_model), 
           vals = map(gc_results, get_vals), 
           model_data = map(gc_results, get_model_data), 
           for_predictions = map(gc_results, make_predictions))
  
  #Generate model predicted values
  nested_data <- nested_data %>% 
    filter(model != "") %>% #Filter out models that weren't able to be fit
    mutate(predict = map2(for_predictions, model, mod_add_predictions))
  
  out <- list()
  
  #Unnest the growth rate data
  out$model_data <- unnest(nested_data, vals, .drop = TRUE)
  
  #Unnest predictions for plotting
  out$predicted_vals <- unnest(nested_data, predict) %>% select(treatments, t, pred) #%>% left_join(model_data_unnested)
  
  return(out)
}
