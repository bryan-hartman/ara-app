
# Resample data function
create_resamples <- function(val, cost, n, seed){
  set.seed(seed)
  values <- val %>% group_by(Alternative) %>% sample_n(n, replace = T) %>% arrange(Alternative)
  costs <- cost %>% group_by(Alternative) %>% sample_n(n, replace = T) %>% arrange(Alternative)
  data <- data.frame(values, costs[,2])
  colnames(data)[3] <- "Cost"
  return(data)
}

# Build the pareto front alternatives (returns the front from top to bottom)
pareto_front <- function(dat){
  # Get 2D expected value
  dat <- dat %>% group_by(Alternative) %>% summarise(mean(Value), mean(Cost))
  
  # Find the pareto front
  D = dat[order(dat$`mean(Value)`,dat$`mean(Cost)`,decreasing=TRUE),]
  front = D[which(!duplicated(cummin(D$`mean(Cost)`))),]
  return(front$Alternative)
}


# Below are the functions for each plot used in the Realization Analysis

# Function for Value Histogram
valhist <- function(dat, alpha) {
  if (is.null(dat)) {
    return(NULL)
  }
  ggplot(dat, aes(x=Value, fill=Alternative)) +
    theme_classic() +
    geom_histogram(alpha = alpha, position = "identity", binwidth = .25, col = "black") +
    scale_color_manual(values = c("blue", "brown", "green", "orange","purple","red"))+
    scale_fill_manual(values = c("blue", "brown", "green", "orange","purple","red")) + 
    labs(title = "Value Histogram") + 
    theme(plot.title = element_text(hjust = 0.5))
}

# Function for Value CDF plot
valcdf <- function(dat){
  if (is.null(dat)) {
    return(NULL)
  }
  ggplot(dat, aes(Value, color = Alternative)) + 
    stat_ecdf(geom = "step") +
    theme_minimal() + 
    ggtitle("Value CDF") +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(y = "Probability") +
    scale_color_manual(values = c("blue", "brown", "green", "orange","purple","red"))
}

# Function for Cost CDF plot
costcdf <- function(dat) {
if (is.null(dat)) {
  return(NULL)
}

ggplot(dat, aes(Cost, color = Alternative)) + 
  stat_ecdf(geom = "step") +
  theme_minimal() + 
  ggtitle("Cost CDF") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = "Probability")+
  scale_color_manual(values = c("blue", "brown", "green", "orange","purple","red"))
}

# Function for Cloud plot
cloudplot <- function(dat){
if (is.null(dat)) {
  return(NULL)
}
  
means <- dat %>% group_by(Alternative) %>% summarise(mean(Value), mean(Cost))
colnames(means) <- c("Alternative", "Value", "Cost")


ggplot(dat, aes(Cost, Value, color = Alternative, fill = Alternative)) + 
  stat_density_2d(geom = "polygon", aes(alpha = ..level.., color = Alternative), contour = TRUE) + 
  geom_point(alpha = .3) +
  geom_point(data=means, color = "black", size = 4.5) +
  geom_point(data=means,  mapping=aes(x = Cost, y = Value), size=3.5)+
  theme_minimal() + 
  guides(color = FALSE, alpha = FALSE, fill = guide_legend(override.aes = list(shape = NA)))+
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1.15))+ 
  scale_color_manual(values = c("blue", "brown", "green", "orange","purple","red")) +
  scale_fill_manual(values = c("blue", "brown", "green", "orange","purple","red"))
}

# Function for Table generation
gen_pareto_table <- function(dat, param1, param2){
  if (is.null(dat)) {
    return(NULL)
  }
  if (param1 == param2){
    return("Please select two different alternatives")
  }

  parameter1 <- subset(dat, Alternative == param1)
  parameter2 <- subset(dat, Alternative == param2)
  level1(parameter1, parameter2) %>%
    as_huxtable(add_columnnames = TRUE, add_rownames = "Outcome") %>%
    set_font_size(14) %>% 
    set_bold(1, everywhere, TRUE) %>%
    set_all_borders(1)
}


############################
#Level 1 Analysis
############################

level1 = function(a, b){
  #X and y are separate alternatives in tibble/dataframe format with cost and value columns
  #nSample has a default of 1000, but can be adjusted as desired
  #function scales well to 1,000,000 samples and still runs (slowly) at 10,000,000
  
  #create pairings
  pairings = full_join(a,b, by = character())
  colnames(pairings) = c("alt.a","a.value","a.cost","alt.b","b.value","b.cost")
  
  #compute a dominance
  pairings = mutate(pairings, a.dominate = if_else(a.value > b.value & a.cost <= b.cost, 1, 0))
  #compute b dominance
  pairings = mutate(pairings, b.dominate = if_else(a.value < b.value & a.cost >= b.cost, 1, 0))
  #compute pareto optimal + 
  pairings = mutate(pairings, pareto_optimal_plus = if_else(a.value >= b.value & a.cost >= b.cost, 1, 0))
  #compute pareto optimal - 
  pairings = mutate(pairings, pareto_optimal_minus = if_else(a.value <= b.value & a.cost <= b.cost, 1, 0))
  #compute a greater than b value
  pairings = mutate(pairings, a.GreaterThan.b = if_else(a.value > b.value, 1, 0))
  #start building final table
  `Final Count` = pairings %>% select(a.dominate, b.dominate, pareto_optimal_plus, pareto_optimal_minus)
  #sum observations
  `Final Count` = `Final Count` %>% colSums()
  final_table = as.data.frame(`Final Count`)
  final_table = final_table %>% mutate(Percent = round(100*(`Final Count`/sum(`Final Count`)),2))
  #change col names to match the alternatives being examined
  row.names(final_table) = c(paste0(str_to_title(a$Alternative[1])," dominates ", str_to_title(b$Alternative[1])), 
                             paste0(str_to_title(b$Alternative[1])," dominates ", str_to_title(a$Alternative[1])), 
                             paste0(str_to_title(a$Alternative[1]), " is pareto optimal (+)"),
                             paste0(str_to_title(a$Alternative[1]), " is pareto optimal (-)"))
  return(final_table)
}

##########################################
#########ADS Score
##########################################


ads_score = function(a, b){
  temp = level1(a,b)
  score = (temp$`Final Count`[1]-temp$`Final Count`[2])/sum(temp$`Final Count`)
  score = set_names(score,paste0(a$Alternative[1], " compared to ", b$Alternative[1]))
  return(as.data.frame(score))
}

##########################################
#########ADS Matrix
##########################################

ads_matrix = function(list_of_alt){
  
  #build square matrix of zeros the size of the deminsions of your list
  ads_mat = matrix(0, nrow = length(list_of_alt), ncol = length(list_of_alt))
  
  #create dummy row/col headers so that we can dynamically fill it later
  dimnames(ads_mat) <- list(rownames(ads_mat, do.NULL = FALSE, prefix = "row"),
                            colnames(ads_mat, do.NULL = FALSE, prefix = "col"))
  
  #fill matrix by iterating through list, computing ads_score of each Alternative against all others
  for (i in 1:length(list_of_alt)) {
    row.names(ads_mat)[i] = c(list_of_alt[[i]]$Alternative[1])
    
    for (j in 1:length(list_of_alt)) {
      if(i == j){
        ads_mat[i,j] = 0
      } else {
        ads_mat[i,j] = as.numeric(ads_score(list_of_alt[[i]],list_of_alt[[j]]))
        colnames(ads_mat)[j] = c(list_of_alt[[j]]$Alternative[1])
      }
    }
  }
  
  #create new row and add rowsums to it
  ads_matrix_final = as_data_frame(ads_mat) %>% mutate(`ADS Score` = rowSums(.)/(length(list_of_alt)-1))
  #add back in row names that dropped when converting to dataframe
  rownames(ads_matrix_final) = rownames(ads_mat)
  
  return(ads_matrix_final)
  
}

# Create ADS table
ads_table <- function(dat) {
  if (is.null(dat)) {
    return(NULL)
  }

  dat <- group_by(dat, Alternative)
  dat_list <- group_split(dat)
  build_matrix <- ads_matrix(dat_list)
  build_matrix$`Alternative` <- names(build_matrix[1:nrow(build_matrix)])
  build_matrix <- build_matrix[order(-build_matrix$`ADS Score`), , drop = FALSE] %>% 
    select(`Alternative`, everything())
  return(build_matrix)
}

#################################
#####Level 2######VERSION 4
#################################

level2 = function(a, b, tolerance = .05){
  #a and b are separate alternatives in tibble/dataframe format with cost and value columns
  #b is the chosen alternative and a is the competitor with LOWER value
  #tolerance is the percentage less expected value the DM would be ok with at the expected cost
  
  b.value.mean = mean(b$Value)
  b.cost.mean = mean(b$Cost)
  a.value.mean = mean(a$Value)
  a.cost.mean = mean(a$Cost)
  
  # alternative b should have greater value and greater cost
  if (a.value.mean > b.value.mean || a.cost.mean > b.cost.mean){
      return(FALSE)
  }
  
  
  #trade1 is the value/cost trade-off that you are considering
  trade1 = (b.value.mean - a.value.mean)/(b.cost.mean - a.cost.mean)
  
  #trade2 is the point at which you would no longer make the value/cost trade off and go with the cheaper alternative
  #This is the minimum acceptable trade ... default at 5% less than expected mean
  trade2 = ((b.value.mean) - a.value.mean)/(b.cost.mean*(1+tolerance) - a.cost.mean)

  #bind alternatives for computation
  pairings = tibble(a.value = a.value.mean, a.cost = a.cost.mean, b.value = b$Value, b.cost = b$Cost)
  
  #compute Zone 6 - Trade worse than expected - Pareto Optimal but worse than A
  pairings = mutate(pairings, zone6 = if_else((pairings$b.value < a.value.mean & pairings$b.cost > a.cost.mean), 1, 0)) 
  #compute Zone 5 - Trade much worse than expected - B dominated by A
  pairings = mutate(pairings, zone5 = if_else(pairings$b.value < a.value.mean & pairings$b.cost < a.cost.mean, 1, 0)) 
  #compute Zone 4 - Trade worse than expected - Unacceptable
  pairings = mutate(pairings, zone4 = if_else((pairings$b.value-a.value.mean)/(pairings$b.cost-a.cost.mean) < trade2 & 
                                                pairings$b.value > a.value.mean & pairings$b.cost > a.cost.mean &
                                                zone6 < 1 & zone5 < 1, 1, 0))
  #compute Zone 3 - Trade worse than expected - Acceptable
  pairings = mutate(pairings, zone3 = if_else(trade2 < (pairings$b.value-a.value.mean)/(pairings$b.cost-a.cost.mean) & 
                                                (pairings$b.value-a.value.mean)/(pairings$b.cost-a.cost.mean) < trade1 &
                                                zone6 < 1 & zone5 < 1, 1, 0))
  #compute Zone 2 - Trade better than expected
  pairings = mutate(pairings, zone2 = if_else((pairings$b.value-a.value.mean)/(pairings$b.cost-a.cost.mean) > trade1 & 
                                                pairings$b.cost > a.cost.mean, 1, 0))
  #compute Zone 1 - Trade better than expected - B dominates A
  pairings = mutate(pairings, zone1 = if_else(pairings$b.value > a.value.mean & pairings$b.cost < a.cost.mean, 1, 0))
  
  
  
  #Compute zones in place in single column named zoneTest for histogram building
  pairings = mutate(pairings, zoneTest = if_else(zone1 == 1, 1, 
                                                 if_else(zone2 == 1, 2, 
                                                         if_else(zone3 == 1, 3, 
                                                                 if_else(zone4 ==1, 4,
                                                                         if_else(zone5 == 1, 5, 6)#end zones 5 and 6
                                                                 )#end zone 4
                                                         )#end zone 3  
                                                 )#end zone 2               
  )#end zone 1  
  )#close zoneTest mutate
  

  return(pairings)
}

####Level 2 Plot#########
# Function for Value CDF plot
gen_level2_plot <- function(dat, param1, param2, tolerance){
  if (is.null(dat)) {
    return(NULL)
  }
  # if (param1 == param2){
  #   return(text(x = 0.5, y = 0.5, paste("Please select two different alternatives"), 
  #               cex = 1.6, col = "black"))
  # }
  
  parameter1 <- subset(dat, Alternative == param1)
  parameter2 <- subset(dat, Alternative == param2)
  hist_plot <- level2(parameter2, parameter1, tolerance)
  
  # if (hist_plot == FALSE){ 
  #   return(text(x = 0.5, y = 0.5, paste("Please select appropriate alternatives (trade zone alternative should be 
  #                                       more expensive and more valuable than the expected value of the compared alternative):"), 
  #                                      cex = 1.6, col = "black"))
  # }

  hist_rollup <- hist_plot%>%
    select(zoneTest) %>% group_by(zoneTest) %>% 
    summarise(count=n()) %>%
    mutate(percent = 100*(count / sum(count)))

  
  hist_rollup$zoneTest <- factor(hist_rollup$zoneTest, levels = c("1", "2", "3", "4", "5","6"))
  
  ggplot(hist_rollup, aes(x = zoneTest, y =  percent,fill = zoneTest)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_minimal()+
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1.15),
    )+
    scale_x_discrete(drop=FALSE)+
    scale_y_continuous(limits = c(0,max(hist_rollup$percent)+20), expand = c(0, 0))+
    scale_color_colorblind()+
    labs(title = paste0("Trade Zones for alternative ", param1, " with respect to EV[", param2,"]")
         , fill = "Zone", y = "% of Total Trades", x = "Zone")+
    geom_text(aes(label=percent), position=position_dodge(width=0.9), vjust=-0.25)+
    scale_fill_manual(values = c("green4","green2","yellow2","orange","red2","red4"),
                      drop = FALSE,
                      breaks=c("1", "2", "3", "4", "5","6"),
                      name = "",
                      labels=c("Zone 1", 
                               "Zone 2",
                               "Zone 3",
                               "Zone 4",
                               "Zone 5",
                               "Zone 6"))
}

# return the delta value associated with the delta parameter
delta_value = function(dat, param1, tolerance = .05){
  b <- subset(dat, Alternative == param1)

  delta = mean(b$Cost)*(tolerance)
  return(delta)
  }
