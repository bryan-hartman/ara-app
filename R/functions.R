###############################################################
# Overall Functions (not specific to Analysis Level (0, 1, 2))#
###############################################################

# Resample data function
# This function takes in the initial raw data files, resamples the value and cost
# measures independently based on the number of resamples selected by the user 
# (1000 default) and outputs the dataframe used through the rest of the app.
#
# Inputs: The initial raw value and cost data, the number of resamples (n), 
# and the seed (default is 123).
# Output: The three column (Alternative, Value, Cost) dataframe 
create_resamples <- function(val, cost, n, seed){
  set.seed(seed)
  values <- val %>% group_by(Alternative) %>% sample_n(n, replace = T) %>% arrange(Alternative)
  costs <- cost %>% group_by(Alternative) %>% sample_n(n, replace = T) %>% arrange(Alternative)
  data <- data.frame(values, costs[,2])
  colnames(data)[3] <- "Cost"
  return(data)
}

# Image Download Function
# The application takes in the file name desired and the function used to generate
# plot images used within the UI.
image_save <- function(name, func) {
  downloadHandler(
    filename = name, 
    content = function(file) {
      save_plot <- func
      ggsave(file, save_plot, width = 10, height = 8)
    }
  )
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

#####################################################
# Level 0 Analysis###################################
#####################################################
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

# no data = no plot
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
# Inputs include the raw data and type of cloud chart selection (default or legend)
# Output is the gg scatter plot for the alternatives.

# no data = no plot
cloudplot <- function(dat, type){
if (is.null(dat)) {
  return(NULL)
}

# The two lines identify the means for each Alternative, which are included within
# the plots to better identify centrality of data.
means <- dat %>% group_by(Alternative) %>% summarise(mean(Value), mean(Cost))
colnames(means) <- c("Alternative", "Value", "Cost")

# Type 0 is the default plot using a separate legend. 
# Type 1 is the optional plot with embedded alternative labels within scatterplot
# The following code builds both chart types based on the toggle selection in the UI.
if (type == 1){
ggplot(dat, aes(Cost, Value, color = Alternative, fill = Alternative)) + 
  stat_density_2d(geom = "polygon", aes(alpha = ..level.., color = Alternative), contour = TRUE) + 
  geom_point(alpha = .3) +
  geom_point(data=means, color = "black", size = 4.5) +
  geom_point(data=means,  mapping=aes(x = Cost, y = Value), size=3.5)+
  guides(color = FALSE, alpha = FALSE, fill = FALSE)+
  scale_color_manual(values = c("blue", "brown", "green", "orange","purple","red")) +
  scale_fill_manual(values = c("blue", "brown", "green", "orange","purple","red")) +
  theme_tufte(ticks = FALSE, base_size = 15)+
  geom_dl(aes(label=Alternative),method="smart.grid")
} else {
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
}

# Function for checking pairwise dominance between alternatives
dominance_check <- function(dat){
  # This function takes in the full data set with each alternatives value and cost
  # outputs and iterates through all pairwise comparisons of alternatives to determine
  # if any alternative fully dominates another alternative. 
  # The function returns a dataframe with all pairs (dominates, dominated) 
  
  # Obtain all unique pairs of alternatives
  alternatives <- unique(dat$Alternative)
  alternatives <- combn(alternatives, 2)
  
  dominance_pairs <- as.data.frame(matrix(nrow=0, ncol=2))
  for (i in 1:length(alternatives[1,])){
    # establish min and max for each alternative 
    min_val_1 <- min(subset(dat, Alternative==alternatives[1,i])$Value)
    max_val_1 <- max(subset(dat, Alternative==alternatives[1,i])$Value)
    min_cost_1 <- min(subset(dat, Alternative==alternatives[1,i])$Cost)
    max_cost_1 <- max(subset(dat, Alternative==alternatives[1,i])$Cost)
    min_val_2 <- min(subset(dat, Alternative==alternatives[2,i])$Value)
    max_val_2 <- max(subset(dat, Alternative==alternatives[2,i])$Value)
    min_cost_2 <- min(subset(dat, Alternative==alternatives[2,i])$Cost)
    max_cost_2 <- max(subset(dat, Alternative==alternatives[2,i])$Cost)
    
    if (min_val_1 > max_val_2 & max_cost_1 < min_cost_2) {
      dominance_pairs[nrow(dominance_pairs)+1,] <- c(alternatives[1,i],alternatives[2, i])
    } else if (min_val_2 > max_val_1 & max_cost_2 < min_cost_1){
      dominance_pairs[nrow(dominance_pairs)+1,] <- c(alternatives[2,i],alternatives[1, i])
    }
  }
  return(dominance_pairs)
}

dominance_plot <- function(dat, alt1, alt2){
  # Do not plot if there are no dominant/dominated pairs
  if (alt1 == "There are no dominant solutions."){
    return(FALSE)
  }
  dat <- subset(dat, Alternative==alt1 | Alternative==alt2)
  min_val_1 <- min(subset(dat, Alternative==alt1)$Value)
  max_val_1 <- max(subset(dat, Alternative==alt1)$Value)
  min_cost_1 <- min(subset(dat, Alternative==alt1)$Cost)
  max_cost_1 <- max(subset(dat, Alternative==alt1)$Cost)
  
  min_val_2 <- min(subset(dat, Alternative==alt2)$Value)
  max_val_2 <- max(subset(dat, Alternative==alt2)$Value)
  min_cost_2 <- min(subset(dat, Alternative==alt2)$Cost)
  max_cost_2 <- max(subset(dat, Alternative==alt2)$Cost)
  mean_val_2 <- mean(subset(dat, Alternative==alt2)$Value)
  mean_cost_2 <- mean(subset(dat, Alternative==alt2)$Cost)

  
  #plot alternative as points w/ densities
  dom_plot = ggplot(dat, aes(Cost, Value, color = Alternative, fill = Alternative))+
    theme_minimal()+
    geom_point(alpha = .3)+
    #add blue lines
    geom_segment(aes(x = max_cost_1, y = min_val_1, xend = max_cost_2 + 5, yend = min_val_1),color = "blue", size = 1, inherit.aes = FALSE)+
    geom_segment(aes(x = max_cost_1, y = min_val_1, xend = max_cost_1, yend = min_val_2 - 5), color = "blue", size = 1, inherit.aes = FALSE)+
    #add black lines
    geom_hline(yintercept = min_val_1, color = "black", linetype = 2, size = .75)+
    geom_vline(xintercept = max_cost_1, color = "black", linetype = 2, size = .75)+
    #add grey lines
    geom_hline(yintercept = max_val_2, color = "grey", linetype = 2, size = .75)+
    geom_vline(xintercept = min_cost_2, color = "grey", linetype = 2, size = .75)+
    #add points
    geom_point(data = data.frame(Cost = c(max_cost_1, mean_cost_2), Value = c(mean_val_2,min_val_1), Alternative = "black"),
               size = 4,
               shape = 1,
               fill = NA,
               color = "blue")+
    scale_fill_manual(values = c("black", "red"))+
    scale_color_manual(values = c("black", "red"))+
    geom_dl(aes(label=Alternative),method="last.points")
  return(dom_plot)
}

cheb_calc <- function(dat, alt1, alt2, k){
  alt1 <- subset(dat, Alternative==alt1)
  alt2 <- subset(dat, Alternative==alt2)
  
  x1 <- c(mean(alt2$Cost), min(alt1$Value))
  x2 <- c(max(alt1$Cost), mean(alt2$Value))
  col_means <- colMeans(alt2[,2:3])
  covar <- cov(alt2[,2:3])
  
  dist1 <- mahalanobis(x1, col_means, covar)
  dist2 <- mahalanobis(x2, col_means, covar)
  
  min_dist <- min(dist1, dist2)
  
  prob <- 1 - min(1, (2*(k^2-1+k*min_dist))/(k^2*min_dist))
  return(prob)
}



############################
#Level 1 Analysis
############################

# Thompson's Method
thompson_method = function(alpha, delta) {
  if(alpha==.5){
    d2n <- .44129
  } else if(alpha == .4){
    d2n <- .50729
  } else if(alpha == .3){
    d2n <- .60123
  } else if(alpha == .2){
    d2n <- .74739
  } else if(alpha == .1){
    d2n <- 1.00635
  } else if(alpha == .05){
    d2n <- 1.27359
  } else if(alpha == .025){
    d2n <- 1.55963
  } else if(alpha == .02){
    d2n <- 1.65872
  } else if(alpha == .01){
    d2n <- 1.96986
  } else if(alpha == .005){
    d2n <- 2.28514
  } else if(alpha == .001){
    d2n <- 3.02892
  } else if(alpha == .0005){
    d2n <- 3.33530
  } else {
    d2n <- 4.11209
  }
  
  n <- d2n / delta^2
  return(ceiling(n))
}


# Function for Table generation
gen_pareto_table <- function(dat, param1, param2, alpha, delta){
  if (is.null(dat)) {
    return(NULL)
  }
  if (param1 == param2){
    return("Please select two different alternatives")
  }

  parameter1 <- subset(dat, Alternative == param1)
  parameter2 <- subset(dat, Alternative == param2)
  
  # determine sample size needed from Thompson's Method
  n <- thompson_method(alpha, delta)
  
  convergence_storage <- as.data.frame(matrix(nrow=0, ncol=4))
  for (i in seq(1, n, ceiling(n/50))){
    results <- level1(parameter1, parameter2, i)
    convergence_storage[nrow(convergence_storage)+1,] <- results$Percent
  }
  
  convergence_storage2 <- as.data.frame(matrix(nrow=0, ncol=4))
  for (i in (n-30):n){
    results2 <- level1(parameter1, parameter2, i)
    convergence_storage2[nrow(convergence_storage2)+1,] <- results2$Percent
  }
  table <- level1(parameter1, parameter2, n)
  
  table$`Min % of Last 30 Sample Sizes` <- c(min(convergence_storage2[,1]),min(convergence_storage2[,2]),
                                             min(convergence_storage2[,3]),min(convergence_storage2[,4]))
  table$`Max % of Last 30 Sample Sizes` <- c(max(convergence_storage2[,1]),max(convergence_storage2[,2]),
                                             max(convergence_storage2[,3]),max(convergence_storage2[,4]))
  
  table$`Convergence: % From Size = 1 to Size = N` <- c(0, 0, 0, 0)
  table$`Convergence: % From Size = 1 to Size = N`[1] <- spk_chr(convergence_storage[,1], spotColor=FALSE)#, chartRangeMin=min(convergence_storage[,1]), chartRangeMax=max(convergence_storage[,1]))
  table$`Convergence: % From Size = 1 to Size = N`[2] <- spk_chr(convergence_storage[,2], spotColor=FALSE)#, chartRangeMin=min(convergence_storage[,2]), chartRangeMax=max(convergence_storage[,2]))
  table$`Convergence: % From Size = 1 to Size = N`[3] <- spk_chr(convergence_storage[,3], spotColor=FALSE)#, chartRangeMin=min(convergence_storage[,3]), chartRangeMax=max(convergence_storage[,3]))
  table$`Convergence: % From Size = 1 to Size = N`[4] <- spk_chr(convergence_storage[,4], spotColor=FALSE)#, chartRangeMin=min(convergence_storage[,4]), chartRangeMax=max(convergence_storage[,4]))

  datatable(table, escape = F,
            options = list(columnDefs = list(list(className = 'dt-center', targets = 0:5)),
                           fnDrawCallback = htmlwidgets::JS('function(){HTMLWidgets.staticRender();}'),
                           dom='t')) %>% spk_add_deps()
  
}

level1 = function(a, b, n){
  #a and b are separate alternatives in tibble/dataframe format with cost and value columns
  # Sample the amount needed to implement Thompson's Method
  a <- sample_n(a, n, replace = TRUE)
  b <- sample_n(b, n, replace = TRUE)
  
  # Merge samples 
  pairings <- as.data.frame(c(a, b))
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


ads_score = function(a, b, n){

  temp = level1(a,b,n)
  score = (temp$`Final Count`[1]-temp$`Final Count`[2])/sum(temp$`Final Count`)
  score = set_names(score,paste0(a$Alternative[1], " compared to ", b$Alternative[1]))
  return(as.data.frame(score))
}

##########################################
#########ADS Matrix
##########################################

ads_matrix = function(list_of_alt, n){

  #build square matrix of zeros the size of the demensions of your list
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
        ads_mat[i,j] = as.numeric(ads_score(list_of_alt[[i]],list_of_alt[[j]], n))
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
ads_table <- function(dat, alpha, delta) {
  if (is.null(dat)) {
    return(NULL)
  }
  
  # determine sample size needed from Thompson's Method
  n <- thompson_method(alpha, delta)
  
  dat <- group_by(dat, Alternative)
  dat_list <- group_split(dat)
  build_matrix <- ads_matrix(dat_list, n)
  build_matrix$`Alternative` <- names(build_matrix[1:nrow(build_matrix)])
  build_matrix <- build_matrix[order(-build_matrix$`ADS Score`), , drop = FALSE] %>% 
    select(`Alternative`, everything())
  return(build_matrix)
}

#################################
#####Level 2######
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

  parameter1 <- subset(dat, Alternative == param1)
  parameter2 <- subset(dat, Alternative == param2)
  hist_plot <- level2(parameter2, parameter1, tolerance)
  
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
  delta_val = round(mean(b$Cost)*(1+tolerance),2)
  return(delta_val)
  }

delta_param = function(dat, param1, dv){
  b <- subset(dat, Alternative == param1)
  delta_p <- dv/mean(b$Cost)-1
  return(delta_p)
}

dynamic_level2 <- function(dat, alt1, alt2, delta){
  mean.alt = dat %>% group_by(Alternative) %>% summarise_all(mean)
  
  # Parameters to set in the tool
  v_dist <- mean.alt[mean.alt$Alternative==alt1,]$Value-mean.alt[mean.alt$Alternative==alt2,]$Value
  h_dist <- mean.alt[mean.alt$Alternative==alt1,]$Cost-mean.alt[mean.alt$Alternative==alt2,]$Cost
  m_dist <- max(v_dist, h_dist)
  
  # Center point for each expected value
  center2 <- c(mean.alt[mean.alt$Alternative==alt2,]$Cost,mean.alt[mean.alt$Alternative==alt2,]$Value)
  center1 <- c(mean.alt[mean.alt$Alternative==alt1,]$Cost,mean.alt[mean.alt$Alternative==alt1,]$Value)
  
  d_val <- delta
  slope_ev <- (mean.alt[mean.alt$Alternative==alt1,]$Value-mean.alt[mean.alt$Alternative==alt2,]$Value) /
    (mean.alt[mean.alt$Alternative==alt1,]$Cost-mean.alt[mean.alt$Alternative==alt2,]$Cost)
  slope_budget <- (mean.alt[mean.alt$Alternative==alt1,]$Value-mean.alt[mean.alt$Alternative==alt2,]$Value) /
    (d_val-mean.alt[mean.alt$Alternative==alt2,]$Cost)
  
  #define zones
  zone1 <- data.frame(Alternative = alt1, 
                      Cost = c( center2[1]-20*m_dist, center2[1]-20*m_dist, 
                                center2[1], center2[1]),
                      Value = c(center2[2], center2[2]+20*m_dist, 
                                center2[2]+20*m_dist, center2[2]))
  zone2 <- data.frame(Alternative = alt1, 
                      Cost = c(center2[1], center2[1], center2[1] + (20*v_dist)/slope_ev),
                      Value = c(center2[2], center2[2]+20*v_dist, center2[2]+(20*h_dist)*slope_ev))
  zone3 <- data.frame(Alternative = alt1, 
                      Cost = c(center2[1], center2[1] + (20*v_dist)/slope_ev, 
                               center2[1]+20*(d_val-center2[1])),
                      Value = c(center2[2], center2[2]+(20*h_dist)*slope_ev, 
                                center2[2]+20*slope_budget*(d_val-center2[1])))
  zone4 <- data.frame(Alternative = alt1, 
                      Cost = c(center2[1], center2[1]+20*(d_val-center2[1]), d_val + (20*h_dist)),
                      Value = c(center2[2], center2[2]+20*slope_budget*(d_val-center2[1]), center2[2]))
  zone6 <- data.frame(Alternative = alt1, 
                      Cost = c(center2[1], d_val+20*h_dist, 
                               d_val+20*h_dist, center2[1]),
                      Value = c(center2[2], center2[2], 
                                center2[2]-20*v_dist, center2[2]-20*v_dist))
  zone5 <- data.frame(Alternative = alt1, 
                      Cost = c(center2[1], center2[1]-20*h_dist, 
                               center2[1]-20*h_dist,center2[1]),
                      Value = c(center2[2], center2[2], 
                                center2[2]-20*v_dist, center2[2]-20*v_dist))
  
  ggplot(data = dat %>% filter(Alternative %in% c(alt1)), aes(Cost, Value, color = Alternative, label = Alternative))+
    geom_point(colour = "grey70")+
    #add polygons
    geom_polygon(data = zone1, aes(Cost, Value), fill = 'green4', alpha = .25)+
    geom_polygon(data = zone2, aes(Cost, Value), fill = 'green2', alpha = .25)+
    geom_polygon(data = zone3, aes(Cost, Value), fill = 'yellow2', alpha = .25)+
    geom_polygon(data = zone4, aes(Cost, Value), fill = 'orange', alpha = .25)+
    geom_polygon(data = zone6, aes(Cost, Value), fill = 'red2', alpha = .25)+
    geom_polygon(data = zone5, aes(Cost, Value), fill = 'red4', alpha = .25)+
    #budget limit
    geom_vline(xintercept = d_val, color = "red", linetype = 2)+
    annotate("text", label = "Budget Limit", x = 1.0005*d_val, y = center2[2]-.5*v_dist, color = "red", size = 4)+
    #add zone labels
    annotate("text", label = "Zone 1", x = center2[1]-2*h_dist, y = center2[2]+3*v_dist, size = 4, hjust = 0)+
    annotate("text", label = "Zone 2", x = 1.005*center2[1], y = center2[2]+3*v_dist, size = 4, hjust = 0)+
    annotate("text", label = "Zone 3", x = center2[1] + (3*v_dist)/slope_ev, y = center2[2]+3*v_dist, size = 4, hjust = 0)+
    annotate("text", label = "Zone 4", x = d_val+3*h_dist, y = 1.005*center2[2], size = 4, hjust = 1, vjust = 0)+
    annotate("text", label = "Zone 5", x = center2[1]-2*h_dist, y = center2[2]-2*v_dist, size = 4, hjust = 0)+
    annotate("text", label = "Zone 6", x = 1.005*center2[1], y = center2[2]-2*v_dist, size = 4, hjust = 0)+
    theme_minimal()+
    coord_fixed(xlim = c(center2[1]-2*m_dist, d_val+3*m_dist), 
                ylim = c(center2[2]-2*m_dist, center2[2]+3*m_dist))+
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1.15))+
    #add zone lines
    geom_segment(aes(x = center2[1], y = center2[2]-5*v_dist, xend = center2[1], yend = center2[2]+5*v_dist),color = "black", linetype = 1)+
    geom_segment(aes(x = center2[1]-4*h_dist, y = center2[2], xend = center2[1]+8*h_dist, yend = center2[2]),color = "black", linetype = 1)+
    #line from E[B] to delta/budget limit
    geom_segment(aes(x = center2[1], y = center2[2], xend = center2[1]+5*(d_val-center2[1]), 
                     yend = center2[2]+5*slope_budget*(d_val-center2[1])),color = "black", linetype = 1)+
    #delta line
    geom_segment(aes(x = center1[1], y = center1[2], xend = d_val, yend = center1[2]),color = "black", linetype = 2, arrow = arrow(type = "closed", length = unit(0.1, "inches")))+
    #annotate("text", label = expression(Delta), x = 71, y = 75.2, size = 4)+
    #line from E[B] to E[G]
    geom_segment(aes(x = center2[1], y = center2[2], xend = center2[1]+5*h_dist, 
                     yend = center2[2]+5*slope_ev*h_dist),color = "black", linetype = 1)+
    geom_point(aes(x = center2[1], center2[2]), colour = "black", size = 8)+
    geom_point(aes(x = center1[1], center1[2]), colour = "gray", size = 8)+
    annotate("text", label = "E[2]", x = center2[1], y = center2[2], color = "white", size = 3)+
    annotate("text", label = "E[1]", x = center1[1], y = center1[2], color = "black", size = 3)+
    scale_color_manual(values = c("black", "gray", "gray"))+
    labs(x = "Cost", y = "Value")+
    guides(color = FALSE, alpha = FALSE, size = FALSE)
}