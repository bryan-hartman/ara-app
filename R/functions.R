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


# This function generates a list of pareto(-) and pareto(+) options in terms
# of expected value from a given dataset and alternative
pareto_options <- function(dat, alt){
  if(alt==""){
    return(FALSE)
  }
  # gather all mean values
  mean.alt <- dat %>% group_by(Alternative) %>% summarise_all(mean)

  # Gather cost and value for alternative selected
  cost_threshold <- mean.alt[mean.alt$Alternative==alt,]$Cost
  value_threshold <- mean.alt[mean.alt$Alternative==alt,]$Value
  
  # Identify options that don't meet criteria
  # Dominated
  dominated <- mean.alt[mean.alt$Value > value_threshold & 
                        mean.alt$Cost < cost_threshold,]$Alternative
  dominating <- mean.alt[mean.alt$Value < value_threshold & 
                           mean.alt$Cost > cost_threshold,]$Alternative
  # remove unavailable options
  mean.alt <- subset(mean.alt, !(Alternative %in% c(dominated, dominating, alt)))
  
  # returned shortened list
  return(mean.alt$Alternative)

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
    theme_minimal() +
    geom_histogram(alpha = alpha, position = "identity", binwidth = .25, col = "black") +
    scale_color_manual(values = c("blue", "brown", "green", "orange","purple","red"))+
    scale_fill_manual(values = c("blue", "brown", "green", "orange","purple","red")) + 
    labs(title = "Value Histogram") + 
    ylab("Count")+
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
  theme_tufte(ticks = FALSE, base_size = 15, base_family = "Arial")+
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

# The function below takes in four inputs: The data, the two alternatives, and
# the sample size (k).
# The output is the probability that one alternative will continue to dominate
# another alternative.
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
# This function takes in parameters alpha and delta and returns the sample size (n)
# necessary to ensure pairwise comparison %s are within delta of their
# actual values.
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
# This function generates the pareto table used within level 1 analysis.
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
  table$`Convergence: % From Size = 1 to Size = N`[1] <- spk_chr(convergence_storage[,1], spotColor=FALSE)
  table$`Convergence: % From Size = 1 to Size = N`[2] <- spk_chr(convergence_storage[,2], spotColor=FALSE)
  table$`Convergence: % From Size = 1 to Size = N`[3] <- spk_chr(convergence_storage[,3], spotColor=FALSE)
  table$`Convergence: % From Size = 1 to Size = N`[4] <- spk_chr(convergence_storage[,4], spotColor=FALSE)

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
    
    for (j in i:length(list_of_alt)) {
      if(i == j){
        ads_mat[i,j] = 0
      } else {
        ads_mat[i,j] = as.numeric(ads_score(list_of_alt[[i]],list_of_alt[[j]], n))
        colnames(ads_mat)[j] = c(list_of_alt[[j]]$Alternative[1])
      } 
    }
    for (j in 1:i){
      if(i == j) {
        ads_mat[i,j] = 0
        } else {
        # matrix is anti-symmatric
        ads_mat[i, j] <- -ads_mat[j, i]
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
  
  # Reorder columns for symmetry
  new_mat <- build_matrix[,build_matrix$Alternative]
  full_mat <- cbind(build_matrix[,1], new_mat, build_matrix[,length(build_matrix)])
  return(full_mat)
}

#################################
#####Level 2######
#################################

level2 = function(alt2, alt1, tolerance = .05){
  # alt1 and alt2 are separate alternatives in tibble/dataframe format with cost and value columns
  # alt1 is the chosen alternative 
  # a may be pareto(-) in relation to b or pareto(+). 
  # Neither alternative expected value dominates the other.
  # tolerance is the percentage less expected value the DM would be ok with at the expected cost
  
  alt1.value.mean = mean(alt1$Value)
  alt1.cost.mean = mean(alt1$Cost)
  alt2.value.mean = mean(alt2$Value)
  alt2.cost.mean = mean(alt2$Cost)
  
  # The case when alt2 is pareto(+) in relation to alt1
  if (alt2.value.mean > alt1.value.mean && alt2.cost.mean > alt1.cost.mean){
      
      #trade1 is the value/cost trade-off that you are considering
      trade1 = (alt2.value.mean - alt1.value.mean)/(alt2.cost.mean - alt1.cost.mean)
    
      #trade2 is the point at which you would no longer make the value/cost trade off and go with the cheaper alternative
      trade2 = (alt2.value.mean - alt1.value.mean)/(alt2.cost.mean - (alt1.cost.mean)*(1+tolerance))
    
      #bind alternatives for computation
      pairings = tibble(alt2.value = alt2.value.mean, alt2.cost = alt2.cost.mean, alt1.value = alt1$Value, alt1.cost = alt1$Cost)
    
      #compute Zone 6 - Trade much worse than expected - alt2 dominates alt1
      pairings = mutate(pairings, zone6 = if_else((pairings$alt1.value < alt2.value.mean & pairings$alt1.cost > alt2.cost.mean), 1, 0)) 
      #compute Zone 5 - Trade worse than expected - alt1 is pareto(+) compared to A, but contradicts decision to select alt1
      pairings = mutate(pairings, zone5 = if_else(pairings$alt1.value > alt2.value.mean & pairings$alt1.cost > alt2.cost.mean, 1, 0)) 
      #compute Zone 4 - Trade worse than expected - Unacceptable
      pairings = mutate(pairings, zone4 = if_else((alt2.value.mean-pairings$alt1.value)/(alt2.cost.mean-pairings$alt1.cost) > trade2 & 
                                                    pairings$alt1.value < alt2.value.mean & pairings$alt1.cost < alt2.cost.mean &
                                                    zone5 < 1 & zone6 < 1, 1, 0))
      #compute Zone 3 - Trade worse than expected - Acceptable
      pairings = mutate(pairings, zone3 = if_else((alt2.value.mean-pairings$alt1.value)/(alt2.cost.mean-pairings$alt1.cost) < trade2 & 
                                                    (alt2.value.mean-pairings$alt1.value)/(alt2.cost.mean-pairings$alt1.cost) > trade1 &
                                                    zone5 < 1 & zone6 < 1 & zone4 < 1, 1, 0))
      #compute Zone 2 - Trade better than expected
      pairings = mutate(pairings, zone2 = if_else((alt2.value.mean-pairings$alt1.value)/(alt2.cost.mean-pairings$alt1.cost) < trade1 & 
                                                    pairings$alt1.value < alt2.value.mean & zone6 < 1, 1, 0))
      #compute Zone 1 - Trade better than expected - alt1 dominates alt2
      pairings = mutate(pairings, zone1 = if_else(pairings$alt1.value > alt2.value.mean & pairings$alt1.cost < alt2.cost.mean, 1, 0))

  } else if (alt1.value.mean > alt2.value.mean && alt1.cost.mean > alt2.cost.mean){
      # alt2 is pareto(-) in relation to alt1
    
      #trade1 is the value/cost trade-off that you are considering
      trade1 = (alt1.value.mean - alt2.value.mean)/(alt1.cost.mean - alt2.cost.mean)
    
      #trade2 is the point at which you would no longer make the value/cost trade off and go with the cheaper alternative
      #This is the minimum acceptable trade ... default at 5% less than expected mean
      trade2 = ((alt1.value.mean) - alt2.value.mean)/(alt1.cost.mean*(1+tolerance) - alt2.cost.mean)
    
      #bind alternatives for computation
      pairings = tibble(alt2.value = alt2.value.mean, alt2.cost = alt2.cost.mean, alt1.value = alt1$Value, alt1.cost = alt1$Cost)
      
      #compute Zone 6 - Trade much worse than expected - alt2 dominates alt1
      pairings = mutate(pairings, zone6 = if_else((pairings$alt1.value < alt2.value.mean & pairings$alt1.cost > alt2.cost.mean), 1, 0)) 
      #compute Zone 5 - Trade worse than expected - alt1 is pareto(-) to A
      pairings = mutate(pairings, zone5 = if_else(pairings$alt1.value < alt2.value.mean & pairings$alt1.cost < alt2.cost.mean, 1, 0)) 
      #compute Zone 4 - Trade worse than expected - Unacceptable
      pairings = mutate(pairings, zone4 = if_else((pairings$alt1.value-alt2.value.mean)/(pairings$alt1.cost-alt2.cost.mean) < trade2 & 
                                                    pairings$alt1.value > alt2.value.mean & pairings$alt1.cost > alt2.cost.mean &
                                                    zone6 < 1 & zone5 < 1, 1, 0))
      #compute Zone 3 - Trade worse than expected - Acceptable
      pairings = mutate(pairings, zone3 = if_else(trade2 < (pairings$alt1.value-alt2.value.mean)/(pairings$alt1.cost-alt2.cost.mean) & 
                                                    (pairings$alt1.value-alt2.value.mean)/(pairings$alt1.cost-alt2.cost.mean) < trade1 &
                                                    zone6 < 1 & zone5 < 1, 1, 0))
      #compute Zone 2 - Trade better than expected
      pairings = mutate(pairings, zone2 = if_else((pairings$alt1.value-alt2.value.mean)/(pairings$alt1.cost-alt2.cost.mean) > trade1 & 
                                                    pairings$alt1.cost > alt2.cost.mean, 1, 0))
      #compute Zone 1 - Trade alt1etter than expected - alt1 dominates alt2
      pairings = mutate(pairings, zone1 = if_else(pairings$alt1.value > alt2.value.mean & pairings$alt1.cost < alt2.cost.mean, 1, 0))
      

  } else {
    return(FALSE)
  }
  #Compute zones in place in single column named zoneTest for histogram alt1uilding
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

####Level 2 Histogram Plot#########
# This function generates the level 2 histogram found on the trade zones
# tab within level 2. It leverages the level2 function to determine
# the trade zone counts and displays the breakdown in a histogram.
gen_level2_plot <- function(dat, alt1, alt2, tolerance){
  # selected alternative is alt1
  if (is.null(dat)) {
    return(NULL)
  }
  # alternatives must be pareto(+)/pareto(-) from each other
  if (alt2 %in% pareto_options(dat, alt1)){
    
  alternative1 <- subset(dat, Alternative == alt1)
  alternative2 <- subset(dat, Alternative == alt2)
  hist_plot <- level2(alternative2, alternative1, tolerance)
  
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
    labs(title = paste0("Trade Zones for alternative ", alt1, " with respect to EV[", alt2,"]")
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
  } else {
    return()
  }
  } 


# return the delta value associated with the delta parameter
delta_value = function(dat, alt1, alt2, tolerance = .05){
  if (is.null(dat) || is.nan(mean(subset(dat, Alternative==alt2)$Cost))) {
    return(NULL)
  }
  # This function does not assume pareto(+/-) between alt1 and alt2
  # alt1 is the chosen alternative
  # Gather expected value for both alternatives to define the budget line
  # Function is defaulted to move delta (tolerance) percent to the right of chosen alternative
  # Logic below supports adjusting delta parameter based on whether we move
  # up or down the pareto.
  if(mean(subset(dat, Alternative==alt2)$Cost) > mean(subset(dat, Alternative==alt1)$Cost)){
    b_alt1 <- subset(dat, Alternative == alt1)
    b_alt2 <- subset(dat, Alternative == alt2)
    delta_val = round(mean(b_alt1$Cost)*(1+tolerance),2)
  } else {
    b_alt1 <- subset(dat, Alternative == alt1)
    delta_val = round(mean(b_alt1$Cost)*(1+tolerance),2)
  }
  return(delta_val)
}

delta_param = function(dat, alt, dv){
  # Given a delta budget amount (dv), return the percentage delta parameter value.
  b <- subset(dat, Alternative == alt)
  delta_p <- dv/mean(b$Cost)-1
  return(delta_p)
}

####Level 2 Dynamic Detailed Plot#########
# This function takes in the delta parameter selected by the user in the trade
# zone tab in the level 2 analysis and returns a plot that shows the realizations
# of the selected alternative displayed within each trade zone.

# Currently, this plot is only built to examine a selected alternative that
# is pareto(+) in comparison to the compared alternative.
dynamic_level2 <- function(dat, alt1, alt2, d_val){
  
  # alternatives must be pareto(+)/pareto(-) from each other
  if (alt2 %in% pareto_options(dat, alt1)){
    
  # Selected alternative is alt1. 
  # Identify mean value/cost for each alternative
  mean.alt = dat %>% group_by(Alternative) %>% summarise_all(mean)

  # Center point for each expected value
  center2 <- c(mean.alt[mean.alt$Alternative==alt2,]$Cost,mean.alt[mean.alt$Alternative==alt2,]$Value)
  center1 <- c(mean.alt[mean.alt$Alternative==alt1,]$Cost,mean.alt[mean.alt$Alternative==alt1,]$Value)
  
  # Set coordinates to match cloud plot
  max_dist <- max(max(dat$Cost)-min(dat$Cost), max(dat$Value)-min(dat$Value))
  x_coord <- c((max(dat$Cost)+min(dat$Cost))/2-max_dist/2, 
               (max(dat$Cost)+min(dat$Cost))/2+max_dist/2)
  y_coord <- c((max(dat$Value)+min(dat$Value))/2-max_dist/2, 
               (max(dat$Value)+min(dat$Value))/2+max_dist/2)
  
  # Plot when EV[Alt1] is pareto(-) compared to EV[Alt2]
  if (mean.alt[mean.alt$Alternative==alt2,]$Cost > mean.alt[mean.alt$Alternative==alt1,]$Cost) {
    # Parameters to set in the tool
    v_dist <- mean.alt[mean.alt$Alternative==alt2,]$Value-mean.alt[mean.alt$Alternative==alt1,]$Value
    h_dist <- mean.alt[mean.alt$Alternative==alt2,]$Cost-mean.alt[mean.alt$Alternative==alt1,]$Cost
    m_dist <- max(v_dist, h_dist)
    
    slope_ev <- v_dist / h_dist
    slope_budget <- (mean.alt[mean.alt$Alternative==alt2,]$Value-mean.alt[mean.alt$Alternative==alt1,]$Value) /
      (mean.alt[mean.alt$Alternative==alt2,]$Cost-d_val)

    #define zones
    zone1 <- data.frame(Alternative = alt1, 
                        Cost = c( center2[1]-20*m_dist, center2[1]-20*m_dist, 
                                  center2[1], center2[1]),
                        Value = c(center2[2], center2[2]+20*m_dist, 
                                  center2[2]+20*m_dist, center2[2]))
    zone2 <- data.frame(Alternative = alt1, 
                        Cost = c(center2[1], center2[1]-20*m_dist, center2[1] - (20*v_dist)/slope_ev),
                        Value = c(center2[2], center2[2], center2[2]-(20*h_dist)*slope_ev))
    zone3 <- data.frame(Alternative = alt1, 
                        Cost = c(center2[1], center2[1] - (20*v_dist)/slope_ev, 
                                 center2[1]-20*(center2[1]-d_val)),
                        Value = c(center2[2], center2[2]-(20*m_dist)*slope_ev, 
                                  center2[2]-20*slope_budget*(center2[1]-d_val)))
    zone4 <- data.frame(Alternative = alt1, 
                        Cost = c(center2[1], center2[1]-20*(center2[1]-d_val), center2[1]),
                        Value = c(center2[2], center2[2]-20*slope_budget*(center2[1]-d_val), center2[2]-20*m_dist))
    zone6 <- data.frame(Alternative = alt1, 
                        Cost = c(center2[1], d_val+20*max_dist, 
                                 d_val+20*max_dist, center2[1]),
                        Value = c(center2[2], center2[2], 
                                  center2[2]-20*v_dist, center2[2]-20*v_dist))
    zone5 <- data.frame(Alternative = alt1, 
                        Cost = c(center2[1], center2[1], 
                                 center2[1]+20*m_dist,center2[1]+20*m_dist),
                        Value = c(center2[2], center2[2]+20*m_dist, 
                                  center2[2]+20*m_dist, center2[2]))
    
    ggplotly(ggplot(data = dat %>% filter(Alternative %in% c(alt1)), aes(Cost, Value, color = Alternative, label = Alternative))+
      geom_point(colour = "grey80")+
      #add polygons
      geom_polygon(data = zone1, aes(Cost, Value), fill = 'green4', alpha = .25)+
      geom_polygon(data = zone2, aes(Cost, Value), fill = 'green2', alpha = .25)+
      geom_polygon(data = zone3, aes(Cost, Value), fill = 'yellow2', alpha = .25)+
      geom_polygon(data = zone4, aes(Cost, Value), fill = 'orange', alpha = .25)+
      geom_polygon(data = zone6, aes(Cost, Value), fill = 'red2', alpha = .25)+
      geom_polygon(data = zone5, aes(Cost, Value), fill = 'red4', alpha = .25)+
      #budget limit
      geom_vline(xintercept = d_val, color = "red", linetype = 2)+
      annotate("text", label = "Budget Limit", x = 1.0005*d_val, y = y_coord[2], 
               vjust = 0,color = "red", size = 4)+
      #add zone labels
      annotate("text", label = "Zone 1", x = x_coord[1], y = y_coord[2], size = 4, hjust = 0)+
      annotate("text", label = "Zone 2", x = x_coord[1], y = .98*center2[2], size = 4, hjust = 0)+
      annotate("text", label = "Zone 3", x = center2[1] - (max_dist/4)/((slope_ev+slope_budget)/2), 
               y = center2[2]-max_dist/4, size = 4, hjust = 0)+
      annotate("text", label = "Zone 4", x = 1.0005*d_val, y = y_coord[1], size = 4, hjust = 1, vjust = 0)+
      annotate("text", label = "Zone 5", x = x_coord[2], y = y_coord[2], size = 4, hjust = 1)+
      annotate("text", label = "Zone 6", x = x_coord[2], y = y_coord[1], size = 4, hjust = 1)+
      theme_minimal()+
      coord_fixed(xlim = x_coord, 
                  ylim = y_coord)+
      theme(panel.border = element_rect(colour = "black", fill = NA, size = 1.15))+

      #delta line
      geom_segment(aes(x = center1[1], y = center1[2], xend = d_val, yend = center1[2]),color = "black", linetype = 2, arrow = arrow(type = "closed", length = unit(0.1, "inches")))+
      
      geom_point(aes(x = center2[1], center2[2]), colour = "black", size = 4)+
      geom_point(aes(x = center1[1], center1[2]), colour = "black", size = 4)+
      annotate("text", label = paste0("E[",alt2,"]"), x = center2[1], 
               y = 1.03*center2[2], color = "black", size = 4)+
      annotate("text", label =  paste0("E[",alt1,"]"), x = center1[1], 
               y = .97*center1[2], color = "black", size = 4)+
      scale_color_manual(values = c("black", "gray", "gray"))+
      labs(x = "Cost", y = "Value")+
      guides(color = FALSE, alpha = FALSE, size = FALSE, fill = FALSE) +
      theme(legend.position = 'none')) %>%
      config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d")) %>%
      style(hoverinfo = 'none') %>%
      layout(font=list(family = "Arial", size = 10))
  } else {
      # Plot when EV[Alt1] is pareto(+) compared to EV[Alt2]
      # Parameters to set in the tool
      v_dist <- mean.alt[mean.alt$Alternative==alt1,]$Value-mean.alt[mean.alt$Alternative==alt2,]$Value
      h_dist <- mean.alt[mean.alt$Alternative==alt1,]$Cost-mean.alt[mean.alt$Alternative==alt2,]$Cost
      m_dist <- max(v_dist, h_dist)
      
      slope_ev <-  v_dist / h_dist
      slope_budget <- (mean.alt[mean.alt$Alternative==alt1,]$Value-mean.alt[mean.alt$Alternative==alt2,]$Value) /
        (d_val-mean.alt[mean.alt$Alternative==alt2,]$Cost)
      
      #define zones
      zone1 <- data.frame(Alternative = alt1, 
                          Cost = c( center2[1]-20*max_dist, center2[1]-20*max_dist, 
                                    center2[1], center2[1]),
                          Value = c(center2[2], center2[2]+20*max_dist, 
                                    center2[2]+20*max_dist, center2[2]))
      zone2 <- data.frame(Alternative = alt1, 
                          Cost = c(center2[1], center2[1], center2[1] + (20*v_dist)/slope_ev),
                          Value = c(center2[2], center2[2]+20*v_dist, center2[2]+(20*h_dist)*slope_ev))
      zone3 <- data.frame(Alternative = alt1, 
                          Cost = c(center2[1], center2[1] + (20*v_dist)/slope_ev, 
                                   center2[1]+20*(d_val-center2[1])),
                          Value = c(center2[2], center2[2]+(20*h_dist)*slope_ev, 
                                    center2[2]+20*slope_budget*(d_val-center2[1])))
      zone4 <- data.frame(Alternative = alt1, 
                          Cost = c(center2[1], center2[1]+20*(d_val-center2[1]), d_val + (20*max_dist)),
                          Value = c(center2[2], center2[2]+20*slope_budget*(d_val-center2[1]), center2[2]))
      zone6 <- data.frame(Alternative = alt1, 
                          Cost = c(center2[1], d_val+20*max_dist, 
                                   d_val+20*max_dist, center2[1]),
                          Value = c(center2[2], center2[2], 
                                    center2[2]-20*max_dist, center2[2]-20*max_dist))
      zone5 <- data.frame(Alternative = alt1, 
                          Cost = c(center2[1], center2[1]-20*max_dist, 
                                   center2[1]-20*max_dist,center2[1]),
                          Value = c(center2[2], center2[2], 
                                    center2[2]-20*max_dist, center2[2]-20*max_dist))
      
      ggplotly(ggplot(data = dat %>% filter(Alternative %in% c(alt1)), aes(Cost, Value, color = Alternative, label = Alternative))+
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
        annotate("text", label = "Zone 1", x = x_coord[1], y = y_coord[2], size = 4, hjust = 0)+
        annotate("text", label = "Zone 2", x = center1[1], y = y_coord[2], size = 4, hjust = 0)+
        annotate("text", label = "Zone 3", x = center2[1] + (max_dist/4)/((slope_ev+slope_budget)/2), y = center2[2]+max_dist/4, size = 4, hjust = 0)+
        annotate("text", label = "Zone 4", x = x_coord[2], y = 1.02*center2[2], size = 4, hjust = 1, vjust = 0)+
        annotate("text", label = "Zone 5", x = x_coord[1], y = y_coord[1], size = 4, hjust = 0)+
        annotate("text", label = "Zone 6", x = x_coord[2], y = y_coord[1], size = 4, hjust = 0)+
        theme_minimal()+
        coord_fixed(xlim = x_coord, 
                    ylim = y_coord)+
        theme(panel.border = element_rect(colour = "black", fill = NA, size = 1.15))+
        #delta line
        geom_segment(aes(x = center1[1], y = center1[2], xend = d_val, yend = center1[2]),color = "black", linetype = 2, arrow = arrow(type = "closed", length = unit(0.1, "inches")))+

        geom_point(aes(x = center2[1], center2[2]), colour = "black", size = 4)+
        geom_point(aes(x = center1[1], center1[2]), colour = "black", size = 4)+
        annotate("text", label = paste0("E[",alt2,"]"), x = center2[1], 
                   y = .97*center2[2], color = "black", size = 4)+
        annotate("text", label =  paste0("E[",alt1,"]"), x = center1[1], 
                   y = 1.03*center1[2], color = "black", size = 4)+
        scale_color_manual(values = c("black", "gray", "gray"))+
        labs(x = "Cost", y = "Value")+
        guides(color = FALSE, alpha = FALSE, size = FALSE, fill = FALSE) +
          theme(legend.position = 'none')) %>%
        config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d")) %>%
        style(hoverinfo = 'none') %>%
        layout(font=list(family = "Arial", size = 10))
  }
  } else {
    return()
  }
}
