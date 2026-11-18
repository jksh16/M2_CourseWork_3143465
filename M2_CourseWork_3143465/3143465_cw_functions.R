# =============================================================================
# Functions for the Essentials of Mathematics and Statistics coursework
# Student ID: <3143465>
#
# Put every function the questions ask for in this file, with comments that
# describe its inputs, outputs and what it does. Load it from your .qmd with
#   source("3143465_cw_functions.R")
# =============================================================================

#Q1 Single hop function

hop <- function(position, p_right = 0.5) {
  #Picking random number from -1 or 1 for next direction, and probability of going right is 0.5, with left as 1-0.5=0.5
  step <- sample(c(-1, 1),
                 size =1, 
                 prob = c(1-p_right,p_right))
  #update the new position, name new position as new_position for better tracking
  new_position <- position+ step
  #return function
  return(new_position)
}


#Define play_frog for Q2.
play_frog<- function(start = 4, n_pads =7, p_right = 0.5 , max_hops=100) {
  #initial variables
  position <- start
  #Set up a vector for path, starting at "start", which is 4
  path <-c(start)
  #Initially hops is 0 as in the frog has not jump yet
  hops <- 0
  #Set bank as NA_character allowing it return as "left", "right", or NA
  bank <- NA_character_
  
  #set up for loop with rounds of max_hops
  for (n in 1:max_hops){
    #initial position with position values, and p_right we have set in Q1
    position<- hop(position, p_right)
    #for vector path, add new position to the list
    path <- c(path, position)
    #hops counter, +1 whenever a extra hops
    hops<- hops+ 1
    #Add while loops to break when position of bank is reached
    if (position==0){
      bank <-"left"
      break
    } else if (position == n_pads+1){
      bank <-"right"
      break
    } 
  }
  #return as list
  return(list(path = path,
              hops = hops,
              bank = bank))
}
  

#Q3 define repeated games, with the inputs in play_frog
simulate_frogs <- function(n_sims, start = 4,
                           n_pads = 7, p_right = 0.5,
                           max_hops = 1000) {
  #setup counter for repeated hops_vector and banks, uses numeric instead of list for efficiency
  hops_vector= numeric(n_sims)
  #setting up a vector with length n_sims, fill with NA first, replace with left or right later
  banks= rep(NA_character_, n_sims)
  
  #for loop for simulates n times, using seq_len for getting sequence with length of n_sims
    for (i in seq_len(n_sims)){
    #store stats from looping play_frog for one times
    result <-play_frog(start = start,
                       n_pads = n_pads,
                       p_right = p_right,
                       max_hops = max_hops)
    #store the result for hops and banks in each play_frog() result
    hops_vector[i] <- result$hops
    banks[i] <- result$bank
    }
  #get a variables named nbankr for getting numbers of "Right" bank reached, drop "NA" before adding
  nbankr<- sum(banks == "right",na.rm = TRUE)
  #calculating the proportion of games ending on the right bank
  p_right_bank <- nbankr/ n_sims
  #using the mean and sd function to get mean and sd of hops
  mean_hops<- mean(hops_vector)
  sd_hops<- sd(hops_vector)
  
  #Updating list of return, formatting the frog_stats
  return(list( hops_vector = hops_vector,
               banks = banks,
               frog_stats= c(p_right_bank = p_right_bank,
                             mean_hops = mean_hops,
                             sd_hops = sd_hops)))
  
}

#Q4b Plot the proportion of games ending on the right bank against the starting pad
plot_rb4b <- function(results_Q4){
  #get result from previous section, store it as the proportion of games ends on the right bank
  #make p_right_bank a 7 digit vector for calculations
  p_right_bank<- numeric(7)
  #take values from hops 1-7, loop it
  for (n in 1:7) {
    p_right_bank[n] <- results_Q4[[n]]$frog_stats["p_right_bank"]
  }

  plot(x = 1:7,
       y = p_right_bank,
       type = "b",
       col = "red",
       main = "Games ends on Right Bank against starting pad 1-7",
       xlab = "Starting Pads",
       ylab = "Proportion of games ending on the right bank")
}

#Q4c Plot the mean number of hops against the starting pad
plot_rb4c <-function(results_Q4){
  ##make mean_hops a 7 digit vector for calculations
  mean_hops <- numeric(7)
  #loop again for taking out values
  for (n in 1:7) {
    mean_hops[n] <- results_Q4[[n]]$frog_stats["mean_hops"]
  }
  #plot again
  plot(x = 1:7,
       y = mean_hops,
       type = "b",
       col = "red",
       main = "Mean Hops against starting pad 1-7",
       xlab = "Starting Pads",
       ylab = "Mean Hops")
}

clean_maternal <- function(data){
  #count and drop duplicated record
  duplicates <- sum(duplicated(data))
  data <- data[!duplicated(data), ]
  #log for removed values
  cat("Removed duplicates:",duplicates, "\n")
  
  #get number of counting non physiologically plausible data
  n0 = sum(data$body_temp == 0 | data$heart_rate == 0, na.rm = TRUE)
  #filtering for non physiologically plausible data
  data <- data[data$body_temp != 0 & data$heart_rate != 0,]
  #log for removed values
  cat("Non Physiologically Plausible Data Removed:",n0, "\n")
  
  #store risk_level and site as factor
  data$risk_level <- as.factor(data$risk_level)
  data$site <- as.factor(data$site)
  
  #get data with "high risk" to TRUE
  data$high_risk <- data$risk_level == "high risk"
  return(data)
  
}

