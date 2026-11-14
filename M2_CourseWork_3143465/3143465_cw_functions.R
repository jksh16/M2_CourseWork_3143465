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



