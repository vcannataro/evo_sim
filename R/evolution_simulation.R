# Evolution simulation
# Vincent L. Cannataro

# starting population size
population_size <- 6

# reproductive advantage


# initial composition of the population
starting_r <- 4
starting_b <- population_size - starting_r

# spatial arrangement of the population
population_structure <- sample(x = c(rep("r",starting_r),rep("b",starting_b)),size = population_size,replace = F)

prop_b <- NULL

sims <- 250
time_to_end <- rep(NA,sims)
winner <- rep(NA,sims)
for(i in 1:sims){
  population_structure <- sample(x = c(rep("r",starting_r),rep("b",starting_b)),size = population_size,replace = F)
  prop_b <- NULL
  # simulate evolution
  while("r" %in% population_structure & "b" %in% population_structure){
    # roll die
    winning_side <- sample(x = 1:population_size,size = 1)
    
    # "flip coins", decide which neighbor to replace
    coin_flip <- sample(x = c(-1,+1),size=1)
    if(winning_side + coin_flip > population_size){
      population_structure[1] <- population_structure[winning_side]
    }else{
      if(winning_side + coin_flip < 1){
        population_structure[population_size] <- population_structure[1]
      }else{
        population_structure[winning_side + coin_flip] <- population_structure[winning_side]
      }
    }
    prop_b <- c(prop_b,length(population_structure[population_structure=="b"])/population_size)
  }
  time_to_end[i] <- length(prop_b)
  winner[i] <- population_structure[1]
  print(i)
}  
hist(time_to_end,breaks=100)
table(winner)/sims

summary(time_to_end)

plot(prop_b,type="l")
