# function to implement a haulout reduction management strategy before salmon show up
# kills a set % and removes their connectivity in the social network

if(reduction > 0){
  hauloutReduction <- function(reduction, network_mat, num_source, specialists = NA){
    num_removed <- round(num_source * reduction)
    which_removed <- sort(sampleBespoke(1:num_source, num_removed))
    new_mat <- network_mat[-which_removed,-which_removed]
    num_adjusted <- round(num_source - num_removed)
    specialists_killed <- specialists[which(specialists %in% which_removed)]
    return(list(Mat = new_mat, Num = num_adjusted, Spec = specialists_killed, 
                Dead = which_removed))
  }
  
  after_reduction <- hauloutReduction(reduction = reduction, network_mat = network_pv, 
                                      num_source = num_seals, specialists = sort(specialist_seals))
  num_seals <- after_reduction$Num
  network_pv <- after_reduction$Mat
  killed <- after_reduction$Dead
  # assign specialists to new indices in new network:
  specialists_to_remove <- after_reduction$Spec
  specialist_seals <- sort(specialist_seals)
  specialists_alive <- specialist_seals[-which(specialist_seals %in% specialists_to_remove)]
  specialist_seals <- which(as.numeric(rownames(network_pv)) %in% specialists_alive)
  # assign new indices to network matrix
  rownames(network_pv) <- 1:num_seals
  colnames(network_pv) <- 1:num_seals
  
  
}

