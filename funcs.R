## General Functions

grow_values <- function(start_value, growth_rate = 0, length_out = 12, rounding = TRUE){
  ## Take a start value and return a vector of length_out after applying a growth_rate.
  if(growth_rate == 0){
    return( rep(start_value, length_out) )
  }else{
    out_vec <- start_value
    new_value <- start_value
    
    for(i in 1:(length_out - 1)){
      new_value <- new_value * (1 + growth_rate)
      if(rounding == TRUE){
        new_value <- round(new_value, digits = 0)
      }
      out_vec <- c(out_vec, new_value)
    }
    
    return( out_vec )
  }
}


expand_growth <- function(start_value, growth_rate = c(0,0,0), length_each = 12, rounding = TRUE){
  ## Take a vector of growth rates and apply them in order, starting with start_value
  out_vec <- c()
  
  for(i in 1:length(growth_rate)){
    rate <- growth_rate[i]
    
    if(i == 1){
      loop_value <- start_value
    }else{
      loop_value <- out_vec[length(out_vec)] * (1 + rate)
    }
    
    loop_vec <- grow_values(start_value = loop_value,
                            growth_rate = rate,
                            length_out = length_each,
                            rounding = rounding)
    out_vec <- c(out_vec, loop_vec)
    
    loop_value <- loop_vec[length(loop_vec)]
  }
  return(out_vec)
}


generate_data <- function(growth_rate_community,
                          growth_rate_leads_per_community,
                          PPC_penalty = 0.9,
                          start_value_community = 6174,
                          start_value_leads_per_community = 4,
                          start_date = as.Date('2018-01-01'),
                          end_date = as.Date('2020-12-01'),
                          length_each = 12){
  ## Take input growth rates and output a data frame with all values needed
  communities <- data.frame('DATE' = seq.Date(from=start_date, to=end_date, by='month'),
                            'NUM_COMMUNITIES_PPL' = expand_growth(start_value = 6174,
                                                                  growth_rate = growth_rate_community),
                            'NUM_COMMUNITIES_PPC' = expand_growth(start_value = 6174,
                                                                  growth_rate = growth_rate_community * PPC_penalty))
  
  leads <- data.frame('DATE' = seq.Date(from=start_date, to=end_date, by='month'),
                      'LEADS_PER_COMMUNITY' = expand_growth(start_value = 4,
                                                            growth_rate = growth_rate_leads_per_community,
                                                            rounding = FALSE))
  
  dat <- merge(communities, leads, by = 'DATE')
  
  dat$NUM_LEADS_PPL <- round(dat$NUM_COMMUNITIES_PPL * dat$LEADS_PER_COMMUNITY, digits = 0)
  dat$NUM_LEADS_PPC <- round(dat$NUM_COMMUNITIES_PPC * dat$LEADS_PER_COMMUNITY, digits = 0)
  
  return(dat)
  
}


gg_color_hue <- function(n) {
  # replicate ggplot's default colors
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}