source("C:/Users/Stefan/documents/Example-Work-Decision-Science/funcs.R")
library(ggplot2)
library(dplyr)
library(scales)
library(reshape2)
library(triangle)
options(scipen=10000)
options(stringsAsFactors = FALSE)

dat <- generate_data(growth_rate_community = c(0.06, 0.04, 0.02),
                     growth_rate_leads_per_community = c(0.05, 0.04, 0.01),
                     PPC_penalty = 0.9)

PPL_price <- 40
PPC_price <- 400

dat$PPL_REVENUE <- round(dat$NUM_LEADS_PPL * PPL_price, digits = 2)
dat$PPC_REVENUE <- round(dat$NUM_COMMUNITIES_PPC * PPC_price, digits = 2)

PPL_total_revenue <- sum(dat$PPL_REVENUE)
PPC_total_revenue <- sum(dat$PPC_REVENUE)

dat$PPL_CUM_REVENUE <- cumsum(dat$PPL_REVENUE)
dat$PPC_CUM_REVENUE <- cumsum(dat$PPC_REVENUE)


# Plot yearly revenue values
d1 <- melt(dat, id.vars = 'DATE', measure.vars =  c('PPL_REVENUE', 'PPC_REVENUE'))

ggplot(d1, aes(x = DATE, y = value/1000000, color = variable)) +
  geom_line(size = 2) +
  ylab('Revenue ($M)') +
  xlab('Date') +
  ggtitle('Monthly Revenue Over Time') +
  scale_y_continuous(label=dollar_format(), limits = c(0,15)) +
  scale_color_discrete(breaks=c("PPL_REVENUE", "PPC_REVENUE"),
                       labels=c("Pay Per Lead", "Pay Per Community")) +
  theme(legend.title = element_blank(), axis.text = element_text(size=12))


# Plot cumulative revenue values
d2 <- melt(dat, id.vars = 'DATE', measure.vars =  c('PPL_CUM_REVENUE', 'PPC_CUM_REVENUE'))

ggplot(d2, aes(x = DATE, y = value/1000000, color = variable)) +
  geom_line(size = 2) +
  ylab('Revenue ($M)') +
  xlab('Date') +
  ggtitle('Cumulative Monthly Revenue Over Time') +
  scale_y_continuous(label=dollar_format()) +
  scale_color_discrete(breaks=c("PPL_CUM_REVENUE", "PPC_CUM_REVENUE"),
                       labels=c("Pay Per Lead", "Pay Per Community")) +
  theme(legend.title = element_blank(), axis.text = element_text(size=12))


# Plot number of communities over time
d3 <- melt(dat, id.vars = 'DATE', measure.vars =  c('NUM_COMMUNITIES_PPL', 'NUM_COMMUNITIES_PPC'))

ggplot(d3, aes(x = DATE, y = value/1000, color = variable)) +
  geom_line(size = 2) +
  ylab('Number of Communities (Thousands)') +
  xlab('Date') +
  ggtitle('Number of Communities Subscribed by Month') +
  scale_y_continuous(label=comma, limits = c(0,25)) +
  scale_color_discrete(breaks=c("NUM_COMMUNITIES_PPL", "NUM_COMMUNITIES_PPC"),
                       labels=c("Pay Per Lead", "Pay Per Community")) +
  theme(legend.title = element_blank(), axis.text = element_text(size=12))


#### Run Sensitivity Analysis ####
PPL_total_revenue_out <- c()
PPC_total_revenue_out <- c()
dat_out <- dat
iters <- 10000

for(i in 1:iters){
  community_tri <- c(rtriangle(n = 1, a = 0.04, b = 0.08, c = 0.06),
                     rtriangle(n = 1, a = 0.02, b = 0.06, c = 0.04),
                     rtriangle(n = 1, a = 0.00, b = 0.04, c = 0.02))
  leads_tri <- c(rtriangle(n = 1, a = 0.03, b = 0.07, c= 0.05),
                 rtriangle(n = 1, a = 0.02, b = 0.06, c = 0.04),
                 rtriangle(n = 1, a = -0.01, b = 0.03, c = 0.01))
  PPC_penalty_tri <- rtriangle(n = 1, a = 0.8, b = 1, c = 0.9)
  
  dat_n <- generate_data(growth_rate_community = community_tri,
                         growth_rate_leads_per_community = leads_tri,
                         PPC_penalty = PPC_penalty_tri)
  
  dat_n$PPL_REVENUE <- round(dat_n$NUM_LEADS_PPL * PPL_price, digits = 2)
  dat_n$PPC_REVENUE <- round(dat_n$NUM_COMMUNITIES_PPC * PPC_price, digits = 2)
  
  PPL_total_revenue_n <- sum(dat_n$PPL_REVENUE)
  PPC_total_revenue_n <- sum(dat_n$PPC_REVENUE)
  
  dat_n$PPL_CUM_REVENUE <- cumsum(dat_n$PPL_REVENUE)
  dat_n$PPC_CUM_REVENUE <- cumsum(dat_n$PPC_REVENUE)
  
  PPL_total_revenue_out <- c(PPL_total_revenue_out, PPL_total_revenue_n)
  PPC_total_revenue_out <- c(PPC_total_revenue_out, PPC_total_revenue_n)
  
  dat_out <- bind_rows(list(dat_out, dat_n))
  
  if(i %% (iters/10) == 0){
    cat('Completed: ', (i / iters) * 100, '%\n')
  }
}

# plot sensitivity of total revenue
d4 <- rbind(data.frame(FLAG = 'PPC', REVENUE = PPC_total_revenue_out),
            data.frame(FLAG = 'PPL', REVENUE = PPL_total_revenue_out))

ggplot(d4, aes(x = REVENUE/1000000, fill = FLAG, order = FLAG)) + 
  geom_vline(aes(xintercept = PPL_total_revenue/1000000), color = gg_color_hue(1), size = 2) +
  geom_vline(aes(xintercept = PPC_total_revenue/1000000), color = gg_color_hue(2)[2], size = 2) +
  geom_density(alpha = 0.6) + 
  xlab('Cumulative Revenue') +
  ggtitle('Likely Cumulative Revenue by End of 2020') +
  scale_fill_manual(values = c(gg_color_hue(2)[2], gg_color_hue(1))) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),
                     label=dollar_format()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        axis.text = element_text(size=12),
        legend.title = element_blank())
  

# Find probability PPL revenue > PPC revenue
dat_p <- data.frame(PPC_total_revenue_out, PPL_total_revenue_out)
dat_p$EXPECTED_REVENUE_PPL_MINUS_PPC <- dat_p$PPL_total_revenue_out - dat_p$PPC_total_revenue_out
dat_p$PPC_GREATER <- ifelse(dat_p$PPC_total_revenue_out > dat_p$PPL_total_revenue_out, 1, 0)

PPL_revenue_greater_prob <- dat_p %>% group_by(PPC_GREATER) %>% summarise(COUNT_PPC_GREATER = length(PPC_GREATER))
PPL_revenue_greater_prob <- round(( PPL_revenue_greater_prob$COUNT_PPC_GREATER[1] / sum(PPL_revenue_greater_prob$COUNT_PPC_GREATER) ) * 100, digits = 1)

t.test(dat_p$EXPECTED_REVENUE_PPL_MINUS_PPC, mu = 0)


# Find probability PPL communities > PPC communities
PPL_communities <- dat_out[which(dat_out$DATE == '2020-12-01'), 'NUM_COMMUNITIES_PPL']
PPC_communities <- dat_out[which(dat_out$DATE == '2020-12-01'), 'NUM_COMMUNITIES_PPC']
expected_communities_PPL_minus_PPC <- PPL_communities - PPC_communities

dat_c <- data.frame(PPL_communities, PPC_communities)
dat_c$PPL_GREATER <- ifelse(dat_c$PPL_communities > dat_c$PPC_communities, 1, 0)

sum(dat_c$PPL_GREATER) / nrow(dat_c)

t.test(expected_communities_PPL_minus_PPC, mu = 0)

quantile(expected_communities_PPL_minus_PPC, probs = c(0.25, 0.75))


# plot expected revenue value of PPL - PPC
dens <- density(dat_p$EXPECTED_REVENUE_PPL_MINUS_PPC / 1000000)
df <- data.frame(x=dens$x, y=dens$y)
quantiles <- quantile(df$y, prob=0.5)
df$quant <- factor(findInterval(df$x,quantiles))

ggplot(df, aes(x,y)) + 
  geom_line(color = 'grey50') + 
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
  geom_vline(aes(xintercept = 0)) +
  geom_text(x=60, y=0.0025, 
            label=paste0('Prob(Lead > Community): ', PPL_revenue_greater_prob, '%'), 
            color = 'grey30') +
  ggtitle('Likely Cumulative Revenue of PPL - PPC by end of 2020') +
  xlab('Cumulative Revenue') +
  scale_fill_manual(values = alpha(c('grey50', gg_color_hue(2)[2]), 0.6),
                    breaks = c(0, 1),
                    labels = c("Pay Per Community Greater", "Pay Per Lead Greater")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),
                     label=dollar_format()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        axis.text = element_text(size=12),
        legend.title = element_blank())


# plot sensitivity of communities
d5 <- rbind(data.frame(FLAG = 'PPC', COMMUNITIES = PPC_communities),
            data.frame(FLAG = 'PPL', COMMUNITIES = PPL_communities))

ggplot(d5, aes(x = COMMUNITIES/1000, fill = FLAG, order = FLAG)) + 
  geom_vline(aes(xintercept = dat[which(dat$DATE == '2020-12-01'), 'NUM_COMMUNITIES_PPL']/1000), 
             color = gg_color_hue(1), size = 2) +
  geom_vline(aes(xintercept = dat[which(dat$DATE == '2020-12-01'), 'NUM_COMMUNITIES_PPC']/1000), 
             color = gg_color_hue(2)[2], size = 2) +
  geom_density(alpha = 0.6) + 
  xlab('Communities Subscribed') +
  ggtitle('Likely Communities Subscribed at End of 2020') +
  scale_fill_manual(values = c(gg_color_hue(2)[2], gg_color_hue(1))) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),
                     label=comma) +
  facet_grid(FLAG ~ .) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        axis.text = element_text(size=12),
        legend.title = element_blank())


# plot expected communities value of PPL - PPC
q95 <- quantile(expected_communities_PPL_minus_PPC, .95)
q5 <- quantile(expected_communities_PPL_minus_PPC, .05)
x.dens  <- density(expected_communities_PPL_minus_PPC)
df.dens <- data.frame(x=x.dens$x, y=x.dens$y)

ggplot(mapping = aes(x = expected_communities_PPL_minus_PPC)) + 
  geom_density(fill = gg_color_hue(1), alpha = 0.4) + 
  geom_area(data = subset(df.dens, x >= q5 & x <= q95), 
            aes(x=x,y=y), fill=gg_color_hue(1), alpha=0.6) +
  geom_vline(aes(xintercept = 2945), size = 2, color = 'grey60') +
  ggtitle('Likely Communities Subscribed of PPL - PPC at end of 2020') +
  xlab('Communities Subscribed') +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),
                     label=comma) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        axis.text = element_text(size=12),
        legend.title = element_blank())


# plot growth in avg leads per community
bounds <- dat_out %>% 
  group_by(DATE) %>% 
  summarise(EXPECTED = mean(LEADS_PER_COMMUNITY),
            UB = quantile(LEADS_PER_COMMUNITY, 0.95),
            LB = quantile(LEADS_PER_COMMUNITY, 0.05))

ggplot(bounds, aes(x = DATE, y = EXPECTED, ymin = LB, ymax = UB)) + 
  geom_ribbon(alpha = 0.2) +  
  geom_line(size = 2, color = 'blue', alpha = 0.5) + 
  geom_hline(aes(yintercept = 10)) +
  xlab('Date') +
  ggtitle('Growth in Average Leads Per Community Over Time') +
  ylab('Avg Leads Per Community')
