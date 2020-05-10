#
# Agent-based publics.
#
# This script plots results
#


#
# Packages ----
#

rm(list=ls())
source("worldfunctions.R")


# Load data ----
#


# Load all csv files from subfolders in data folder
loadState("data")

#
# Summarize network metrics ----
#

log.net.summary <- log.contentnetwork %>% 
  gather("metric","value",edges,nodes,density,transitivity,components,distance) %>% 
  group_by(scenario,public,epoch,metric) %>%
  summarize(p0=min(value),
            p100=max(value),
            sd=sd(value),
            mean=mean(value)) %>% 
  ungroup() %>% 
  mutate(network = ifelse(public,"public","nonpublic")) %>% 
  mutate(network = recode(network,.missing="all"))


#
# Plot results ----
#


# World plot (first world, last epoch)
fieldSpace()


# Trace plot of public co-attention network metrics
log.net.summary %>%
  filter(network=="public") %>% 
  ggplot(aes(x=epoch,y=mean,color=scenario)) +
  
  geom_line(size=0.8) +
  geom_ribbon(aes(ymin=p0,ymax=p100),alpha=0.1,linetype=0) +
  facet_wrap(~metric,ncol=1,scales="free_y") +
  
  ggtitle("Network metrics in the public co-attention network") +
  theme(legend.position = "bottom",text=element_text(size=11)) +
  scale_color_brewer(palette="Set1")

# Network plot of public co-attention network (last epoch)
fieldNetwork(public=T)


# Trace plot of friendship network
log.net.summary %>%
  filter(network=="nonpublic") %>% 
  ggplot(aes(x=epoch,y=mean,color=scenario)) +
  
  geom_line(size=0.8) +
  geom_ribbon(aes(ymin=p0,ymax=p100),alpha=0.1,linetype=0) +
  facet_wrap(~metric,ncol=1,scales="free_y") +
  
  ggtitle("Network metrics in the nonpublic co-attention network") +
  theme(legend.position = "bottom",text=element_text(size=11)) +
  scale_color_brewer(palette="Set1")

# Network plot of friendship network
fieldNetwork(public=F)

