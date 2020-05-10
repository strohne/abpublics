#
# Agent-based publics.
#
# This script runs the scenario: recommendation (aka recommend_global)
#


rm(list=ls())
source("worldfunctions.R")

# Setup world definition
world.definition <- tribble(
  ~phase,  ~method,         ~params,
  "setup", "resetWorld",  list(),
  
  "setup", "addRole",     list("default",c(disclose.create=25,disclose.replicate=25,perceive=50),c(low=1,high=10)),
  "setup", "addRole",     list("mediator",c(mediate=100),c(low=10,high=50)),
  
  "setup", "spawnAgents", list(100,"default"),
  "setup", "spawnAgents", list(4,"mediator"),
  
  "play",  "moveAgents",  list("friends",10),
  
  "play",  "chooseActions",list(),
  
  "play",  "createMessages",  list(),
  "play",  "addressMessages", list("directed"),
  
  "play",  "mediateMessages", list("recommend","global"),
  
  "play",  "accessMessages",  list("closest"),
  "play",  "perceiveMessages",list("directed"),
  
  "play",  "buildRelations",list(turns = 1,epochs = 100),
  
  
  "log",  "generateLogs",list(),
  "log",  "saveHistory",list(),
  "log",  "pruneHistory",list(),
  "log",  "savePresent",list()
)

# Uncomments to generate data for one world and 10 epochs
# Result is saved in directory "logs"
#runWorld(world.definition,100, world.name="log/test")


# Generate data for 20 epochs x 8 worlds
# Result is saved in directory "logs"
# Takes some time (~1 minute for 20 epochs at 4GHz)
registerDoFuture()
plan(multiprocess)


runWorlds(world.definition,20,c(1:8),world.name = "logs/recommend_global")

plan(sequential)

# Gather generated logs. 
# Result is saved in directory "data".
gatherAndSaveHistory("logs/recommend_global","data/recommend_global")

#...use analysis.R to load and vizualize data
