#
# Agent-based publics.
#
# This script runs the scenario: dissemination (aka disseminate_global)
#


source("worldfunctions.R")

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
  
  "play",  "mediateMessages", list("disseminate","global"),
  
  "play",  "accessMessages",  list("closest"),
  "play",  "perceiveMessages",list("directed"),
  
  "play",  "buildRelations",list(turns = 1,epochs = 100),
  
  
  "log",  "generateLogs",list(),
  "log",  "saveHistory",list(),
  "log",  "pruneHistory",list(),
  "log",  "savePresent",list()
)


# Generate data for 20 epochs x 8 worlds
# Result is saved in directory "logs"
registerDoFuture()
plan(multiprocess)


runWorlds(world.definition,20,c(1:8),world.name = "logs/disseminate_global")

plan(sequential)

# Gather generated logs. 
# Result is saved in directory "data".
gatherAndSaveHistory("logs/disseminate_global","data/disseminate_global")

#...use analysis.R to load and vizualize data
