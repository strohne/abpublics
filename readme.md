# Agent-based publics

This repository contains scripts to run an agent-based simulation model in R. 
The model implements communication rules based on the theory of unclear publicness (Jünger 2018, Unklare Öffentlichkeit. Wiesbaden: Springer VS).
The simulation is used to analyze the structure of resulting publics, answering the research question: Why aren't there any filter bubbles?

- scenario_disseminate_global.R models global diffusion of messages.
- scenario_disseminate_local.R models local diffusion of messages.
- scenario_recommend_global.R models global diffusion by collaborative filtering (recommender systems).
- scenario_recommend_local.R models models local bounded diffusion by collaborative filtering (recommender systems).

After running one of the scenarios, you find the data in the subfolders "logs" and "data". Use analyze.R to vizualize the results.


