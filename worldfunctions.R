#
# Agent-based publics.
#
# This script contains all functions to run and analyze the model.
#
# Sections:
# - Helper: Join data and calculate distances.
# - Clear world: Clear the world.
# - Populate world: Spawn the agents.
# - Run agents: Move agents and create messages.
# - Run world: Start the simulation.
# - Analysis: Aggregate data.
# - Summarize: Calculate statistics.
# - Plot: Plot playground and networks.
# - Save & load: Manage the logs and load the data.

#
# Packages ----
#

library(tidyverse)
library(magrittr)
library(stringr)
library(skimr)

library(data.table)

library(ggplot2)
library(ggforce)
theme_set(theme_bw())

library(igraph) 
library(ggraph)
library(tidygraph)
library(qlcMatrix)


library(doFuture)
library(foreach)
#library(doSNOW)
#library(future.apply)

library(tictoc)

#
# Helper functions ----
#

# @by named vector
updateJoin <- function(x,y,by,update = NA) {
  if (is.na(update)) {
    update <-  names(y)[names(y) != by]
    names(update) <- update
  }

  x.old <- anti_join(x,y,by=by)

  y.rename <- setNames(c(names(by),names(update)), c(by,update))
  y.keep <- c(names(by),names(update))
  y.new <- y %>%
    select(c(update,by)) %>% 
    plyr::rename(y.rename) %>% 
    select(y.keep)
  
  x.by <- names(by)
  x.update <- names(update)
  x.update <- x.update[x.update %in% names(x)]
  x.new <- x %>% 
    select(-x.update) %>% 
    inner_join(y.new,by=x.by)

  bind_rows(x.old,x.new)
  
}


pairwise_count <- function (tbl, item, feature, ...)
{

  # Recursivley process grouped data_frame
  if (inherits(tbl, "grouped_df")) {
    
    # Empty
    if (nrow(tbl) == 0) {
      ret <- select(tbl,dplyr::group_vars(tbl)) %>% 
        mutate(item1=character(0),item2=character(0),value=numeric(0))
    } 
    
    else {
      ret <- tbl %>% tidyr::nest(..data = c(!! enquo(item), !! enquo(feature))) %>% 
        mutate(..data = purrr::map(..data,pairwise_count,!! enquo(item),!! enquo(feature))) %>% 
        tidyr::unnest(..data) 
    }
    
    ret <- ret  %>%
      group_by(.dots = dplyr::groups(tbl))
    
    return(ret)
  }
  
  # Empty
  if (nrow(tbl) == 0) {
    ret <- tibble(item1=character(0),item2=character(0),value=numeric(0))
    return(ret)
  } 
  
  
  # Quote input
  item <- enquo(item) #enquo(item)
  feature <- enquo(feature) #enquo(feature)
  
  # Distinct
  tbl <- tbl %>% 
    distinct(!!item, !!feature, .keep_all = TRUE)
  
  # Create sparse matrix
  row_col <- quo_name(item)
  column_col <- quo_name(feature)
  
  row_names <- tbl[[row_col]]
  col_names <- tbl[[column_col]]
  values <- 1
  
  row_u <- unique(row_names)
  i <- match(row_names, row_u)
  
  col_u <- unique(col_names)
  j <- match(col_names, col_u)
  
  mat <- Matrix::sparseMatrix(i = i, j = j, x = values, dimnames = list(row_u,col_u), ...)
  
  # Matrix multiplication
  t <- selectMethod("t", class(mat))
  mat <-  mat %*% t(mat)
  
  # Reshape to data_frame
  ret <- reshape2::melt(as.matrix(mat),varnames = c("item1", "item2"), as.is = TRUE) %>% 
    tbl_df() %>%
    rename(n = value) %>% 
    filter(item1 != item2)
  
  ret
  
}

pairwise_similarity <- function (tbl, item, feature, ...)
{
  # Recursivley process grouped data_frame
  if (inherits(tbl, "grouped_df")) {

    # Empty
    if (nrow(tbl) == 0) {
      ret <- select(tbl,dplyr::group_vars(tbl)) %>% 
        mutate(item1=character(0),item2=character(0),value=numeric(0))
    } 
    

    
    else {
      ret <- tbl %>% tidyr::nest(..data=c(!! enquo(item), !! enquo(feature))) %>% 
        mutate(..data = purrr::map(..data,pairwise_similarity,!! enquo(item),!! enquo(feature))) %>% 
        tidyr::unnest(..data)
        
    }
    
    ret <- ret %>% 
      group_by(.dots = dplyr::groups(tbl))
    
    # # Empty
    # if (nrow(tbl) == 0) {
    #   ret <- tibble(item1=character(0),item2=character(0),value=numeric(0))
    # }
    
    return(ret)
  }
  
  
  # Empty
  if (nrow(tbl) == 0) {
    ret <- tibble(item1=character(0),item2=character(0),value=numeric(0))
    return(ret)
  } 
  
  
  # Quote input
  item <- enquo(item) #enquo(item)
  feature <- enquo(feature) #enquo(feature)

  
  # Distinct
  tbl <- tbl %>% 
    distinct(!!item, !!feature, .keep_all = TRUE)
  
  # Create sparse matrix
  row_col <- quo_name(feature)
  column_col <- quo_name(item)
  
  row_names <- tbl[[row_col]]
  col_names <- tbl[[column_col]]
  values <- 1
  
  row_u <- unique(row_names)
  i <- match(row_names, row_u)
  
  col_u <- unique(col_names)
  j <- match(col_names, col_u)
  
  mat <- Matrix::sparseMatrix(i = i, j = j, x = values, dimnames = list(row_u,col_u), ...)
  mat <- cosSparse(mat)

  # Reshape to data_frame
  ret <- reshape2::melt(as.matrix(mat),varnames = c("item1", "item2"), as.is = TRUE) %>%
    tbl_df() %>%
    filter(item1 != item2)


  return(ret)

}


bindRowsKeepFactors <- function(...) {
  ## Identify all factors
  factors <- unique(unlist(
    map(list(...), ~ select_if(..., is.factor) %>% names())
  ))
  ## Bind dataframes, convert characters back to factors
  suppressWarnings(bind_rows(...)) %>% 
    mutate_at(vars(one_of(factors)), factor)  
}


# Get closest messages
# (in case of ties select random targets)
# @source tibble (e.g. with four columns: no, lat, lon, epoch)
# @targets tibble (e.g. with four columns: no, lat, lon, epoch)
# @by vector of column names to match, e.g. c('lat','lon','epoch')
# @n.closest maximum number of closest targets to select from, can be Inf. If NA value of n.max is copied
# @n.max maximum number of targets to  return, randomly choosen from selected targets, can be Inf
# @dist.max only return targets with distance lower or equal. if character: name of column. can be Inf
# @exclude name of ID column if identic source and target should be excluded
# @by.join if you want to keep only a subsegment, provide column names to feed to semi_join
# @return tibble with source, matched rows from target and distance. Target columns are prefixed with "target."

targetClosest <- function(source,targets,by=c('lat','lon','epoch'),n.max=1,n.closest=NA,dist.max="energy",exclude = NA,by.join=NA) {
  #  by=c('lat','lon')
   # n.max=Inf
   # dist.max = "energy"
   # exclude = NA
   # by.join = NA
   

  source <- as_tibble(source)
  
  # Empty source -> empty result
  if (nrow(source) == 0) {
    out <- source %>% 
      tidyr::crossing(
        targets %>% 
          mutate(dist=NA) %>% 
          rename_all(.funs = ~ paste0("target.", .))
      )
    
  }
  
  # Multiple rows in source -> handle rowwise
  else if (nrow(source) > 1) {
    
    # # Standard way
    out <- source %>%
      rowwise() %>%
      do(targetClosest(.,targets,by,n.max,n.closest,dist.max,exclude,by.join)) %>%
      ungroup()

    # # Future way
    # source.shards <- ntile(c(1:nrow(source)),nrow(source))
    # source.split <- split(source,source.shards)
    # out.split <- future_lapply(source.split,targetClosest_,targets,by,n.max,dist.max,exclude,by.join)
    # out <- do.call(bindRowsKeepFactors,out.split)
    
    # # dopar way
    # out <- foreach(source.1 = source.split,.combine=bindRowsKeepFactors) %dopar% {
    #   targetClosest(source.1,targets,by,n.max,dist.max,exclude,by.join)
    # }
    # 
    
  } 
  
  
  # Single row
  else  {
    out <- targetClosest_(source,targets,by,n.max,n.closest,dist.max,exclude,by.join)
  }    
  
  return (out)
}

# For single rows only (source must have one row!)
targetClosest_ <- function(source,targets,by=c('lat','lon','epoch'),n.max=1,n.closest=NA,dist.max="energy",exclude = NA,by.join=NA) {  
  # Prepare params
  if (is.na(n.closest))
    n.closest <- n.max
    
  # Exclude records with matching criterium (ID)
  if (!is.na(exclude))
    targets <- targets[targets[[exclude]] != unlist(source[1,exclude]),]

  # Exclude records not matching criteria
  if (!is.na(by.join))
    targets <- targets %>% 
      semi_join(source,by=by.join)
  
  # Select maximum distance
  if (is.character(dist.max)) {
    radius.source = unlist(source[1,dist.max])
  }   else {
    radius.source = dist.max
  }  
  
  # Calculate distance to every target  
  v.source <- unlist(source[1,by])
  if (length(v.source)) {
    m.target <- t(as.matrix(targets[,by]))
    targets$dist <- colSums((m.target - v.source) ^ 2) ^ 0.5
    
    if (is.character(dist.max)) {
      targets$dist <- (targets$dist - unlist(targets[,dist.max]))
    }
    
  } else {
    targets <- targets %>% 
      mutate(dist = Inf)
  }  
  
  # Keep only targets in distance
  targets <-   targets %>% 
    arrange(dist) %>% 
    filter(dplyr::row_number() <= min(n.closest,nrow(.)),
           dist <= radius.source)
  
  colnames(targets) <- paste0("target.", colnames(targets))
  
  # Break ties
  if (nrow(targets) > n.max) {
    targets <- sample_n(targets,n.max)
  }
  
  # Output
  if (nrow(targets) == 0) {
    out <- tidyr::crossing(source, targets)
  } else {
    out <- cbind(source, targets)
  } 
    
  
  return(out)
}


# Assign one random message from targets to each source
# TODO: n.max
targetRandom <- function(source,targets,n.max = 1,exclude = NA) {
  if (nrow(targets) > 0) {
    source %>% 
      bind_cols(
        sample_n(targets,nrow(source),replace=T) %>% 
         rename_all(.funs = ~ paste0("target.", .))
      )
  }
  else {
    source %>% 
      tidyr::crossing(
        targets %>% 
          rename_all(.funs = ~ paste0("target.", .))
      )
  }
}

targetDirected <- function(source,targets,ag.agents,ag.relations) {

  attention.public <- source %>%
    filter(is.na(public) | (public==T)) %>%
    targetClosest(targets,by = c("lat","lon"),n.max=1,dist.max = Inf)
  
  # Select friends
  attention.nonpublic <- source %>%
    filter(public==F) %>%
    left_join(select(ag.relations,-epoch),by=c("world.no","agent_no"="source_no")) %>% 
    filter(distance == 0) %>% 
    select(-distance) %>% 
    rename(target.agent_no = target_no)
  
  # Get perceiver only
  attention.nonpublic <- attention.nonpublic %>% 
    semi_join(targets,by=c("target.agent_no"="agent_no"))
  
  # Calculate attention distance
  attention.nonpublic <- attention.nonpublic %>% 
    left_join(
      ag.agents %>% 
        select(world.no,agent_no=no,lat,lon,energy) %>% 
        rename_all(.funs = ~ paste0("target.", .)),
      by=c("world.no"="target.world.no","target.agent_no")
    )
  
  if (nrow(attention.nonpublic)) {
    attention.nonpublic <- attention.nonpublic %>% 
      mutate(target.dist = rowSums((
        select(.,lat,lon) - select(.,target.lat,target.lon)) ^ 2) ^ 0.5) 
  }  
  else {
    attention.nonpublic <- attention.nonpublic %>% 
      mutate(target.dist = NA)
  }  
  
  
  # Select closest
  attention.nonpublic <- attention.nonpublic %>%
    group_by(world.no,action_no) %>% 
    top_n(-1,target.dist) %>% 
    ungroup()
  
  attention.new <- bind_rows(attention.public,attention.nonpublic)
  attention.new
}

targetAll <- function(source,targets) {
  source %>% 
    tidyr::crossing(
      targets %>% 
        rename_all(.funs = ~ paste0("target.", .))
      
    )
}


# Only keep attention records with messages that were accessed  
semiJoinAccessed <- function(perceived,com.attention) {
  accessed <- com.attention %>%
    filter(perceiver_relation == "accessed") %>%
    semi_join(perceived,by=c("world.no","epoch","message_no","perceiver_no"))
  
  perceived %>%
    semi_join(accessed,by=c("world.no","epoch","message_no","perceiver_no"))
}

# Filter attention records with messages that were accessed and perceived
filterAccessedAndPerceived <- function(com.attention) {
  com.attention %>%
    filter(perceiver_relation=="perceived") %>% 
    semiJoinAccessed(com.attention)
}

# Gets messages from own history, 
# i.e. formerly created or successfully perceived messages
# @source data_frame with at least world.no and author_no columns
# @types = c("created","perceived","accessed")
targetOwn <- function(source,types = c("created","perceived")) {
  
  # ...from formerly self created
  if ("created" %in% types) {
  content.pool.created <- com.messages %>%
    select(world.no,author_no,message_no=no,content_no) %>%
    distinct() %>% 
    semi_join(source,by=c("world.no","author_no")) 
  } else 
    content.pool.created <- tibble()
    
  
  # ...from formerly perceived & accessed
  if ("perceived" %in% types) {
    content.pool.perceived <- com.attention %>%
      filter(perceiver_relation == "perceived") %>%
      semi_join(source,by=c("world.no","perceiver_no"="author_no")) %>% 
      semiJoinAccessed(com.attention) %>% 
      select(world.no,author_no=perceiver_no,message_no,content_no,public)

  } else 
    content.pool.perceived <- tibble()    
  
  # ...from formerly accessed
  if ("accessed" %in% types) {
    content.pool.accessed <- com.attention %>%
      filter(perceiver_relation == "accessed") %>%
      semi_join(source,by=c("world.no","perceiver_no"="author_no")) %>% 
      select(world.no,author_no=perceiver_no,message_no,content_no,public)
    
  } else 
    content.pool.accessed <- tibble()    
  
  # bind, sample and return
  content.pool <- bind_rows(content.pool.created,content.pool.perceived,content.pool.accessed) %>% 
    rename_all(.funs = ~ paste0("target.", .))
  
  source %>% 
    inner_join(content.pool,by=c("world.no"="target.world.no","author_no"="target.author_no")) %>%     
    group_by(world.no,author_no) %>%
    sample_n(1) %>% 
    ungroup()
}

# Adds a column denoting whether the relation is public or not
# Actor numbers are compared to ag.relations
# @data input tibble
# @source_col, @target_col numbers of actors
addDistance <- function(data,ag.relations,source_col,target_col) {
  
  # Capture arguments
  source_col <- substitute(source_col)
  target_col <- substitute(target_col)

  by <- c("world.no","source_no","target_no")
  names(by) <- c("world.no",source_col,target_col)

  data %>% 
    left_join(select(ag.relations,world.no,source_no,target_no,distance),by=by) %>%
    mutate(public = is.na(distance) | distance > 0)  
}

setTypeOrder <- function(df,col) {
  typelevels <- c("0_0_0","0_0_1","0_1_0","1_0_0",
                  "0_1_1","1_0_1","1_1_0","1_1_1")
  df %>%
    mutate_at(vars(col),function(x) {factor(x,levels=typelevels)})
}


setPerceivedOrder <- function(df,col = "perceiver_relation") {
  typelevels <- c("addressed","accessed","perceived")
  df %>%
    mutate_at(vars(col),function(x) {factor(x,levels=typelevels)})
}

meanFun <- function(x) {
  ret <- data.frame(y =max(x)+(max(x)/5), 
                    label = paste0("Ø ",round(mean(x),2)))
  return(ret)
}

# Calculate start and end points of segments for ggplot
# (shorten segment and offset segment)
segementsOffset <- function(data, shorten.start, shorten.end, offset){
  
  data$dx = data$xend - data$x
  data$dy = data$yend - data$y
  data$dist = sqrt( data$dx^2 + data$dy^2 )
  data$px = data$dx/data$dist
  data$py = data$dy/data$dist
  
  data$x = data$x + data$px * shorten.start
  data$y = data$y + data$py * shorten.start
  data$xend = data$xend - data$px * shorten.end
  data$yend = data$yend - data$py * shorten.end
  data$x = data$x - data$py * offset
  data$xend = data$xend - data$py * offset
  data$y = data$y + data$px * offset
  data$yend = data$yend + data$px * offset
  
  return(data)
}


# Mean transitivity of a whole graph

graph.transitivity <- function(gr) {
  gr <- gr %>%
    activate(nodes) %>%
    mutate(tr = local_transitivity() ) %>%
    as_tibble()
  
  mean(gr$tr)
  
}


# Unpack data to global environment
dataToEnvironment <- function(data) {
  data <- data %>% 
    group_by(name) %>% 
    summarize(value = list(bind_rows(value))) %>% 
    ungroup()
  
  for (n in data$name) {
    v <- filter(data,name==n)$value[[1]]
    assign(n,v,envir = .GlobalEnv)
  }
}

#
# Clear world ----
#

clearWorld <- function( width = 100,height= 100,epoch = 0) {

  world.epoch <<- epoch
  world.state <<- tribble(
    ~world.no,  ~property,  ~value,
    world.no,   "width",    width,
    world.no,   "height",   height,
    world.no,   "margin",   0.1 * height
  )  

}

getWorldNo <- function() {
  world.no
}
  
getEpoch <- function() {
  world.epoch  
}

getState <- function(name) {
  world.state[world.state$property == name,]$value[[1]]
}

setState <- function(name,value) {
  world.state[world.state$property == name,"value"] <<- value
}


# Actors
clearAgents <- function () {
  
  ag.roles <<- tibble(
    world.no = numeric(0),
    name = character(0)
  )
  
  ag.agents <<- tibble(
    world.no = numeric(0),
    
    # Who
    no = numeric(0),
    role = character(0),
    
    # Where
    lat = numeric(0),
    lon = numeric(0),
    
    # Traits (named vector with names 'disclose' or 'perceive')
    #traits =  numeric(0),
    
    #Energy
    #energy = numeric(0)
  )
  
}

clearRelations <- function() {
  # Social distance
  ag.relations <<- tibble(
    world.no = numeric(0),
    epoch.start = numeric(0),
    epoch = numeric(0),       #=epoch.end
    source_no = numeric(0),
    target_no = numeric(0),
    distance = numeric(0)
  )
  
  ag.relations.active <<- ag.relations
  ag.graph <<- graph_from_edgelist(as.matrix(ag.relations.active[,c("source_no","target_no")]),F)
  
}

clearActions <- function () {
  # Actions
  com.actions <<- tibble(
    world.no = numeric(0),
    no = numeric(0),
    
    #Who (author, recipient, mediator)
    agent_no = numeric(0),
    
    #What (disclose, replicate, mediate, perceive...)
    action = character(0),
    how = character(0),
    energy = numeric(0),
    
    #Where & when
    lat = numeric(0),
    lon = numeric(0),
    epoch = numeric(0),
    
    #What
    message_no = numeric(0)
  )
  
  # Messages
  com.messages <<- tibble(
    world.no = numeric(0),
    no = numeric(0),
  
    # By whom
    author_no = numeric(0),
    
    # Where (time & space)
    epoch = numeric(0),
    lat = numeric(0),
    lon = numeric(0),
    
    
    # What (topic / proposition)
    content_no = numeric(0),
    
    # How (mode = energy)
    energy = numeric(0)
  )

  # Communication
  com.attention <<- tibble(
    world.no = numeric(0),
    epoch = numeric(0),
    
    no = numeric(0),
    
    #What
    message_no = numeric(0),
    content_no = numeric(0),
    
    #Whom (addressee, accessee, recipient)
    perceiver_no = numeric(0),
    perceiver_relation = character(0),
    public = logical(0)
  )  
}

clearLogs <- function() {
  rm(list = ls(pattern = "^log\\."),envir = .GlobalEnv)
  
  log.likelihood <<- tibble()
  log.cumulation <<- tibble()
  log.reach <<- tibble()
  log.dynamic <<- tibble()
  
  log.ambiguity <<- tibble()
  log.complexity <<- tibble()
  log.congruency <<- tibble()
  
  
  log.contentnetwork <<- tibble()   
  log.distancenetwork <<- tibble()
  
}

resetWorld <- function() {
  clearWorld()
  clearRelations()
  clearAgents()
  clearActions()  
  clearLogs()
}


#
# Populate world ----
#

# Create roles with default or given traits and set energy
addRole <- function(name,traits,energy) {
  
  # Traits
  defaults <- list(disclose.new=0,mediate.copy=0,perceive=0)
  traits <- modifyList(defaults,as.list(traits))
  names(traits) <- paste0("trait.",names(traits))
  
  # role.new <- tibble(name=name)  %>% 
  #   mutate(traits=list(traits)) %>% 
  #   mutate(traits = map(traits, ~ data.frame(t(.))))  %>% 
  #   unnest(traits)

  role.new <- tibble(name=name)  %>% 
    mutate(traits=list(traits))
  
  # Energy
  role.new <- role.new %>% 
    mutate(world.no = getWorldNo()) %>% 
    mutate(energy.low = energy['low'],
           energy.high = energy['high'])
  
  # Update roles
  ag.roles <<- bind_rows(ag.roles,role.new)
}

# Create n agents at random place with random traits
spawnAgents <- function(n, role = "default") {
  
  if (n > 0) {
    agents.new <- tibble(
        world.no = getWorldNo(),
        
        #Who
        no=c(1:n),
        
        #Where
        lat = sample(c(1:getState("width")),n),
        lon = sample(c(1:getState("height")),n)
    ) 
    
    agents.new <- agents.new %>% 
      mutate(no = dplyr::row_number() + max(c(ag.agents$no,0)))
    
    # Traits
    agents.new <- agents.new %>% 
      tidyr::crossing(filter(select(ag.roles,-world.no),name==role))   %>% 
      rename(role = "name")
    
    # Energy
    agents.new <- agents.new %>%
      rowwise() %>% 
      mutate(energy = sample(c(energy.low:energy.high),1,replace=T)) %>%
      ungroup() %>% 
      select(-energy.low,-energy.high)
    
    ag.agents <<- ag.agents %>%
      bind_rows(agents.new)
  } 
  else if (n < 0) {
    keep <- max(nrow(ag.agents) + n,0)
    ag.agents <<- ag.agents %>% 
      sample_n(keep)
      
  }
  
  ag.graph <<- graph_from_data_frame(as.matrix(ag.relations.active[,c("source_no","target_no")]),T,ag.agents$no)
  
}

spawnRelations <- function(density = 0.5, distance= 0) {
  
  #density = 0.2
  # ag.relations <<- ag.relations %>% 
  #   filter(F) %>% 
  #   select(source_no,target_no, distance)
  
  n <- round(density * (((nrow(ag.agents) ^ 2) - nrow(ag.agents)) ))

  while (n > nrow(ag.relations)) {
    # Sample n relations
    relations.new1 <- tibble(
        source_no =  sample(ag.agents$no,n,replace = T),
        target_no =  sample(ag.agents$no,n,replace = T),
        distance = distance
      )
    
    # Reverse relations
    relations.new2 <- relations.new1 %>% 
      select(source_no=target_no,target_no=source_no,distance) 
    
    # Combine
    relations.new <- bind_rows(relations.new1,relations.new2) %>% 
      filter(source_no!=target_no) %>% 
      distinct() %>% 
      filter(dplyr::row_number() <= n) %>% 
      mutate(world.no =  getWorldNo(),epoch.start= getEpoch(),epoch = Inf)
      
    ag.relations <<- ag.relations %>% 
      bind_rows(relations.new) 
  }
  
  ag.relations.active <<- filter(ag.relations,epoch >= getEpoch()-1)
  ag.graph <<- graph_from_data_frame(as.matrix(ag.relations.active[,c("source_no","target_no")]),T,ag.agents$no)

  # relations <<- relations %>% 
  #   left_join(select(agents,source_no=no,source_lat=lat,source_lon=lon),by=c("source_no")) %>%
  #   left_join(select(agents,target_no=no,target_lat=lat,target_lon=lon),by=c("target_no"))  

}


# setRoles <- function(name,n) {
#   #disclose = 100,replicate = 100, mediate = 100,perceive = 100,nothing = 100
#   # n=15
#   
#   # Traits
#   role <- ag.agents %>% 
#     filter(is.na(role)) %>% 
#     select(no)  %>% 
#     slice(1:n)   %>% 
#     crossing(filter(ag.roles,name==name))   %>% 
#     rename(role = "name")
# 
#   # Energy
#   role <- role %>%
#     rowwise() %>% 
#     mutate(energy = sample(c(energy.low:energy.high),1,replace=T)) %>%
#     #mutate(energy = 10) %>% 
#     ungroup() %>% 
#     select(-energy.low,-energy.high)
#   
#   # Update agents
#   ag.agents <<- ag.agents %>%
#     updateJoin(role,by=c("no"="no"))
# 
# }



#
# Run agents ----
#

# Choose which action comes next for every agent
# disclose, mediate, perceive
chooseActions <- function() {

    # Create actions
  actions.new <- ag.agents %>%
    rowwise() %>% 
    mutate(action = sample(names(traits),1,prob=traits)) %>% 
    ungroup() %>% 
    mutate(action = str_replace(action,"^trait\\.","")) %>% 
    separate(action,into=c("action","how"),sep="\\.",fill="right") %>% 
    mutate(epoch = getEpoch()) %>% 
    select(agent_no = no,action,how,energy,lat,lon,epoch) %>% 
    mutate(no = nrow(com.actions) + dplyr::row_number()) %>% 
    mutate(world.no =  getWorldNo())
  
  # Return
  com.actions <<- com.actions %>%
    bind_rows(actions.new)
  
}

moveAgents <- function(mode = "random",amount = 1,iterations=1) {
  if (mode == "friends") {
      #ag.graph <<- graph_from_data_frame(as.matrix(ag.relations[,c("source_no","target_no")]),F,ag.agents$no)  
      #iterations=1
      #amount=1
      ag.coords.old <- as.matrix(ag.agents[,c("lat","lon")])
      ag.coords.new <- layout_with_fr(ag.graph,ag.coords.old,
                                      start.temp=amount,niter=iterations,
                                      grid="grid",
                                      minx=rep(1,nrow(ag.agents)),
                                      miny=rep(1,nrow(ag.agents)),
                                      maxx=rep(getState("width"),nrow(ag.agents)),
                                      maxy=rep(getState("height"),nrow(ag.agents)))
      
      colnames(ag.coords.new) <- c("lat.new","lon.new")
      ag.coords.new <- as_tibble(ag.coords.new)
      
      # Update positions (except for mediators)
      ag.agents <<- ag.agents %>% 
        bind_cols(ag.coords.new) %>% 
        mutate(lat = ifelse(role != "mediator",lat.new,lat),
               lon = ifelse(role != "mediator",lon.new,lon)) %>% 
        select(-lat.new,-lon.new)
  }

  else {
    ag.agents <<- ag.agents %>% 
      mutate(lat = lat + sample(c(-amount:amount),nrow(ag.agents),replace = T)) %>% 
      mutate(lon = lon + sample(c(-amount:amount),nrow(ag.agents),replace = T)) %>%
      mutate(lat = pmax(0,lat),lat = pmin(getState("width"),lat)) %>% 
      mutate(lon = pmax(0,lon),lon = pmin(getState("height"),lon)) 

  }
}


# For every disclosing agent create a message at her/his location
createMessages <- function() {

  # Create new messages from actions (public = NA)
  messages.created <- com.actions %>%
    filter(action == "disclose",how=="create",epoch == getEpoch(),world.no==world.no) %>% 
    rename(action_no = no, author_no = agent_no) %>% 
    mutate(content_no = dplyr::row_number() + max(c(com.messages$content_no,0))) %>% 
    mutate(public=NA,origin_message_no = NA) %>% 
    select(world.no,author_no,content_no,lat,lon,epoch,energy,public,origin_message_no,action_no)

  # Replicate messages (public = T | F)
  messages.replicated <- com.actions %>%
    filter(action == "disclose",how=="replicate",epoch == getEpoch(),world.no==world.no) %>% 
    rename(action_no = no, author_no = agent_no) %>% 
    targetOwn() %>% 
    select(world.no,author_no,content_no=target.content_no,public=target.public,origin_message_no=target.message_no,
           lat,lon,epoch,energy,action_no) 
    
  messages.new <- bind_rows(messages.created,messages.replicated) %>% 
    mutate(no =  dplyr::row_number() + max(c(com.messages$no,0),na.rm=T)) 
  

  # Update actions
  com.actions <<- com.actions %>% 
    updateJoin(messages.new,by=c("no"="action_no","world.no"="world.no"),update=c("message_no"="no"))

  # Update messages
  com.messages <<- com.messages %>%
    bind_rows(select(messages.new,-action_no))
}

addressMessages <- function(type = "closest") {

  # Source (discloser)
  source <- com.actions %>%
    filter(action =="disclose",!is.na(message_no),epoch == getEpoch()) %>% 
    left_join(select(com.messages,world.no,no,public),by=c("world.no","message_no"="no")) %>% 
    distinct() %>% 
    rename(action_no = no)
  
  # Targets (perceiver)
  targets <- com.actions %>%
    filter(action =="perceive",epoch == getEpoch()) %>% 
    select(agent_no,lat,lon,energy)

  # Link (communication)
  if (type == "closest") {
    attention.new <- source %>%
      targetClosest(targets,by = c("lat","lon"),n.max=1,dist.max = Inf) #, exclude = "agent_no"
  } 
  else if (type == "directed") {
    attention.new <- source %>%
      targetDirected(targets,ag.agents,ag.relations.active)
  }
  else {
    attention.new <- source %>%
      targetRandom(targets) #,exclude = "agent_no"
  }
  
  # Add distance, content, relation, no
  attention.new <- attention.new %>% 
    addDistance(ag.relations.active,agent_no,target.agent_no) %>%
    left_join(select(com.messages,world.no,message_no=no,content_no),by=c("world.no","message_no")) %>% 
    distinct() %>% 
    select(world.no,action_no,message_no,content_no,perceiver_no=target.agent_no,epoch,public) %>% 
    mutate(perceiver_relation = "addressed") %>%
    mutate(no =  dplyr::row_number() + max(c(com.attention$no,0))) 
    
  
  # Update communication
  com.attention <<- com.attention %>% 
    bind_rows(attention.new)
}


#
# @choose.message = c("closest","reach")
# @type = c("dissemninate","recommend")
# @pool = c("global","local")
# @perceivers.count how many perceivers will be adressed
#
mediateMessages <- function(type="disseminate", pool ="local", choose.message = "closest",perceivers.count=10, memory = 100) {
  # Mediator
  mediator <- com.actions %>%
    filter(action =="mediate",epoch == getEpoch()) %>% 
    rename(mediator_no = agent_no,action_no=no) %>% 
    select(-message_no)
  

  # Mediate: disseminate local or global content
  if (type == "disseminate") {

      # Target perceiver: Ten random perceiver in distance
      perceivers <- com.actions %>%
        filter(action =="perceive",epoch == getEpoch()) %>% 
        select(world.no,agent_no,lat,lon,energy)
      
      perceivers.target <- mediator %>%
        targetClosest(perceivers,by = c("lat","lon"),n.max = perceivers.count,n.closest=Inf,dist.max = "energy")
     
    
      # Target message: One random public message in distance
      messages <- com.messages %>%
        filter(epoch == getEpoch(),public == TRUE) %>% 
        distinct(world.no,no,.keep_all = T)
      
      if (pool=="local") {
        messages.target <- mediator %>%
          targetClosest(messages,by = c("lat","lon"),n.max = 1,n.closest=Inf,dist.max = "energy")
      } else {
        messages.target <- mediator %>%
          targetRandom(messages,n.max=1)        
      }

      # Create new message
      messages.new <- messages.target %>% 
        select(world.no,epoch,author_no = mediator_no,content_no=target.content_no,public=target.public,origin_message_no=target.no,action_no) %>% 
        inner_join(select(perceivers.target,world.no,action_no,lat=target.lat,lon=target.lon),by=c("world.no","action_no")) %>% 
        mutate(energy=1) %>% 
        mutate(no =  dplyr::row_number() + max(c(com.messages$no,0),na.rm=T)) 
        
      # Create new attention (address)
      attention.new <- perceivers.target %>% 
        inner_join(select(messages.new,world.no,action_no,message_no=no,content_no),by=c("world.no","action_no")) %>% 
        addDistance(ag.relations.active,mediator_no,target.agent_no) %>%
        distinct() %>% 
        select(world.no,action_no,message_no,content_no,perceiver_no=target.agent_no,epoch,public) %>% 
        mutate(perceiver_relation = "addressed") %>%
        mutate(no =  dplyr::row_number() + max(c(com.attention$no,0))) 
      
      # Update actions
      com.actions <<- com.actions %>% 
        updateJoin(messages.new,by=c("no"="action_no"),update=c("message_no"="no"))

      # Update messages
      com.messages <<- com.messages %>%
        bind_rows(select(messages.new,-action_no))        

      # Update communication
      com.attention <<- com.attention %>% 
        bind_rows(attention.new)
  }
  
  # Recommendation on item-item similarity
  # commonly used in practice, e.g. by YouTube or LinkedId
  # see https://towardsdatascience.com/recommender-systems-in-practice-cef9033bb23a 
  
  else if (type == "recommend") {
   
    # Target perceiver: One random perceiver in distance
    perceivers <- com.actions %>%
      filter(action =="perceive",epoch == getEpoch()) %>% 
      select(world.no,agent_no,lat,lon,energy)

    perceiver.target <- mediator %>%
      targetClosest(perceivers,by = c("lat","lon"),n.max = perceivers.count,n.closest=Inf,dist.max = "energy")


    if (pool == "local") {
      perceiver.source <- mediator %>%
        targetClosest(perceivers,by = c("lat","lon"),n.max = Inf,dist.max = "energy")
    } else {
      perceiver.source <- mediator %>%
        targetAll(perceivers)
    }
    
    #memory=100
    # Source (content)
    perceiver.content <- com.attention %>%
      filter(public == TRUE, epoch >= getEpoch() - memory) %>% 
      semi_join(perceivers,by=c("world.no"="world.no","perceiver_no"="agent_no")) %>%
      filterAccessedAndPerceived()
    
    # Add mediators
    perceiver.content <- perceiver.content %>% 
      inner_join(select(perceiver.source,world.no,target.agent_no,mediator_no),by=c("world.no","perceiver_no"="target.agent_no"))

    # Content similarity = cosine similarity / number of perceivers who perceived both items    
    content.similarity <- perceiver.content %>% 
      group_by(world.no,mediator_no) %>% 
      pairwise_similarity(content_no, perceiver_no)  %>% 
      ungroup()  %>% 
      filter(value > 0) %>% 
      mutate(item1=as.numeric(item1),item2=as.numeric(item2))


    # Join all perceived content accessible to mediators to targets
    perceiver.content <- setNames(perceiver.content,paste0("perceived.",colnames(perceiver.content)))
    perceiver.target <- perceiver.target %>% 
      inner_join(perceiver.content,by=c("world.no"="perceived.world.no","mediator_no"="perceived.mediator_no","target.agent_no"="perceived.perceiver_no"))
    
    # Join similar content to targets
    content.similarity <- setNames(content.similarity,paste0("recommended.",colnames(content.similarity)))
    perceiver.target <- perceiver.target %>% 
      inner_join(content.similarity,by=c("world.no"="recommended.world.no","mediator_no"="recommended.mediator_no", "perceived.content_no"="recommended.item1")) %>% 
      rename(recommended.content_no = recommended.item2)

    # Remove already perceived
    perceiver.target <- perceiver.target %>% 
      anti_join(perceiver.content,by=c("world.no"="perceived.world.no","target.agent_no"="perceived.perceiver_no","recommended.content_no"="perceived.content_no"))

    # Create message
    content.new <- perceiver.target %>% 
      group_by(world.no,mediator_no,target.agent_no) %>% 
      top_n(1,-recommended.value) %>% 
      sample_n(1) %>% 
      ungroup() 
    
    
    messages.new <- content.new %>% 
      select(world.no,action_no,author_no=mediator_no,epoch,
             target.agent_no,lat=target.lat,lon=target.lon,
             content_no=recommended.content_no) %>% 
      mutate(energy=1,public=T) %>% 
      mutate(no =  dplyr::row_number() + max(c(com.messages$no,0),na.rm=T)) 
    
    # Update actions
    com.actions <<- com.actions %>% 
      updateJoin(messages.new,by=c("no"="action_no"),update=c("message_no"="no"))

    
    # Update messages
    com.messages <<- com.messages %>%
      bind_rows(select(messages.new,-action_no,-target.agent_no))

    # Address message
    attention.new <- messages.new %>% 
      addDistance(ag.relations.active,target.agent_no,author_no) %>% 
      select(world.no,action_no,message_no=no,content_no,perceiver_no=target.agent_no,epoch,public) %>% 
      mutate(perceiver_relation = "addressed") %>%
      mutate(no =  dplyr::row_number() + max(c(com.attention$no,0))) 
    
    # Update communication
    com.attention <<- com.attention %>% 
      bind_rows(attention.new)

  }
}

accessMessages <- function(type="closest") {
  
  # Source (perceiver)
  source <- com.actions %>%
    filter(action %in% c("perceive"),epoch == getEpoch()) %>% 
    rename(perceiver_no = agent_no,action_no=no)
  
  # Targets (messages)
  targets <- com.messages %>%
    filter(epoch == getEpoch()) %>%
    select(message_no=no,lat,lon,epoch,energy,author_no,content_no)
  
  # Link (communication)
  if (type == "closest") {
    attention.new <- source %>%
      targetClosest(targets,by = c("lat","lon"),n.max = Inf)
  } 
  else if (type=="all") {
    attention.new <- source %>%
      targetAll(targets)
  }
  else {
    attention.new <- source %>%
      targetRandom(targets)
  }
  
  attention.new <- attention.new %>% 
    left_join(select(ag.relations.active,world.no,perceiver_no=source_no,target.author_no=target_no,distance),by=c("world.no","perceiver_no","target.author_no")) %>% 
    mutate(public = is.na(distance)) %>% 
    select(world.no,action_no,message_no=target.message_no,content_no=target.content_no,perceiver_no,epoch,public) %>% 
    mutate(perceiver_relation = "accessed") %>% 
    mutate(no =  dplyr::row_number() + max(c(com.attention$no,0)))
  
  # Update communication
  com.attention <<- com.attention %>% 
    bind_rows(attention.new)
  
}



#
# For every perceiving agent perceive message
#
perceiveMessages <- function(type="closest") {
  # Source (perceiver)
  source <- com.actions %>%
    filter(action =="perceive",epoch == getEpoch()) %>% 
    rename(perceiver_no = agent_no,action_no=no)
  
  # Targets (messages)
  targets <- com.messages %>%
    filter(epoch == getEpoch()) %>%
    select(message_no=no,lat,lon,epoch,energy,author_no,content_no)
  
  # Link (communication)
  if (type == "closest") {
    attention.new <- source %>%
      targetClosest(targets,by = c("lat","lon"),n.max=1,dist.max = Inf)
  } 
  else if (type == "directed") {
    addressed <- com.attention %>% 
      filter(epoch == getEpoch()) %>% 
      filter(perceiver_relation=="addressed") %>% 
      group_by(perceiver_no) %>% 
      sample_n(1) %>% 
      ungroup() %>% 
      select(target.message_no=message_no, perceiver_no)
    
    targets <- targets %>% 
      rename_all(.funs = ~ paste0("target.", .)) 
    
    attention.new <- source %>%
      inner_join(addressed,by="perceiver_no") %>% 
      inner_join(targets,by="target.message_no")
    
    
  }
  else {
    attention.new <- source %>%
      targetRandom(targets,n.max=1)
  }
  
  attention.new <- attention.new %>% 
    left_join(select(ag.relations.active,world.no,perceiver_no=source_no,target.author_no=target_no,distance),by=c("world.no","perceiver_no","target.author_no")) %>% 
    mutate(public = is.na(distance)) %>% 
    
    select(world.no,action_no,message_no=target.message_no,content_no=target.content_no,perceiver_no,epoch,public) %>% 
    mutate(perceiver_relation = "perceived") %>%
    mutate(no =  dplyr::row_number() + max(c(com.attention$no,0)))
 
  
  # Update actions
  com.actions <<- com.actions %>% 
    updateJoin(select(attention.new,-no),by=c("no"="action_no"),update=c("message_no"="message_no"))
  
  
  # Update communication
  com.attention <<- com.attention %>% 
    bind_rows(attention.new)
}

# Create relations if communication was succesful and reciprocal
# @turns:  how many succesfull communication turns are necessary for each agent 
# @epochs: how many epochs to look back (or Inf if relations shall never vanish)

buildRelations <- function(turns = 1,epochs = 100) {
  # world.epoch <- 100
  # duration = 1000
  # turns = 1
  # epochs <- Inf

  # Get attention to messages
  relations.ack <- com.attention %>%
    filter(epoch > getEpoch() - epochs) %>% 
    group_by(world.no,epoch) %>%
    count(message_no,perceiver_no,perceiver_relation) %>%
    mutate(n = as.numeric(as.logical(n))) %>%
    ungroup()
  
  # Filter successful=congruent reception
  relations.ack <- relations.ack %>% 
    group_by(world.no,epoch,message_no,perceiver_no) %>% 
    summarize(n = sum(n)) %>% 
    ungroup() %>% 
    filter(n == 3) %>% 
    select(-n)
  
  # Count successful=congruent reception between author and recipient
  relations.ack <- relations.ack %>% 
    left_join(select(com.messages,world.no,message_no=no,author_no),by=c("world.no","message_no")) %>% 
    distinct() %>% 
    count(world.no,source_no=perceiver_no,target_no=author_no) %>% 
    filter(n >= turns) %>% 
    select(-n)

  # Check reciprocity
  relations.ack <- relations.ack %>%
    mutate(distance=0) %>%
    semi_join(select(relations.ack,world.no,source_no,target_no),by=c("world.no","source_no"="target_no","target_no"="source_no"))

  # Inactive (formerly outdated)
  relations.inactive <- ag.relations %>% 
    filter(epoch < getEpoch() -1)

  # Active (not outdated yet)
  relations.active <- ag.relations %>% 
    filter(epoch >= getEpoch()-1)
  
  # Now outdated (= not acknowledged by actions in the last epochs)
  relations.out <- relations.active %>% 
    anti_join(relations.ack,by=c("world.no","source_no","target_no")) %>% 
    mutate(epoch = getEpoch()-1)
  
  # Unchanged   
  relations.stable <- relations.active %>% 
    semi_join(relations.ack,by=c("world.no","source_no","target_no")) %>% 
    mutate(epoch = getEpoch())

  # Now new
  relations.new <- relations.ack %>% 
    anti_join(relations.active,by=c("world.no","source_no","target_no")) %>% 
    mutate(epoch.start = getEpoch(), epoch = getEpoch())
  
  # Combine
  ag.relations.active <<- bind_rows(relations.stable,
                         relations.new)
  ag.relations.inactive <- bind_rows(relations.inactive,
                           relations.out)
  
  ag.relations <<- bind_rows(ag.relations.inactive,
                             ag.relations.active)

  ag.graph <<- graph_from_data_frame(as.matrix(ag.relations.active[,c("source_no","target_no")]),T,ag.agents$no)  
  
}


#
# Run world ----
#


# Run multiple rounds at once
# @world.name: Folder where data is logged
# @world.shard: prefix of logged data files

runWorld <- function(world.definition = NA, worlds.rounds = 1,world.no = 0,world.name = "unknown", world.shard = 1, setup = T) {
  # Measure time
  tic(paste0("world ",world.no))
  
  # Set global variables
  if (setup) {
    if (is.na(list(world.definition)))
      world.definition <- get("world.definition",envir=.GlobalEnv)
        
    world.no <<- world.no
    world.epoch <<- 1
    world.end <<- worlds.rounds
    world.name <<- world.name
    world.shard <<- world.shard
  
    
    # Setup
    world.phase <- filter(world.definition,phase=="setup")
    if (nrow(world.phase) > 0)
      for (n in c(1:nrow(world.phase))) {
        do.call(world.phase$method[[n]],world.phase$params[[n]])  
      }
  }  
  
  # Run
  world.phase <- filter(world.definition,phase %in% c("play","log"))
  
  for (i in c(1:worlds.rounds)) {
    
    #Pass epoch
    world.epoch <<- world.epoch + 1
    
    # Go
    for (n in c(1:nrow(world.phase))) {
      do.call(world.phase$method[[n]],world.phase$params[[n]])  
    }
  }
  
  # Log world state
  log.definition <<- world.definition %>% 
    mutate(world.no = world.no) %>% 
    mutate_at(vars(params),function(x) {as.character( lapply(x,paste0,collapse=", "))})
  
  log.state <<- world.state
  
  # Measure time
  toc()
  
  #log.computing <<- tic.log(format = TRUE)  
}

# Run parallel worlds with multiple rounds at once
# @worlds.no: vector with world numbers, each number starts one run
# @world.name: Folder where data is logged
# @world.shard: prefix of logged data files
runWorlds <- function(world.definition = NA, worlds.rounds = 1, worlds.no = 1, world.name = "unknown", world.shard = 1) {
  
  tic("total")

  par.progress <- txtProgressBar(0,max = length(worlds.no) , style = 3)
  
  # Do processing
  set.seed(42)
  worlds.data <- foreach(no=worlds.no,
                         #.options.snow = list(progress =  function(n) setTxtProgressBar(par.progress, n)),
                         .combine=bindRowsKeepFactors,
                         .packages='tidyverse') %dopar% {
                           
                           source("worldfunctions.R")   
                           runWorld(world.definition,worlds.rounds,no,world.name,world.shard)
                           
                           # Yield all data.frames
                           vars <- eapply(.GlobalEnv,is.data.frame)
                           vars <- names(vars[unlist(vars)])
                           data <- tibble(world.no = no,name = vars, value = lapply(vars,get))
                         }

  toc()
  worlds.data
}


# @epoch="last"   only generate record for current epoch and append to log
generateLogs <- function(epoch.log = "last") {

  if (epoch.log == "last") {
    current.attention <- filter(com.attention,epoch == max(epoch))
  } else {
    current.attention <- com.attention
  }
  
  log.likelihood <<- bind_rows(log.likelihood,getAttentionLikelihood(current.attention))
  log.cumulation <<- bind_rows(log.cumulation,getAttentionCumulation(current.attention))
  log.reach <<- bind_rows(log.reach,getAttentionReach(current.attention))
  log.dynamic <<- bind_rows(log.dynamic,getAttentionDynamic(current.attention))

  log.ambiguity <<- bind_rows(log.ambiguity,getAttentionAmbiguity(current.attention))
  log.complexity <<- bind_rows(log.complexity,getAttentionComplexity(current.attention))
  log.congruency <<- bind_rows(log.congruency,getAttentionCongruency(current.attention))
  
  log.contentnetwork <<- bind_rows(log.contentnetwork,getContentNetworkMetrics(current.attention))
  log.distancenetwork <<- bind_rows(log.distancenetwork,getDistanceNetworkMetrics(current.attention))
}



#
# Analysis functions ----
#

# Analyze attention cumulation
getAttentionCumulation <- function(com.attention) {
  dispersion <- com.attention  %>% 
    #left_join(select(com.messages,world.no,message_no=no,content_no),by=c("world.no","message_no")) %>% 
    mutate(epoch = max(epoch)) %>% 
    group_by(world.no,epoch,content_no,public) %>% 
    count(perceiver_relation) %>% 
    spread(perceiver_relation,n) %>% 
    #rename(perceiver_count = n) %>% 
    ungroup() 

}

# Analyze attention reach
getAttentionReach <- function(filter.attention,epochs.past = 100) {
  filter.epoch.to = max(filter.attention$epoch)
  filter.epoch.from = filter.epoch.to - epochs.past
  
  attention.filtered <- filter(com.attention,
                               epoch <= filter.epoch.to,
                               epoch >= filter.epoch.from) %>% 
    semi_join(filter.attention,by=c("world.no"))
  
  attention.filtered  %>%
    filterAccessedAndPerceived() %>% 
    mutate(epoch = max(epoch)) %>%
    distinct(world.no,epoch,content_no,perceiver_no) %>% 
    count(world.no,epoch,content_no) %>% 
    count(world.no,epoch,n) %>% 
    rename(reach=n,n=nn)

}




# Calculates whether successful perceiving actions are crossing 
# borders of publicness. The social distance between perceiver, 
# discloser/author and original creator of the message are compared.
# Open = nonpublic message gets public
# close = public message gets nonpublic
# public = public message stays public
# nonpublic = nonpublic message stays nonpublic

getAttentionDynamic <- function(com.attention) {
  constellations <- com.attention %>%
    filterAccessedAndPerceived() %>%
    rename(p_to_author = public) %>% 
    left_join(
      select(com.messages,world.no,no,author_no,a_to_origin=public,origin_message_no),
      by=c("world.no","message_no"="no")
    ) %>% 
    distinct() %>% 
    filter(!is.na(origin_message_no)) %>% 
    left_join(
      select(com.messages,world.no,no,origin_author_no=author_no),
      by=c("world.no","origin_message_no"="no")
    ) %>% 
    distinct() %>% 
    addDistance(ag.relations.active,perceiver_no,origin_author_no) %>% 
    rename(p_to_origin=public,perceiver_origin_distance=distance)
  
  constellations %>%
    mutate(epoch = max(epoch)) %>% 
    count(world.no,epoch,p_to_origin,p_to_author,a_to_origin) %>% 
    unite(type,p_to_author,p_to_origin,sep="_",remove = F) %>% 
    mutate(type = case_when(
      type == "TRUE_TRUE" ~ "public",
      type == "FALSE_FALSE" ~ "nonpublic",
      type == "FALSE_TRUE" ~ "open",
      type == "TRUE_FALSE" ~ "close"
    ))  
}

# Analyze likelihood of communication (incongruent publicness + unpublicness)
getAttentionLikelihood <- function(com.attention, relative = F) {
  
  # Count nonnull attention
  constellations.nonnull <- com.attention %>%
    group_by(world.no,epoch) %>% 
    count(message_no,perceiver_no,perceiver_relation) %>%
    mutate(n = as.numeric(as.logical(n))) %>% 
    ungroup()
  
  
  # Unite type
  if (nrow(constellations.nonnull) > 0) {
    constellations.nonnull <- constellations.nonnull %>% 
      setPerceivedOrder() %>% 
      spread(perceiver_relation,n,fill=0,drop=F) %>% 
      unite(type,addressed,accessed,perceived,remove = T) %>% 
      filter(type != "0_0_0") 
  } 
  else {
    constellations.nonnull <- constellations.nonnull %>% 
      select(-perceiver_relation,-n) %>% 
      mutate(type = NA)
  }
  
  # Count types of nonnull
  constellations.nonnull <- constellations.nonnull %>% 
    count(world.no,epoch,type)   
  
  # Calculate null relations 
  # (too expensive to calculate with complete-functions)
  constellations.null <- com.attention %>%
    group_by(world.no,epoch) %>% 
    summarize(type.total = n_distinct(message_no) * n_distinct(perceiver_no)) %>% 
    ungroup()
  
  constellations.null <- constellations.null %>% 
    left_join(
      constellations.nonnull %>%
        group_by(world.no,epoch) %>% 
        summarize(type.nonnull=sum(n)) %>% 
        ungroup()
      ,by=c("world.no","epoch")
    )
  
  constellations.null <- constellations.null %>% 
    mutate(n = type.total - type.nonnull) %>% 
    mutate(type="0_0_0") %>% 
    select(world.no,epoch, type,n)
  
  
  # Bind null and nonnull
  constellations <- bind_rows(constellations.nonnull,constellations.null)
  
  constellations <- constellations %>% 
    setTypeOrder("type") %>% 
    complete(type,fill=list(n=0)) %>% 
    group_by(world.no,epoch) %>% 
    mutate(p = n / sum(n)) %>% 
    ungroup()
  
  # Return
  constellations
}

# Analyze incongruency (incongruent publicness + unpublicness)
getAttentionCongruency <- function(com.attention, relative = F) {

  # Count nonnull attention
  com.attention.perceivers <- com.attention %>%
    distinct(world.no,epoch,message_no,perceiver_no) %>% 
    count(world.no,epoch,message_no) %>% 
    rename(total = n)

  # Count nonnull attention
  com.attention.relations <- com.attention %>%
    distinct(world.no,epoch,message_no,perceiver_no,perceiver_relation) %>% 
    count(world.no,epoch,message_no,perceiver_relation)  

  # Congruency
  com.attention.congurency <- com.attention.relations %>% 
    left_join(com.attention.perceivers,by=c("world.no","epoch","message_no")) %>% 
    mutate(congruent = as.numeric(n==total)) %>% 
    select(-n,-total)
 
  # Unite type
  com.attention.congurency <- com.attention.congurency %>% 
    setPerceivedOrder() %>% 
    spread(perceiver_relation,congruent,fill=0,drop=F) %>% 
    unite(type,addressed,accessed,perceived) 
  

  # Count types
  com.attention.congurency <- com.attention.congurency %>% 
    count(world.no,epoch,type)   

  com.attention.congurency <- com.attention.congurency %>% 
    setTypeOrder("type") %>% 
    complete(type,fill=list(n=0)) %>% 
    group_by(world.no,epoch) %>% 
    mutate(p = n / sum(n)) %>% 
    ungroup()
    
  # Return
  com.attention.congurency
}


# Analyze ambiguous publicness
getAttentionAmbiguity <- function(com.attention) {
  
  # Check publicness
  #constellations <- com.attention %>%
  #  left_join(select(com.messages,world.no,message_no=no,author_no),by=c("world.no","message_no")) 
    #left_join(select(ag.relations,world.no,author_no=source_no,perceiver_no=target_no,public=distance),by=c("world.no","author_no","perceiver_no")) %>%
    #mutate(public=as.logical(replace_na(public,1)))

  # Count attention (separated by publicness)
  constellations <- com.attention %>%
    group_by(world.no,epoch) %>%
    count(message_no,perceiver_relation,public) %>%
    complete(message_no,perceiver_relation,public,fill=list(n=0)) %>%
    mutate(n = as.numeric(as.logical(n))) %>%
    ungroup()

  # Unite type
  if (nrow(constellations) > 0) {
    constellations <- constellations %>%
      setPerceivedOrder() %>% 
      spread(perceiver_relation,n,fill=0,drop=F) %>%
      unite(type,addressed,accessed,perceived,remove = T)
  }
  else {
    constellations <- constellations %>% 
      select(-perceiver_relation,-n) %>% 
      mutate(type = NA)
  }

  # Count types
  constellations <- constellations %>%
    count(world.no,epoch,public,type) %>%
    setTypeOrder("type") %>%
    complete(public,type,fill=list(n=0))

  constellations <- constellations %>% 
    group_by(world.no,epoch,public) %>% 
    mutate(p = n / sum(n)) %>% 
    ungroup()  
  
  # Return
  constellations
}


getAttentionComplexity <- function(com.attention) {
  
  # Check publicness
  #constellations <- com.attention %>%
  #  left_join(select(com.messages,world.no,message_no=no,author_no),by=c("world.no","message_no")) 
    #left_join(select(ag.relations,world.no,author_no=source_no,perceiver_no=target_no,public=distance),by=c("world.no","author_no","perceiver_no")) %>%
    #mutate(public=as.logical(replace_na(public,1))) %>%
    #mutate(public=factor(public,c("TRUE","FALSE")))
  
  # Count attention (separated by publicness)
  constellations <- com.attention %>%
    group_by(world.no,epoch) %>%
    count(message_no,perceiver_relation,public) %>%
    mutate(public=factor(public,levels=c("TRUE","FALSE"))) %>% 
    complete(message_no,perceiver_relation,public,fill=list(n=0)) %>%
    mutate(n = as.logical(n)) %>%
    ungroup()

  # Analyze complexity

  # Unite type
  if (nrow(constellations) > 0) {
    constellations <- constellations %>% 
      spread(key=public,value=n,sep="_") %>%
      mutate(complex = as.numeric(and(public_FALSE,public_TRUE))) %>% 
      select(-public_FALSE,-public_TRUE)
    
    constellations <- constellations %>%
      setPerceivedOrder() %>% 
      spread(perceiver_relation,complex,fill=0,drop=F) %>%
      unite(type,addressed,accessed,perceived,remove = T)
  }
  else {
    constellations <- constellations %>% 
      select(-public,-n) %>%
      mutate(complex = NA) 

    constellations <- constellations %>% 
      select(-perceiver_relation,-complex) %>% 
      mutate(type = NA)
  }  
  
  # Count types
  constellations <- constellations %>%
    count(world.no,epoch,type) %>%
    setTypeOrder("type") %>%
    complete(type,fill=list(n=0))
  
  constellations <- constellations %>% 
    group_by(world.no,epoch) %>% 
    mutate(p = n / sum(n)) %>% 
    ungroup()
  
  # Return
  constellations
}  



# Co-knowledge of content
getContentNetwork <- function(filter.attention, epochs.past = 100) {

    if (max(filter.attention$epoch) == -Inf) {
    stop(paste0("No attention in world ",world.no,", epoch ",world.epoch))
  }
  
  filter.epoch.to = max(filter.attention$epoch)
  filter.epoch.from = filter.epoch.to - epochs.past

  attention.filtered <- filter(com.attention,epoch <= filter.epoch.to,epoch >= filter.epoch.from) %>% 
    semi_join(filter.attention,by=c("world.no"))
  messages.filtered <- filter(com.messages,epoch <= filter.epoch.to,epoch >= filter.epoch.from) %>% 
    semi_join(filter.attention,by=c("world.no"))
  relations.filtered <- filter(ag.relations,epoch.start <= filter.epoch.to,epoch >= filter.epoch.from) %>% 
    semi_join(filter.attention,by=c("world.no"))

  gr.perceived <- attention.filtered %>%
    filterAccessedAndPerceived() %>% 
    select(world.no,epoch,content_no,agent_no=perceiver_no)
  
  gr.diclosed <- messages.filtered %>%
    select(world.no,epoch,content_no,agent_no=author_no) %>% 
    distinct()
  
  gr.content <- bind_rows(gr.perceived,gr.diclosed) %>% 
    select(-epoch) %>% 
    group_by(world.no) %>% 
    pairwise_count(agent_no,content_no) %>% 
    filter(n > 0) %>% 
    ungroup() %>% 
    mutate(item1 = as.numeric(item1),item2 = as.numeric(item2))
  
  gr.content <- gr.content %>% 
    addDistance(relations.filtered,item1,item2) %>% 
    rename(agent1=item1,agent2=item2) %>% 
    select(-distance)
  
  gr.content %>% 
    mutate(epoch = filter.epoch.to)
}


getContentNetworkMetrics <- function(com.attention,epochs.past = 100) {
  epoch.to = max(com.attention$epoch)
  log.content <- getContentNetwork(com.attention, epochs.past)

  gr.worlds <- unique(log.content$world.no)
  
  gr.graphs.nonpublic <- lapply(gr.worlds,function(x) {
    nodes <- filter(ag.agents,world.no == x,role != "mediator") %>%  select(no)
    edges <- log.content[log.content$world.no==x,c("agent1","agent2","public")]
    edges <- semi_join(edges,nodes,by=c("agent1"="no"))
    edges <- semi_join(edges,nodes,by=c("agent2"="no"))
    tbl_graph(nodes,edges[edges$public == F,], directed = T)
  }) 

  gr.graphs.public <- lapply(gr.worlds,function(x) {
    nodes <- filter(ag.agents,world.no == x,role != "mediator") %>%  select(no)
    edges <- log.content[log.content$world.no==x,c("agent1","agent2","public")]
    edges <- semi_join(edges,nodes,by=c("agent1"="no"))
    edges <- semi_join(edges,nodes,by=c("agent2"="no"))
    tbl_graph(nodes,edges[edges$public == T,], directed = T)
  }) 
  

  gr.graphs.all <- lapply(gr.worlds,function(x) {
    nodes <- filter(ag.agents,world.no == x,role != "mediator") %>%  select(no)
    edges <- log.content[log.content$world.no==x,c("agent1","agent2","public")]
    edges <- semi_join(edges,nodes,by=c("agent1"="no"))
    edges <- semi_join(edges,nodes,by=c("agent2"="no"))
    
    tbl_graph(nodes,edges, directed = T)
  }) 
  
  
  gr.graphs <- bind_rows(
    tibble(world.no = gr.worlds,public=T,graph = gr.graphs.public),
    tibble(world.no = gr.worlds,public=F,graph = gr.graphs.nonpublic),
    tibble(world.no = gr.worlds,public=NA,graph = gr.graphs.all)
  )

  
  gr.metrics <- gr.graphs %>% 
    rowwise() %>%
    do(tibble(
      world.no = .$world.no,
      epoch = epoch.to,
      public = .$public,
      edges = with_graph(.$graph,graph_size()),
      nodes = with_graph(.$graph,graph_order()),
      density = graph.density(.$graph),
      transitivity = graph.transitivity(.$graph),
      distance = mean_distance(.$graph),
      components = count_components(.$graph)
      )) %>% 
    ungroup()

  gr.metrics

}   


# Distance network
getDistanceNetwork <- function(filter.attention, epochs.past=0) {

  if (max(filter.attention$epoch) == -Inf) {
    stop(paste0("No attention in world ",world.no,", epoch ",world.epoch))
  }
  
  filter.epoch.to = max(filter.attention$epoch)
  filter.epoch.from = filter.epoch.to - epochs.past
  
  relations.filtered <- ag.relations %>% 
    filter(epoch.start <= filter.epoch.to,epoch >= filter.epoch.from)

  relations.filtered <- relations.filtered %>% 
    semi_join(filter.attention,by="world.no")

  relations.filtered

}


getDistanceNetworkMetrics <- function(com.attention,epochs.past = 0) {
  
  
  # Get agents and relations
  relations.filtered <- getDistanceNetwork(com.attention,epochs.past)
  agents.filtered <- ag.agents %>% 
    semi_join(com.attention,by="world.no")

  gr.worlds <- unique(agents.filtered$world.no)

  # Create graphs
  gr.graphs <- lapply(gr.worlds,
    function(x) {
      edges <- relations.filtered[relations.filtered$world.no==x,c("source_no","target_no")]
      nodes <- agents.filtered[agents.filtered$world.no==x,"no"]
      as_tbl_graph(graph_from_data_frame(as.matrix(edges),T,nodes))
    })


  gr.metrics <- lapply(gr.graphs,
     function(x) {
       tibble(
         edges = with_graph(x,graph_size()),
         nodes = with_graph(x,graph_order()),
         density = graph.density(x),
         transitivity = graph.transitivity(x),
         components = count_components(x)
       )
     })

  tibble(world.no = gr.worlds,epoch=max(com.attention$epoch),gr.metrics = gr.metrics) %>%
    unnest(gr.metrics)

}


#
# Summarize ----
#

sumAttentionLikelihood <- function()  {

  log.likelihood %>% 
    group_by(type) %>% 
    summarise(n = sum(n)) %>% 
    ungroup() %>% 
    mutate(p=n/sum(n))
}

sumContentNetwork <- function() {

log.contentnetwork %>% 
    group_by(public,epoch) %>% 
    skim(edges,nodes,density,components)
  
}

sumDistanceNetwork <- function() {

  log.distancenetwork %>% 
    skim(-world.no)
  
}

#
# Plot ----
#

fieldSpace <- function(messages = F, actions = T, relations = F, 
                      attention = F,agents=F,
                      net.content.public = NA,
                      scenario = NA, world = NA,epoch = NA,
                      world.margin = 10,
                      world.width = 100,
                      world.height = 100) {
  
  cbbPaletteColor <- c('transparent','#aaaaaa','transparent','#a6761d','#666666','#1b9e77','#66a61e','#d95f02')
  palFill = c('#e41a1c','transparent','#377eb8')

  filter.scenario = ifelse(is.na(scenario),com.actions$scenario[1],scenario)
  filter.world = ifelse(is.na(world),max(com.actions$world.no,0),world)
  filter.epoch = ifelse(is.na(epoch),max(com.actions$epoch,0),epoch)
  
  pl <- ggplot()
  
  # Messages
  if (messages == T) {
    
    com.messages.coords <- com.messages %>% 
      filter(world.no == filter.world,epoch==filter.epoch,scenario == filter.scenario)
    
    pl <- pl +
      geom_circle(aes(x0=lat,y0=lon,r=energy),fill="transparent",color="black",alpha=1.0,data=com.messages.coords)
  }    

  # Relations
  if (relations == T) {
    ag.relations.coords <- ag.relations %>% 
      filter(world.no == filter.world,epoch.start <= filter.epoch,epoch >= filter.epoch,
             scenario == filter.scenario) %>%
      mutate(epoch = filter.epoch) %>% 
      left_join(select(com.actions,scenario,world.no,agent_no,lat,lon,epoch),by=c("source_no"="agent_no","scenario","world.no","epoch")) %>% 
      left_join(select(com.actions,scenario,world.no,agent_no,xend=lat,yend=lon,epoch),by=c("target_no"="agent_no","scenario","world.no","epoch"))
    
    pl <- pl + 
      geom_segment(aes(x=lat,y=lon,xend=xend,yend=yend),alpha=0.8,data=ag.relations.coords) +
      geom_point(aes(x=lat,y=lon),color="#ffffff",alpha=1.0, size=2,data=ag.relations.coords)
  }
  
  # Agents
  if (agents == T) {
    agents.filtered <- filter(ag.agents,scenario == filter.scenario,world.no == filter.world) 

    # Content network components
    if (!is.na(net.content.public)) {
      attention.filtered <- filter(com.attention,world.no == filter.world,
                                   scenario == filter.scenario)
      
      agents.graph <- getContentNetwork(attention.filtered) %>% 
        filter(public == T) %>% 
        select(agent1,agent2) %>% 
        graph_from_data_frame(directed=T,vertices=agents.filtered$no) %>% 
        as_tbl_graph() %>% 
        convert(to_undirected) %>% 
        mutate(component = as.factor(group_components())) %>% 
        as_tibble() %>% 
        mutate(no =as.numeric(name)) %>% 
        select(no,component)
      
      agents.filtered <- agents.filtered %>% 
        left_join(agents.graph,by="no")
        
      
      pl <- pl + 
        geom_circle(aes(x0=lat,y0=lon,r=energy,fill=component),color="orange",alpha=1.0,data=agents.filtered)
      
    }  
    else {
      
      pl <- pl + 
        geom_circle(aes(x0=lat,y0=lon,r=energy),fill="transparent",color="orange",alpha=1.0,data=agents.filtered)
      
    }

  }
  
  
  # Attention
  if (attention == T) {
    com.actions.filtered <- filter(com.actions,scenario == filter.scenario,world.no == filter.world,epoch==filter.epoch)
    com.messages.filtered <- filter(com.messages,scenario == filter.scenario,world.no == filter.world,epoch==filter.epoch)
    
    com.attention.coords <- com.attention %>% 
      filter(world.no == filter.world,epoch==filter.epoch,perceiver_relation != "accessed")  %>%
      select(world.no,perceiver_no,perceiver_relation,public,message_no,epoch)   %>% 
      left_join(select(com.actions.filtered,world.no,epoch,agent_no,p.lat=lat,p.lon=lon),by=c("perceiver_no"="agent_no","epoch","world.no","scenario")) %>% 
      left_join(select(com.messages.filtered,world.no,no,a.lat=lat,a.lon=lon),by=c("message_no"="no","world.no","scenario")) %>% 
      
      mutate(x = ifelse(perceiver_relation=="addressed",a.lat,p.lat),
             y = ifelse(perceiver_relation=="addressed",a.lon,p.lon),
             xend = ifelse(perceiver_relation=="addressed",p.lat,a.lat),
             yend = ifelse(perceiver_relation=="addressed",p.lon,a.lon))
    
    
    com.attention.coords <- com.attention.coords %>% 
      segementsOffset(1,1,0)
      
      
    pl <- pl + 
      geom_segment(aes(x=x,y=y,xend=xend,yend=yend,color=perceiver_relation),
                   size=2,arrow=arrow(type="closed",length=unit(4,"mm")),lineend="square",
                   data=com.attention.coords) 
  
  }

  # Actions
  if (actions == T) {
    
      com.actions.coords <- com.actions %>% 
        filter(scenario == filter.scenario,world.no == filter.world,epoch==filter.epoch) 
      
      pl <- pl +
        geom_circle(aes(x0=lat,y0=lon,r=energy,fill=action,color=action),alpha=0.4,data=com.actions.coords) +
        scale_fill_manual(values=palFill) +
        scale_color_manual(values=cbbPaletteColor)
  }
  

  pl <- pl +
    coord_fixed(xlim=c(1-world.margin,world.width+world.margin),
                ylim=c(1-world.margin,world.height+world.margin)) +
    theme_bw() +
    theme(legend.position="bottom",
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()
          ) 
  
  pl
}


fieldTime <- function() {
  cbbPalette <- c('#7570b3','#e7298a','#e6ab02','#a6761d','#666666','#1b9e77','#66a61e','#d95f02')
  
  com.actions %>%
    
    ggplot() +
    geom_circle(aes(x0=lat,y0=epoch,r=energy,fill=action),color="transparent",alpha=0.1) +
    geom_point(aes(x=lat,y=epoch),color="#ffffff",alpha=0.2, size=8) +
    geom_text(aes(x=lat,y=epoch,label=agent_no),color="black") +
    
    scale_fill_manual(values=cbbPalette) +
    coord_fixed() +
    theme_bw() 
  
}  


fieldNetwork <- function(world = NA,scenario=NA,epoch.from = -Inf,epoch.to= Inf,public = T) {
  filter.epoch.from = ifelse(!exists("epoch.from") | is.na(epoch.from),min(com.attention$epoch),epoch.from)
  filter.epoch.to = ifelse(!exists("epoch.to") | is.na(epoch.to),max(com.attention$epoch),epoch.to)
  filter.world = ifelse(!exists("world") | is.na(world),max(com.attention$world.no),world)
  filter.scenario = ifelse(is.na(scenario),max(com.attention$scenario),scenario)
  filter.public = public

  agents.filtered <- filter(ag.agents,scenario == filter.scenario,world.no == filter.world,
                            role=="default") %>%  select(no)
  attention.filtered <- filter(com.attention,scenario == filter.scenario,world.no == filter.world,
                               epoch >= filter.epoch.from,epoch <= filter.epoch.to)
  
  log.content <- getContentNetwork(attention.filtered) %>% 
    select(-world.no) %>% 
    filter(public == filter.public) 
  
  log.content <- log.content %>% 
    semi_join(agents.filtered,by=c("agent1"="no")) %>% 
    semi_join(agents.filtered,by=c("agent2"="no"))
  
  gr.content <- log.content %>%
    graph_from_data_frame(directed=T,vertices=agents.filtered) %>% 
    as_tbl_graph() %>% 
    convert(to_undirected)
  
  if ((vcount(gr.content)) == 0 ) {
    print("No nodes to plot")
    return()
  }  


  gr.content <- gr.content %>% 
    mutate(c = as.factor(group_components())) 
  
  pl <- ggraph(gr.content,layout="fr")
  
  
  if (nrow(log.content) > 0) {
    pl <- pl + 
      geom_edge_link(color="gray",alpha=0.8,width=0.8) 
  }    
  
  pl <- pl + 
    geom_node_point(aes(color=c),size=4) +
    
    coord_fixed() +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      panel.grid.major = element_blank(),
    ) 
  
  plot(pl)
  
}



plotAttentionDynamic <- function() {

  pl <- log.dynamic %>% 
    group_by(world.no,type) %>% 
    summarize(n=sum(n)) %>% 
    ungroup() %>%
    group_by(world.no) %>% 
    mutate(p=n/sum(n)) %>% 
    ungroup()  %>% 
    complete(world.no,type,fill=list(p=0))

  pl <-  pl %>% 
    ggplot(aes(x=type,y=p,fill=type)) +
    geom_boxplot() +
    stat_summary(fun.data=meanFun,geom="text") +
    ggtitle("Dynamic") +
    theme(axis.title.x  = element_blank(),text=element_text(size=11))
  
  pl
}


plotAttentionCumulation <- function() {

  dispersion <- log.cumulation %>%
    gather(key="perceiver_relation",value="perceiver_count",accessed,addressed,perceived) %>% 
    group_by(content_no,public,perceiver_relation) %>% 
    summarize(perceiver_count = sum(perceiver_count,na.rm=T)) %>% 
    ungroup() %>% 
    
    count(public,perceiver_relation,perceiver_count) %>%
    setPerceivedOrder("perceiver_relation") %>%
    complete(public,perceiver_relation,fill=list(perceiver_count=0))
  

  dispersion <- dispersion %>% 
    group_by(public,perceiver_relation) %>% 
    mutate(p = n / sum(n)) %>% 
    ungroup()
    
  pl <- dispersion %>% 
    ggplot(aes(x=perceiver_count,y=p,fill=perceiver_relation))


  pl <- pl +
    geom_col(position = "dodge") +
    ggtitle("Dispersion") +
    facet_wrap(~public)
  
  plot(pl)
}

plotAttentionCongruency <- function() {


  log <- log.congruency %>% 
    #mutate(nonzero = (type != "0_0_0")) %>% 
    setTypeOrder("type") %>% 
    group_by(type,world.no) %>% 
    summarize(n=sum(n)) %>% 
    ungroup() %>% 
    group_by(world.no) %>% 
    mutate(p = n / sum(n)) %>% 
    ungroup()
    
  pl <- log %>% 
    ggplot(aes(x=type,y=p,fill=type))   
    
  pl <- pl +
    geom_boxplot() +
    stat_summary(fun.data=meanFun,geom="text")  +
    ggtitle("Incongruency") +
    theme(strip.text.x = element_blank())
    
  
  plot(pl)
}






#@segment public, nonpublic, both
plotAttentionAmbiguity <- function(segment = "public",epochs = NA) {

  if (is.na(epochs)) 
    epochs <- unique(log.ambiguity$epoch)
  
  pl <- log.ambiguity %>%
    filter(epoch %in% epochs) %>% 
    mutate(zero = recode_factor(type,"0_0_0"="zero",.default="nonzero")) %>% 
    mutate(public = if_else(public,"public","nonpublic")) %>% 
    setTypeOrder("type") 
  
  if (segment != "both") {
    pl <- pl %>% 
      filter(public == segment)
  }

  
  pl <- pl %>% 
    ggplot(aes(x=type,y=p,fill=type))  

  if (segment == "both") {
    pl <- pl +
      facet_wrap(~ public,ncol=1,scales = "free")     
  }  
    

  pl <-  pl +
    geom_boxplot() +
    stat_summary(fun.data=meanFun,geom="text") +
    ggtitle("Ambiguity") +
    theme(strip.text.y = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "none",
          text=element_text(size=11))
  plot(pl)
}

plotAttentionComplexity <- function(relative = T) {

  pl <- log.complexity %>% 
    mutate(nonzero = (type != "0_0_0")) %>% 
    setTypeOrder("type")
    
  if (relative) {
    pl <- pl %>% 
      ggplot(aes(x=type,y=p,fill=type))     
  } else {
    pl <- pl %>% 
      ggplot(aes(x=type,y=n,fill=type))
  }
  
    
  pl <- pl +
    geom_boxplot() +
    #scale_y_log10() +
    stat_summary(fun.data=meanFun,geom="text") +
    facet_wrap(~ nonzero,scales = "free") +
    ggtitle("Complexity") +
    theme(strip.text.x = element_blank())
  
  
  plot(pl)
}

traceReach <- function() { #segment = "public"
  
  
  log <- log.reach %>% 
    distinct(world.no,epoch,content_no,perceiver_no) %>% 
    count(world.no,epoch,content_no) %>% 

    group_by(epoch) %>%
    summarize(mean = mean(n),p0 = min(n),p100 = max(n)) %>% 
    ungroup() 
  
  
  pl <- log %>% 
    ggplot(aes(x=epoch,y=mean)) + 
    geom_line(size=1) +
    ggtitle("Reach")
  
  plot(pl)
  
}

traceAttentionAmbiguity <- function(segment = "public") {
  
  
  log <- log.ambiguity %>% 
    group_by(world.no,epoch,public) %>% 
    mutate(p=n/sum(n)) %>% 
    ungroup() %>% 
    
    group_by(epoch,public,type) %>%
    summarize(mean = mean(p),p0 = min(p),p100 = max(p)) %>% 
    ungroup() 
  
  
  pl <- log %>% 
    ggplot(aes(x=epoch,y=mean,color=type))   +
    geom_line(size=1) +
    
    scale_y_continuous(limits=c(0,1)) +
    facet_wrap(~public,ncol=1) +
    ggtitle("Ambiguity")
  
  plot(pl)
  
}


traceAttentionUnclear <- function() {

  log.unclear <- log.congruency %>% 
    mutate(log = "incongruency") %>% 
    mutate(unclear = !(type %in% c("1_1_1")))
  
  log.unclear <- log.ambiguity %>% 
    mutate(log = "ambiguity") %>% 
    mutate(unclear = !(type %in% c("0_0_0","1_1_1"))) %>% 
    bind_rows(log.unclear)
  
  log.unclear <- log.complexity %>% 
    mutate(log = "complexity") %>%
    mutate(unclear = !(type %in% c("0_0_0","1_1_1"))) %>% 
    bind_rows(log.unclear)
  
  log.unclear <- log.unclear %>% 
    group_by(world.no,epoch,log,unclear) %>% 
    summarize(n = sum(n)) %>% 
    ungroup() %>% 
    
    group_by(world.no,epoch,log) %>% 
    mutate(p = n / sum(n)) %>% 
    ungroup() %>% 
    
    filter(unclear == T)
  
  log.unclear <- log.unclear %>% 
    group_by(epoch,log,unclear) %>% 
    summarize(mean = mean(p),p0 = min(p),p100 = max(p)) %>% 
    ungroup() 
  
  
  pl <- log.unclear %>% 
    ggplot(aes(x=epoch,y=mean,color=log))   +
    
    geom_line(size=1) +
    geom_ribbon(aes(ymin=p0,ymax=p100),alpha=0.1,linetype=0) +
    
    scale_y_continuous(limits=c(0,1)) +
    ggtitle("Unclearness")
  
  plot(pl)
}

# Content network components

traceNetworkComponents <- function() {
  
  
  log.net.summary <- log.contentnetwork %>% 
    gather("metric","value",edges,nodes,density,transitivity,components,distance) %>% 
    group_by(public,epoch,metric) %>%
    summarize(p0=min(value),
              p100=max(value),
              mean=mean(value)) %>% 
    ungroup() %>% 
    mutate(network = ifelse(public,"public","nonpublic")) %>% 
    mutate(network = recode(network,.missing="all"))
  
  log.net.summary %>%
    filter(metric=="components") %>% 
    ggplot(aes(x=epoch,y=mean,color=network)) +
    geom_line(size=1) +
    geom_ribbon(aes(ymin=p0,ymax=p100),alpha=0.1,linetype=0) +
    ggtitle("Number of components in the content network") +
    theme(legend.position = "bottom",
          text=element_text(size=11)) +
    scale_color_brewer(palette="Set1")

}


traceNetworkDistance <- function() {
  
  log.net.summary <- log.contentnetwork %>% 
    gather("metric","value",edges,nodes,density,transitivity,components,distance) %>% 
    group_by(public,epoch,metric) %>%
    summarize(p0=min(value),
              p100=max(value),
              mean=mean(value)) %>% 
    ungroup() %>% 
    mutate(network = ifelse(public,"public","nonpublic")) %>% 
    mutate(network = recode(network,.missing="all"))

  log.net.summary %>%
    filter(metric=="distance") %>% 
    ggplot(aes(x=epoch,y=mean,color=network)) +
    geom_line(size=1) +
    geom_ribbon(aes(ymin=p0,ymax=p100),alpha=0.1,linetype=0) +
    ggtitle("Average shortest paths in the content network") +
    theme(legend.position = "bottom",
          text=element_text(size=11)) +
    scale_color_brewer(palette="Set1")
  
}

traceNetworkDensity <- function() {
  
  log.net.summary <- log.contentnetwork %>% 
    gather("metric","value",edges,nodes,density,transitivity,components,distance) %>% 
    group_by(public,epoch,metric) %>%
    summarize(p0=min(value),
              p100=max(value),
              mean=mean(value)) %>% 
    ungroup() %>% 
    mutate(network = ifelse(public,"public","nonpublic")) %>% 
    mutate(network = recode(network,.missing="all"))
  
  log.net.summary %>%
    filter(metric=="density") %>% 
    ggplot(aes(x=epoch,y=mean,color=network)) +
    geom_line(size=1) +
    geom_ribbon(aes(ymin=p0,ymax=p100),alpha=0.1,linetype=0) +
    ggtitle("Density in the content network") +
    theme(legend.position = "bottom",
          text=element_text(size=11)) +
    scale_color_brewer(palette="Set1")
  
}



#
# Save and load data ----
#



# Save definition, logs and plots
saveHistory <- function(memory = 100) {
  
  # Save old data (e.g. 200)
  if (((world.epoch %% memory) == 0) & (world.epoch > memory)) {

    dir.create(world.name, showWarnings = FALSE)
    
    # Only save data that will be pruned or last data (e.g. 100)
    epoch.filter <- world.epoch - memory
    
    # Agents,  actions and logs
    dfvars <- eapply(.GlobalEnv,is.data.frame)
    dfvars <- names(dfvars[unlist(dfvars)])
    dfvars <- str_subset(dfvars,"^ag|com|log\\.")
    
    for (df in dfvars) {
      filename <- paste0(world.name,"/",world.shard,"_",world.no,"_",epoch.filter,"_",df,".csv")
      print(filename)
      
      data <- get(df)
      data <- data %>% 
        mutate_if(is.list,function(x) {as.character( lapply(x,paste0,collapse=", "))})
      
      # Only saved data that will be pruned
      if ("epoch" %in% colnames(data)) {
        data <- filter(data,epoch <= epoch.filter)
        write_csv2(data,filename)
      }
    }

  }
}


pruneHistory <- function(memory = 100) {
  if ((world.epoch %% memory) == 0) {
    
    epoch.filter <- world.epoch - memory
    
    # Agents,  actions and logs
    dfvars <- eapply(.GlobalEnv,is.data.frame)
    dfvars <- names(dfvars[unlist(dfvars)])
    dfvars <- str_subset(dfvars,"^ag|com|log\\.")
    
    for (df in dfvars) {
      data <- get(df)
      if ("epoch" %in% colnames(data)) {
        data <- filter(data,epoch > epoch.filter)
        assign(df,data,.GlobalEnv)
        
      }
    }
  }
}

# Save definition, logs and plots
savePresent <- function(memory = 100) {
  
  # Save current data
  if (world.epoch == world.end) {
    
    dir.create(world.name, showWarnings = FALSE)
    
    # Only save data that will be pruned or last data
    epoch.filter <- world.epoch - memory
    
    # Agents,  actions and logs
    dfvars <- eapply(.GlobalEnv,is.data.frame)
    dfvars <- names(dfvars[unlist(dfvars)])
    dfvars <- str_subset(dfvars,"^ag|com|log\\.")
    
    for (df in dfvars) {
      filename <- paste0(world.name,"/",world.shard,"_",world.no,"_",world.epoch,"_",df,".csv")
      print(filename)
      
      data <- get(df)
      data <- data %>% 
        mutate_if(is.list,function(x) {as.character( lapply(x,paste0,collapse=", "))})
      
      # Only saved data that will be pruned
      if ("epoch" %in% colnames(data))
        data <- filter(data,epoch > epoch.filter)
      
      
      write_csv2(data,filename)
    }
  }
}

gatherHistory <- function(folder = "unknown",pattern=".*") {
  #pattern <- "^log.contentnetwork"
  #folder <- "unknown"
  #folder <- "C:/Users/Jakob/OneDrive/Forschung/UnclearPublicness/hpc/social"
  
  folder = paste0(folder,"/")

  files <- tibble(filename=list.files(folder,pattern="*.csv")) %>% 
    separate(filename,into=c("shard","world","epoch","name"),sep="_",remove=F) %>% 
    mutate(filename = paste0(folder,filename),
           name=str_replace(name,"\\.csv$",""))
  
  print(count(files,name))
  
  files <- files %>% 
    filter(str_detect(name,pattern))

  data <- files %>% 
    group_by(name) %>% 
    do(data = rbindlist(lapply(.$filename, fread), idcol=TRUE)) %>% 
    ungroup()
  
  data <- mapply(assign,data$name,data$data,MoreArgs = list(envir = .GlobalEnv))

}


saveState <- function(folder = "unknown", prefix="") {

  dir.create(folder, showWarnings = FALSE)
  
  # Agents,  actions and logs
  dfvars <- eapply(.GlobalEnv,is.data.frame)
  dfvars <- names(dfvars[unlist(dfvars)])
  dfvars <- str_subset(dfvars,"^ag|com|log\\.")
  
  for (df in dfvars) {
    filename <- paste0(folder,"/",prefix,df,".csv")
    print(filename)

    data <- get(df)
    data <- data %>% 
      mutate_if(is.list,function(x) {as.character( lapply(x,paste0,collapse=", "))})
    
    write_csv2(data,filename)
  }
  
}

gatherAndSaveHistory <- function(folder.input = "unknown",folder.output = "unknown_combined",pattern=".*", prefix="") {
  #pattern <- "^log.contentnetwork"
  #folder <- "unknown"
  #pattern <- ".*"
  # prefix <- ""
  #folder.input <- "C:/Users/Jakob/Documents/Datenanalyse/unclearpublicness/select"
  #folder.output <- "C:/Users/Jakob/Documents/Datenanalyse/unclearpublicness/select_combined2"
  
  folder.input = paste0(folder.input,"/")
  folder.output = paste0(folder.output,"/")
  dir.create(folder.output, showWarnings = FALSE)
  
  files <- tibble(filename=list.files(folder.input,pattern="*.csv")) %>% 
    separate(filename,into=c("shard","world","epoch","name"),sep="_",remove=F) %>% 
    mutate(filename = paste0(folder.input,filename),
           name=str_replace(name,"\\.csv$",""))
  
  files <- files %>% 
    filter(str_detect(name,pattern))
  
  files.out <- unique(files$name)
  
  for (file.out in files.out) {
    
    files.in <- files %>% 
      filter(name == file.out)
    
    print(count(files.in,name))
    
    data = rbindlist(lapply(files.in$filename, fread), idcol=TRUE)


    filename.out <- paste0(folder.output,prefix,file.out,".csv")

    write_csv2(data,filename.out)
    rm(data)
  }
}

# Load csv files
# @folder parent folder of world data, 
#         every subfolder is loaded and rowbinded, 
#         new world variable with folder name is created
# 
loadState <- function(folder,scenarios=NA,pattern="\\.csv$",prefix.remove = "",prefix.add = "",world.filter = NA,epoch.min = NA,epoch.max = NA) {

  if (is.na(scenarios)) {
    scenarios <- list.dirs(folder,full.names = F)
    scenarios <- scenarios[scenarios != ""]
  }  
  
  for (scenario in scenarios) {

    files <- list.files(paste0(folder,"/",scenario),pattern= pattern)
    
    for (file in files) {
      df <- file
      if (prefix.remove != "")
        df <- str_replace(df,prefix.remove,"")
      
      if (prefix.add != "")
       df <- paste0(prefix.add,df)
      
      print(file)
      data <- read_csv2(paste0(folder,"/",scenario,"/",file))
      if (!is.na(world.filter) & ("world.no" %in% colnames(data))) {
        data <- filter(data,world.no == world.filter)
      }
      if (!is.na(epoch.min) & ("epoch" %in% colnames(data))) {
        data <- filter(data,epoch >= epoch.min)
      }
      if (!is.na(epoch.max) & ("epoch" %in% colnames(data))) {
        data <- filter(data,epoch <= epoch.max)
      }
      
      data <- data %>% 
        mutate(scenario = scenario)
      
      df <- str_replace(df,"\\.csv$","")
      
      if (exists(df,envir = .GlobalEnv)) {
        data <- bind_rows(get(df,envir = .GlobalEnv),data)
      }
      
      assign(df,data,envir = .GlobalEnv)
    }
  }
}


# Save plots
savePlots <- function(folder) {
  folder <- paste0(folder,"/")
  dir.create(folder, showWarnings = FALSE)
  
  plotAttentionDynamic()
  ggsave(paste0(folder,"log.dynamic.png"),scale=2)

  plotAttentionCumulation()
  ggsave(paste0(folder,"log.cumulation.png"),scale=2)
  
  plotAttentionCongruency()
  ggsave(paste0(folder,"log.congruency.png"),scale=2)
  
  plotAttentionAmbiguity("public")
  ggsave(paste0(folder,"log.public.ambiguity.png"),width=16, height=8, units="cm", dpi=600)  

  plotAttentionAmbiguity("nonpublic")
  ggsave(paste0(folder,"log.nonpublic.ambiguity.png"),width=16, height=8, units="cm", dpi=600)  
  
  plotAttentionComplexity()
  ggsave(paste0(folder,"log.complexity.png"),scale=2)

  traceAttentionAmbiguity()
  ggsave(paste0(folder,"trace.ambiguity.png"),scale=2)
  
  traceAttentionUnclear()
  ggsave(paste0(folder,"trace.unclear.png"),scale=2)
  
  traceNetworkComponents()
  ggsave(paste0(folder,"trace.components.png"),width=16, height=8, units="cm", dpi=600)

  traceNetworkDensity()
  ggsave(paste0(folder,"trace.density.png"),width=16, height=8, units="cm", dpi=600)

  traceNetworkDistance()
  ggsave(paste0(folder,"trace.distance.png"),width=16, height=8, units="cm", dpi=600)
  
}


saveNetworkPlots <- function(folder,epochs = 1,world.no=1) {
  folder <- paste0(folder,"/")
 
  for (e in epochs) {
    fieldNetwork(world.no,e,T)
    filename <- paste0(folder,"net.content.public",sprintf("%02d",e),".png")
    ggsave(filename,scale=2)  
    
    fieldNetwork(world.no,e,F)
    filename <- paste0(folder,"net.content.nonpublic",sprintf("%02d",e),".png")
    ggsave(filename,scale=2)  
    
  }    
 

}

saveFieldPlots <- function(folder,epochs = 1,world.no=1) {
  folder <- paste0(folder,"/")
  
  for (e in epochs) {
    plotField(world=world.no,epoch=e)
    filename <- paste0(folder,"field",sprintf("%02d",e),".png")
    ggsave(filename,scale=2)  
    
  }    
}

