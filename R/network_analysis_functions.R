# - - - - - - - - - - - - - - - - - - 
# Main network analysis functions
# - - - - - - - - - - - - - - - - - - 


# - - - 
# Convert network matrix to output function
# - - - -

n_convert <- function(NetworkMatrixIn,xBALL){
  
  NetworkMatrix <- matrix(match(NetworkMatrixIn,xBALL),ncol=7)   # Match to NetworkMatrix
  
  # Convert into vector of links
  x1ALL=apply(NetworkMatrix, 1, function(x) c(rbind(rep(x[1],6),x[2:7])) )
  x2ALL=matrix(x1ALL,ncol=2,byrow=TRUE)
  x2ALL = x2ALL[!is.na(x2ALL[,2]),] # Remove NA entries
  x2ALL = x2ALL[(x2ALL[,1]-x2ALL[,2])!=0,] # Remove self links
  xBALLN = 1:max(x2ALL[,1]) # New link IDs #unique(NetworkMatrix[,1])
  
  list(x2out=x2ALL,xBout=xBALLN)
  
}


# - - - - - - - - - - - - - - - - - - 
# Output outside-school contacts
# - - - - - - - - - - - - - - - - - - 

calc.contacts<-function(school1){
  
  idlistAll = idlistAll[idlistAll!="0_ID_NA"] # Remove NA entries
  
  # See how many times each person appears
  count1 = sapply(idlistAll,function(x){ sum(dataset1a[,1]==x)})
  idlist4 = idlistAll[(count1>=4)]
  contactDistn = sapply(idlist4,function(x){ dataset1[dataset1a[,1]==x,"Convo"][1:4]} ) %>% t()
  contactDistn = contactDistn[as.numeric(rowSums(is.na(contactDistn)))==0,] # Remove NA entries
  
  contactDistn
  
}




# - - - - - - - - - - - - - - - - - - 
# Network plots and statistics
# - - - - - - - - - - - - - - - - - - 

network.analysis <- function(round1,school1,mutualPick=0,plotALL=F,plotClass=F,plotRound=F){

  # round1 = 1; mutualPick=0; school1=1 ; plotALL=T; plotClass=T; plotRound = F
  
  # Edit matrices to put in correct format
  # Change to ensure only draw from people in current survey
  dataset1a[dataset1a=="0_ID_NA"] <- NA # Remove NA entries
  #dataset1a = dataset1a[!is.na(dataset1a[,1]),] # Remove NA participants
  
  datasetP0=matrix(match(dataset1a, idlist),ncol=7) # Need this here as use participants only in plot
  datasetMAIN=dataset1[!is.na(datasetP0[,1]),] # Characteristics for participants - for file
  datasetP1=matrix(match(dataset1a, idlist),ncol=7) # Need to use participants only in plot
  datasetP1=datasetP1[!is.na(datasetP1[,1]),] # Exclude non-participants from matrix - for plot
  
  # - - - - - - - - - - - - - - - -
  # Plot all links
  
  datasetP=datasetP1
  NetworkMatrix=datasetP
  xBALL <- unique(NetworkMatrix[,1])  # Identify unique links in first section
  x2ALL <- n_convert(NetworkMatrix,xBALL)$x2out
  x2ALL=t(x2ALL); dim(x2ALL) <- NULL
  g2All <- make_graph(edges=x2ALL,n=length(xBALL) ,directed=F)
  
  set.seed(50)
  coordAll <- as.matrix(cbind(xBALL,layout.fruchterman.reingold(g2All) )) ; colnames(coordAll)=c("ID","xx","yy")
  
  if(plotALL==T){
    
    if(plotClass==T){
      class.names=unique(datasetMAIN$Class)
      colclass0=datasetMAIN[match(xBALL,datasetP1[,1]),"Class"]
      colclass1 = match(colclass0,class.names)
      col_list = rainbow_hcl(length(class.names),l=80,c=100)
      col_list[length(col_list)] = "white"
      col1mf = col_list[colclass1]
      
      col1=datasetMAIN[match(xBALL,datasetP1[,1]),"MF"]
      col1[is.na(col1)]=0
      pickshape=sapply(col1,function(x){ifelse(x==1,'circle','square')})
    }
    
    par(mar=c(0,0,1,0))
    plot(g2All,layout=coordAll[,2:3],vertex.size=6,vertex.shape=pickshape,vertex.color=col1mf,vertex.label=NA,vertex.label.cex=0.5,vertex.label.family="",edge.color=rgb(0.2,0.2,0.2,0.25),edge.arrow.size=0,edge.width=1) #,
    title(LETTERS[2*(school1-1)+mutualPick+1], adj=0,cex.main=1.5)
    
  }

  # Plot mutual links
  
  mutual = 1
  if(mutual==1){
    datasetM=datasetP
    for(ii in 1:length(datasetP[,1])){
      # Find mutual links - check indexing carefully!
      datasetM[ii,2:7]=sapply(datasetM[ii,2:7],function(x){  if( !is.na(match(x,datasetP[,1])) ){if( sum(datasetM[match(x,datasetP[,1]),!is.na(datasetM[match(x,datasetP[,1]),])] ==datasetP[ii,1])==1 ){x}else{NA}}else{NA}  })
    }
    NetworkMatrix=datasetM
    
  }else{
    NetworkMatrix=datasetP
  }
  
  xBALL <- unique(NetworkMatrix[,1])  # Identify unique links in first section
  x2ALL <- n_convert(NetworkMatrix,xBALL)$x2out
  x2ALL=t(x2ALL); dim(x2ALL) <- NULL
  g2All <- make_graph(edges=x2ALL,n=length(xBALL) ,directed=T)
  
  namesNet <- datasetMAIN[match(xBALL,datasetP1[,1]),"Firstname0"] %>% as.character()
  col1=datasetMAIN[match(xBALL,datasetP1[,1]),"MF"]; col1[is.na(col1)]=0; col1mf=sapply(col1,function(x){ifelse(x==1,'white','grey')})
  pickshape=sapply(col1,function(x){ifelse(x==1,'circle','square')})
  
  if(plotALL==T){
    
    if(plotClass==T){
      class.names=unique(datasetMAIN$Class)
      colclass0=datasetMAIN[match(xBALL,datasetP1[,1]),"Class"]
      colclass1 = match(colclass0,class.names)
      col_list = rainbow_hcl(length(class.names),l=80,c=100)
      col_list[length(col_list)] = "white"
      col1mf = col_list[colclass1]
    }
    
    # Plot graph
    par(mar=c(0,0,1,0))
    plot(g2All,layout=coordAll[,2:3],vertex.size=6,vertex.shape=pickshape,vertex.color=col1mf,vertex.label=NA,vertex.label.cex=0.5,vertex.label.family="",edge.color=rgb(0.2,0.2,0.2,0.25),edge.arrow.size=0,edge.width=1) #,
    title(LETTERS[2*(school1-1)+mutualPick+2], adj=0,cex.main=1.5)
    
    return()

  }
  

  
  # - - - - - - - - - - - - - - - -
  # Plot round-specific graph with all links

  graph.store = list()
  
  # Iterate across rounds so locations are fixed
  
  for(round0 in 1:4){
  
    # Calculate mutual contacts
    datasetP=datasetP1
    datasetP[datasetMAIN$Round!=round0,1:7]=NA # Pick only contacts in this round - for plot
    
    mutual=mutualPick
    if(mutual==1){
      datasetM=datasetP
      for(ii in 1:length(datasetP[,1])){
        # Find mutual links - check indexing carefully!
        datasetM[ii,2:7]=sapply(datasetM[ii,2:7],function(x){  if( !is.na(match(x,datasetP[,1])) ){if( sum(datasetM[match(x,datasetP[,1]),!is.na(datasetM[match(x,datasetP[,1]),])] ==datasetP[ii,1])==1 ){x}else{NA}}else{NA}  })
      }
      NetworkMatrix=datasetM
      
    }else{
      NetworkMatrix=datasetP
    }
    
    # - - - - - - - - - - - - - - - - - - 
    # Plotting tools - Still issues here?
    # - - - - - - - - - - - - - - - - - - 
    
    # Convert to network readable code
    xB <- unique(NetworkMatrix[,1]); xB <- xB[!is.na(xB)]  # Identify unique links in first section
    x2 <- n_convert(NetworkMatrix,xB)$x2out
    x2 <- t(x2); dim(x2) <- NULL
    
    graph.store[[round0]]=list(edges=x2,nodes=xB)
    
  }
    
  # - - - - - - - - - - - - - - - - - - 
  # Plot round specific graph with all links
  
  if(plotRound==T){
    
    for(round0 in 1:4){
      edge.include = graph.store[[round0]]$edges # extract edges for that round
      node.include = unique(graph.store[[round0]]$nodes ) # extract nodes for that round
      
      g2 <- make_graph(edges=edge.include,n=length(node.include) ,directed=T) # define directed graph
      
      # Set graph co-ordinates
      set.seed(50)
      coord1=coordAll[match(node.include,coordAll[,1]),2:3]
      
      # Extract MF colours and match so in correct sequence
      namesNet <- datasetMAIN[match(node.include,datasetP1[,1]),"Firstname0"] %>% as.character()
      col1=datasetMAIN[match(node.include,datasetP1[,1]),"MF"]
      col1[is.na(col1)]=0
      pickshape=sapply(col1,function(x){ifelse(x==1,'circle','square')})
      pickcol=sapply(col1,function(x){ifelse(x==1,'white','grey')}) # 2=F 1=M

      # Plot graph
      par(mar=c(0,0,1,0))
      
      #xRange = c(min(coordAll[,2]),max(coordAll[,2]))
      #yRange = c(min(coordAll[,3]),max(coordAll[,3])) #xlim=xRange,ylim=yRange,asp=1,rescale=F,
      
      plot(g2,layout=coord1,vertex.size=10,vertex.shape=pickshape,vertex.label=NA,vertex.color=pickcol,vertex.label.cex=0.5,vertex.label.family="",edge.arrow.width=2,edge.arrow.size=0.1,edge.color=rgb(0.6,0.6,0.6),edge.width=0.5) #,
      title(LETTERS[4*(school1-1)+round0],adj=0)
      
    }
    
    # Export pdf
    if(school1 == 4){
      dev.copy(pdf,paste("plots/Figure_1.pdf",sep=""),width=6,height=6)
      dev.off()
    }
    
    return()
    
  }
  
  # - - - - - - - - - - - - - - - - - - 
  # Calculate homophily and summary statistics
  
  edge.include = graph.store[[round1]]$edges # extract edges for that round
  node.include = unique(graph.store[[round1]]$nodes ) # extract nodes for that round
  
  g2.0 <- make_graph(edges=edge.include,n=length(node.include) ,directed=T) # define directed graph
  
  # Calculate communities
  g2 = as.undirected(g2.0,"each") # make graph undirected - create undirected edge for each directed one
  ceb <- cluster_edge_betweenness(g2) # Communities using edge betweeness
  clp <- cluster_label_prop(g2) # Communities using edge betweeness
  
  # Degree of each node
  datasetP = datasetP1
  datasetP[datasetMAIN$Round!=round1,1:7]=NA # Pick only contacts in this round - for plot
  NetworkMatrix=datasetP
  
  xB <- unique(NetworkMatrix[,1]); xB <- xB[!is.na(xB)]  # Identify unique links in first section
  x2 <- n_convert(NetworkMatrix,xB)$x2out
  x2 <- t(x2); dim(x2) <- NULL
  g2.01 <- make_graph(edges=x2,n=length(xB) ,directed=T)
  
  # Set up colours
  col1=datasetMAIN[match(xB,datasetP1[,1]),"MF"]
  col1[is.na(col1)]=0
  pickshape=sapply(col1,function(x){ifelse(x==1,'circle','square')})
  pickcol=sapply(col1,function(x){ifelse(x==1,'white','grey')}) # 2=F 1=M
  
  
  degree.nodes=data.frame(cbind(xB,degree(g2.01,mode="in"))); names(degree.nodes)=c("node","degree")
  nodeID=unique(datasetP[,1]); nodeID=nodeID[!is.na(nodeID)]
  
  # Gender assortativity
  factor0=datasetMAIN[match(xB,datasetP1[,1]),"Class"]; factor0[is.na(factor0)]=0
  flink=c(mf=assortativity_nominal(g2.0, as.factor(pickcol), directed=T),
          class=assortativity_nominal(g2.0, as.factor(factor0), directed=T)
  )
  
  # - - - 
  # Calculate clustering of Males Only
  NetworkMatrix=datasetP
  NetworkMatrix[datasetMAIN$MF!=1,1:7]=NA
  
  # Set up network in correct format
  xB <- unique(NetworkMatrix[,1]); xB <- xB[!is.na(xB)]  # Identify unique links in first section
  x2 <- n_convert(NetworkMatrix,xB)$x2out
  x2 <- t(x2); dim(x2) <- NULL
  g2m <- make_graph(edges=x2,n=length(xB) ,directed=T)
  
  # - - - 
  # Calculate clustering of Females Only
  if(school1 != 3){
    NetworkMatrix=datasetP
    NetworkMatrix[datasetMAIN$MF!=2,1:7]=NA
    
    # Convert to network readable code
    xB <- unique(NetworkMatrix[,1]); xB <- xB[!is.na(xB)]  # Identify unique links in first section
    x2 <- n_convert(NetworkMatrix,xB)$x2out
    x2 <- t(x2); dim(x2) <- NULL
    g2f <- make_graph(edges=x2,n=length(xB) ,directed=T)
    tg2 <- transitivity(g2f)
  }else{g2f=-1;tg2=-1}
  
  # Graph stats, including degree distribution and global clustering
  summary=list(diameter=diameter(g2.0),ddistn=degree_distribution(g2.0,mode = "in"),dnodes=degree.nodes,nreport=nodeID,clustering=transitivity(g2.0),
               clusteringM=transitivity(g2m),clusteringF=tg2,
               mfassort=flink,community=c(length(ceb),length(clp)))
  
  summary
  
  
}



# - - - - - - - - - - - - - - - - - - 
# Network bootstrap sampling
# - - - - - - - - - - - - - - - - - - 

network.bootstrap <- function(round1,school1,mutualPick=0,plotALL=F,boostrap_runs=10){
  
  # round1 = 1; mutualPick=0; school1=3 ; plotALL=T; boostrap_runs=10
  
  # Edit matrices to put in correct format
  # Change to ensure only draw from people in current survey
  dataset1a[dataset1a=="0_ID_NA"] <- NA # Remove NA entries
  
  datasetP0=matrix(match(dataset1a, idlist),ncol=7) # Need this here as use participants only in plot
  datasetMAIN=dataset1[!is.na(datasetP0[,1]),] # Characteristics for participants - for file

  

  # Set up bootstrap
  mean_participants = c(77,121,92,24)   # Mean partipants for each school
  n_participants = mean_participants[school1] # Pick relevant school
  bootstrap_table = NULL
  
  # - - -
  # Boostrap sample from the contact distribution
  
  for(kk in 1:boostrap_runs){
    
    sample_participants = sample(idlist[idlist!="0_ID_NA"],n_participants,replace=F) %>% sort() # Sample participants
    sample_entries = sapply(sample_participants,function(x){
                                                    y = which(x==dataset1a[,1]); 
                                                    if(length(y)>1){w = sample(y,1)}else{w = y}; 
                                                    w } ) %>% as.numeric()  # Pick entries from matrix
    
    # Sample matrix entries
    dataset1a0 = dataset1a[sample_entries,]
    datasetP1=matrix(match(dataset1a0, idlist),ncol=7) # Need to use participants only in plot
    datasetP1=datasetP1[!is.na(datasetP1[,1]),] # Exclude non-participants from matrix - for plot
    datasetP = datasetP1
    
    # Define network characteristics
    NetworkMatrix=datasetP
    xB <- unique(NetworkMatrix[,1]); xB <- xB[!is.na(xB)]  # Identify unique links in first section
    x2 <- n_convert(NetworkMatrix,xB)$x2out
    x2 <- t(x2); dim(x2) <- NULL
    g2.01 <- make_graph(edges=x2,n=length(xB) ,directed=T)
  
    # Set up colours
    col1 = dataset1[match(sample_participants,dataset1[,1]),"MF"]# Match to original matrix
    col1[is.na(col1)]=0
    pickshape=sapply(col1,function(x){ifelse(x==1,'circle','square')})
    pickcol=sapply(col1,function(x){ifelse(x==1,'white','grey')}) # 2=F 1=M
  
    # Communities
    g2.02 = as.undirected(g2.01,"each") # make graph undirected - create undirected edge for each directed one
    ceb <- cluster_edge_betweenness(g2.02) # Communities using edge betweeness
    clp <- cluster_label_prop(g2.02) # Communities using edge betweeness
    
    
    
    # Class assortativity
    factor0 = datasetMAIN[match(xB,datasetP0[,1]),"Class"]; factor0[is.na(factor0)]=0
    
    flink=c(mf=assortativity_nominal(g2.01, as.factor(pickcol), directed=T),
            class=assortativity_nominal(g2.01, as.factor(factor0), directed=T)
    )
    
    if(school1==3){flink[["mf"]]=0}
    
    # - - - 
    # Calculate clustering of Males Only
    NetworkMatrix=datasetP
    NetworkMatrix[col1!=1,1:7]=NA # Pick out males
    
    # Set up network in correct format
    xB <- unique(NetworkMatrix[,1]); xB <- xB[!is.na(xB)]  # Identify unique links in first section
    x2 <- n_convert(NetworkMatrix,xB)$x2out # DEBUG HERE
    x2 <- t(x2); dim(x2) <- NULL
    g2m <- make_graph(edges=x2,n=length(xB) ,directed=T)
    
    # - - - 
    # Calculate clustering of Females Only
    if(school1 != 3){
      NetworkMatrix=datasetP
      NetworkMatrix[col1!=2,1:7]=NA # Pick out females
      
      # Convert to network readable code
      xB <- unique(NetworkMatrix[,1]); xB <- xB[!is.na(xB)]  # Identify unique links in first section
      x2 <- n_convert(NetworkMatrix,xB)$x2out
      x2 <- t(x2); dim(x2) <- NULL
      g2f <- make_graph(edges=x2,n=length(xB) ,directed=T)
      tg2 <- transitivity(g2f)
    }else{g2f=0;tg2=0}
    
    # Graph stats, including degree distribution and global clustering
    bootstrap_table = rbind(bootstrap_table, c(n_participants,diameter(g2.01),transitivity(g2.01),transitivity(g2m),tg2,flink[["mf"]],flink[["class"]],c(length(ceb),length(clp)),c(n_participants/length(ceb),n_participants/length(clp)),transitivity(g2m)-tg2) )
    
  }
  
  bootstrap_table = bootstrap_table %>% data.frame()
  
  names(bootstrap_table) = c("n_participants","diameter","clustering","clusteringM","clusteringF","assortativity_MF","assortativity_class","community_EB","community_LP","part_community_EB","part_community_LP","clusteringM_minus_clusteringF")
  
  apply(bootstrap_table,2,c.text)

}

c.text <- function(x,sigF=2){
  x=x[!is.na(x)] # remove NA
  bp1=signif(c(median(x),quantile(x,0.025),quantile(x,0.975)),sigF)
  paste(bp1[1]," (",bp1[2],"-",bp1[3],")",sep="")
}

c.nume<-function(x){
  x=x[!is.na(x)] # remove NA
  bp1=c(median(x),quantile(x,0.025),quantile(x,0.975))
  as.numeric(bp1)
}



