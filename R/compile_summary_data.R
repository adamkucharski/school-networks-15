# - - - - - - - - - - - - - - - - - - 
# Compile summary datasets
# - - - - - - - - - - - - - - - - - - 


for(school1 in 1:4){
  source("R/load_school_data.R",local = F)
  
  diamt.store=NULL
  distn.store=list()
  clust.store=NULL
  clust.storeM=NULL; clust.storeF=NULL
  node.store = matrix(NA,ncol=4,nrow=length(idlistAll))
  comm.store=NULL
  factor.store=NULL
  mutPick=0
  n.total = NULL # participants in round
  
  # Main data loop as well
  
  for(ii in 1:4){
    outputA = network.analysis(ii,school1,mutualPick = mutPick)
    
    # Store node ranks
    degree.nodes = outputA$dnodes
    node.rank = cbind(outputA$nreport,degree.nodes[match(outputA$nreport,degree.nodes[,1]),2]) # If NA because no contacts reported
    node.store[node.rank[,1],ii]=node.rank[,2]
    
    diamt.store=c(diamt.store,outputA$diameter)
    
    if(mutPick==0){
      distn.store[[ii]]=outputA$ddistn
    }else{
      ddist=outputA$ddistn
      distn.store[[ii]]=outputA$ddistn[seq(1,length(ddist),2)]
    }
    clust.store=c(clust.store,outputA$clustering)
    clust.storeM=c(clust.storeM,outputA$clusteringM)
    clust.storeF=c(clust.storeF,outputA$clusteringF)
    comm.store=rbind(comm.store,outputA$community)
    factor.store=rbind(factor.store,outputA$mfassort)
    n.total=c(n.total,length(outputA$nreport)) 
  }
  
  
  # - - - - - - - - - - - - - - - - - - - - -
  # Examine node rankings
  # Compile data
  node.data = node.store[rowSums(is.na(node.store))<=2,] # Pick entries with at least 2 data points
  
  
  # SAVE DATA
  distn.outside.class = calc.contacts(school1)
  
  save(n.total,distn.store,comm.store,distn.outside.class,diamt.store,node.data,clust.store,factor.store,clust.storeM,clust.storeF,file=paste("outputs/outputs",school1,".RData",sep="")) #plot_Sposteriors

}