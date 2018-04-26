# - - - - - - - - - - - - - - - - - - 
# PLOT SUMMARY RESULTS
# - - - - - - - - - - - - - - - - - - 

# Define colours

alpha1=0.7
colourpick <- c(rgb(1,0,0.2,alpha1),rgb(0,0.2,1,alpha1),rgb(0,0.6,0,alpha1),rgb(1,0.5,0,alpha1))
colourpick1 <- c(rgb(1,0,0.2),rgb(0,0.2,1),rgb(0,0.6,0),rgb(1,0.5,0))

# - - - - - - - - - - - - - - - - - - - - -
# Plot degree distribution consistency

plot_degree_distn <- function(){

  par(mfrow=c(2,2),mar = c(3,3,1,1),mgp=c(2,0.7,0))
  

  #layout(matrix(c(1,2,3,4,5,5,6,6), 4,2, byrow=T))
  
  # Loop over the four schools
  for(ii in 1:4){
    
    load(file=paste("outputs/outputs",ii,".RData",sep="")) #plot_Sposteriors
    
  
    # Normalise degree distribution?
    norm.distn <- function(ii){
      c(1:length(distn.store[[ii]])) #/(node.store[ii]-1)
    }

    plot(norm.distn(1),distn.store[[1]],type="l",ylim=c(0,ifelse(mutPick==0,0.45,0.6)),xlab="degree",ylab="proportion",xlim=c(0,25),col=colourpick[ii])
    for(kk in 2:4){
      lines(norm.distn(kk),distn.store[[kk]],col=colourpick[ii])
    }

    title(LETTERS[ii],adj=0)
  }
  
  # # Plot clustering
  # 
  # for(ii in 1:4){
  #   
  #   load(file=paste("outputs/outputs",ii,".RData",sep="")) #plot_Sposteriors
  # 
  #   if(ii==1){
  #     plot(0,0,type="l",col="white",yaxt="n",xlab="",ylab="",ylim=c(0,1),xlim=c(0,1))
  #     axis(side = 2, at = c(0.8,0.5,0.2), labels = c("All","F","M") ) #, tcl = -0.7, las = 2)
  #   }
  #   #axis(4,col="blue",col.axis="blue")
  #   #mtext("clustering coefficient", side=4, line=3,col="blue") # Label for 2nd axis
  #   
  #   for(kk in 1:4){
  #     points(y=0.8,x=clust.store[[kk]],col=colourpick[ii])
  #     if(ii>0){ points(y=0.2,x=clust.storeM[[kk]],col=colourpick[ii]) }
  #     if(ii!=3){ points(y=0.5,x=clust.storeF[[kk]],col=colourpick[ii]) }
  #   }
  # 
  # }
  # title(LETTERS[5],adj=0)
  # 
  # # Plot assortativity
  # 
  # for(ii in 1:4){
  #   
  #   load(file=paste("outputs/outputs",ii,".RData",sep="")) #plot_Sposteriors
  #   
  #   if(ii==1){
  #     plot(0,0,type="l",col="white",yaxt="n",xlab="",ylab="",ylim=c(0,1),xlim=c(0,1))
  #     axis(side = 2, at = c(0.75,0.25), labels = c("gender","class") ) #, tcl = -0.7, las = 2)
  #   }
  #   #axis(4,col="blue",col.axis="blue")
  #   #mtext("clustering coefficient", side=4, line=3,col="blue") # Label for 2nd axis
  #   
  #   for(kk in 1:4){
  #     if(ii==3){
  #       points(y=0.75,x=1,col=colourpick[ii]) # assortativity for single-sex school = 1
  #     }else{
  #       points(y=0.75,x=factor.store[kk,1],col=colourpick[ii])
  #     }
  #     if(ii>0){ points(y=0.25,x=factor.store[kk,2],col=colourpick[ii]) }
  #   }
  #   
  # }
  # 
  # title(LETTERS[6],adj=0)

  dev.copy(pdf,paste("plots/Figure_2.pdf",sep=""),width=5,height=3)
  dev.off()

}

# - - - - - - - - - - - - - - - - - - - - -
# Predictive power of one round etc.

plot_consistency_pred <- function(school1){
  
  paired1=4 # Only show repeat links
  
  # Gather network data
  dataset1a[dataset1a=="0_ID_NA"] <- NA # Remove NA entries
  datasetP0=matrix(match(dataset1a, idlist),ncol=7) # Need this here as use participants only in plot
  datasetMAIN=dataset1[!is.na(dataset1a[,1]),] # Characteristics for participants - for file
  datasetP1=datasetP0 # Need to use participants only in plot
  datasetP1=datasetP1[!is.na(datasetP1[,1]),] # Exclude non-participants from matrix - for plot

  idlist0 = unique(datasetP1[,1])
  
  # Find participant names that appear in all rounds
  name_four=sapply(idlist0,function(x){if( sum(datasetP1[datasetMAIN$Round==1,1]==x)==1 & sum(datasetP1[datasetMAIN$Round==2,1]==x)==1 & sum(datasetP1[datasetMAIN$Round==3,1]==x)==1 & sum(datasetP1[datasetMAIN$Round==4,1]==x)==1 ){TRUE}else{FALSE} })
  if(paired1==4){idlist0=idlist0[name_four]} # **** Only 4-link data used ****
  
  # Who reported whom - compile matrix
  store.links = list()
  
  for(roundID in 1:4){
    round1.c = datasetP1[datasetMAIN$Round==roundID,]
    round1.c = round1.c[match(idlist0,round1.c),] # Pick out people in all rounds
    
    contact.m = matrix(NA,nrow=length(idlist0),ncol=length(idlist0))
    
    for(ii in 1:length(idlist0)){
      for(jj in 1:length(idlist0)){
        if(ii<jj){
          # Unidirectional in either way
          if(  !is.na(match(idlist0[jj],round1.c[ii,])) | !is.na(match(idlist0[ii],round1.c[jj,])) ){
            contact.m[ii,jj]=1
          }else{
            contact.m[ii,jj]=0
          }
        }
      }
    }# End matrix loop
    
    store.links[[roundID]]=contact.m
    
  }
  
 # - - - - - - - - - - - - - - - - - - - - - - - - 
 # Predictive power of matrix in round 1
  null.connect = total.connections + length(idlist0) #Total connections plus diagonal
  store.ROC.1 = NULL
  
  for(roundID in 2:4){
    baseline.links = store.links[[1]]
    calc.diff = baseline.links - store.links[[roundID]] # calculate differences
    dim(calc.diff)=NULL; calc.diff = calc.diff[!is.na(calc.diff)] # collapse and remove NA
    collapse.baseline = baseline.links; dim(collapse.baseline) = NULL; collapse.baseline = collapse.baseline[!is.na(collapse.baseline)]
    
    total.connections = length(calc.diff)
    # False positive=1, false negative =-1
    
    # Calculate precision, recall and F
    calc.fp = sum(calc.diff== 1)
    calc.tp = sum(calc.diff[collapse.baseline==1]==0)
    calc.tn = sum(calc.diff[collapse.baseline==0]==0)
    calc.fn = sum(calc.diff== -1)
    
    calc.precision = calc.tp/(calc.tp+calc.fp)
    calc.recall = calc.tp/(calc.tp+calc.fn)
    calc.f.measure = 2*(calc.precision*calc.recall)/(calc.precision+calc.recall)
    calc.accuracy = (calc.tp+calc.tn)/total.connections
    
    # c("type","round","no_link","amb","link","DpredP","DpredF","accuracy","contacts")
    
    store.ROC.1=rbind(store.ROC.1,
            c(1, # Training round
              roundID, # Test round
              sum(collapse.baseline==0), # Total with no link
              0, # Total ambigious
              sum(collapse.baseline==1), # Total with at least one unidirectional link
              calc.precision, # Precision
              calc.recall, #Recall
              calc.f.measure, # F-measure
              calc.accuracy, # Accuracy
              total.connections) ) # Total links
  }
  
  # Train on round 1-2 -- use probabilistic accuracy measurement?
  
  store.ROC.2 = NULL
  
  for(roundID in 3:4){

    baseline.links =  (store.links[[1]] + store.links[[2]] ) / 2
    calc.diff = baseline.links - store.links[[roundID]] # calculate differences
    dim(calc.diff)=NULL; calc.diff = calc.diff[!is.na(calc.diff)] # collapse and remove NA
    collapse.baseline = baseline.links; dim(collapse.baseline) = NULL; collapse.baseline = collapse.baseline[!is.na(collapse.baseline)]
    
    # False positive = 1, false negative = -1, weighted = -0.5/0.5, correct = 0 
    total.connections = length(calc.diff)
    # False positive=1, false negative =-1
    
    # Calculate expected precision, recall and F
    calc.fp = sum(calc.diff== 1) + 0.5 * sum(calc.diff==0.5)
    calc.tp = sum(calc.diff[collapse.baseline==1]==0) + 0.5 * sum(calc.diff[collapse.baseline==0.5]==-0.5) # Pick +ve
    calc.tn = sum(calc.diff[collapse.baseline==0]==0) + 0.5 * sum(calc.diff[collapse.baseline==0.5]==0.5) # Pick -ve
    calc.fn = sum(calc.diff== -1) + 0.5 * sum(calc.diff == -0.5)
    
    # DEBUG: calc.fn+calc.tn+calc.tp+calc.fp
    
    calc.precision = calc.tp/(calc.tp+calc.fp)
    calc.recall = calc.tp/(calc.tp+calc.fn)
    calc.f.measure = 2*(calc.precision*calc.recall)/(calc.precision+calc.recall)
    calc.accuracy = (calc.tp+calc.tn)/total.connections
    
    # c("type","round","no_link","amb","link","DpredP","DpredF","accuracy","contacts")
    
    store.ROC.2=rbind(store.ROC.2,
                      c(2, # Training round
                        roundID, # Test round
                        sum(collapse.baseline==0), # Total with no link
                        sum(collapse.baseline==0.5), # Total ambigious
                        sum(collapse.baseline==1), # Total with at least one unidirectional link in both rounds
                        calc.precision, # Precision
                        calc.recall, #Recall
                        calc.f.measure, # F-measure
                        calc.accuracy, # Accuracy
                        total.connections) ) # Total links
    
  }
  
  # Train on round 1-3
  
  store.ROC.3 = NULL
  
  for(roundID in 4){
    baseline.links =  (store.links[[1]] + store.links[[2]] + store.links[[3]] ) /3
    calc.diff = baseline.links -  store.links[[roundID]] # calculate differences
    dim(calc.diff)=NULL; calc.diff = calc.diff[!is.na(calc.diff)] # collapse and remove NA
    collapse.baseline = baseline.links; dim(collapse.baseline) = NULL; collapse.baseline = collapse.baseline[!is.na(collapse.baseline)]
    
    calc.diff = round(calc.diff,1) # round to make machine readable
    collapse.baseline = round(collapse.baseline,1) # round to make machine readable
    
    # False positive=1, false negative =-1, ambiguous 1/3 or 2/3 , correct = 0 
    
    total.connections = length(calc.diff)
    
    # Calculate expected precision, recall and F
    calc.fp = sum(calc.diff== 1) + (1/3) * sum(calc.diff== 0.3) + (2/3) * sum(calc.diff== 0.7)
    calc.tp = sum(calc.diff[collapse.baseline==1]==0) + (1/3) * sum(calc.diff[collapse.baseline==0.3]== -0.7) + (2/3) * sum(calc.diff[collapse.baseline==0.7]== -0.3)
    calc.tn = sum(calc.diff[collapse.baseline==0]==0) + (2/3) * sum(calc.diff[collapse.baseline== 0.3]== 0.3) + (1/3) * sum(calc.diff[collapse.baseline== 0.7]== 0.7)
    calc.fn = sum(calc.diff== -1) + (1/3) * sum(calc.diff== -0.3) + (2/3) * sum(calc.diff== -0.7)
    
    # DEBUG: calc.fn+calc.tn+calc.tp+calc.fp
    
    calc.precision = calc.tp/(calc.tp+calc.fp)
    calc.recall = calc.tp/(calc.tp+calc.fn)
    calc.f.measure = 2*(calc.precision*calc.recall)/(calc.precision+calc.recall)
    calc.accuracy = (calc.tp+calc.tn)/total.connections

    store.ROC.3=rbind(store.ROC.3,
                      c(3, # Training round
                        roundID, # Test round
                        sum(collapse.baseline==0), # Total with no link
                        sum(collapse.baseline==0.5), # Total ambigious
                        sum(collapse.baseline==1), # Total with at least one unidirectional link in both rounds
                        calc.precision, # Precision
                        calc.recall, #Recall
                        calc.f.measure, # F-measure
                        calc.accuracy, # Accuracy
                        total.connections) ) # Total links
    
  }
  
  store.ROC.All = rbind(store.ROC.1,store.ROC.2,store.ROC.3) %>% data.frame()
  names(store.ROC.All) = c("train","test","no_link","amb","link","precision","recall","ff","accuracy","contacts")
  store.ROC.All$precision = signif(  store.ROC.All$precision,2)
  store.ROC.All$recall = signif(  store.ROC.All$recall,2)
  store.ROC.All$accuracy = signif(  store.ROC.All$accuracy,2)
  store.ROC.All$ff = signif(  store.ROC.All$ff,2)
  
  save(store.ROC.All,file=paste("outputs/consist",school1,".RData",sep="")) #plot_Sposteriors
  
  
}



# - - - - - - - - - - - - - - - - - - - - -
# Plot distribution of consistency

plot_predictive_power <- function(){
  
  par(mfrow=c(2,2))
  par(mar = c(3,3,1,1),mgp=c(2,0.7,0))
  
  # PLOT Accuracy
  xshift=0.06
  
  store.ROC.All.out = NULL
  for(ii in 1:4){
    
    load(file=paste("outputs/consist",ii,".RData",sep="")) #plot_Sposteriors
    store.ROC.All.out = rbind( store.ROC.All.out,cbind(rep(ii,6),store.ROC.All))
    
    for(typeii in 1:3){
      plotdat = store.ROC.All[store.ROC.All$train==typeii,]
      if(ii==1 & typeii==1){
        plot(plotdat$train,plotdat$accuracy,main="" ,xlab="rounds of training data",ylab="accuracy",xlim=c(0.5,3.5),ylim=c(0.8,1),pch=19,col="white",yaxs="i",xaxt="n")
        grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
      }
      points(plotdat$train -2*xshift+xshift*ii ,plotdat$accuracy, pch=19,col=colourpick1[ii])
    }

  }
  axis(1, at=c(1,2,3), labels=c(1,2,3)) 
  title(LETTERS[1],adj=0)
  
  # PLOT F score
  store.ROC.All.out = NULL
  for(ii in 1:4){
    
    load(file=paste("outputs/consist",ii,".RData",sep="")) #plot_Sposteriors
    store.ROC.All.out = rbind( store.ROC.All.out,cbind(rep(ii,6),store.ROC.All))
    
    for(typeii in 1:3){
      plotdat = store.ROC.All[store.ROC.All$train==typeii,]
      if(ii==1 & typeii==1){
        plot(plotdat$train,plotdat$ff,main="" ,xlab="rounds of training data",ylab="F-measure",xlim=c(0.5,3.5),ylim=c(0.3,1),pch=19,col="white",yaxs="i",xaxt="n")
        grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
      }
      points(plotdat$train -2*xshift+xshift*ii,plotdat$ff, pch=19,col=colourpick1[ii])
    }
    
  }
  axis(1, at=c(1,2,3), labels=c(1,2,3)) 
  
  
  
  names(store.ROC.All.out)=c("school",names(store.ROC.All))
  write.csv(store.ROC.All.out,"plots/Table3.csv")
  title(LETTERS[2],adj=0)
  
  # PLOT INTERNAL VARIABILITY
  
  for(ii in 1:4){
    
    load(file=paste("outputs/outputs",ii,".RData",sep="")) #plot_Sposteriors
    
    node.data4 = node.data[!is.na(rowSums(node.data)),]
    part.4 = length(node.data4[,1])
    
    # Difference for future rounds
    predict.2 = abs(node.data4[,2:4] - node.data4[,1])
    predict.3 = abs(node.data4[,3:4] - apply(node.data4[,1:2],1,mean) )
    predict.4 = abs(node.data4[,4] - apply(node.data4[,1:3],1,mean) )

    if(ii==1){
      plot(2,2,main="" ,xlab="rounds of training data",ylab="difference in degree",xlim=c(0.5,3.5),ylim=c(0,4),pch=19,col="white",yaxs="i",xaxt="n")
      #lines(c(0,4),c(0,0),col="grey")
      grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
    }

    #points(data.plot.degreeX,data.plot.degree1, pch=19,col=colourpick1[ii])
    #lines(data.plot.degreeX,data.plot.degree1, pch=19,col=colourpick[ii])
    points(rep(1,3)-2*xshift+xshift*ii,apply(predict.2,2,mean), pch=19,col=colourpick1[ii])
    points(rep(2,2)-2*xshift+xshift*ii,apply(predict.3,2,mean), pch=19,col=colourpick1[ii])
    points(rep(3,1)-2*xshift+xshift*ii,mean(predict.4), pch=19,col=colourpick1[ii])

  }
  axis(1, at=c(1,2,3), labels=c(1,2,3)) 
  title(LETTERS[3],adj=0)
  
  # PLOT EXTERNAL VARIABILITY
  
  for(ii in 1:4){
    
    load(file=paste("outputs/outputs",ii,".RData",sep="")) #plot_Sposteriors
    
    predict.2 = abs(distn.outside.class[,2:4] - distn.outside.class[,1])
    predict.3 = abs(distn.outside.class[,3:4] - apply(distn.outside.class[,1:2],1,mean) )
    predict.4 = abs(distn.outside.class[,4] - apply(distn.outside.class[,1:3],1,mean) )
    
    if(ii==1){
      plot(2,2,main="" ,xlab="rounds of training data",ylab="difference in external contacts",xlim=c(0.5,3.5),ylim=c(0,1.5),pch=19,col="white",yaxs="i",xaxt="n")
      #lines(c(0,4),c(0,0),col="grey")
      grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
    }
    #points(data.plot.degreeX,data.plot.degree1, pch=19,col=colourpick1[ii])
    #lines(data.plot.degreeX,data.plot.degree1, pch=19,col=colourpick[ii])
    points(rep(1,3)-2*xshift+xshift*ii,apply(predict.2,2,mean), pch=19,col=colourpick1[ii])
    points(rep(2,2)-2*xshift+xshift*ii,apply(predict.3,2,mean), pch=19,col=colourpick1[ii])
    points(rep(3,1)-2*xshift+xshift*ii,mean(predict.4), pch=19,col=colourpick1[ii])
    
  }
  axis(1, at=c(1,2,3), labels=c(1,2,3)) 
  title(LETTERS[4],adj=0)
  
  
  dev.copy(pdf,paste("plots/Figure_4.pdf",sep=""),width=7,height=6)
  dev.off()
  
}

