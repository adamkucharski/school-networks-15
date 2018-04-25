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

  par(mar = c(3,3,1,1))
  mgp=c(2,0.7,0)

  layout(matrix(c(1,2,3,4,5,5,6,6), 4,2, byrow=T))
  
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
  
  # Plot clustering
  
  for(ii in 1:4){
    
    load(file=paste("outputs/outputs",ii,".RData",sep="")) #plot_Sposteriors

    if(ii==1){
      plot(0,0,type="l",col="white",yaxt="n",xlab="",ylab="",ylim=c(0,1),xlim=c(0,1))
      axis(side = 2, at = c(0.8,0.5,0.2), labels = c("All","F","M") ) #, tcl = -0.7, las = 2)
    }
    #axis(4,col="blue",col.axis="blue")
    #mtext("clustering coefficient", side=4, line=3,col="blue") # Label for 2nd axis
    
    for(kk in 1:4){
      points(y=0.8,x=clust.store[[kk]],col=colourpick[ii])
      if(ii>0){ points(y=0.2,x=clust.storeM[[kk]],col=colourpick[ii]) }
      if(ii!=3){ points(y=0.5,x=clust.storeF[[kk]],col=colourpick[ii]) }
    }

  }
  title(LETTERS[5],adj=0)
  
  # Plot assortativity
  
  for(ii in 1:4){
    
    load(file=paste("outputs/outputs",ii,".RData",sep="")) #plot_Sposteriors
    
    if(ii==1){
      plot(0,0,type="l",col="white",yaxt="n",xlab="",ylab="",ylim=c(0,1),xlim=c(0,1))
      axis(side = 2, at = c(0.75,0.25), labels = c("gender","class") ) #, tcl = -0.7, las = 2)
    }
    #axis(4,col="blue",col.axis="blue")
    #mtext("clustering coefficient", side=4, line=3,col="blue") # Label for 2nd axis
    
    for(kk in 1:4){
      if(ii==3){
        points(y=0.75,x=1,col=colourpick[ii]) # assortativity for single-sex school = 1
      }else{
        points(y=0.75,x=factor.store[kk,1],col=colourpick[ii])
      }
      if(ii>0){ points(y=0.25,x=factor.store[kk,2],col=colourpick[ii]) }
    }
    
  }
  
  title(LETTERS[6],adj=0)

  dev.copy(pdf,paste("plots/Figure_2.pdf",sep=""),width=5,height=6)
  dev.off()

}

# - - - - - - - - - - - - - - - - - - - - -
# Plot distribution of consistency

plot_consistency <- function(){

  par(mfrow=c(2,1),mar = c(5,5,1,1),mgp=c(2,0.7,0))
  
  # PLOT INTERNAL VARIABILITY
  
  for(ii in 1:4){
    
    load(file=paste("data_analysis/outputR",ii,".RData",sep="")) #plot_Sposteriors
    
    
    # Calculate variance of degree distribution
    variance.degree = apply(node.data,1,function(x){y=x[!is.na(x)];max(y)-min(y)}) %>% as.numeric()
    
    data.plot.degree = table(variance.degree)
    data.plot.degree1 = as.numeric(data.plot.degree)/sum(as.numeric(data.plot.degree))
    data.plot.degreeX = as.numeric(names(data.plot.degree))
    
    if(ii==1){
      plot(data.plot.degreeX,data.plot.degree1,main="" ,xlab="difference in node degree",ylab="cumulative proportion",xlim=c(0,13),ylim=c(0,1),pch=19,col="white",yaxs="i")
    }
    #points(data.plot.degreeX,data.plot.degree1, pch=19,col=colourpick1[ii])
    #lines(data.plot.degreeX,data.plot.degree1, pch=19,col=colourpick[ii])
    points(data.plot.degreeX,cumsum(data.plot.degree1), pch=19,col=colourpick1[ii])
    lines(data.plot.degreeX,cumsum(data.plot.degree1), pch=19,col=colourpick[ii])
    
    
  }
  
  title(LETTERS[1],adj=0)
  
  # PLOT EXTERNAL VARIABILITY
  
  for(ii in 1:4){
    
    load(file=paste("data_analysis/outputR",ii,".RData",sep="")) #plot_Sposteriors
  
    data.plot.outside = table(distn.outside.class)
    data.plot.outside1 = as.numeric(data.plot.outside)/sum(as.numeric(data.plot.outside))
    
    if(ii==1){
      plot(as.numeric(names(data.plot.outside)),data.plot.outside1,main="" ,xlab="difference in external contact category",ylab="cumulative proportion",xlim=c(-0.2,4.2),ylim=c(0,1),pch=19,col="white",yaxs="i")
    }
   # points(as.numeric(names(data.plot.outside)),data.plot.outside1, pch=19,col=colourpick1[ii])
    #lines(as.numeric(names(data.plot.outside)),data.plot.outside1, pch=19,col=colourpick[ii])
    points(as.numeric(names(data.plot.outside)),cumsum(data.plot.outside1), pch=19,col=colourpick1[ii])
    lines(as.numeric(names(data.plot.outside)),cumsum(data.plot.outside1), pch=19,col=colourpick[ii])
    
  }

  title(LETTERS[2],adj=0)
  
  
  dev.copy(pdf,paste("plots/Contacts_distn.pdf",sep=""),width=4,height=7)
  dev.off()

}

#flu.output <- tally_flu()

# Output diameter and community stats

output_stats <- function(){
  
  table1=NULL
  
  par(mfrow=c(2,2),mar = c(3,3,1,1),mgp=c(2,0.7,0))

  for(ii in 1:4){
    
    load(file=paste("data_analysis/outputR",ii,".RData",sep="")) #plot_Sposteriors

    totalP = n.total#colSums(node.data>0,na.rm=T)
    
    plot(totalP,col=colourpick1[ii],pch=19,ylim=c(0,10*ceiling(max(totalP)/10)),xlab="round" ,xlim=c(0.5,4.5),ylab="size")
    grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
    
    points(n.total,col=colourpick1[ii],pch=19)
    points(comm.store[,1],col=colourpick1[ii],pch=2)
    points(comm.store[,2],col=colourpick1[ii],pch=3)
    points(diamt.store,col=colourpick1[ii],pch=5)
    title(LETTERS[ii],adj=0)

    table1 = rbind(table1, c(comm.store[,1],comm.store[,2],diamt.store,totalP) )
    
  }
  
  dev.copy(pdf,paste("plots/Diameter_communities.pdf",sep=""),width=5,height=5)
  dev.off()
  
  table1 %>% data.frame()
  rownames(table1)=LETTERS[1:4]
  colnames(table1)=rep(c("Round 1","Round 2","Round 3","Round 4"),4)
  write.csv(table1,"plots/output.csv")

  
}


# - - - - - - - - - - - - - - - - - - - - -
# Predictive power of one round etc.

plot_consistency_pred <- function(){
  
  paired1=4 # Only show repeat links
  
  # Gather network data
  dataset1a[dataset1a=="0_ID_NA"] <- NA # Remove NA entries
  datasetP0=matrix(match(dataset1a, idlist),ncol=7) # Need this here as use participants only in plot
  datasetMAIN=dataset1[!is.na(datasetP0[,1]),] # Characteristics for participants - for file
  datasetP1=matrix(match(dataset1a, idlist),ncol=7) # Need to use participants only in plot
  datasetP1=datasetP1[!is.na(datasetP1[,1]),] # Exclude non-participants from matrix - for plot

  idlist0 = unique(datasetP1[,1])
  
  # Find participant names that appear in all rounds
  #name_twice=sapply(idlist0,function(x){if( (sum(datasetP1[datasetMAIN$Round==1,1]==x)+sum(datasetP1[datasetMAIN$Round==2,1]==x))==2){TRUE}else{FALSE} })
  #name_thrice=sapply(idlist0,function(x){if( (sum(datasetP1[datasetMAIN$Round==1,1]==x)+sum(datasetP1[datasetMAIN$Round==2,1]==x)+sum(datasetP1[datasetMAIN$Round==3,1]==x))==3){TRUE}else{FALSE} })
  name_four=sapply(idlist0,function(x){if( sum(datasetP1[datasetMAIN$Round==1,1]==x)==1 & sum(datasetP1[datasetMAIN$Round==2,1]==x)==1 & sum(datasetP1[datasetMAIN$Round==3,1]==x)==1 & sum(datasetP1[datasetMAIN$Round==4,1]==x)==1 ){TRUE}else{FALSE} })
  
  #if(paired1==2){idlist0=idlist0[name_twice]} # **** Only paired data used ****
  #if(paired1==3){idlist0=idlist0[name_thrice]} # **** Only 3-link data used ****
  if(paired1==4){idlist0=idlist0[name_four]} # **** Only 4-link data used ****
  
  
  # Who reported whom - compile matrix
  store.links = list()
  
  for(roundID in 1:4){
    round1.c = datasetP1[datasetMAIN$Round==roundID,]
    contact.m = matrix(0,nrow=length(idlist0),ncol=length(idlist0))
    
    for(ii in 1:length(idlist0)){
      for(jj in 1:length(idlist0)){
        if(ii<jj){
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
  total.connections = length(idlist0)*(length(idlist0)-1)/2 # Total possible connections
  null.connect = total.connections + length(idlist0) #Total connections plus diagonal
  store.ROC.1 = NULL
  
  for(roundID in 2:4){
    baseline.links = store.links[[1]]
    calc.diff = baseline.links - store.links[[roundID]]
    dim(calc.diff)=NULL
    # False positive=1, false negative =-1
    
    # c("type","round","no_link","amb","link","DpredP","DpredF","accuracy","contacts")
    
    store.ROC.1=rbind(store.ROC.1,
            c(1,roundID,sum(baseline.links==0)-null.connect,0,sum(baseline.links==1),sum(calc.diff==1),sum(calc.diff==-1), 1-(sum(calc.diff==1) + sum(calc.diff==-1))/total.connections, total.connections) )
  }
  
  # Round 1-2 -- use probabilistic accuracy measurement?
  
  store.ROC.2 = NULL
  
  for(roundID in 3:4){
    #baseline.links = (store.links[[1]] + store.links[[2]] ) > 0 
    baseline.links = (store.links[[1]] + store.links[[2]] ) #> 0 # Allow ambiguity
    
    # baseline.links = 2:both positive  1:either  0:both negative
    #calc.diff = baseline.links[baseline.links!=1] - store.links[[roundID]] [baseline.links!=1] # Remove either/or links
    calc.diff = baseline.links - 2 * store.links[[roundID]] # False positive=2, false negative =-2, ambiguous = -1/1, correct = 0 
    
    dim(calc.diff)=NULL
    # False positive=2, false negative =-1  # ambiguous = 0 
    ambig = sum(baseline.links==1)
    
    # Accuracy minus ambiguous = 1-(sum(calc.diff==2) + sum(calc.diff==-1))/(total.connections-ambig)
    accuracy.A = 1-(0.5* (sum(calc.diff==1) + sum(calc.diff==-1)) + sum(calc.diff==2) + sum(calc.diff==-2))/(total.connections) # Accuracy with 50% on ambiguous 
    
    store.ROC.2=rbind(store.ROC.2,
                      c(2,roundID,sum(baseline.links==0)-null.connect,sum(baseline.links==1),sum(baseline.links==2),sum(calc.diff==2),sum(calc.diff==-1), 
                        accuracy.A ,total.connections) )
  }
  
  # Round 1-3
  
  store.ROC.3 = NULL
  
  for(roundID in 4){
    #baseline.links = (store.links[[1]] + store.links[[2]] + store.links[[3]] ) > 0 
    baseline.links = (store.links[[1]] + store.links[[2]] + store.links[[3]] )
    
    # Set link as majority rule (i.e. 2 links = linked):
    #baseline.links[baseline.links==1] = 0;     baseline.links[baseline.links==2] = 3
    calc.diff = baseline.links - 3 * store.links[[roundID]] 

    # False positive=3, false negative =-3, ambiguous 1/3 = 2/-2  ambiguous 2/3 = 1/-1, correct = 0 
    
    #calc.diff = baseline.links - store.links[[roundID]]
    dim(calc.diff)=NULL
    
    #accuracy.A = 1-((1/3)* (sum(calc.diff==2) + sum(calc.diff==-2)) + (2/3)* (sum(calc.diff==1) + sum(calc.diff==-1)) + sum(calc.diff==3) + sum(calc.diff==-3))/(total.connections) # Accuracy with 50% on ambiguous 
    accuracy.A = 1-((1/3)* (sum(calc.diff==2) + sum(calc.diff==-2)) + (2/3)* (sum(calc.diff==1) + sum(calc.diff==-1)) + sum(calc.diff==3) + sum(calc.diff==-3))/(total.connections) # Accuracy with 50% on ambiguous 
    
    
    # False positive=1, false negative =-1
    store.ROC.3=rbind(store.ROC.3,
                      c(3,roundID,sum(baseline.links==0)-null.connect,sum(baseline.links==2) + sum(baseline.links==1),sum(baseline.links==3),sum(calc.diff==3),sum(calc.diff==-1), 
                        accuracy.A,total.connections) )
  }
  
  store.ROC.All = rbind(store.ROC.1,store.ROC.2,store.ROC.3) %>% data.frame()
  names(store.ROC.All) = c("train","test","no_link","amb","link","DpredP","DpredF","accuracy","contacts")
  store.ROC.All$accuracy = signif(  store.ROC.All$accuracy,3)
  
  save(store.ROC.All,file=paste("data_analysis/consist",school1,".RData",sep="")) #plot_Sposteriors
  
  
}



# - - - - - - - - - - - - - - - - - - - - -
# Plot distribution of consistency

plot_predictive_power <- function(){
  
  par(mfrow=c(3,1))
  par(mar = c(3,3,1,1),mgp=c(2,0.7,0))
  
  # PLOT CONTACT PREDICTABILITY
  store.ROC.All.out = NULL
  
  for(ii in 1:4){
    
    load(file=paste("data_analysis/consist",ii,".RData",sep="")) #plot_Sposteriors
    store.ROC.All.out = rbind( store.ROC.All.out,cbind(rep(ii,6),store.ROC.All))
    
    for(typeii in 1:3){
      plotdat = store.ROC.All[store.ROC.All$train==typeii,]
      
      if(ii==1 & typeii==1){
        plot(plotdat$train,plotdat$accuracy,main="" ,xlab="rounds of training data",ylab="accuracy",xlim=c(0.5,3.5),ylim=c(0.5,1),pch=19,col="white",yaxs="i",xaxt="n")
        grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
      }
      #points(data.plot.degreeX,data.plot.degree1, pch=19,col=colourpick1[ii])
      #lines(data.plot.degreeX,data.plot.degree1, pch=19,col=colourpick[ii])
      points(plotdat$train,plotdat$accuracy, pch=19,col=colourpick1[ii])
      #lines(plotdat$round,plotdat$accuracy, pch=19,col=colourpick[ii])
    
    }

  }
  axis(1, at=c(1,2,3), labels=c(1,2,3)) 
  
  
  
  names(store.ROC.All.out)=c("school",names(store.ROC.All))
  write.csv(store.ROC.All.out,"plots/predict.csv")
  title(LETTERS[1],adj=0)
  
  # PLOT INTERNAL VARIABILITY
  
  for(ii in 1:4){
    
    load(file=paste("data_analysis/outputR",ii,".RData",sep="")) #plot_Sposteriors
    
    node.data4 = node.data[!is.na(rowSums(node.data)),]
    part.4 = length(node.data4[,1])
    
    # Predict future rounds

    predict.2 = abs(node.data4[,2:4] - node.data4[,1])
    predict.3 = abs(node.data4[,3:4] - apply(node.data4[,1:2],1,mean) )
    predict.4 = abs(node.data4[,4] - apply(node.data4[,1:3],1,mean) )

    #plot(rep(2,3),apply(predict.2,2,mean))

    if(ii==1){
      plot(2,2,main="" ,xlab="rounds of training data",ylab="difference in degree",xlim=c(0.5,3.5),ylim=c(0,4),pch=19,col="white",yaxs="i",xaxt="n")
      #lines(c(0,4),c(0,0),col="grey")
      grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
    }

    #points(data.plot.degreeX,data.plot.degree1, pch=19,col=colourpick1[ii])
    #lines(data.plot.degreeX,data.plot.degree1, pch=19,col=colourpick[ii])
    points(rep(1,3),apply(predict.2,2,mean), pch=19,col=colourpick1[ii])
    points(rep(2,2),apply(predict.3,2,mean), pch=19,col=colourpick1[ii])
    points(rep(3,1),mean(predict.4), pch=19,col=colourpick1[ii])

  }
  axis(1, at=c(1,2,3), labels=c(1,2,3)) 
  title(LETTERS[2],adj=0)
  
  # PLOT EXTERNAL VARIABILITY
  
  for(ii in 1:4){
    
    load(file=paste("data_analysis/outputR",ii,".RData",sep="")) #plot_Sposteriors
    
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
    points(rep(1,3),apply(predict.2,2,mean), pch=19,col=colourpick1[ii])
    points(rep(2,2),apply(predict.3,2,mean), pch=19,col=colourpick1[ii])
    points(rep(3,1),mean(predict.4), pch=19,col=colourpick1[ii])
    
  }
  axis(1, at=c(1,2,3), labels=c(1,2,3)) 
  title(LETTERS[3],adj=0)
  
  
  dev.copy(pdf,paste("plots/Contacts_distn.pdf",sep=""),width=4,height=6)
  dev.off()
  
}

