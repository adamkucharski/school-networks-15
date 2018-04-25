# - - - - - - - - - - - - - - - - - - 
# Load specific school data
# - - - - - - - - - - - - - - - - - - 

# - - - 
# Define school, load contact data and meta data
schoolID=schooltab[school1]

dataset1a = read.csv(paste("data/school_data_",school1,"_1a.csv",sep=""), stringsAsFactors = F) %>% as.matrix(); dataset1a = dataset1a[,2:8]
dataset1 = read.csv(paste("data/school_data_",school1,"_1.csv",sep=""), stringsAsFactors = F) %>% data.frame() ; dataset1$X = NULL

# In dataset1, 1=M, 2=F

# - - - 
# Set up participants and contact lists
# Pick out unique participants
idlist = unique(dataset1a[,1])

# Pick out unique contacts
idlistAll=sort(as.character(melt(dataset1a)$value))
reorderN=c(match(idlist,idlistAll),match(idlistAll[!idlistAll %in% idlist],idlistAll)) # Put participants first
idlistAll=sort(idlistAll[reorderN])
idlistAll=unique(c(idlist,idlistAll)) 
idlistAll[idlistAll=="_"]=NA # Remove null entries

