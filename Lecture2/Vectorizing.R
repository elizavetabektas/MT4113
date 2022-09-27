##data from https://medium.com/nerd-for-tech/chess-heatmap-where-does-most-of-the-action-take-place-52b2a007dfa2

# Where do knights spend most of their time on the chess board?

KnightHM<-matrix(c(910,3200,2900,3600,4600,4100,1700,440,
  2000,4400,6400,32000,14000,5600,2900,2600,
  5200,12000,51000,14000,12000,67000,8400,3700,
  11000,12000,20000,38000,36000,16000,13000,8200,
  8900,12000,20000,36000,35000,15000,12000,7300,
  4100,11000,56000,14000,14000,63000,8000,3300,
  2600,3000,5600,22000,13000,5000,2500,2300,
  700,1900,3300,3300,4200,4300,1300,390),ncol=8,byrow=T)

image(KnightHM)

rowheat<-rep(NA,8)
for(i in 1:8){
  rowheat[i]<-sum(KnightHM[i,])
}

apply(KnightHM,1,sum)


## The reference sequence of the human mitochondrial genome
chrM<-scan("Lecture2/chrM.fasta")
chrM<-chrM[-c(1:6)]
## How many Gs and Cs do we see?

chrMGCcounts<-rep(NA,length(chrM))

system.time(for(i in 1:length(chrM)){
  counter<-0
  for(j in 1:nchar(chrM[i])){
    if(substr(chrM[i],j,j) %in% c("G","C")){counter<-counter+1}
  }
  chrMGCcounts[i]<-counter
})


sapply(chrM,function(x){sum(unlist(strsplit(x,"")) %in% c("C","G"))})

