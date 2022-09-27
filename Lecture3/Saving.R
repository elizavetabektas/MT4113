tmpmat<-matrix(rnorm(100000),ncol=1000)

save(tmpmat,file="tmpmat1.RData",version=1)
save(tmpmat,file="tmpmat2.RData",version=2)
save(tmpmat,file="tmpmat3.RData",version=3)

save(tmpmat,file="tmpmat3c.RData",version=3,compression_level = 0)




saveRDS(tmpmat, 'tmpmat.rds', compress='xz')

## can load into a different name

