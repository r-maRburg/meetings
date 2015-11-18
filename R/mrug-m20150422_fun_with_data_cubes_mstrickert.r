####  Fun with data cubes  ####
#
# Mon Apr 20 21:26:44 CEST 2015
#
# Marc.Strickert@uni-marburg.de
#

# load experimental data cube to variable rn_all
load("meeting_marc.Rdata")

# overview: target_id X time_steps X experiment_id 
str(rn_all)

# get target names
nms <- dimnames(rn_all)[[1]]
head(nms)
idx_16 <- grep("-.*16.*-",nms)
nms[idx_16]

targ_id <- nms[idx_16[1]]
plot(rn_all[targ_id , ,1],type='l',lwd=2,col="purple",
     main=targ_id,xlab="time",ylab="Intensity")
grid()

# get mean values for each target
dat_sub_16 <- rn_all[idx_16,,1]
rmns <- rowMeans(dat_sub_16)
abline(h=rmns[1],lwd=3,lty="dotted")

# similar case...
idx_887 <- grep("887",nms)
dat_sub_887 <- rn_all[idx_887, ,1]
rowMeans(dat_sub_887)  # oops!
nms[idx_887]
dat_sub_887
str(dat_sub_887)

# ...needs more general handling
dat_sub_887 <- rn_all[idx_887, ,1,drop=F] # good advice to use drop in such scenarios
dat_sub_887
rowMeans(dat_sub_887)

# now: get all experiment names
nms <- dimnames(rn_all)[[3]]
nms

# take some data
dat_s3_ctr_09 <- rn_all[ , , "S3_CTR_09"]
dim(dat_s3_ctr_09)

# transpose...
dat_s3_ctr_09_t1 <- t(dat_s3_ctr_09)
# ...alternatively: generalize transpose by dimension swap for later use 
dat_s3_ctr_09_t2 <- aperm(dat_s3_ctr_09,c(2,1))

# how many differences are there?
sum(dat_s3_ctr_09_t1 != dat_s3_ctr_09_t2)

# take dimensions (of non-transposed data)
dm <- dim(dat_s3_ctr_09)

# plot connected lines
plot(rep(1:dm[2],dm[1]), dat_s3_ctr_09_t1, type='l')

# plot, disconnect lines via introduction of NA
plot(rep(c(1:dm[2],NA),dm[1]), rbind(dat_s3_ctr_09_t2,NA), type='l', 
     col="#1111FF55", lwd=1.5,
     main="Amp. curves for S3_CTR_09",xlab="cycle",ylab="intensity")

# at which positions are the maxima?
table(apply(dat_s3_ctr_09,1,which.max))

# apply for average calculation and faster specialized version
mn2 <- mn1 <- NULL
system.time(replicate(1000,mn1 <<- apply(dat_s3_ctr_09,2,mean)))
system.time(replicate(1000,mn2 <<- colMeans(dat_s3_ctr_09)))

# not exactly identical, but similar enough
all.equal(mn1,mn2,tolerance = 1e-16)

# add mean to plot
lines(1:dm[2],mn1,lwd=4,col="red")

# define color gradient blue to red via white
clr_fun <- colorRampPalette(c('blue','white','red'))

par(mfrow=c(1,2))
# heat map with low values as blue
image(1:dm[2],1:dm[1],t(dat_s3_ctr_09),col=clr_fun(100),main="raw data")

# approximate first derivative by diff function
# that produces vectors, not single values like mean()
dat_s3_ctr_09_diff_t <- apply(dat_s3_ctr_09,1,diff)

# if apply to rows (1st dimension) column vectors are returned
dm <- dim(dat_s3_ctr_09_diff_t)
dm
image(1:dm[1],1:dm[2],dat_s3_ctr_09_diff_t,col=clr_fun(100),main="1st difference")

# focus on controls at time point 9 (-> three experiment plates)
idx_ctr_09 <- grep("CTR.*09", nms)

dat_sub <- log(rn_all[ , ,idx_ctr_09]) # log for more plot emphasiz
dim(dat_sub)

# check what you can do inside a loop over data dimensions
apply(dat_sub,3,function(x) browser())
apply(dat_sub,c(1,2),function(x) browser())

# take median of the control plates
dat_sub_median_plate <- apply(dat_sub,c(1,2),median)

# create plot matrix, abusing sapply, and also show median plate
par(mfrow=c(2,2)) 
sapply(1:3,function(idx) image(dat_sub[ , ,idx],main=idx,col=clr_fun(32)))
image(dat_sub_median_plate,main="Median",col=clr_fun(32))


# for each target (1st dim) and experiment(3rd dim) calculate amplication derivatives
rn_all_diff <- apply(rn_all,c(1,3),diff)
dim(rn_all_diff) # result
dim(rn_all) # original reference data

# correct for ordering issues
rn_all_diff <- aperm(rn_all_diff,c(2,1,3))

dim(rn_all_diff)

# Summary:
#
# apply   --- over multiple data dimensions
# aperm   --- generalized transpose for rotating the data cube
# grep    --- flexible name picking
# browser --- in situ debugging
# [,, drop=F] maintains original data dimension in sub-array, cf. drop()
# colMeans vs. apply -> speed of execution
# semi-transparent plotting by color-spec "#RRGGBBAA"
# using NA for plotting disconnected lines
#
# fin, thanks!
