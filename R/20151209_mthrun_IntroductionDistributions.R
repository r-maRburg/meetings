# Folie 2: Echter Datensatz
path=ReDi('Hydrologie2015/09Originale')
file.name='HydrologieData.lrn'
V=ReadLRN(file.name,path)
V$Header[1]
# Gaengigste Visualisierungsform univariater Daten (1 Variable)
hist(V$Data[,8])

# Folie 3:Choice of bin critical!
# OptimalNoBins {AdaptGauss}
optNrOfBins = OptimalNoBins(V$Data[,8])
minData = min(V$Data[,8],na.rm = TRUE)
maxData = max(V$Data[,8],na.rm = TRUE)
i = maxData-minData
optBreaks = seq(minData, maxData, i/optNrOfBins) # bins in fixed intervals
hist(V$Data[,8], breaks=optBreaks)
vline(mean(V$Data[,8]))

# Folie 4: Visualisierungen einer Variablen
getVarByHeader(V,5) #Wt13
# Variable Anschauen
InspectVariable(Wt13,'Wt13',5)
PDEnormrobust(Wt13)
vline(mean(Wt13))

#Folie5: EM
#Normaler Expectation Maximization Algorithmus
library(mclust)
MaxNumberofIterations=100
L=2 #Anzahl Gaussians geschaetzt
em=densityMclust(Wt13,G=L,modelName='V',parameters=list(control=list(itmax=c(MaxNumberofIterations,MaxNumberofIterations))))
res=em$parameters
output <- list(Means=unname(res$mean),SDs=unname(res$variance$sigmasq),Weights=unname(res$pro))
PlotGaussianMixtures(Wt13,output$Means,output$SDs,output$Weights, SingleGausses=T,ylim=c(0,0.18))
# eductated guess der start parameter
Means=c(4,13)
SDs=c(1.5,2)
Weights=c(0.1,0.9)
PlotGaussianMixtures(Wt13,Means,SDs,Weights, SingleGausses=T,ylim=c(0,0.18))

em=densityMclust(Wt13,G=L,modelName='V',parameters=list(pro=Weights,mean=Means,variance=SDs,control=list(itmax=c(MaxNumberofIterations,MaxNumberofIterations))))
PlotGaussianMixtures(Wt13,output$Means,output$SDs,output$Weights, SingleGausses=T,ylim=c(0,0.18))


library(shiny)
library(caTools)
library(mclust)
# GMM
res=AdaptGauss(Wt13)
dput(res)
res=list(Means = c(3.75384083970312, 13.298048), SDs = c(1.01018108347295, 
2.556969), Weights = c(0.0674245578010382, 0.932575442198962))
res=AdaptGauss(Wt13,res$Means,res$SDs,res$Weights)
#################################################
## Verifizierung
# QQplot mehrmals ausfuehren -> Zahlenzufallsgenerator
qqplotGMM(Wt13,M=res$Means,S=res$SDs,W=res$Weights,Line=T,PointWidth=0.3,ylab='Wt13')
# einmal ausfueren
Vchi=Chi2testMixtures(Wt13,Means=res$Means,SDs=res$SDs,Weights=res$Weights,PlotIt=T,VarName = 'Wt13')
#Vks=KStestMixtures(Wt13,Means=res$Means,SDs=res$SDs,Weights=res$Weights,PlotIt=T)

res2=BayesForMixes(Wt13,Means=res$Means,SDs=res$SDs,Weights=res$Weights,PlotIt=T)
DecisionBoundaries = BayesDecisionBoundaries(res$Means,res$SDs,res$Weights)
DecisionBoundaries

Cls = ClassifyByDecisionBoundaries(Wt13,DecisionBoundaries)
## Klassen anschauen
PlotGaussMixesAndBoundaries(Wt13,Means=res$Means,SDs=res$SDs,Weights=res$Weight,SingleGausses=T,xlab='Wt13(black)')
grid()
########################################################################
# ungleich Verteilung
getVarByHeader(V,7) #sol71
InspectVariable(sol71,'sol71',7)
library('plotrix')
library('Hmisc')
res=ABCanalysisPlot(sol71)
InspectVariable(sol71[res$ABCanalysis$Aind])

# ABCanalyse mit PCA von mlbench.waveform Datensatz
library('mlbench')
data=mlbench.waveform(150)

res <- prcomp(x=data$x,retx=T,scale=F,tol = 0,center=F)
Eigenwerte=res$sdev

GesamtVarianz  = sum(Eigenwerte)
ErklaerteVarianzen = Eigenwerte/GesamtVarianz *100
KummulierteVarianzen = cumsum(ErklaerteVarianzen)

plot(Eigenwerte,type='l',main='scree plot, waveform data, n=150',xlab='sorted Eigenvalues of PCA',ylab='Eigenvalues')
library('Hmisc')
ABCplot(Eigenwerte)

library('plotrix')
res2=ABCanalysisPlot(Eigenwerte)
plot(res$x[,1],res$x[,2])
ClassPlot(res$x[,1],res$x[,2],Cls=data$classes)
title('waveform data, n=150')