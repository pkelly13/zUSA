#Figures

#start with boxplots showing just large scale differences between reservoirs and natural lakes
#load NLA zoop data
setwd('~/Documents/NLA zoops')

dat<-read.csv('NLA_zooplankton_dataFamily.csv')
#rotifer richness and abundance - need to use data from the 80 net

#use only Primary and 243 mesh size for zoops and 80 for rotifers - gets rid of duplicate samples
d<-dat[dat$SAMPLE.TYPE=='PRIMARY' & dat$MESH.SIZE==243 & dat$VISIT.ID==1,]
d.80<-dat[dat$SAMPLE.TYPE=='PRIMARY' & dat$MESH.SIZE==80 & dat$VISIT.ID==1,]

#remove #NA 
d<-d[d$Reservoir.or.Natural.Lake!='#N/A',]
d.80<-d.80[d.80$Reservoir.or.Natural.Lake!='#N/A',]

good.regions<-c('II','III','IV','V','VI','VII')
d<-d[d$NUT.REG %in% good.regions,]
d$NUT.REG<-factor(d$NUT.REG,levels=good.regions)


#Make data farem of zooplankton type abundances
type<-data.frame(lakeID=d$LAKE.ID,Cladocera=d$Cladoceran.Abundance,Copepod=d$Copepod.Abundance,Calanoid=d$Calanoid.Abundance,Daphnia=d$Daphnia.Abundance,lakeType=d$Reservoir.or.Natural.Lake)
type[is.na(type)]=0

t<-melt(type)

library(scales)
setwd('~/Documents/NLA zoops/poster figs')

png('zooplanktonByTaxa.png',width=600,height=500)
ggplot(data=t,aes(x=factor(variable),y=value+min(t$value[t$value>0]),fill=lakeType))+geom_boxplot()+scale_y_log10(breaks=c(0.01,1,100),labels=c('0.01','1','100'))+theme_bw()+ylab(expression(paste('Zooplankton Abundance (ind L'^-1,')')))+xlab('')+scale_fill_manual(values=c('darkorange','deepskyblue'),name='Lake Type')+theme(text=element_text(size=25,face='bold'),axis.text.y=element_text(size=25,face='bold'),axis.text.x=element_text(size=15,face='bold'))+geom_text(aes(x=2,y=150),label='*',size=12)+geom_text(aes(x=3,y=150),label='*',size=12)
dev.off()

#Now do the same for richness
richness<-data.frame(lakeID=d$LAKE.ID,Cladocera=d$CladFamilyRichness,Copepod=d$CopepodFamilyRichness,Calanoid=d$CalanoidFamilyRichness,lakeType=d$Reservoir.or.Natural.Lake)
richness[is.na(richness)]=0
rich<-melt(richness)

avg.rich<-aggregate(rich$value,by=list(rich$lakeType,rich$variable),mean)
colnames(avg.rich)<-c('lakeType','taxa','richness')
se.rich<-aggregate(rich$value,by=list(rich$lakeType,rich$variable),std.error)
colnames(se.rich)<-c('lakeType','taxa','se')
combined<-merge(avg.rich,se.rich,by=c('lakeType','taxa'))
limits<-aes(ymax=combined$avg+combined$se,ymin=combined$avg-combined$se)
dodge <- position_dodge(width=0.9)

png('zooplanktonRichness.png',width=600,height=500)
ggplot(data=combined,aes(x=taxa,y=richness,fill=lakeType))+geom_bar(stat='identity',position=position_dodge())+ylab('Richness')+xlab('')+theme_bw()+scale_fill_manual(values=c('darkorange','deepskyblue'),name='Lake Type')+theme(text=element_text(size=25),axis.text.y=element_text(size=25,face='bold'))+coord_fixed(ratio=1.5)+geom_errorbar(aes(ymax=combined$richness+combined$se,ymin=combined$richness-combined$se), position=dodge, width=0.25)+geom_text(aes(x=2,y=2.3),label='*',size=12)+geom_text(aes(x=3,y=2.3),label='*',size=12)
dev.off()

png('totalZooplanktonAbundance_EcoRegion.png',width=600,height=500)
ggplot(data=d,aes(x=NUT.REG,y=ZooAbundance+min(d$ZooAbundance[d$ZooAbundance>0]),fill=Reservoir.or.Natural.Lake.))+geom_boxplot()+scale_y_log10(breaks=c(0.1,1,10,100),labels=c('0.1','1','10','100'))+theme_bw()+scale_fill_manual(values=c('darkorange','deepskyblue'),name='Lake Type')+theme(text=element_text(size=25,face='bold'),axis.text.y=element_text(size=25,face='bold'))+ylab(expression(paste('Abundance (ind L'^-1,')')))+xlab('')
dev.off()

#cladocerans
png('CladoceranAbundance_EcoRegion.png',width=600,height=500)
ggplot(data=d,aes(x=NUT.REG,y=Cladoceran.Abundance+min(d$Cladoceran.Abundance[d$Cladoceran.Abundance>0],na.rm=T),fill=Reservoir.or.Natural.Lake.))+geom_boxplot()+scale_y_log10(breaks=c(0.1,1,10,100),labels=c('0.1','1','10','100'))+theme_bw()+scale_fill_manual(values=c('darkorange','deepskyblue'),name='Lake Type')+theme(text=element_text(size=25,face='bold'),axis.text.y=element_text(size=25,face='bold'))+ylab(expression(paste('Abundance (ind L'^-1,')')))+xlab('')
dev.off()

#copepods
png('CopepodAbundance_EcoRegion.png',width=600,height=500)
ggplot(data=d,aes(x=NUT.REG,y=Copepod.Abundance+min(d$Copepod.Abundance[d$Copepod.Abundance>0],na.rm=T),fill=Reservoir.or.Natural.Lake.))+geom_boxplot()+scale_y_log10(breaks=c(0.1,1,10,100),labels=c('0.1','1','10','100'))+theme_bw()+scale_fill_manual(values=c('darkorange','deepskyblue'),name='Lake Type')+theme(text=element_text(size=25,face='bold'),axis.text.y=element_text(size=25,face='bold'))+ylab(expression(paste('Abundance (ind L'^-1,')')))+xlab('')
dev.off()