library(DESeq2)
library(ggplot2)
library(dplyr)

res <- readRDS("Bulk_DEG_data.rds")

ggplot(data=res,aes(x=logFC,y=-log10(padj)))+geom_point(data=subset(res,abs(res$logFC)<=1),aes(size=abs(logFC)),color="gray",alpha=0.5)+
  geom_point(data=subset(res,res$padj>=0.05&abs(res$logFC)>1),aes(size=abs(logFC)),color="gray",alpha=0.5)+
  geom_point(data=subset(res,res$padj<0.05&res$logFC>1),aes(size=abs(logFC)),color="red",alpha=0.5)+
  geom_point(data=subset(res,res$padj<0.05&res$logFC<(-1)),aes(size=abs(logFC)),color="blue",alpha=0.5)+
  geom_hline(yintercept=-log10(0.05),lty=4,lwd=0.6,alpha=0.8)+
  geom_vline(xintercept=c(-1,1),lty=4,lwd=0.6,alpha=0.8)+
  theme_bw()+theme(panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))+
  labs(x="logFC",y="-log10(padj)")+theme(legend.position='none')