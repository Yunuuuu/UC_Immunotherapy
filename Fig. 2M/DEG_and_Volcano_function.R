library(ggrepel)
library(ggplot2)

sce.E.markers <- readRDS("DEG_data.rds")

########
U511UTUCOE.genes<- sce.E.markers %>% dplyr::filter(cluster == "U511UTUCOE") %>% arrange(desc(avg_log2FC)) 
E.genes<-U511UTUCOE.genes
logFC_cutoff=0
E.genes$sig=as.factor(ifelse(E.genes$p_val_adj<0.05&abs(E.genes$avg_log2FC)>logFC_cutoff,ifelse(E.genes$avg_log2FC>logFC_cutoff,'UP','DOWN'),'NOT'))
rownames(E.genes)<-gsub("\\.1$", "", rownames(E.genes))


########
show_list<-c("XCL2",
             "CCL4L2",
             "IFNG",
             "GZMB",
             "XCL1",
             "CCL4")

ggplot(data=E.genes,aes(x=avg_log2FC,y=-log10(p_val_adj)))+geom_point(data=subset(E.genes,abs(E.genes$avg_log2FC)<=0),aes(size=abs(avg_log2FC)),color="gray",alpha=0.5)+
  geom_point(data=subset(E.genes,E.genes$p_val_adj>=0.05&abs(E.genes$avg_log2FC)>0),
             aes(size=abs(avg_log2FC)),
             color="gray",alpha=0.5)+
  geom_point(data=subset(E.genes,E.genes$p_val_adj<0.05&E.genes$avg_log2FC>0),
             aes(size=abs(avg_log2FC)),
             color="red",alpha=0.5)+
  geom_point(data=subset(E.genes,E.genes$p_val_adj<0.05&E.genes$avg_log2FC<0),
             aes(size=abs(avg_log2FC)),
             color="blue",alpha=0.5)+
  geom_hline(yintercept=-log10(0.05),lty=4,lwd=0.6,alpha=0.8)+geom_vline(xintercept=c(0,0),lty=4,lwd=0.6,alpha=0.8)+
  theme_bw()+ theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    axis.line = element_line(colour = "black"))+
  labs(x="logFC",y="-log10(padj)")+theme(legend.position='none')+
  geom_text_repel(data=E.genes[show_list,],aes(label=gene),color="black",alpha=0.8)
