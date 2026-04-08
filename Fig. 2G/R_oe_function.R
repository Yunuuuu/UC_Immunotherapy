library(ComplexHeatmap)

R_oe <- as.matrix(
  read.table("R_oe_data.txt",
             sep = "\t",
             header = TRUE,
             row.names = 1)
)

Heatmap(as.matrix(R_oe),border = T,
        show_heatmap_legend=T,
        cluster_rows=F,
        cluster_columns=F,
        row_names_side='right',
        show_column_names=T,
        show_row_names=T,
        col=c("#0099CC", "white", "#CC0033"),
        row_names_gp=gpar(fontsize=10),
        column_names_gp=gpar(fontsize=10),
        heatmap_legend_param=list(
          title="R_o/e",
          at=seq(0,2,by=0.5),
          labels=seq(0,2,by=0.5),
          legend_gp=gpar(fill=c("#0099CC", "white", "#CC0033"))
        ),
        cell_fun=function(j,i,x,y,width,height,fill){
          grid.text(sprintf("%.2f",R_oe[i,j]),x,y,gp=gpar(fontsize=8))
        }
)
