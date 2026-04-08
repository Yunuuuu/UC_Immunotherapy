library(msigdbr)
library(fgsea)
library(dplyr)
library(ggplot2)
library(presto)

###GNLY###
fgseaRes_GNLY <- readRDS("fgseaRes_GNLY.rds")

fgseaResTidy <- fgseaRes_GNLY %>%  as_tibble() %>% arrange(desc(NES))
fgseaResTidy_data <- fgseaResTidy %>% dplyr::select(-leadingEdge, -ES, -nMoreExtreme) %>% arrange(padj) 

# 筛选padj小于0.05的数据
filtered_data <- fgseaResTidy_data[fgseaResTidy_data$padj < 0.05, ]
filtered_data$pathway <- gsub("HALLMARK_", "", filtered_data$pathway)

# 筛选大于0的前10个最大的NES数据
top_positive_NES <- filtered_data[filtered_data$NES > 0, ]
top_positive_NES <- top_positive_NES[order(top_positive_NES$NES, decreasing = TRUE), ][1:16, ]
# 筛选小于0的最小的10个NES数据
top_negative_NES <- filtered_data[filtered_data$NES < 0, ]
top_negative_NES <- top_negative_NES[order(top_negative_NES$NES), ][1:4, ]
# 合并这20个数据
final_data <- rbind(top_positive_NES, top_negative_NES)
final_data$pathway <- factor(final_data$pathway, levels = c(top_positive_NES$pathway, rev(top_negative_NES$pathway)))

# 绘制柱状图
ggplot(final_data, aes(x = NES, y = pathway)) +
  geom_bar(stat = "identity", aes(fill = NES),width=0.8) +
  scale_fill_gradient2(low = "blue", mid="white", high = "red", midpoint = 0)+
  scale_y_discrete(limits = rev(levels(final_data$pathway)))+
  labs(x = "NES", y = "Pathway", title = "GNLY+T_cell") +
  theme_minimal()+ # 设置背景为白色
  theme(axis.title.x = element_text(colour = "black"),  # X轴标题字体颜色为黑色
        axis.title.y = element_text(colour = "black"),  # Y轴标题字体颜色为黑色
        axis.text.x = element_text(colour = "black"),  # X轴刻度标签字体颜色为黑色
        axis.text.y = element_text(colour = "black",size = 8),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        plot.title = element_text(size = 10, hjust = 0.5),
        aspect.ratio = 1.8 )




###Epithelial###
fgseaRes_Epithelial <- readRDS("fgseaRes_Epithelial.rds")

fgseaResTidy <- fgseaRes_Epithelial %>%  as_tibble() %>% arrange(desc(NES))
fgseaResTidy_data <- fgseaResTidy %>% dplyr::select(-leadingEdge, -ES, -nMoreExtreme) %>% arrange(padj) 

# 筛选padj小于0.05的数据
filtered_data <- fgseaResTidy_data[fgseaResTidy_data$padj < 0.05, ]
filtered_data$pathway <- gsub("HALLMARK_", "", filtered_data$pathway)

# 筛选大于0的前10个最大的NES数据
top_positive_NES <- filtered_data[filtered_data$NES > 0, ]
top_positive_NES <- top_positive_NES[order(top_positive_NES$NES, decreasing = TRUE), ][1:12, ]
# 筛选小于0的最小的10个NES数据
top_negative_NES <- filtered_data[filtered_data$NES < 0, ]
top_negative_NES <- top_negative_NES[order(top_negative_NES$NES), ][1:8, ]
# 合并这20个数据
final_data <- rbind(top_positive_NES, top_negative_NES)
final_data$pathway <- factor(final_data$pathway, levels = c(top_positive_NES$pathway, rev(top_negative_NES$pathway)))

# 绘制柱状图
ggplot(final_data, aes(x = NES, y = pathway)) +
  geom_bar(stat = "identity", aes(fill = NES),width=0.8) +
  scale_fill_gradient2(low = "blue", mid="white", high = "red", midpoint = 0)+
  scale_y_discrete(limits = rev(levels(final_data$pathway)))+
  labs(x = "NES", y = "Pathway", title = "Epithelial_cell") +
  theme_minimal()+ # 设置背景为白色
  theme(axis.title.x = element_text(colour = "black"),  # X轴标题字体颜色为黑色
        axis.title.y = element_text(colour = "black"),  # Y轴标题字体颜色为黑色
        axis.text.x = element_text(colour = "black"),  # X轴刻度标签字体颜色为黑色
        axis.text.y = element_text(colour = "black",size = 8),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        plot.title = element_text(size = 10, hjust = 0.5),
        aspect.ratio = 1.8 )



