cell_proportion <- as.data.frame(
  read.table("cell_proportion_data.txt",
             sep = "\t",
             header = TRUE,
             row.names = 1)
)

df_percent <- cell_proportion %>%
  group_by(sample_id, cell_type) %>%
  summarise(count = n()) %>%
  group_by(sample_id) %>%
  mutate(percent = count / sum(count) * 100)

#####
data1 <- df_percent %>%
  filter(cell_type == "PD-1+TIM-3+T_cell")
ggplot(data1, aes(x = sample_id, y = percent, fill = sample_id))  +
  labs(x = "", y = "Proportion (%)",title = "PD-1+TIM-3+T_cell") +
  geom_bar(stat = "identity", position = "dodge", width = 0.7,color="black") +
  scale_fill_manual(values = c("#cbf4e7", "#fddd96")) +
  theme_bw() + 
  theme(legend.position = "none") +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    axis.title.x = element_text(colour = "black"),
    axis.title.y = element_text(colour = "black", size = 12),
    axis.text.x = element_text(colour = "black", angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(colour = "black"),
    panel.grid = element_blank(),plot.title = element_text(size = 12, hjust = 0.5),
    aspect.ratio = 2
  )

#####
data2 <- df_percent %>%
  filter(cell_type == "PD-1+T_cell")
ggplot(data2, aes(x = sample_id, y = percent, fill = sample_id))  +
  labs(x = "", y = "Proportion (%)",title = "PD-1+T_cell") +
  geom_bar(stat = "identity", position = "dodge", width = 0.7,color="black") +
  scale_fill_manual(values = c("#cbf4e7", "#fddd96")) +
  theme_bw() + 
  theme(legend.position = "none") +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    axis.title.x = element_text(colour = "black"),
    axis.title.y = element_text(colour = "black", size = 12),
    axis.text.x = element_text(colour = "black", angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(colour = "black"),
    panel.grid = element_blank(),plot.title = element_text(size = 12, hjust = 0.5),
    aspect.ratio = 2
  )

#####
data3 <- df_percent %>%
  filter(cell_type == "TIM-3+T_cell")
ggplot(data3, aes(x = sample_id, y = percent, fill = sample_id))  +
  labs(x = "", y = "Proportion (%)",title = "TIM-3+T_cell") +
  geom_bar(stat = "identity", position = "dodge", width = 0.7,color="black") +
  scale_fill_manual(values = c("#cbf4e7", "#fddd96")) +
  theme_bw() + 
  theme(legend.position = "none") +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    axis.title.x = element_text(colour = "black"),
    axis.title.y = element_text(colour = "black", size = 12),
    axis.text.x = element_text(colour = "black", angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(colour = "black"),
    panel.grid = element_blank(),plot.title = element_text(size = 12, hjust = 0.5),
    aspect.ratio = 2
  )

#####
data4 <- df_percent %>%
  filter(cell_type == "NK_like")
ggplot(data4, aes(x = sample_id, y = percent, fill = sample_id))  +
  labs(x = "", y = "Proportion (%)",title = " NK_like") +
  geom_bar(stat = "identity", position = "dodge", width = 0.7,color="black") +
  scale_fill_manual(values = c("#cbf4e7", "#fddd96")) +
  theme_bw() + 
  theme(legend.position = "none") +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    axis.title.x = element_text(colour = "black"),
    axis.title.y = element_text(colour = "black", size = 12),
    axis.text.x = element_text(colour = "black", angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(colour = "black"),
    panel.grid = element_blank(),plot.title = element_text(size = 12, hjust = 0.5),
    aspect.ratio = 2
  )

#####
data5 <- df_percent %>%
  filter(cell_type == "GNLY+T_cell")
ggplot(data5, aes(x = sample_id, y = percent, fill = sample_id))  +
  labs(x = "", y = "Proportion (%)",title = " GNLY+T_cell") +
  geom_bar(stat = "identity", position = "dodge", width = 0.7,color="black") +
  scale_fill_manual(values = c("#cbf4e7", "#fddd96")) +
  theme_bw() + 
  theme(legend.position = "none") +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    axis.title.x = element_text(colour = "black"),
    axis.title.y = element_text(colour = "black", size = 12),
    axis.text.x = element_text(colour = "black", angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(colour = "black"),
    panel.grid = element_blank(),plot.title = element_text(size = 12, hjust = 0.5),
    aspect.ratio = 2
  )
