library(ggplot2)

scRNA_data <- as.data.frame(
  read.table("scRNA_data.txt",
             sep = "\t",
             header = TRUE,
             row.names = 1)
)

ggplot(scRNA_data, aes(UMAP_1, UMAP_2, color = Finalcelltype)) +
  geom_point(size = 0.6) +
  facet_wrap(~orig.ident, ncol = 2) +
  theme_classic() +
  labs(color = "") +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

