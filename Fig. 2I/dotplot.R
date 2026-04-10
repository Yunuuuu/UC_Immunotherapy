library(dplyr)
library(GseaVis)


emgt_erda <- readRDS("Fig. 2I/DEG_erda_treatment_egmt.rds")

# 定义感兴趣的GO term
select_go_terms <- c("GOBP_NATURAL_KILLER_CELL_ACTIVATION",
                     "GOBP_T_CELL_ACTIVATION",
                     "GOBP_T_CELL_PROLIFERATION",
                     "GOBP_IMMUNOGLOBULIN_PRODUCTION",
                     "GOBP_CELL_SURFACE_RECEPTOR_SIGNALING_PATHWAY_VIA_STAT",
                     "GOBP_POSITIVE_REGULATION_OF_TYPE_II_INTERFERON_PRODUCTION",
                     "GOBP_NATURAL_KILLER_CELL_MEDIATED_IMMUNITY",
                     "GOBP_LYMPHOCYTE_ACTIVATION_INVOLVED_IN_IMMUNE_RESPONSE",
                     "GOBP_APOPTOTIC_PROCESS",
                     "GOBP_EPITHELIAL_CELL_PROLIFERATION",
                     "GOBP_CELL_CELL_JUNCTION_ORGANIZATION")

# 按 NES 排序
selected_terms <- emgt_erda@result %>%
  filter(Description %in% select_go_terms) %>%
  arrange(desc(NES)) %>%
  pull(Description)

# 构建用于绘图的子集对象
egmt_subset <- emgt_erda
egmt_subset@result <- emgt_erda@result %>% filter(Description %in% selected_terms)

# 绘图
dotplotGsea(data = egmt_subset,
            order.by = "NES",
            str.width = 30)
