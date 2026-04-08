# pak::pak("Yunuuuu/ggstattest")
# pak::pak("Yunuuuu/biotidy")
# tsce: A `SingleCellExperiment` object for all T cells
tcell_markers <- c("CD8A", "CD8B", "IFNG", "GZMB", "GNLY", "HAVCR2", "PDCD1")
tcell_marker_box_data <- biotidy::makePerCellDF(
    tsce,
    features = tcell_markers,
    use_coldata = c("Sample", "FGFR3"),
    assay = "logcounts",
    melt = TRUE
)
pvalues <- vapply(
    split(tcell_marker_box_data, ~.features),
    function(data) wilcox.test(.assay ~ FGFR3, data = data)$p.value,
    numeric(1L)
)
tcell_marker_box <- lapply(
    list(tcell_markers[seq_len(5L)], tcell_markers[-seq_len(5L)]),
    function(markers) {
        labels <- stats::symnum(
            p.adjust(pvalues[markers]),
            cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
            symbols = c("****", "***", "**", "*", "ns")
        )
        tcell_sample_level_marker_box_data |>
            dplyr::filter(.features %in% markers) |>
            dplyr::mutate(.features = droplevels(.features)) |>
            ggplot(aes(FGFR3, expr)) +
            ggplot2::facet_grid(cols = vars(.features)) +
            geom_boxplot(
                aes(y = expr, fill = FGFR3),
                outliers = FALSE, linewidth = 0.2
            ) +
            geom_jitter(color = "#c1cec9") +
            ggstattest::geom_comparetest(
                linewidth = 0.2,
                method = "none",
                label_fn = as.list(labels),
                size = 4
            ) +
            scale_fill_manual(values = c("#cbf4e7", "#fddd96")) +
            scale_x_discrete(breaks = NULL, name = NULL) +
            scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
            ylab("Gene Expression") +
            theme_classic() +
            theme(
                axis.line = element_blank(),
                panel.border = element_rect(fill = NA),
                strip.background = element_blank()
            )
    }
)
tcell_marker_box[[1L]]
tcell_marker_box[[2L]]
