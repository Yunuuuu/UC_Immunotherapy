tcell_markers <- c("CD8A", "CD8B", "IFNG", "GZMB", "GNLY", "HAVCR2", "PDCD1")
tcell_marker_umap <- lapply(tcell_markers, function(marker) {
    out <- lapply(tcells_subset_list, function(obj) {
        scater::plotReducedDim(
            obj,
            "UMAP",
            colour_by = marker,
            point_size = 0.1,
            point_shape = 19,
            point.padding = 0,
            force = 0
        ) +
            labs(x = NULL, y = NULL) +
            # scale_color_gradientn(
            #     name = NULL, colours = rev(rainbow(100, rev = TRUE)),
            #     limits = c(0, 9)
            # ) +
            guides(
                x = guide_axis_trunc(upper = 0.3),
                y = guide_axis_trunc(upper = 0.3),
                color = guide_colorbar(
                    direction = "vertical",
                    theme = theme(
                        legend.key.height = unit(1, "null"),
                        legend.text = element_text(size = 18)
                    )
                )
            ) +
            theme(
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                axis.line = element_blank(),
                legend.ticks = element_blank(),
                plot.title = element_text(size = 18, face = "bold")
            )
    })
    limits <- lapply(out, function(p) {
        color_scale <- ggplot_build(p)$plot$scales$get_scales("colour")
        color_scale$get_limits()
    })
    limits <- c(
        min(vapply(limits, .subset2, numeric(1L), 1L)),
        max(vapply(limits, .subset2, numeric(1L), 2L))
    )
    out <- lapply(out, function(o) {
        o +
            scale_colour_distiller(
                palette = "BuPu", direction = 1,
                limits = limits, breaks = limits,
                labels = c("min", "max"),
                name = NULL,
                values = c(0, 0.2, 1)
            )
    })
    out[[1L]] <- out[[1L]] + ggtitle(marker)
    ggalign::align_plots(!!!out, ncol = 1L)
})

ggalign::align_plots(
    !!!tcell_marker_umap[seq_len(5)],
    nrow = 1L, guides = "tlbr"
)

ggalign::align_plots(
    !!!c(tcell_marker_umap[-seq_len(5)]),
    nrow = 1L, guides = "tlbr"
)
