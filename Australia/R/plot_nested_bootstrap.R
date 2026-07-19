# Figure asset for the journal manuscript's Figure 2: nested-bootstrap sampling
# distributions from australia_nested_bootstrap.csv (draw-level output of
# nested_bootstrap.R). Left panel: lambda draws with the 95% percentile interval
# and the full-sample point estimate. Right panel: the six structural gammas,
# standardised per coefficient (divided by the interval half-width) so they share
# an axis; each straddles zero once knot-selection uncertainty is propagated.
suppressPackageStartupMessages(library(ggplot2))

out_dir <- file.path(dirname(dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])))), "outputs")
draws <- read.csv(file.path(out_dir, "australia_nested_bootstrap.csv"))
cis   <- read.csv(file.path(out_dir, "australia_nested_bootstrap_ci.csv"))
draws <- draws[draws$success & is.finite(draws$lambda), ]

lam_ci <- quantile(draws$lambda, c(0.025, 0.975))
lam_pt <- cis$point[cis$coef == "lambda"][1]

p1 <- ggplot(draws, aes(x = lambda)) +
  geom_histogram(bins = 40, fill = "grey65", colour = "white", linewidth = 0.2) +
  geom_vline(xintercept = 0, linetype = "solid", colour = "grey30") +
  geom_vline(xintercept = lam_ci, linetype = "dashed") +
  geom_vline(xintercept = lam_pt, colour = "black", linewidth = 0.8) +
  labs(x = expression(lambda~"(speed of adjustment)"), y = "Draws",
       subtitle = "Speed of adjustment: interval excludes zero") +
  theme_minimal(base_size = 11)

gcols <- c("gamma_nla_y", "gamma_ilfa_y", "gamma_ln_yp_over_y",
           "gamma_yp_x_cci", "gamma_ha_x_cci", "gamma_hp_x_1_minus_cci")
glabs <- c("Net liquid", "Illiquid fin.", "Perm. income",
           "PI x CCI", "Housing x CCI", "Affordability")
long <- do.call(rbind, lapply(seq_along(gcols), function(i) {
  x <- draws[[gcols[i]]]
  x <- x[is.finite(x)]
  q <- quantile(x, c(0.025, 0.975))
  hw <- max(abs(q))
  data.frame(term = glabs[i], value = pmax(pmin(x / hw, 1.5), -1.5))
}))
long$term <- factor(long$term, levels = glabs)

p2 <- ggplot(long, aes(x = term, y = value)) +
  geom_hline(yintercept = 0, colour = "grey30") +
  geom_violin(fill = "grey65", colour = "grey40", linewidth = 0.3, scale = "width") +
  stat_summary(fun = median, geom = "point", size = 1.4) +
  labs(x = NULL, y = "Standardised draw (per-coefficient scale)",
       subtitle = "Structural elasticities: every interval straddles zero") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

png(file.path(out_dir, "australia_nested_bootstrap_dist.png"),
    width = 2400, height = 1000, res = 220)
print(cowplot::plot_grid(p1, p2, ncol = 2, rel_widths = c(1, 1.3)))
dev.off()
cat("written:", file.path(out_dir, "australia_nested_bootstrap_dist.png"), "\n")
