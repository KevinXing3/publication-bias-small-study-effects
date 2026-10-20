## Figure 1. Schematic 2x2: publication bias (rows) by funnel plot asymmetry (columns).
## Illustrative points only; no data.

## pdf() renders ASCII "-" as a minus sign; 0xAD maps to the hyphen glyph.
hy <- function(x) gsub("-", "\u00ad", x, fixed = TRUE)

truth <- 0.4
se_top <- 0.045; se_bot <- 0.475
xr <- c(-0.85, 1.6); yr <- c(se_bot, se_top)

shade_contours <- function() {
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col = "gray92", border = NA)
  ses <- seq(se_top, se_bot, length.out = 60)
  for (b in list(c(2.576, "gray80"), c(1.96, "gray62"), c(1.645, "white"))) {
    z <- as.numeric(b[1])
    polygon(c(-z * ses, rev(z * ses)), c(ses, rev(ses)), col = b[2], border = NA)
  }
}
funnel_guides <- function(pooled) {
  ses <- c(se_top, se_bot)
  lines(truth + 1.96 * ses, ses, lty = 3, col = "gray35")
  lines(truth - 1.96 * ses, ses, lty = 3, col = "gray35")
  segments(truth, se_bot, truth, se_top, lty = 3, col = "gray35")
  segments(0, se_bot, 0, se_top, lty = 1, col = "gray75", lwd = 0.8)
  segments(pooled, se_bot, pooled, se_top, lty = 1, col = "black", lwd = 1.5)
}
panel_fun <- function(filled, ghost, pooled, tag, verdict) {
  plot(NA, xlim = xr, ylim = yr, axes = FALSE, xlab = "", ylab = "",
       xaxs = "i", yaxs = "i")
  shade_contours()
  funnel_guides(pooled)
  if (!is.null(ghost))
    points(ghost[, 1], ghost[, 2], pch = 21, bg = "white", col = "gray45",
           lwd = 1.6, cex = 1.15)
  points(filled[, 1], filled[, 2], pch = 19, col = "gray10", cex = 1.05)
  box(col = "gray55")
  axis(1, at = 0, labels = "0", lwd = 0, lwd.ticks = 0.8, col.ticks = "gray55",
       tcl = -0.25, mgp = c(0, 0.25, 0), cex.axis = 0.8, col.axis = "gray20")
  mtext(tag, side = 3, line = 0.55, adj = 0, cex = 0.86, font = 2)
  mtext(verdict, side = 3, line = 0.55, adj = 1, cex = 0.74, font = 3, col = "gray30")
}

fillA <- rbind(c(0.34,0.08), c(0.47,0.08),
               c(0.22,0.14), c(0.41,0.14), c(0.58,0.14),
               c(0.09,0.22), c(0.31,0.22), c(0.52,0.22), c(0.72,0.22),
               c(-0.05,0.32), c(0.38,0.32), c(0.80,0.32),
               c(-0.15,0.42), c(0.42,0.42), c(0.98,0.42))
fillB <- rbind(c(0.36,0.08), c(0.46,0.08),
               c(0.28,0.14), c(0.44,0.14), c(0.60,0.14),
               c(0.30,0.22), c(0.52,0.22), c(0.75,0.22), c(0.95,0.22),
               c(0.45,0.32), c(0.78,0.32), c(1.10,0.32),
               c(0.60,0.42), c(0.95,0.42), c(1.30,0.42))
fillC <- rbind(c(0.50,0.08), c(0.62,0.08),
               c(0.40,0.14), c(0.56,0.14), c(0.72,0.14),
               c(0.30,0.22), c(0.52,0.22), c(0.75,0.22), c(0.90,0.22),
               c(0.22,0.32), c(0.58,0.32), c(0.92,0.32),
               c(0.15,0.42), c(0.62,0.42), c(1.05,0.42))
ghostC <- rbind(c(0.12,0.09), c(0.02,0.20), c(-0.18,0.33), c(-0.30,0.44))
fillD <- rbind(c(0.36,0.08), c(0.47,0.08),
               c(0.25,0.14), c(0.42,0.14), c(0.58,0.14),
               c(0.35,0.22), c(0.55,0.22), c(0.72,0.22),
               c(0.48,0.32), c(0.75,0.32), c(0.95,0.32),
               c(0.88,0.42), c(1.10,0.42))
ghostD <- rbind(c(0.05,0.30), c(-0.10,0.40), c(0.15,0.44), c(0.12,0.36))

pdf("Figure1.pdf", width = 8, height = 7.0, pointsize = 12,
    family = "Helvetica", useDingbats = FALSE)
layout(rbind(c(1, 2), c(3, 4), c(5, 5)), heights = c(1, 1, 0.30))
par(oma = c(2.6, 3.0, 3.4, 0.4), mar = c(1.7, 1.1, 2.0, 0.9))

panel_fun(fillA, NULL, truth, "(A)", "all is well")
arrows(-0.72, 0.42, -0.72, 0.16, length = 0.06, col = "gray35", lwd = 1.1)
text(-0.66, 0.29, "larger studies", srt = 90, adj = 0.5, cex = 0.78, col = "gray35")
panel_fun(fillB, NULL, 0.52, "(B)", "false alarm if read as publication bias")
panel_fun(fillC, ghostC, 0.57, "(C)", "false reassurance")
panel_fun(fillD, ghostD, 0.54, "(D)", "the textbook picture")

mtext(hy("Funnel plot symmetric\n(no small-study effects)"), side = 3, outer = TRUE,
      line = 0.6, at = 0.25, cex = 0.88, font = 2)
mtext(hy("Funnel plot asymmetric\n(small-study effects present)"), side = 3,
      outer = TRUE, line = 0.6, at = 0.75, cex = 0.88, font = 2)
mtext("No publication bias", side = 2, outer = TRUE, line = 1.0, at = 0.80,
      cex = 0.88, font = 2)
mtext("Publication bias", side = 2, outer = TRUE, line = 1.0, at = 0.36,
      cex = 0.88, font = 2)
mtext("Treatment effect", side = 1, outer = TRUE, line = 0.5, at = 0.5, cex = 0.78)

par(mar = c(0, 0, 0, 0))
plot.new()
legend(x = 0.01, y = 1.05, ncol = 2, bty = "n", cex = 0.9, xjust = 0,
       legend = hy(c("reported study",
                     "conducted but never reported\n(invisible in an actual review)",
                     "pooled estimate of reported studies",
                     "true effect and its expected 95% spread (dotted)",
                     "no effect (gray)")),
       pch = c(19, 21, NA, NA, NA),
       pt.bg = c(NA, "white", NA, NA, NA),
       col = c("gray10", "gray45", "black", "gray35", "gray75"),
       lty = c(NA, NA, 1, 3, 1),
       lwd = c(NA, 1.6, 1.5, 1, 0.8),
       pt.cex = c(1.05, 1.15, NA, NA, NA))
legend(x = 0.72, y = 1.05, ncol = 2, bty = "n", cex = 0.9, xjust = 0,
       title = "significance contours (vs. no effect)", title.cex = 0.82,
       legend = c("P > 0.10", "0.05 < P < 0.10", "0.01 < P < 0.05", "P < 0.01"),
       fill = c("white", "gray62", "gray80", "gray92"), border = "gray60")
dev.off()

sessionInfo()
