## Figure 2. Contour-enhanced funnel plots of the 73 FDA-registered antidepressant
## trials (Turner et al. 2008). Data columns: 3-4 FDA SMD/SE (all trials);
## 5-6 journal-reported SMD/SE (NA if unpublished). Panel A uses FDA values;
## panels B and C use journal values. Pooled estimates: equal-effects model.

suppressMessages(library(metafor))

dat <- read.csv("antidepressant_data.csv", header = FALSE)
y   <- dat[, 3]
s   <- dat[, 4]
pub <- !is.na(dat[, 5])
yj  <- dat[pub, 5]
sj  <- dat[pub, 6]
K <- length(y); K.pub <- sum(pub)

fit.all <- rma(yi = y,  sei = s,  method = "EE")
fit.pub <- rma(yi = yj, sei = sj, method = "EE")
fit.pubF <- rma(yi = y[pub], sei = s[pub], method = "EE")   # for the text only
mu.all <- as.vector(fit.all$b); mu.pub <- as.vector(fit.pub$b); mu.pubF <- as.vector(fit.pubF$b)

egger <- regtest(fit.pub, model = "lm", predictor = "sei")
b.egger <- coef(egger$fit)

tf <- trimfill(fit.pub, estimator = "L0")
y.tf <- tf$yi; s.tf <- sqrt(tf$vi); imputed <- tf$fill; mu.tf <- as.vector(tf$b)

cat(sprintf("Trials: %d total, %d published, %d unpublished\n", K, K.pub, K - K.pub))
cat(sprintf("Pooled SMD, all 73 (FDA):                  %.3f (95%% CI %.3f to %.3f)\n", mu.all, fit.all$ci.lb, fit.all$ci.ub))
cat(sprintf("Pooled SMD, published, FDA values:         %.3f (95%% CI %.3f to %.3f)\n", mu.pubF, fit.pubF$ci.lb, fit.pubF$ci.ub))
cat(sprintf("Pooled SMD, published, journal values:     %.3f (95%% CI %.3f to %.3f)\n", mu.pub, fit.pub$ci.lb, fit.pub$ci.ub))
cat(sprintf("Egger slope (SMD per unit SE): %.2f, p = %.2g\n", b.egger[2], egger$pval))
cat(sprintf("Trim-and-fill: side = %s, k0 = %d, adjusted SMD %.3f (95%% CI %.3f to %.3f)\n",
            tf$side, tf$k0, mu.tf, tf$ci.lb, tf$ci.ub))

xlim <- c(-0.8, 1.2)
ylim <- c(0.42, 0)
col.sig <- c("gray92", "gray80", "gray62")   # P < 0.01, 0.01-0.05, 0.05-0.10
col.blue <- "blue"
col.ref  <- "gray30"
lcex <- 0.8

## pdf() renders ASCII "-" as a minus sign; 0xAD maps to the hyphen glyph.
hy <- function(x) gsub("-", "\u00ad", x, fixed = TRUE)

funnel.base <- function(main) {
  plot(NA, xlim = xlim, ylim = ylim, axes = FALSE, xlab = "", ylab = "",
       xaxs = "i", yaxs = "i")
  rect(-10, 10, 10, 0, border = NA, col = col.sig[1])
  polygon(c(0, 10 * qnorm(0.005), 10 * qnorm(0.995)), c(0, 10, 10), border = NA, col = col.sig[2])
  polygon(c(0, 10 * qnorm(0.025), 10 * qnorm(0.975)), c(0, 10, 10), border = NA, col = col.sig[3])
  polygon(c(0, 10 * qnorm(0.05),  10 * qnorm(0.95)),  c(0, 10, 10), border = NA, col = "white")
  axis(1, cex.axis = 0.85, mgp = c(2, 0.6, 0))
  axis(2, cex.axis = 0.85, mgp = c(2, 0.6, 0))
  box()
  title(main = hy(main), font.main = 2, cex.main = 0.95, line = 0.6)
  title(xlab = "Standardized mean difference", line = 2.1, cex.lab = 0.9)
  title(ylab = "Standard error", line = 2.4, cex.lab = 0.9)
}

draw <- function() {
  layout(rbind(c(1, 2, 3), c(4, 4, 4)), heights = c(1, 0.24))
  par(mar = c(3.3, 3.6, 1.8, 0.7))

  funnel.base(sprintf("(A) Complete FDA evidence (%d trials)", K))
  points(y[pub],  s[pub],  pch = 16, cex = 0.8)
  points(y[!pub], s[!pub], pch = 1,  cex = 0.8)
  abline(v = mu.all, lwd = 1.6)

  funnel.base(sprintf("(B) Published literature (%d trials)", K.pub))
  points(yj, sj, pch = 16, cex = 0.8)
  abline(v = mu.all, lty = 3, col = col.ref, lwd = 1.4)
  abline(v = mu.pub, lwd = 1.6)
  se.grid <- seq(0, ylim[1], length.out = 100)
  lines(b.egger[1] + b.egger[2] * se.grid, se.grid, lty = 2, lwd = 1.8, col = col.blue)

  funnel.base(hy("(C) Trim-and-fill applied to (B)"))
  points(y.tf[!imputed], s.tf[!imputed], pch = 16, cex = 0.8)
  points(y.tf[imputed],  s.tf[imputed],  pch = 2,  cex = 0.8)
  points(y[!pub], s[!pub], pch = 3, cex = 0.8, col = "gray30")
  abline(v = mu.all, lty = 3, col = col.ref, lwd = 1.4)
  abline(v = mu.tf, lty = 4, lwd = 1.8, col = col.blue)

  par(mar = c(0, 0, 0, 0))
  plot.new()
  legend(x = 0.02, y = 1.0, ncol = 2, bty = "n", cex = lcex, xjust = 0, pt.cex = 0.8,
         title = "Trials", title.adj = 0, title.font = 2,
         legend = hy(c("published (A, B, C)", "unpublished (A)",
                       "imputed by trim-and-fill (C)", "truly unpublished, from A (C)")),
         pch = c(16, 1, 2, 3), col = c("black", "black", "black", "gray30"))
  legend(x = 0.38, y = 1.0, ncol = 2, bty = "n", cex = lcex, xjust = 0,
         title = "Vertical lines", title.adj = 0, title.font = 2,
         legend = hy(c("pooled SMD, all 73 trials (A)",
                       "same, as reference (B, C)",
                       "pooled SMD, published trials (B)",
                       "Egger regression line (B)",
                       "trim-and-fill adjusted SMD (C)")),
         lty = c(1, 3, 1, 2, 4), lwd = c(1.6, 1.4, 1.6, 1.8, 1.8),
         col = c("black", col.ref, "black", col.blue, col.blue))
  legend(x = 0.80, y = 1.0, ncol = 2, bty = "n", cex = lcex, xjust = 0,
         title = hy("Two-sided P vs. no effect"), title.adj = 0, title.font = 2,
         legend = c("P > 0.10", "0.05 < P < 0.10", "0.01 < P < 0.05", "P < 0.01"),
         fill = c("white", col.sig[3], col.sig[2], col.sig[1]), border = "gray50")
}

pdf("Figure2.pdf", width = 10.5, height = 4.2, family = "Helvetica", useDingbats = FALSE)
draw()
dev.off()

png("Figure2.png", width = 10.5, height = 4.2, units = "in", res = 300)
draw()
dev.off()

sessionInfo()
