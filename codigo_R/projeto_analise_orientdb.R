# ============================================================
# Análise Estatística Completa – OrientDB
# Dataset: orientdb-Class.csv (principal) + orientdb-File.csv
# Disciplina: Engenharia de Software – AP1
# ============================================================

# ---------- Pacotes necessários -----------------------------
if (!require("PerformanceAnalytics")) install.packages("PerformanceAnalytics")
if (!require("ggplot2"))              install.packages("ggplot2")
if (!require("dplyr"))                install.packages("dplyr")
if (!require("tidyr"))                install.packages("tidyr")
if (!require("nortest"))              install.packages("nortest")
if (!require("e1071"))                install.packages("e1071")
if (!require("corrplot"))             install.packages("corrplot")
if (!require("reshape2"))             install.packages("reshape2")
if (!require("gridExtra"))            install.packages("gridExtra")

library(PerformanceAnalytics)
library(ggplot2)
library(dplyr)
library(tidyr)
library(nortest)
library(corrplot)
library(reshape2)
library(gridExtra)

# ---------- 1. Carregamento dos dados -----------------------
df       <- read.csv("orientdb-Class.csv", stringsAsFactors = FALSE)
df_file  <- read.csv("orientdb-File.csv",  stringsAsFactors = FALSE)

cat("Dimensões (Classe):", dim(df), "\n")
cat("Dimensões (Arquivo):", dim(df_file), "\n")

# Métricas de interesse
METRICS <- c("WMC","CBO","RFC","LOC","LLOC","NM","NOS","DIT","LCOM5","NII","NOI","TNA","TNM","NL","CLOC")
df_m    <- df[, METRICS]

# ============================================================
# 2. MEDIDAS DE TENDÊNCIA CENTRAL
# ============================================================
cat("\n===== TENDÊNCIA CENTRAL =====\n")
for (col in c("WMC","CBO","RFC","LOC","LLOC","NM","DIT","LCOM5")) {
  cat(sprintf("%-8s  média=%.2f  mediana=%.2f  moda=%s\n",
              col,
              mean(df_m[[col]], na.rm=TRUE),
              median(df_m[[col]], na.rm=TRUE),
              as.character(as.numeric(names(which.max(table(df_m[[col]])))))))
}

# ============================================================
# 3. MEDIDAS DE DISPERSÃO
# ============================================================
cat("\n===== DISPERSÃO =====\n")
for (col in c("WMC","CBO","RFC","LOC","LLOC","NM","DIT","LCOM5")) {
  v <- df_m[[col]]
  cat(sprintf("%-8s  amplitude=%.0f  variância=%.2f  dp=%.2f\n",
              col,
              diff(range(v, na.rm=TRUE)),
              var(v, na.rm=TRUE),
              sd(v, na.rm=TRUE)))
}

# ============================================================
# 4. MEDIDAS DE POSIÇÃO RELATIVA (Quartis e Percentis)
# ============================================================
cat("\n===== POSIÇÃO RELATIVA =====\n")
print(
  sapply(df_m[, c("WMC","CBO","RFC","LOC","NM","DIT","LCOM5")], function(x)
    quantile(x, probs=c(0,.25,.50,.75,.90,.95,1), na.rm=TRUE))
)

# ============================================================
# 5. GRÁFICOS
# ============================================================

# --- 5a. Histogramas ---
pdf("fig1_histogramas.pdf", width=14, height=9)
par(mfrow=c(3,5), mar=c(4,4,2,1))
for (col in METRICS) {
  hist(df_m[[col]], breaks=40,
       main=col, xlab="Valor", ylab="Frequência",
       col="#4472C4", border="white",
       xlim=c(0, quantile(df_m[[col]], 0.99, na.rm=TRUE)))
}
dev.off()

# --- 5b. Boxplots ---
pdf("fig2_boxplots.pdf", width=14, height=6)
par(mfrow=c(2,5), mar=c(4,4,2,1))
for (col in c("WMC","CBO","RFC","LOC","NM","NOS","DIT","LCOM5","NII","NOI")) {
  boxplot(df_m[[col]], main=col, ylab="Valor",
          col="#4472C4", border="#333333",
          outline=TRUE, outpch=16, outcex=0.3, outcol="gray50")
}
dev.off()

# --- 5c. Matriz de Correlação (PerformanceAnalytics) ---
pdf("fig3_correlacao.pdf", width=12, height=10)
chart.Correlation(
  df_m[, c("WMC","CBO","RFC","LOC","NM","NOS","DIT","LCOM5","NII","NOI","TNA","TNM")],
  histogram=TRUE, pch=19
)
dev.off()

# --- 5d. Scatter plots ---
pdf("fig4_scatter.pdf", width=14, height=5)
par(mfrow=c(1,3), mar=c(4,4,3,1))
pairs_list <- list(c("LOC","WMC"), c("RFC","CBO"), c("NM","NOS"))
for (p in pairs_list) {
  x <- pmin(df_m[[p[1]]], quantile(df_m[[p[1]]], 0.99, na.rm=TRUE))
  y <- pmin(df_m[[p[2]]], quantile(df_m[[p[2]]], 0.99, na.rm=TRUE))
  plot(x, y, pch=16, cex=0.3, col=adjustcolor("#4472C4",0.3),
       xlab=p[1], ylab=p[2],
       main=paste(p[1], "vs", p[2], sprintf("(r=%.3f)", cor(x,y,use="complete.obs"))))
  abline(lm(y~x), col="red", lwd=1.5)
}
dev.off()

# --- 5e. Q-Q plots ---
pdf("fig7_qqplots.pdf", width=14, height=4)
par(mfrow=c(1,4), mar=c(4,4,3,1))
for (col in c("WMC","CBO","RFC","LOC")) {
  qqnorm(log1p(df_m[[col]]), main=paste0("Q-Q: log(", col,"+1)"), pch=16, cex=0.3)
  qqline(log1p(df_m[[col]]), col="red", lwd=1.5)
}
dev.off()

# --- 5f. DIT e LCOM5 ---
pdf("fig5_dit_lcom5.pdf", width=10, height=5)
par(mfrow=c(1,2), mar=c(4,4,3,1))
barplot(table(df_m$DIT), col="#ED7D31", border="white",
        main="Distribuição de DIT", xlab="DIT", ylab="Número de Classes")
hist(df_m$LCOM5[df_m$LCOM5 <= quantile(df_m$LCOM5, 0.95, na.rm=TRUE)],
     breaks=30, col="#4472C4", border="white",
     main="Distribuição de LCOM5", xlab="LCOM5", ylab="Frequência")
dev.off()

# ============================================================
# 6. IDENTIFICAÇÃO DE OUTLIERS (IQR)
# ============================================================
cat("\n===== OUTLIERS (IQR) =====\n")
for (col in c("WMC","CBO","RFC","LOC","NM","NOS")) {
  v   <- df_m[[col]]
  q1  <- quantile(v, 0.25, na.rm=TRUE)
  q3  <- quantile(v, 0.75, na.rm=TRUE)
  iqr <- q3 - q1
  out <- sum(v < (q1 - 1.5*iqr) | v > (q3 + 1.5*iqr), na.rm=TRUE)
  cat(sprintf("%-6s Q1=%.1f Q3=%.1f IQR=%.1f sup=%.1f outliers=%d (%.1f%%)\n",
              col, q1, q3, iqr, q3+1.5*iqr, out, out/length(v)*100))
}

# ============================================================
# 7. TESTES DE NORMALIDADE
# ============================================================
cat("\n===== SHAPIRO-WILK (n=500) =====\n")
set.seed(42)
for (col in c("WMC","CBO","RFC","LOC","DIT","LCOM5")) {
  amostra <- sample(df_m[[col]], 500, replace=FALSE)
  sw <- shapiro.test(amostra)
  cat(sprintf("%-8s W=%.4f p=%.6f → %s\n",
              col, sw$statistic, sw$p.value,
              ifelse(sw$p.value < 0.05, "NÃO normal", "Normal")))
}

cat("\n===== SHAPIRO-WILK LOG-TRANSFORMADO =====\n")
for (col in c("WMC","CBO","RFC","LOC")) {
  amostra <- log1p(sample(df_m[[col]][df_m[[col]] > 0], 500, replace=FALSE))
  sw <- shapiro.test(amostra)
  cat(sprintf("log(%-5s) W=%.4f p=%.6f → %s\n",
              col, sw$statistic, sw$p.value,
              ifelse(sw$p.value < 0.05, "NÃO normal", "Normal")))
}

# ============================================================
# 8. COEFICIENTES DE CORRELAÇÃO
# ============================================================
cat("\n===== CORRELAÇÕES PEARSON =====\n")
pairs_corr <- list(c("LOC","WMC"), c("LOC","NM"), c("WMC","RFC"),
                   c("CBO","RFC"), c("NM","NOS"), c("LOC","NOS"), c("WMC","NOS"))
for (p in pairs_corr) {
  r <- cor.test(df_m[[p[1]]], df_m[[p[2]]], method="pearson")
  cat(sprintf("%-6s x %-6s r=%.4f p=%.2e\n", p[1], p[2], r$estimate, r$p.value))
}

cat("\n===== CORRELAÇÕES SPEARMAN (não-paramétrica) =====\n")
for (p in pairs_corr) {
  r <- cor.test(df_m[[p[1]]], df_m[[p[2]]], method="spearman")
  cat(sprintf("%-6s x %-6s rho=%.4f p=%.2e\n", p[1], p[2], r$estimate, r$p.value))
}

# ============================================================
# 9. MODELAGEM: REGRESSÃO LINEAR (log(LOC) → log(WMC))
# ============================================================
cat("\n===== REGRESSÃO LINEAR =====\n")
dados_reg <- df_m[df_m$LOC > 0 & df_m$WMC > 0, ]
log_loc   <- log1p(dados_reg$LOC)
log_wmc   <- log1p(dados_reg$WMC)

modelo <- lm(log_wmc ~ log_loc)
cat(summary(modelo)$coefficients %>% as.data.frame() %>% round(4) %>% capture.output() %>% paste(collapse="\n"), "\n")
cat("R² ajustado:", round(summary(modelo)$adj.r.squared, 4), "\n")
cat("RMSE:", round(sqrt(mean(modelo$residuals^2)), 4), "\n")

# Gráfico de regressão
pdf("fig8_regressao.pdf", width=10, height=5)
par(mfrow=c(1,2))
plot(log_loc, log_wmc, pch=16, cex=0.3, col=adjustcolor("#4472C4",0.3),
     xlab="log(LOC + 1)", ylab="log(WMC + 1)",
     main=sprintf("Regressão Linear\nR² = %.4f", summary(modelo)$r.squared))
abline(modelo, col="red", lwd=1.5)
legend("topleft", legend=sprintf("y = %.3fx + %.3f", coef(modelo)[2], coef(modelo)[1]),
       col="red", lty=1, cex=0.8)
plot(modelo$fitted.values, modelo$residuals, pch=16, cex=0.3, col=adjustcolor("#ED7D31",0.4),
     xlab="Valores Ajustados", ylab="Resíduos",
     main=sprintf("Gráfico de Resíduos\nRMSE = %.4f", sqrt(mean(modelo$residuals^2))))
abline(h=0, col="red", lwd=1.5)
dev.off()

cat("\nAnálise concluída! Todos os PDFs de figuras foram gerados.\n")
