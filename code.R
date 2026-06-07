###############################################################################
#  PUBLICATION-QUALITY BARPLOTS — WATER STRESS IN BARLEY (Hordeum vulgare)
#  Grayscale + hatching (no colour), significance letters above bars
#
#  Exp.1  Germination stage : Variety x PEG 6000  (5 x 4)
#  Exp.2  Adult stage        : Genotype x Water deficit (2 x 3)
#
#  Input : Analyse_stat.xlsx  (sheets "Données Exp1" / "Données Exp2")
#  Output: high-resolution PNG + PDF, one figure per trait
###############################################################################

## ---- 0. PACKAGES ----------------------------------------------------------
pkgs <- c("readxl","car","agricolae","FSA","rcompanion","multcompView",
          "ggplot2","ggpattern","dplyr")

invisible(lapply(pkgs, library, character.only = TRUE))

## ---- 1. PATHS -------------------------------------------------------------
          
fichier <- "données  stat article mknv.xlsx"   # <-- adapt path if needed
feuille_exp1 <- "Sheet1"
feuille_exp2 <- "Sheet2"
dir_out = "C:/Users/meked/Desktop/belkhoudja"
dir.create(dir_out, showWarnings = FALSE)
ALPHA   <- 0.05

## ---- 2. DATA --------------------------------------------------------------
exp1 <- read_excel(path = "données  stat article (mk)nv.xlsx", sheet = feuille_exp1)
exp2 <- read_excel(path = "données  stat article (mk)nv.xlsx", sheet = feuille_exp2)

names(exp1) <- c("Variety","PEG","Precocity","FinalRate","FreshWeight","DryWeight","Length")
names(exp2) <- c("Genotype","Treatment","HP","WC","RWC","WD","GN")

exp1$Variety <- factor(exp1$Variety)
exp1$PEG     <- factor(exp1$PEG,       levels = c("T","Dh10","Dh15","Dh20"))
exp2$Genotype  <- factor(exp2$Genotype)
exp2$Treatment <- factor(exp2$Treatment, levels = c("T","DH1","DH2"))

for (v in c("Precocity","FinalRate","FreshWeight","DryWeight","Length"))
  exp1[[v]] <- as.numeric(exp1[[v]])
for (v in c("HP","WC","RWC","WD","GN"))
  exp2[[v]] <- as.numeric(exp2[[v]])

## ---- 2b. DATA CLEANING --------------
exp2$WD[exp2$WD >= 100] <- NA

# significance letters: ANOVA+Tukey if assumptions hold, else KW+Dunn(Holm)
get_letters <- function(d, fa, fb, y){
  dd <- d[!is.na(d[[y]]), c(fa, fb, y)]
  names(dd) <- c("A","B","Y")
  dd$A <- droplevels(factor(dd$A)); dd$B <- droplevels(factor(dd$B))
  dd$grp <- interaction(dd$A, dd$B, sep = ":")
  
  mod <- aov(Y ~ A * B, data = dd)
  sw  <- shapiro.test(residuals(mod))$p.value
  lev <- tryCatch(car::leveneTest(Y ~ A * B, data = dd)[1,"Pr(>F)"],
                  error = function(e) NA)
  ok  <- (sw > ALPHA) && (!is.na(lev) && lev > ALPHA)
  
  if (ok){
    method <- "ANOVA + Tukey HSD"
    hsd <- agricolae::HSD.test(mod, trt = c("A","B"), group = TRUE)
    L <- data.frame(grp = rownames(hsd$groups), letter = hsd$groups$groups,
                    stringsAsFactors = FALSE)
  } else {
    method <- "Kruskal-Wallis + Dunn (Holm)"
    dt <- FSA::dunnTest(Y ~ grp, data = dd, method = "holm")$res
    comp <- gsub(" ", "", dt$Comparison)
    pv   <- setNames(dt$P.adj, comp)
    let  <- multcompView::multcompLetters(pv, threshold = ALPHA)$Letters
    L <- data.frame(grp = names(let), letter = as.character(let),
                    stringsAsFactors = FALSE)
  }
  list(letters = L, method = method)
}

# mean + SE per cell
summ <- function(d, fa, fb, y){
  d <- d[!is.na(d[[y]]), ]
  agg <- d %>% group_by(.data[[fa]], .data[[fb]]) %>%
    summarise(mean = mean(.data[[y]]),
              se   = sd(.data[[y]])/sqrt(dplyr::n()),
              .groups = "drop")
  names(agg)[1:2] <- c("A","B")
  agg$grp <- interaction(agg$A, agg$B, sep = ":")
  agg
}

## ---- 4. PLOT  -----------------------------
make_plot <- function(d, fa, fb, y, ttl, ylab){
  gl  <- get_letters(d, fa, fb, y)
  agg <- summ(d, fa, fb, y)
  agg <- merge(agg, gl$letters, by = "grp", all.x = TRUE)
  agg$letter[is.na(agg$letter)] <- ""
  agg$A <- factor(agg$A, levels = levels(d[[fa]]))
  agg$B <- factor(agg$B, levels = levels(d[[fb]]))
  
  nlev <- nlevels(agg$B)
    fills    <- gray.colors(nlev, start = 0.35, end = 0.95)
  patterns <- c("stripe","crosshatch","circle","none","weave")[seq_len(nlev)]
  angles   <- c(45, 135, 0, 90, 30)[seq_len(nlev)]
  
  ggplot(agg, aes(x = A, y = mean, group = B)) +
    ggpattern::geom_col_pattern(
      aes(pattern = B, pattern_angle = B, fill = B),
      position = position_dodge(0.8), width = 0.72,
      colour = "black", linewidth = 0.4,
      pattern_fill = "black", pattern_colour = "black",
      pattern_density = 0.12, pattern_spacing = 0.03,
      pattern_key_scale_factor = 0.7) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                  position = position_dodge(0.8), width = 0.22,
                  linewidth = 0.4, colour = "black") +
    geom_text(aes(label = letter, y = mean + se),
              position = position_dodge(0.8), vjust = -0.5,
              size = 3.3, fontface = "bold") +
    scale_fill_manual(values = fills, name = fb) +
    ggpattern::scale_pattern_manual(values = patterns, name = fb) +
    scale_pattern_angle_manual(values = angles, name = fb) +
    labs(title = ttl, x = NULL, y = ylab) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    theme_classic(base_size = 12) +
    theme(
      plot.title   = element_text(face = "bold", hjust = 0.5, size = 13,
                                  margin = margin(b = 8)),
      plot.title.position = "plot",
      axis.title.y = element_text(size = 11),
      axis.text.x  = element_text(angle = 0, colour = "black"),
      axis.text.y  = element_text(colour = "black"),
      axis.line    = element_line(linewidth = 0.5, colour = "black"),
      axis.ticks   = element_line(colour = "black"),
      legend.position = "bottom",
      legend.key.size = unit(0.8, "lines"),
      legend.title    = element_text(face = "bold")
    )
}

save_plot <- function(g, name, w = 8, h = 5){
  ggsave(file.path(dir_out, paste0(name, ".png")), g,
         width = w, height = h, dpi = 600, bg = "white")
  ggsave(file.path(dir_out, paste0(name, ".pdf")), g,
         width = w, height = h, bg = "white")
  cat("saved:", name, "\n")
}

save_plot(make_plot(exp1,"Variety","PEG","Precocity",
                    "Germination precocity index", "Precocity index"),"Fig1_Precocity")
save_plot(make_plot(exp1,"Variety","PEG","FinalRate",
                    "Final germination rate", "Final germination (count)"),"Fig2_FinalRate")
save_plot(make_plot(exp1,"Variety","PEG","FreshWeight",
                    "Seedling fresh weight", "Fresh weight (mg)"),"Fig3_FreshWeight")
save_plot(make_plot(exp1,"Variety","PEG","DryWeight",
                    "Seedling dry weight", "Dry weight (mg)"),"Fig4_DryWeight")
save_plot(make_plot(exp1,"Variety","PEG","Length",
                    "Seedling length", "Length (mm)"),"Fig5_Length")

save_plot(make_plot(exp2,"Genotype","Treatment","HP",
                    "Weight-based moisture", "Moisture (%)"),"Fig6_HP", 6.5, 5)
save_plot(make_plot(exp2,"Genotype","Treatment","WC",
                    "Water content", "Water content (%)"),"Fig7_WaterContent", 6.5, 5)
save_plot(make_plot(exp2,"Genotype","Treatment","RWC",
                    "Relative water content", "RWC (%)"),"Fig8_RWC", 6.5, 5)
save_plot(make_plot(exp2,"Genotype","Treatment","WD",
                    "Water deficit", "Water deficit (%)"),"Fig9_WaterDeficit", 6.5, 5)

# Grain number cleaning: Nailia did not produce grain (set all GN to 0)
exp2$GN[exp2$Genotype == "Nailia"] <- 0
save_plot(make_plot(exp2,"Genotype","Treatment","GN",
                    "Grain number", "Grain number"),"Fig10_GrainNumber", 6.5, 5)

cat("\n>>> Done. Publication figures (PNG 600 dpi + PDF) in:", dir_out, "\n")
