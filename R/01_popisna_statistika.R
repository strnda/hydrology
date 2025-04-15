# packages ---------------------------

lop <- c("data.table", "ggplot2", "readxl")

to_instal <- lop[which(x = !(lop %in% installed.packages()[,"Package"]))]

if(length(to_instal) != 0) {
  
  install.packages(to_instal)
}

temp <- lapply(X = lop, 
               FUN = library, 
               character.only = T)

rm(lop, to_instal, temp)

# import dat ---------------------------

dta <- read_excel(path = "./data/proj_1/Data.xlsx",
                  col_names = c("date", "value"),
                  col_types = c("date", "numeric"), 
                  skip = 32)
dta <- as.data.table(x = dta)
dta[, date := as.IDate(x = date)]

saveRDS(object = dta,
        file = "./data/data.rds")

dta <- readRDS(file = "./data/data.rds")

# vypocty ---------------------------

rok <- 1999
dta_rok <- dta[between(x = date,
                       lower = as.IDate(x = paste0(rok - 1, "-11-1")), 
                       upper = as.IDate(x = paste0(rok, "-10-31")),
                       incbounds = TRUE)]

dta_rok[, `:=`(tyden = frollmean(x = value,
                                 n = 7, 
                                 align = "center"),
               mesic = frollmean(x = value,
                                 n = 30, 
                                 align = "center"))]
dta_rok_m <- melt(data = dta_rok,
                  id.vars = 1)

regr <- data.table(x = c(NA, dta_rok$value),
                   y = c(dta_rok$value, NA))
fit <- lm(formula = y ~ x,
          data = regr)
eq <- substitute(italic(y) == a %.% italic(x) + b*";  "~~italic(r)^2~"="~r2, 
                 list(b = format(x = unname(obj = coef(fit)[1]), 
                                 digits = 2),
                      a = format(x = unname(obj = coef(fit)[2]), 
                                 digits = 2),
                      r2 = format(x = summary(object = fit)$r.squared, 
                                  digits = 3)))
eq <- as.character(x = as.expression(x = eq))

acf_e <- acf(x = dta_rok$value,
             plot = FALSE)
acf_df <- with(data = acf_e, 
               expr = data.frame(lag, acf))

# desc_stat ---------------------------

stat <- dta_rok_m[, .(prumer = mean(x = value, 
                                    na.rm = TRUE),
                      min = min(x = value, 
                                na.rm = TRUE),
                      q_05 = quantile(x = value, 
                                      probs = .05, 
                                      na.rm = TRUE),
                      q_25 = quantile(x = value, 
                                      probs = .25, 
                                      na.rm = TRUE),
                      median = median(x = value,
                                      na.rm = TRUE),
                      q_75 = quantile(x = value, 
                                      probs = .75,
                                      na.rm = TRUE),
                      q_95 = quantile(x = value, 
                                      probs = .95, 
                                      na.rm = TRUE),
                      max = max(x = value, 
                                na.rm = TRUE),
                      sm_odchylka = sd(x = value,
                                       na.rm = TRUE),
                      koef_variace = sd(x = value, 
                                        na.rm = TRUE) / mean(x = value, 
                                                             na.rm = TRUE),
                      iqr = IQR(x = value, 
                                na.rm = TRUE)),
                  by = 'variable']

stat

# ecdf ---------------------------

e <- ecdf(x = dta_rok$value)

e(v = 10)

1 - e(v = 10)

# vizual ---------------------------

ggplot(data = dta,
       mapping = aes(x = date,
                     y = value)) +
  geom_line(colour = "royalblue4") +
  theme_light() +
  labs(x = "Čas",
       y = expression(Průtok~(m^{3} %.% s^{"-1"})),
       title = "Divoká Orlice")

ggplot(data = dta_rok,
       mapping = aes(x = date,
                     y = value)) +
  geom_line(colour = "royalblue4") +
  theme_light() +
  labs(x = "Čas",
       y = expression(Průtok~(m^{3} %.% s^{"-1"})),
       title = paste("Divoká Orlice - hydrologický rok", rok))

ggplot(data = dta_rok_m,
       mapping = aes(x = date,
                     y = value, 
                     colour = variable)) +
  geom_line(na.rm = TRUE) +
  scale_colour_manual(values = c("royalblue4", "red4", "olivedrab4"),
                      labels = c("Den", "Týden", "Měsíc"),
                      name = "Časový krok") +
  theme_light() +
  labs(x = "Čas",
       y = expression(Průtok~(m^{3} %.% s^{"-1"})),
       title = paste("Divoká Orlice - hydrologický rok", rok)) +
  theme(legend.position = "inside",
        legend.position.inside = c(.9, .75),
        legend.background = element_blank())

ggplot(data = dta_rok,
       mapping = aes(x = value)) +
  geom_histogram(binwidth = 1,
                 mapping = aes(fill = "Četnost"),
                 colour = "grey75",
                 show.legend = FALSE) +
  scale_fill_manual(values = "royalblue4",
                    name = NULL) +
  theme_light() +
  labs(y = "Četnost výskytu",
       x = expression(Průtok~(m^{3} %.% s^{"-1"})),
       title = "Histogram")

ggplot(data = dta_rok,
       mapping = aes(x = value,
                     y = after_stat(x = density))) +
  geom_histogram(binwidth = 1,
                 mapping = aes(fill = "Relativní\nčetnost"),
                 colour = "grey75",
                 show.legend = FALSE) +
  scale_fill_manual(values = "royalblue4",
                    name = NULL) +
  stat_density(trim = TRUE,
               mapping = aes(colour = "Hustota\npravděpodobností"),
               geom = "line") +
  scale_colour_manual(values = "red4", 
                      name = NULL) +
  theme_light() +
  labs(y = "Relativní četnost výskytu",
       x = expression(Průtok~(m^{3} %.% s^{"-1"})),
       title = "Histogram") +
  theme(legend.position = "inside",
        legend.position.inside = c(.9, .5),
        legend.background = element_blank())

ggplot(data = dta_rok) +
  stat_ecdf(mapping = aes(x = value,
                          colour = "Distribuční funkce\nP(x ≥ X)"), 
            geom = "line") +
  stat_ecdf(mapping = aes(x = value,
                          y = 1 - after_stat(x = y),
                          colour = "Čára překročení\nP(x < X)"),
            geom = "line") +
  scale_colour_manual(values = c("red4", "royalblue4"),
                      name = NULL) +
  theme_light() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = "p",
       x = expression(Průtok~(m^{3} %.% s^{"-1"})),
       title = "Empirická distribuční funkce a čára překročení") +
  theme(legend.position = "inside",
        legend.position.inside = c(.9, .5),
        legend.background = element_blank())

ggplot()+
  geom_point(data = regr, 
             mapping = aes(x = x, 
                           y = y),
             colour = "royalblue4", 
             na.rm = TRUE) +
  geom_line(data = fortify(model = fit), 
            aes(x = x, 
                y = .fitted), 
            colour = "red4") + 
  labs(x = expression(Průtok~(m^{3} %.% s^{"-1"})~v~čase~t-1),
       y = expression(Průtok~(m^{3} %.% s^{"-1"})~v~čase~t),
       title = "Autoregrese denních dat") +
  geom_text(data = NULL,
            mapping = aes(x = mean(x = regr$x, 
                                   na.rm = TRUE) * 1.5,
                          y = max(x = regr$x, 
                                  na.rm = TRUE) * .925,
                          label = eq),
            colour = "grey15", 
            parse = TRUE) +
  theme_light()

ggplot(data = acf_df, 
       mapping = aes(x = lag, 
                     y = acf)) +
  geom_hline(yintercept = 0, 
             colour = "grey15") +
  geom_hline(yintercept = c(-.1, .1), 
             colour = "red4",
             linetype = "dashed") +
  geom_segment(mapping = aes(xend = lag, 
                             yend = 0), 
               colour = "royalblue4") +
  labs(x = "Lag (časový krok)",
       y = "Korelační koeficient",
       title = "Autokorelační funkce") +
  theme_light()
