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

dta <- read_excel(path = "./data/proj_3/data_MK.xlsx",
                  col_names = c("date", "q", "h"), 
                  skip = 3)
dta <- as.data.table(x = dta)

ggplot() +
  geom_point(data = dta,
             mapping = aes(x = q,
                           y = h),
             colour = "royalblue4") +
  labs(x = expression(Průtok~(m^{3} %.% s^{"-1"})),
       y = "Vodní stav (cm)",
       title = "Vodní stav vs. průtok")+
  theme_light()

# MK ---------------------------

fit <- nls(q ~ a * (b)^h, 
           data = dta,
           start = list(a = 50, 
                        b = 1),
           control = nls.control(maxiter = 2000))

coef(object = fit)

dta_predict <- data.table(h = seq(from = 10, 
                                  to = 300, 
                                  by = 10))

dta_predict[, q := predict(object = fit, 
                           newdata = dta_predict)]

dta_predict

ggplot() +
  geom_line(data = dta_predict,
            mapping = aes(x = q,
                          y = h),
            colour = "red4") +
  geom_point(data = dta,
             mapping = aes(x = q,
                           y = h),
             colour = "royalblue4") +
  labs(x = expression(Průtok~(m^{3} %.% s^{"-1"})),
       y = "Vodní stav (cm)",
       title = "Měrná křivka průtoku")+
  theme_light()
