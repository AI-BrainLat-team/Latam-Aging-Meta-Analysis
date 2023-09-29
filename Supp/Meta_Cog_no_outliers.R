# Erase environment variables
rm(list = ls())
gc() 

# Download (if necessary) and load packages
packages = c( "tidyverse", "DT", "htmltools", "htmlwidgets",  "knitr", "hrbrthemes",
          "data.table", "reticulate", "kableExtra", "formattable", "ggplotify", 
          "rvest", "dmetar", "meta", "metafor", "esc", "easystats")#, "extrafont")

package.check <- lapply(
packages,
FUN = function(x) {
    if (!require(x, character.only = TRUE, quietly = TRUE)) {
        install.packages(x, dependencies = TRUE, quiet = TRUE)
        library(x, character.only = TRUE, quietly = TRUE)
    }
}
)  

hrbrthemes::import_roboto_condensed()

data <- read_csv("/home/marcelo/Documents/stored_GitRepos/Latam-Aging-Meta-Analysis/Data/Cog.csv")
data$country <- gsub("Brasil", "Brazil", data$country, ignore.case = TRUE) # Brasil -> Brazil
data <- data[!(data$predictor_cat %in% c("Cognition", "Functionality")), ] # Drop Cog and Fun as predictors

#setwd for saveing results
setwd("/home/marcelo/Documents/stored_GitRepos/Latam-Aging-Meta-Analysis/Figures/Cognition/wo_o")
# make factors of cat variables
columns_to_convert <- c("outcome_cat", "predictor_cat", "country", "income", "study") 

for (col in columns_to_convert) {
data[[col]] <- factor(data[[col]])
}

# # Check levels
# levels(data$income)
# levels(data$country)
# levels(data$predictor_cat)
# levels(data$outcome_cat)

dim(data) # .shape
table(data$predictor_cat) # value counts

# make dataframes to do Meta-analysis
Demo <- data[(data$predictor_cat %in% c("Demographic")), ]
Health <- data[(data$predictor_cat %in% c("Health")), ]
LifeS <- data[(data$predictor_cat %in% c("Lifestyle factor")), ]
MentalH <- data[(data$predictor_cat %in% c("Mental health")), ]
Sdoh <- data[(data$predictor_cat %in% c("SDOH")), ]
Other <- data[(data$predictor_cat %in% c("Other")), ]

# Make a df taking the mean for all factors by author
grouped_data <- data %>%
group_by(author, income, country, outcome_cat) %>%
summarise(
    N = as.integer(mean(N)),
    TE = mean(TE),
    seTE = mean(seTE),
    lower = mean(lower),
    upper = mean(upper),
    log_TE = mean(log_TE),
    log_seTE = mean(log_seTE),
    log_lower = mean(log_lower),
    log_upper = mean(log_upper)
)

# remove outliers
grouped_data<-subset(grouped_data, !(author %in% c("Borda", "O´Donnovan", "Valle", "Villa")) )
Demo<-subset(Demo, !(author %in% c("Borda", "da Silva")))
Health<-subset(Health, !(author %in% c("García-Channes", "Borda", "Llibre")) )
MentalH<-subset(MentalH, !(author %in% c("Santamaría-García")) )
LifeS<- subset(LifeS, !(author %in% c("O´Donnovan", "Solis-Urra", "Valle")) )
Sdoh<- subset(Sdoh, !(author %in% c("Borda", "Valle" )) )

# Pooling ORs on Demographic factors with Paule-Mandel estimator
meta_pm <- metagen(TE=log_TE, seTE = log_seTE, lower=log_lower, upper=log_upper, #func.transf = log,
                    # func.backtransf = exp,
                    studlab = author, data=grouped_data, sm="OR", method.tau = "PM", prediction = TRUE,
                    comb.fixed=FALSE, comb.random=TRUE, hakn = TRUE,
                    title = "Meta PM ln(OR) on All Factors")

# meta_pm 

# Pooling ORs on Demographic factors with Paule-Mandel estimator
meta_pm_demo <- metagen(TE=log_TE, seTE = log_seTE, lower=log_lower, upper=log_upper, #func.transf = log,
                        # func.backtransf = exp,
                       studlab = author, data=Demo, sm="OR", method.tau = "PM", prediction = TRUE,
                       comb.fixed=FALSE, comb.random=TRUE, hakn = TRUE,
                       title = "Meta PM ln(OR) on Demographic Factors")

# meta_pm_demo 

# Pooling ORs on Health factors with Paule-Mandel method
meta_pm_health <- metagen(TE=log_TE, seTE = log_seTE, lower=log_lower, upper=log_upper, #func.transf = log,
                          # func.backtransf = exp,
                        studlab = author, data=Health, sm="OR", method.tau = "PM", prediction = TRUE,
                        comb.fixed=FALSE, comb.random=TRUE, hakn = TRUE,
                        title = "Meta PM ln(OR) on Health Factors")

# meta_pm_health 

# Pooling ORs on Life Style factors with Paule-Mandel method
meta_pm_life <- metagen(TE=log_TE, seTE = log_seTE, lower=log_lower, upper=log_upper, #func.transf = log,
                        # func.backtransf = exp,
                          studlab = author, data=LifeS, sm="OR", method.tau = "PM", prediction = TRUE,
                          comb.fixed=FALSE, comb.random=TRUE, hakn = TRUE,
                          title = "Meta PM ln(OR) on Life style Factors")

# meta_pm_life # ns

# Pooling ORs on Life Style factors with Paule-Mandel method
meta_pm_mentalh <- metagen(TE=log_TE, seTE = log_seTE, lower=log_lower, upper=log_upper, #func.transf = log,
                           # func.backtransf = exp,
                        studlab = author, data=MentalH, sm="OR", method.tau = "PM", prediction = TRUE,
                        comb.fixed=FALSE, comb.random=TRUE, hakn = TRUE,
                        title = "Meta PM ln(OR) on Mental Health Factors")

# meta_pm_mentalh 

# Pooling ORs on Life Style factors with Paule-Mandel method
meta_pm_sdoh <- metagen(TE=log_TE, seTE = log_seTE, lower=log_lower, upper=log_upper, #func.transf = log,
                        # func.backtransf = exp,
                        studlab = author, data=Sdoh, sm="OR", method.tau = "PM", prediction = TRUE,
                        comb.fixed=FALSE, comb.random=TRUE, hakn = TRUE,
                        title = "Meta PM ln(OR) on SDOH Factors")

# meta_pm_sdoh # ns

### Subgroup analysis
gmeta_pm <- update.meta(meta_pm, subgroup = country, tau.common = FALSE)
gmeta_pm_demo <- update.meta(meta_pm_demo, subgroup = country, tau.common = FALSE)
gmeta_pm_health <- update.meta(meta_pm_health, subgroup = country, tau.common = FALSE)
gmeta_pm_mentalh <- update.meta(meta_pm_mentalh, subgroup = country, tau.common = FALSE)
gmeta_pm_life <- update.meta(meta_pm_life, subgroup = country, tau.common = FALSE)
gmeta_pm_sdoh <- update.meta(meta_pm_sdoh, subgroup = country, tau.common = FALSE)

#####################

# Forest All predictors
library(extrafont)
par(family = "Arial")  # Set the font family to Arial
par(cex.axis = 1.1, cex.lab = 1.1)
font = 'Arial'


### Forest plots
names <- c("Cog_All_Forest.tiff", "Cog_Demographics_Forest.tiff", "Cog_Health_Forest.tiff",
           "Cog_Lifestyle_Forest.tiff", "Cog_Mental_Health_Forest.tiff", "Cog_SDOH_Forest.tiff")

modelos <- list(meta_pm, meta_pm_demo, meta_pm_health, meta_pm_life, meta_pm_mentalh, meta_pm_sdoh)


for (i in 1:length(names)){
    # pdf(file=names[i], width = 9, height = 8, fonts =c(font), bg='white', title='A. Cognition')
    tiff(file=names[i], width = 6, height = 8, unit='in', res=300,  bg='white')
    forest.meta(modelos[[i]], sortvar = modelos[[i]]$TE,
                prediction=TRUE, zero.pval = TRUE, 
                xlim=c(.25, 10), colgap="0 mm",colgap.studlab = "0 mm",cex = 2,
                # calcwidth.hetstat = TRUE,
                text.random = "Random effects model",
                text.predict="Prediction Interval",
                addrows.below.overall = 3,# cex.axis=2, cex.lab=2, cex.main=2, cex.sub=2,
                fontfamily=grid::gpar(fontfamily=font),
                print.tau2 = TRUE, print.I2 = TRUE,
                test.overall = TRUE,
                leftcols = c("author"), leftlabs=c("Author"),
                rightcols=c( "ci", "w.random"), rightlabs = c("CI-95%", "W"))
    dev.off()
}


### Enhanced Funnel plots
names <- c("Cog_All_funnel_en.tiff", "Cog_Demographics_funnel_en.tiff","Cog_Health_funnel_en.tiff","Cog_Lifestyle_funnel_en.tiff","Cog_Mental_Health_funnel_en.tiff", "Cog_SDOH_funnel_en.tiff")

col.contour = c("gray75", "gray85", "gray95")

# Loop para generar y guardar los gráficos
for(i in 1:length(names)) {
    # pdf(file = names[i], width = 9, height = 8, fonts =c(font), bg='white')
    tiff(file=names[i], width = 6, height = 7, unit='in', res=300,  bg='white')
    # Generate funnel plot (we do not include study labels here)
    metafor::funnel(modelos[[i]], steps=5, yaxs = "i", lwd = 1, cex = 2, hlines='black', 
                    cex.axis = 1.1,cex.lab = 1.1,
                    contour = c(0.9, 0.95, 0.99), col.contour = col.contour, xlim = c(0.1, 8))
    
    
    if (i != length(names)){# Quiero hacerlo distinto para el último modelo
        # Add a legend
        legend(x = 3.3, y = 0.01,
               legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
               fill = col.contour)
    }
    else {
        legend(x = 3.4, y = 0.825,
               legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
               fill = col.contour)
    }
    # Puedes agregar un título aquí si lo necesitas, por ahora lo dejaré comentado
    # title("Contour-Enhanced Funnel Plot (" ... ")")
    
    dev.off()
}


####  Eggers test
#
names <- c("Cog_All_Eggers.txt", "Cog_Demographics_Eggers.txt","Cog_Health_Eggers.txt",
           "Cog_Lifestyle_Eggers.txt","Cog_Mental_Health_Eggers.txt", "Cog_SDOH_Eggers.txt")

for (i in 1:length(names)){
    # substr(names[i], start=1, stop=nchar(names[i])-3)
    sink(paste0(substr(names[i], start=1, stop=nchar(names[i])-3), "txt"))
    print(eggers.test(modelos[[i]]))
    sink()
}

### P-Curve plots

names <- c("Cog_All_pcurve.tiff", "Cog_Demographics_pcurve.tiff","Cog_Health_pcurve.tiff",
           "Cog_Lifestyle_pcurve.tiff","Cog_Mental_Health_pcurve.tiff", "Cog_SDOH_pcurve.tiff")

for (i in 1:length(names)){
    tiff(file=names[i], width = 6, height = 7, unit='in', res=300,  bg='white')
    # tiff(file = names[i], width = 9, height = 8, fonts =c(font), bg='white')
    # substr(names[i], start=1, stop=nchar(names[i])-3)
    sink(paste0(substr(names[i], start=1, stop=nchar(names[i])-3), "txt"))
    print(pcurve(modelos[[i]]))
    dev.off()
    sink()
}


################ 
##GOSH

gosh_list <- list() # list to save gosh plots

names <- c("Cog_All_Gosh.tiff", "Cog_Demographics_Gosh.tiff","Cog_Health_Gosh.tiff",
           "Cog_Lifestyle_Gosh.tiff","Cog_Mental_Health_Gosh.tiff", "Cog_SDOH_Gosh.tiff")

for (i in 1:length(names)){
    tiff(file=names[i], width = 6, height = 7, unit='in', res=300,  bg='white')
    # png(file=names[i], bg = "white", pointsize=10, width=1400, height=960, res=300)
    par(mar=c(5, 4, 4, 2))
    sink(paste0(substr(names[i], start=1, stop=nchar(names[i])-3), "txt"))
    meta_rma <- rma(yi=modelos[[i]]$TE, sei=modelos[[i]]$seTE, method=modelos[[i]]$method.tau ,test="knha")
    res_gosh <- gosh(meta_rma, parallel = 'multicore', ncpus = 4, progbar=TRUE)
    gosh_list[[i]]<-res_gosh #Guardar res en una lista?
    print(res_gosh)
    # plot(res_gosh, alpha = 0.01, het="I2", out =outliers[[i]] , col=c("#1b8ec9","#0d5c0a"))
    # plot(res_gosh, alpha = 0.1, het="I2", col=c("#A6C0CE"))
    # plot(res_gosh, alpha = 0.2, het="I2", out = out[i], col=c("#3199cf","#0d5c0a") )
    plot(res_gosh, alpha = 0.2, het="I2", col=c("#0d5c0a"), cex.axis = 1.4,cex.lab = 1.5)
    
    dev.off()
    sink()
}

