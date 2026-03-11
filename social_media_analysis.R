
# Install & load libraries

library(Hmisc)
library(pastecs)
library(psych)
library(ggplot2)
library(GGally)
library(corrplot)
library(dplyr)
library(tidyr)
library(knitr)


#setwd("C:/Users/tasnia/OneDrive - UWA/study materials/SEM 3/Big data/grp assignment")
# Load datasets

fb <- read.csv("A2-Sleep-Apps-Facebook.csv")
ig <- read.csv("A2-Sleep-Apps-Instagram.csv")

# Select numeric variables

fb_vars <- c("statistics.comment_count", 
             "statistics.like_count", 
             "statistics.share_count", 
             "statistics.reaction_count", 
             "statistics.views")

ig_vars <- c("statistics.comment_count", 
             "statistics.like_count", 
             "statistics.views")


# Descriptive statistics


# Facebook
summary(fb[fb_vars])
describe(fb[fb_vars])              
stat.desc(fb[fb_vars])             
psych::describe(fb[fb_vars])    

# Instagram
summary(ig[ig_vars])
describe(ig[ig_vars])
stat.desc(ig[ig_vars])
psych::describe(ig[ig_vars])


# descriptive stats (mean, sd, skew, kurtosis)

mystats <- function(x, na.omit=FALSE){
  if (na.omit) x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m)^3/s^3)/n
  kurt <- sum((x-m)^4/s^4)/n - 3
  return(c(n=n, mean=m, stdev=s, skew=skew, kurtosis=kurt))
}

sapply(fb[fb_vars], mystats)
sapply(ig[ig_vars], mystats)


# Grouped descriptive stats (by Brand)

aggregate(fb[fb_vars], by=list(Brand=fb$post_owner.name), mean)
aggregate(ig[ig_vars], by=list(Brand=ig$post_owner.name), mean)

aggregate(fb[fb_vars], by=list(Brand=fb$post_owner.name), sd)
aggregate(ig[ig_vars], by=list(Brand=ig$post_owner.name), sd)


# Frequency tables for categorical variables

table(fb$content_type)
prop.table(table(fb$content_type)) * 100

table(ig$content_type)
prop.table(table(ig$content_type)) * 100



# Correlations

# Facebook Correlation matrix

cor(fb[fb_vars], use="complete.obs")


# Significance test for correlation (example: Likes vs Views)
cor.test(fb$statistics.like_count, fb$statistics.views)


# correlation with p-values
corr.test(fb[fb_vars], use="complete")



# Instagram Correlation matrix

cor(ig[ig_vars], use="complete.obs")

# Significance test for correlation (example: Likes vs Views)
cor.test(ig$statistics.like_count, ig$statistics.views)


# correlation with p-values
corr.test(ig[ig_vars], use="complete")



#Correlations by Brand

# Facebook
brands_fb <- unique(fb$post_owner.name)
for (b in brands_fb) {
  cat("\n=== Facebook Brand:", b, "===\n")
  subdata <- subset(fb, post_owner.name == b)
  print(corr.test(subdata[fb_vars], use="complete"))
}

# Instagram
brands_ig <- unique(ig$post_owner.name)
for (b in brands_ig) {
  cat("\n=== Instagram Brand:", b, "===\n")
  subdata <- subset(ig, post_owner.name == b)
  print(corr.test(subdata[ig_vars], use="complete"))
}

#  Correlation Heatmaps

#  Facebook 
cor_fb <- cor(fb[fb_vars], use="complete.obs")
corrplot(cor_fb, method="color", type="upper", tl.col="black", tl.cex=0.8, title="Facebook Correlations", mar=c(0,0,1,0))

# Instagram
cor_ig <- cor(ig[ig_vars], use="complete.obs")
corrplot(cor_ig, method="color", type="upper", tl.col="black", tl.cex=0.8, title="Instagram Correlations", mar=c(0,0,1,0))


#  Scatterplots (Likes vs Views)

#  Facebook 
ggplot(fb, aes(x=statistics.like_count, y=statistics.views, color=post_owner.name)) +
  geom_point(alpha=0.6) +
  geom_smooth(method="lm", se=FALSE) +
  labs(title="Facebook: Likes vs Views", x="Likes", y="Views") +
  theme_minimal()

# Instagram
ggplot(ig, aes(x=statistics.like_count, y=statistics.views, color=post_owner.name)) +
  geom_point(alpha=0.6) +
  geom_smooth(method="lm", se=FALSE) +
  labs(title="Instagram: Likes vs Views", x="Likes", y="Views") +
  theme_minimal()



#Brand-specific Correlation Heatmaps


# Facebook by Brand
brands_fb <- unique(fb$post_owner.name)
for (b in brands_fb) {
  subdata <- subset(fb, post_owner.name == b)
  cor_mat <- cor(subdata[fb_vars], use="complete.obs")
  corrplot(cor_mat, method="color", type="upper", tl.col="black", tl.cex=0.8,
           title=paste("Facebook Correlations -", b), mar=c(0,0,1,0))
}

# Instagram by Brand
brands_ig <- unique(ig$post_owner.name)
for (b in brands_ig) {
  subdata <- subset(ig, post_owner.name == b)
  cor_mat <- cor(subdata[ig_vars], use="complete.obs")
  corrplot(cor_mat, method="color", type="upper", tl.col="black", tl.cex=0.8,
           title=paste("Instagram Correlations -", b), mar=c(0,0,1,0))
}


# Facebook - Likes
ggplot(fb, aes(x=post_owner.name, y=statistics.like_count, fill=post_owner.name)) +
  geom_boxplot(alpha=0.7) +
  labs(title="Facebook Likes by Brand", x="Brand", y="Likes") +
  theme_minimal()

# Instagram - Views
ggplot(ig, aes(x=post_owner.name, y=statistics.views, fill=post_owner.name)) +
  geom_boxplot(alpha=0.7) +
  labs(title="Instagram Views by Brand", x="Brand", y="Views") +
  theme_minimal()



# CrossTable Facebook

fb_tab <- xtabs(~ post_owner.name + content_type, data = fb)
fb_tab
margin.table(fb_tab, 1)            # row sums
prop.table(fb_tab, 1)              # row proportions
margin.table(fb_tab, 2)            # column sums
prop.table(fb_tab, 2)              # column proportions
prop.table(fb_tab)                 # cell proportions


# CrossTable Instagram
ig_tab <- xtabs(~ post_owner.name + content_type, data = ig)
ig_tab
margin.table(ig_tab, 1)
prop.table(ig_tab, 1)
margin.table(ig_tab, 2)
prop.table(ig_tab, 2)
prop.table(ig_tab)


# Chi-square test of independence
chisq.test(fb_tab)
chisq.test(ig_tab)


# Independent-samples t-tests (Brand differences)   


#  compare Likes by brand (Calm vs Headspace)
t.test(statistics.like_count ~ post_owner.name, data = fb)
t.test(statistics.like_count ~ post_owner.name, data = ig)

# comment,view, share count of fb
t.test(statistics.comment_count ~ post_owner.name, data = fb)
t.test(statistics.share_count ~ post_owner.name, data = fb)
t.test(statistics.reaction_count ~ post_owner.name, data = fb)
t.test(statistics.views ~ post_owner.name, data = fb)

#comment, view count of ig
t.test(statistics.comment_count ~ post_owner.name, data = ig)
t.test(statistics.views ~ post_owner.name, data = ig)



#text analysis

#  Packages
pkgs <- c(
  "dplyr","ggplot2","stringr","lubridate",
  "quanteda","readtext","SnowballC",
  "car","psych","Hmisc","pastecs","gvlma","effects",
  "caret","broom",
  # NEW:
  "tidytext","textdata"
)
for (p in pkgs) {
  if (!require(p, character.only=TRUE)) {
    # install.packages(p)
    library(p, character.only=TRUE)
  }
}

OUT_DIR <- "outputs"
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR)

#  Paths & Load Data
PATH_FB <- "A2-Sleep-Apps-Facebook.csv"
PATH_IG <- "A2-Sleep-Apps-Instagram.csv"

fb <- read.csv(PATH_FB, header=TRUE, na.strings=c("", "NA"), stringsAsFactors=FALSE)
ig <- read.csv(PATH_IG, header=TRUE, na.strings=c("", "NA"), stringsAsFactors=FALSE)

fb$platform <- "Facebook"
ig$platform <- "Instagram"


#  Derived Time
derive_time_feats <- function(df) {
  df$weekday <- weekdays(as.Date(df$creation_time))
  df$weekday <- factor(df$weekday,
                       levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),
                       ordered=TRUE)
  df$weekend <- factor(ifelse(df$weekday %in% c("Saturday","Sunday"), 1, 0))
  dt <- df$creation_time
  df$time_of_day_seconds <- lubridate::hour(dt)*3600 + lubridate::minute(dt)*60 + lubridate::second(dt)
  df
}
fb <- derive_time_feats(fb)
ig <- derive_time_feats(ig)


# Facebook reaction percentages

if (all(c("statistics.love_count","statistics.wow_count","statistics.haha_count",
          "statistics.sad_count","statistics.angry_count","statistics.care_count") %in% names(fb))) {
  nz <- function(x) ifelse(is.na(x), 0, x)
  love  <- nz(fb$statistics.love_count)
  wow   <- nz(fb$statistics.wow_count)
  haha  <- nz(fb$statistics.haha_count)
  sad   <- nz(fb$statistics.sad_count)
  angry <- nz(fb$statistics.angry_count)
  care  <- nz(fb$statistics.care_count)
  total <- love+wow+haha+sad+angry+care
  div <- function(a,b) ifelse(b>0, a/b, NA_real_)
  fb$love_pct  <- div(love,total)
  fb$wow_pct   <- div(wow,total)
  fb$haha_pct  <- div(haha,total)
  fb$sad_pct   <- div(sad,total)
  fb$angry_pct <- div(angry,total)
  fb$care_pct  <- div(care,total)
  fb$reaction_total <- total
}


# IG reaction percentages
if (!"statistics.share_count" %in% names(ig)) ig$statistics.share_count <- NA_real_

# Views
fb$views_log1p <- log1p(fb$statistics.views)
ig$views_log1p <- log1p(ig$statistics.views)



# Dictionaries 


# Regulatory Focus

dict_RF <- dictionary(list(
  promotion = c("accomplish","achievement","advancement","aspiration","aspire","attain",
                "desire","earn","expand","gain","grow","hope","hoping",
                "ideal","improve","increase","momentum","obtain","optimistic",
                "progress","promoting","promotion","speed","swift","toward",
                "velocity","wish"),
  prevention = c("accuracy","afraid","anxious","avoid",
                 "careful","conservative","defend","duty","escape","escaping","evade",
                 "fail","fear","loss","obligation","ought","pain","prevent","protect",
                 "responsible","risk","safety","security","threat","vigilance")
))


# Regulatory Mode

dict_RM_unstemmed <- dictionary(list(
  locomotion_unstemmed = c("can't wait","do it","done","go","going",
                           "motion","momentum","dynamic","hurry*","move*","proceed*",
                           "obstacle*","urg*"),
  assessment_unstemmed = c("exhaustive","methodical","right","unsure",
                           "accur*","alternat*","careful*","critic*","calculat*",
                           "consider*","meticulous*","procrastinat*","reconsider*","ruminat*")
))
dict_RM_stemmed <- dictionary(list(
  locomotion_stemmed = c("act","chang","dare","doer","drive","elimin",
                         "fast","flow","get","initiat","launch","lead",
                         "make","mobil","quick","reduc","reject","remov",
                         "smooth","speed","start"),
  assessment_stemmed = c("assess","compar","consult","correct",
                         "detail","evaluat","examin","judg","observ",
                         "perfect","ponder","question","reflect","regret",
                         "review","think","thorough","thought","true","truth")
))

# Brand Personality

dict_brand_personality <- dictionary(list(
  competence = c("able","able bodied","adept","adroit","assiduous","assured","astute","award winning",
                 "blooming","booming","brainy","celebratory","certified","competence","competent",
                 "complete","comprehensive","conscientious","consistent","crafty","dependable","diligent",
                 "experienced","fail safe","firm","flourishing","foolproof","forefront","genius","gifted",
                 "governance","guarantee","guaranteed","hard working","hardworking","hi tech","illustrious",
                 "industrial","industrious","intelligent","knowledgeable","lasting","leader","logical",
                 "long lasting","loyal","lucrative","methodological","meticulous","outstanding","painstaking",
                 "reliable","responsible","safe","scientific","secure","self assured","smart","solid",
                 "steadfast","successful","superior","systematic","talented","technical","thorough",
                 "thriving","trusty","unbeatable","unfailing","unwavering","up and coming","victorious","witty"),
  excitement = c("active","aggressive","artistic","avant garde","awesome","bold","brave","breathtaking",
                 "brisk","colorful","cool","courageous","creative","current","daring","dazzling","designer",
                 "energise","energize","enliven","excite","excited","excitement","exhilarating","feisty",
                 "fresh","gutsy","heroic","high spirited","hip","imaginative","innovative","inspiring",
                 "intrepid","inventive","invigorating","modern","moving","new","nifty","present","risky",
                 "rousing","sharp","smashing","spirited","stimulating","stirring","strong willed","thrilling",
                 "trendy","unique","unconstrained","valiant","vibrant","vital","young","youthful"),
  ruggedness = c("animal","arduous","beefy","boisterous","brutal","bumpy","callous","challenge",
                 "coarse","cowboy","craggy","cruel","dangerous","demanding","desert","difficult",
                 "durable","endure","extreme","ferocious","frontier","gravel","grueling","hard","hard hitting",
                 "harsh","hazardous","hunting","jagged","jeans","jerky","jungle","macho","manly","mountains",
                 "outdoor","outdoors","outside","perilous","physical","plucky","prairie","precarious","resilient",
                 "rigorous","robust","rocky","rough","rugged","ruggedness","rutted","safari","scratchy",
                 "severe","spartan","stony","strenuous","sturdy","survivor","tough","toughness","treacherous",
                 "trek","trekking","uneven","unforgiving","untamed","violent","weathered","western","wild","woodland"),
  sincerity = c("above board","accommodating","accurate","affable","authentic","benevolent","blunt","bright",
                "buoyant","candid","charitable","cheerful","civil","clean cut","clear cut","compassionate",
                "congenial","content","conventional","cooperative","cordial","correct","courteous","customary",
                "decent","earnest","emotional","everyday","factual","faithful","forthcoming","forthright","friendly",
                "generous","genial","genuine","glad","good","good hearted","gracious","gregarious","guileless",
                "healthful","heartfelt","helpful","honest","honesty","honourable","humane","humble","indisputable",
                "inspired","jovial","kind","kindly","legitimate","legitimize","natural","obliging","open","ordinary",
                "plainspoken","pleasant","polite","positive","practical","proper","reliable","respectable","responsive",
                "scrupulous","self effacing","sentimental","simple","sincere","smiling","sociable","straight",
                "straightforward","sympathetic","true","trustworthy","truthful","typical","unadulterated","unassuming",
                "understanding","unpretentious","valid","veritable","warm","welcoming","wholesome"),
  sophistication = c("a la mode","alluring","appealing","aristocratic","attractive","baronial","beautiful",
                     "captivating","celebrated","charismatic","charming","classy","cosmopolitan","couture",
                     "cultivated","cultured","cute","delicate","dignified","distinguished","elegant",
                     "enchanting","engaging","esteemed","excellent","exclusive","expensive","exquisite",
                     "extravagant","eye catching","fabulous","fascinating","fashionable","feminine","first class",
                     "flossy","fragrant","genteel","gentle","glamorous","glamour","glittering","glossy","gorgeous",
                     "graceful","handsome","haute couture","high class","in style","in vogue","indulgent",
                     "irreplaceable","lady","lustrous","luxurious","magnificent","matchless","nobility","noble",
                     "photogenic","picturesque","pleasing","polished","posh","precious","prestigious","pretty",
                     "princely","queenly","refined","renowned","royal","satin","scintillating","seductive",
                     "sensational","shining","silk","silky","spectacular","splendid","striking","stunning","stylish",
                     "suave","superfine","top notch","upmarket","velvet","velvety","voguish","voluptuous","well bred","womanly")
))


# NRC Emotion & AFINN Sentiment 


nrc_tbl <- suppressMessages(textdata::lexicon_nrc())
if ("term" %in% names(nrc_tbl)) names(nrc_tbl)[names(nrc_tbl)=="term"] <- "word"
nrc_tbl$word <- tolower(nrc_tbl$word)
dict_NRC <- quanteda::dictionary(split(nrc_tbl$word, nrc_tbl$sentiment))

afinn_tbl <- suppressMessages(textdata::lexicon_afinn())
if ("term"  %in% names(afinn_tbl)) names(afinn_tbl)[names(afinn_tbl)=="term"]  <- "word"
if ("score" %in% names(afinn_tbl)) names(afinn_tbl)[names(afinn_tbl)=="score"] <- "value"
afinn_tbl$word <- tolower(afinn_tbl$word)


# Tokenize


# Build corpus
corp <- corpus(fb, text_field = "text")

# Tokenize into words
tokens1 <- tokens(corp, what = "word")

# Lowercase all tokens 
tokens2 <- tokens1 |> tokens_tolower()

# Remove punctuation, symbols, numbers, URLs, split hyphens 
tokens3 <- tokens2 |> tokens(
  remove_punct      = TRUE,
  remove_symbols    = TRUE,
  remove_numbers    = TRUE,
  remove_url        = TRUE,
  remove_separators = TRUE,
  split_hyphens     = TRUE
)

# Remove stopwords 
exceptions <- unique(tolower(unlist(dict_RF)))   # keep dictionary words
stop_all   <- stopwords("en")
custom_sw  <- setdiff(stop_all, exceptions)
tokens4    <- tokens_remove(tokens3, pattern = custom_sw)

# Remove hashtags, @mentions, 1-char, etc
tokens5 <- tokens_remove(
  tokens4,
  pattern = c(".*\\..*", "^.*_.*$", "#\\w+", "@\\w+", "^.$"),
  valuetype = "regex"
)

# Remove symbols/emojis
tokens6 <- tokens_select(
  tokens5,
  pattern   = "[\\p{So}\\p{Sk}\\p{Sc}\\p{Sm}]",
  selection = "remove",
  valuetype = "regex"
)

# Create document-feature matrix
dfm_base   <- dfm(tokens6)
dfm_scored <- dfm_lookup(dfm_base, dict_RF, exclusive = TRUE)
scores     <- convert(dfm_scored, to = "data.frame")

# Add word count 
scores$word_count <- ntoken(tokens6)


#  Apply dictionaries to Facebook & Instagram

apply_dictionary <- function(df, dict, dims, prefix){
  out <- tokenise_and_score(df, "text", dict)
  out$scores <- ensure_cols(out$scores, dims)
  res <- out$scores
  wc_name <- paste0(prefix, "_word_count")
  names(res)[names(res)=="word_count"] <- wc_name
  for (d in dims) {
    res[[paste0(d,"_pct")]] <- ifelse(res[[wc_name]] > 0,
                                      round(res[[d]]/res[[wc_name]], 3), NA)
  }
  res[, c(dims, wc_name, paste0(dims,"_pct"))]
}


# Regulatory Focus 
rf_dims <- c("promotion","prevention")
rf_fb_df <- apply_dictionary(fb, dict_RF, rf_dims, "rf")
rf_ig_df <- apply_dictionary(ig, dict_RF, rf_dims, "rf")


# Brand Personality 
bp_dims <- c("competence","excitement","ruggedness","sincerity","sophistication")
bp_fb_df <- apply_dictionary(fb, dict_brand_personality, bp_dims, "bp")
bp_ig_df <- apply_dictionary(ig, dict_brand_personality, bp_dims, "bp")


# NRC Emotion
apply_nrc <- function(df, dict){
  out <- tokenise_and_score(df, "text", dict)
  res <- out$scores
  names(res)[names(res)=="word_count"] <- "nrc_word_count"
  emo <- setdiff(names(res), c("doc_id","nrc_word_count"))
  for (e in emo) {
    res[[paste0(e,"_pct")]] <- ifelse(res$nrc_word_count > 0,
                                      round(res[[e]]/res$nrc_word_count, 3), NA)
  }
  res
}
nrc_fb_df <- apply_nrc(fb, dict_NRC)
nrc_ig_df <- apply_nrc(ig, dict_NRC)


# Regulatory Mode 
rm_apply <- function(df, dict_un, dict_stem){
  un <- tokenise_and_score(df, "text", dict_un)
  un$scores <- ensure_cols(un$scores, c("locomotion_unstemmed","assessment_unstemmed"))
  st <- dfm_wordstem(un$dfm_base, language=quanteda_options("language_stemmer")) |>
    dfm_lookup(dict_stem, exclusive=TRUE) |>
    convert(to="data.frame")
  st <- ensure_cols(st, c("locomotion_stemmed","assessment_stemmed"))
  wc <- un$scores["word_count"]; names(wc)[1] <- "rm_word_count"
  data.frame(
    locomotion_unstemmed = un$scores$locomotion_unstemmed,
    assessment_unstemmed = un$scores$assessment_unstemmed,
    locomotion_stemmed   = st$locomotion_stemmed,
    assessment_stemmed   = st$assessment_stemmed,
    rm_word_count        = wc$rm_word_count,
    locomotion_pct       = ifelse(wc$rm_word_count>0,
                                  round((un$scores$locomotion_unstemmed+st$locomotion_stemmed)/wc$rm_word_count,3), NA),
    assessment_pct       = ifelse(wc$rm_word_count>0,
                                  round((un$scores$assessment_unstemmed+st$assessment_stemmed)/wc$rm_word_count,3), NA)
  )
}
rm_fb_df <- rm_apply(fb, dict_RM_unstemmed, dict_RM_stemmed)
rm_ig_df <- rm_apply(ig, dict_RM_unstemmed, dict_RM_stemmed)


# AFINN Sentiment
make_afinn <- function(df, tbl){
  toks <- df %>%
    select(row_id = row_number(), text) %>%
    tidytext::unnest_tokens(token, text, token="words", to_lower=TRUE)
  wc <- toks %>% count(row_id, name="afinn_word_count")
  toks %>%
    inner_join(tbl, by=c("token"="word")) %>%
    group_by(row_id) %>%
    summarise(afinn_sum = sum(value), afinn_mean = mean(value), .groups="drop") %>%
    left_join(wc, by="row_id") %>%
    mutate(afinn_per100 = ifelse(afinn_word_count > 0, 100*afinn_sum/afinn_word_count, NA_real_))
}



# Merge features
fbX <- cbind(fb, rf_fb_df, rm_fb_df, bp_fb_df)
igX <- cbind(ig, rf_ig_df, rm_ig_df, bp_ig_df)

# add NRC 
if ("doc_id" %in% names(nrc_fb_df)) nrc_fb_df$doc_id <- NULL
if ("doc_id" %in% names(nrc_ig_df)) nrc_ig_df$doc_id <- NULL
fbX <- cbind(fbX, nrc_fb_df)
igX <- cbind(igX, nrc_ig_df)


# word count
fbX$text_word_count <- fbX$rf_word_count
igX$text_word_count <- igX$rf_word_count

write.csv(fbX, file.path(OUT_DIR,"facebook_engineered.csv"), row.names=FALSE)
write.csv(igX, file.path(OUT_DIR,"instagram_engineered.csv"), row.names=FALSE)



# recommendations
recommend_from_model <- function(fit_list, label){
  if (is.null(fit_list)) return(invisible(NULL))
  cat("\n Recommendations: ", label, "\n", sep="")
  sm <- broom::tidy(fit_list$model)
  drivers <- sm %>% dplyr::filter(term!="(Intercept)") %>% dplyr::arrange(dplyr::desc(estimate))
  cat("\nTop positive drivers:\n"); print(head(drivers[,c("term","estimate","p.value")], 5), row.names=FALSE)
  cat("\nPotential negatives:\n");  print(head(drivers %>% dplyr::arrange(estimate) %>% dplyr::select(term,estimate,p.value), 5), row.names=FALSE)
}
recommend_from_model(fb_like,  "Facebook — Likes")
recommend_from_model(fb_comm,  "Facebook — Comments")
recommend_from_model(fb_share, "Facebook — Shares")
recommend_from_model(ig_like,  "Instagram — Likes")
recommend_from_model(ig_comm,  "Instagram — Comments")



# Brand × Platform text analysis 

brands_keep <- c("Calm","Headspace")
bothX <- dplyr::bind_rows(fbX, igX) %>%
  filter(post_owner.name %in% brands_keep)


# summarise by Brand × Platform 

summarise_by_brand_platform <- function(df, cols, label) {
  cols <- intersect(cols, names(df))
  if (length(cols) == 0) return(invisible(NULL))
  
  tab <- df %>%
    group_by(platform, post_owner.name) %>%
    summarise(across(all_of(cols), ~ mean(.x, na.rm=TRUE)), n_posts = n(), .groups="drop")
  
  cat("\n", label, " (Brand × Platform)\n")
  print(knitr::kable(tab, digits=3))
  
  invisible(tab)
}


# Calm vs Headspace

contrast_brands_within_platform <- function(df, cols, platform_value) {
  cols <- intersect(cols, names(df))
  sub <- df %>%
    filter(platform == platform_value, post_owner.name %in% brands_keep) %>%
    group_by(post_owner.name) %>%
    summarise(across(all_of(cols), ~ mean(.x, na.rm=TRUE)), .groups="drop")
  
  if (nrow(sub) == 2) {
    diff <- sub[1,cols] - sub[2,cols]
    out <- data.frame(feature = cols, Calm_minus_Headspace = as.numeric(diff))
    cat("\n Calm – Headspace in", platform_value, "\n")
    print(knitr::kable(out, digits=3))
  }
}


# FB vs IG within brand

contrast_platforms_within_brand <- function(df, cols, brand_value) {
  cols <- intersect(cols, names(df))
  sub <- df %>%
    filter(post_owner.name == brand_value) %>%
    group_by(platform) %>%
    summarise(across(all_of(cols), ~ mean(.x, na.rm=TRUE)), .groups="drop")
  
  if (nrow(sub) == 2) {
    diff <- sub[sub$platform=="Facebook", cols] - sub[sub$platform=="Instagram", cols]
    out <- data.frame(feature = cols, Facebook_minus_Instagram = as.numeric(diff))
    cat("\nFacebook – Instagram for", brand_value, "\n")
    print(knitr::kable(out, digits=3))
  }
}

# Define dictionary column sets
rf_cols    <- c("promotion_pct","prevention_pct","rf_word_count")
rm_cols    <- c("locomotion_pct","assessment_pct","rm_word_count")
bp_cols    <- c("competence_pct","excitement_pct","ruggedness_pct","sincerity_pct","sophistication_pct","bp_word_count")
nrc_base   <- c("positive","negative","anger","anticipation","disgust","fear","joy","sadness","surprise","trust")
nrc_cols   <- intersect(paste0(nrc_base,"_pct"), names(bothX))
nrc_cols   <- c(nrc_cols,"nrc_word_count")
afinn_cols <- "afinn_per100"
all_cols <- unique(c(rf_cols, rm_cols, bp_cols, nrc_cols, afinn_cols))


# Summary
summarise_by_brand_platform(bothX, rf_cols,    "Regulatory Focus")
summarise_by_brand_platform(bothX, rm_cols,    "Regulatory Mode")
summarise_by_brand_platform(bothX, bp_cols,    "Brand Personality")
summarise_by_brand_platform(bothX, nrc_cols,   "NRC Emotions")
summarise_by_brand_platform(bothX, afinn_cols, "AFINN Sentiment")


# Contrasts
contrast_brands_within_platform(bothX, all_cols, "Facebook")
contrast_brands_within_platform(bothX, all_cols, "Instagram")

contrast_platforms_within_brand(bothX, all_cols, "Calm")
contrast_platforms_within_brand(bothX, all_cols, "Headspace")


# Regressions analysis by Brand × Platform

# Load datasets
fbX <- read.csv("outputs/facebook_engineered.csv",  stringsAsFactors = FALSE)
igX <- read.csv("outputs/instagram_engineered.csv", stringsAsFactors = FALSE)

# combine columns
common_cols <- intersect(names(fbX), names(igX))
fbX2 <- fbX[, common_cols]
igX2 <- igX[, common_cols]

fbX2$platform <- "Facebook"
igX2$platform <- "Instagram"
bothX <- rbind(fbX2, igX2)


# weekend as factor
if ("weekend" %in% names(bothX)) {
  if (is.character(bothX$weekend)) {
    bothX$weekend <- ifelse(bothX$weekend %in% c("1","TRUE","True","Weekend","Sat","Sun"), 1, 0)
  }
  bothX$weekend <- factor(bothX$weekend, levels = c(0,1))
}
if ("content_type" %in% names(bothX)) bothX$content_type <- factor(bothX$content_type)


# Dependent variables 

if ("statistics.like_count"    %in% names(bothX)) bothX$like_log1p    <- log1p(bothX$statistics.like_count)
if ("statistics.comment_count" %in% names(bothX)) bothX$comment_log1p <- log1p(bothX$statistics.comment_count)
if ("statistics.share_count"   %in% names(bothX)) bothX$share_log1p   <- log1p(bothX$statistics.share_count)


# Predictor set

preds_simple <- c(
  "views_log1p","duration","time_of_day_seconds","weekend","content_type",
  "promotion_pct","prevention_pct","locomotion_pct","assessment_pct",
  "competence_pct","excitement_pct","ruggedness_pct","sincerity_pct","sophistication_pct",
  "joy_pct","fear_pct","sadness_pct","trust_pct","positive_pct","negative_pct",
  "afinn_per100"
)


# formula

build_formula <- function(dv, data) {
  rhs_ok <- intersect(preds_simple, names(data))
  as.formula(paste(dv, "~", paste(rhs_ok, collapse = " + ")))
}


# lm function

safe_lm <- function(formula, data) {
  keep <- all.vars(formula)
  dat  <- data[, keep, drop = FALSE]
  dat  <- stats::na.omit(dat)
  if (nrow(dat) < 30) return(NULL)
  lm(formula, data = dat)
}


# data by platform/brand

fb_only <- subset(bothX, platform == "Facebook")
ig_only <- subset(bothX, platform == "Instagram")

calm_fb <- subset(bothX, platform == "Facebook"  & post_owner.name == "Calm")
calm_ig <- subset(bothX, platform == "Instagram" & post_owner.name == "Calm")
head_fb <- subset(bothX, platform == "Facebook"  & post_owner.name == "Headspace")
head_ig <- subset(bothX, platform == "Instagram" & post_owner.name == "Headspace")


# Facebook overall

form <- build_formula("like_log1p", fb_only);    fit_fb_like  <- if (!is.null(form)) safe_lm(form, fb_only) else NULL
form <- build_formula("comment_log1p", fb_only); fit_fb_comm  <- if (!is.null(form)) safe_lm(form, fb_only) else NULL
if ("share_log1p" %in% names(fb_only)) {
  form <- build_formula("share_log1p", fb_only); fit_fb_share <- if (!is.null(form)) safe_lm(form, fb_only) else NULL
} else fit_fb_share <- NULL


# Instagram overall

form <- build_formula("like_log1p", ig_only);    fit_ig_like  <- if (!is.null(form)) safe_lm(form, ig_only) else NULL
form <- build_formula("comment_log1p", ig_only); fit_ig_comm  <- if (!is.null(form)) safe_lm(form, ig_only) else NULL


# Calm
form <- build_formula("like_log1p", calm_fb);    fit_calm_fb_like  <- if (!is.null(form)) safe_lm(form, calm_fb) else NULL
form <- build_formula("comment_log1p", calm_fb); fit_calm_fb_comm  <- if (!is.null(form)) safe_lm(form, calm_fb) else NULL
if ("share_log1p" %in% names(calm_fb)) {
  form <- build_formula("share_log1p", calm_fb); fit_calm_fb_share <- if (!is.null(form)) safe_lm(form, calm_fb) else NULL
} else fit_calm_fb_share <- NULL

form <- build_formula("like_log1p", calm_ig);    fit_calm_ig_like  <- if (!is.null(form)) safe_lm(form, calm_ig) else NULL
form <- build_formula("comment_log1p", calm_ig); fit_calm_ig_comm  <- if (!is.null(form)) safe_lm(form, calm_ig) else NULL


# Headspace
form <- build_formula("like_log1p", head_fb);    fit_head_fb_like  <- if (!is.null(form)) safe_lm(form, head_fb) else NULL
form <- build_formula("comment_log1p", head_fb); fit_head_fb_comm  <- if (!is.null(form)) safe_lm(form, head_fb) else NULL
if ("share_log1p" %in% names(head_fb)) {
  form <- build_formula("share_log1p", head_fb); fit_head_fb_share <- if (!is.null(form)) safe_lm(form, head_fb) else NULL
} else fit_head_fb_share <- NULL

form <- build_formula("like_log1p", head_ig);    fit_head_ig_like  <- if (!is.null(form)) safe_lm(form, head_ig) else NULL
form <- build_formula("comment_log1p", head_ig); fit_head_ig_comm  <- if (!is.null(form)) safe_lm(form, head_ig) else NULL


# summaries
cat("\nFacebook Overall\n")
if (!is.null(fit_fb_like))  print(summary(fit_fb_like))
if (!is.null(fit_fb_comm))  print(summary(fit_fb_comm))
if (!is.null(fit_fb_share)) print(summary(fit_fb_share))

cat("\nInstagram Overall\n")
if (!is.null(fit_ig_like))  print(summary(fit_ig_like))
if (!is.null(fit_ig_comm))  print(summary(fit_ig_comm))

cat("\nCalm Facebook\n")
if (!is.null(fit_calm_fb_like))  print(summary(fit_calm_fb_like))
if (!is.null(fit_calm_fb_comm))  print(summary(fit_calm_fb_comm))
if (!is.null(fit_calm_fb_share)) print(summary(fit_calm_fb_share))

cat("\nCalm Instagram\n")
if (!is.null(fit_calm_ig_like))  print(summary(fit_calm_ig_like))
if (!is.null(fit_calm_ig_comm))  print(summary(fit_calm_ig_comm))

cat("\nHeadspace Facebook\n")
if (!is.null(fit_head_fb_like))  print(summary(fit_head_fb_like))
if (!is.null(fit_head_fb_comm))  print(summary(fit_head_fb_comm))
if (!is.null(fit_head_fb_share)) print(summary(fit_head_fb_share))

cat("\nHeadspace Instagram\n")
if (!is.null(fit_head_ig_like))  print(summary(fit_head_ig_like))
if (!is.null(fit_head_ig_comm))  print(summary(fit_head_ig_comm))
