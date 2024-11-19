# Vignette 1
source("qvr_processing.r")

appls <- read.csv("hd_admin_fy25_noText.csv", stringsAsFactors = FALSE)
appls <- process_qvr_data(appls)

clean_abstracts("hd_admin_fy25_text.csv")
abs <- read.csv("hd_admin_fy25_text_cleaned.csv", stringsAsFactors = FALSE)
abs <- process_qvr_data(abs)
appls <- merge(appls, abs, by = "appl_id")
rm(abs)

appls$dtext <- paste(appls$title, appls$abstract_text_with_public_health_rel, appls$title, appls$title)

source("vbic_classify.r")

tterms <- read.csv("nichd_theme_terms2.csv", stringsAsFactors = FALSE, allowEscapes = TRUE)
tterms <- tterms[grepl("theme|ncmrr", tterms$concept),]

vbic_out <- vbic_classify(appls, tterms, doc_id_column = "appl_id", doc_text_column = "dtext")

appls <- merge(appls, vbic_out$concept_list[,c(1,3,4)], by.x = "appl_id", by.y = "doc_id", all.x = TRUE)

write.csv(appls, file = "clustered_applications.csv", row.names = FALSE)


# Vignette 2
source("vbic_classify.r")

source("get_nih_reporter.r")

mq <- create_query(IC = "NICHD", include_active = TRUE, exclude_subprojects = TRUE)

grants <- get_nih_reporter(mq, "nichd_active.txt")

grants$dtext <- paste(grants$project_title, grants$abstract_text, grants$phr_text, grants$project_title, grants$project_title)

tterms <- read.csv("nichd_theme_terms2.csv", stringsAsFactors = FALSE, allowEscapes = TRUE)
tterms <- tterms[grepl("theme|ncmrr", tterms$concept),]

vbic_out <- vbic_classify(grants, tterms, concept_threshold = 50, doc_id_column = "appl_id", doc_text_column = "dtext")

grants <- merge(grants, vbic_out$concept_list[,c(1,3,4)], by.x = "appl_id", by.y = "doc_id", all.x = TRUE)

write.csv(grants, file = "clustered_grants.csv", row.names = FALSE)
