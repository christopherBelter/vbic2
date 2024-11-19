# vbic2
R code for running the VBIC classification algorithm on real-world data


## About this repo
TBD


## Vignette 1: Using QVR data
First, read in the `qvr_processing()` set of functions to process the QVR data.
```r
source("qvr_processing.r")
```

Next, read in the application data and process it using the `qvr_processing()` function. This vignette assumes you're working with data from QVR, so if you're not, then skip the second line of this code block.
```r
appls <- read.csv("hd_admin_fy25_noText.csv", stringsAsFactors = FALSE)
appls <- process_qvr_data(appls)
```

Then clean the abstract data using the `clean_abstracts()` function, read the cleaned version into R, process it, and then merge it into the application data.
```r
clean_abstracts("hd_admin_fy25_text.csv")
abs <- read.csv("hd_admin_fy25_text_cleaned.csv", stringsAsFactors = FALSE)
abs <- process_qvr_data(abs)
appls <- merge(appls, abs, by = "appl_id")
rm(abs)
```

Next, create a concatenated version of the document text by pasting together the titles and abstracts of each application. Pasting the application title in three times, as in the code, allows terms appearing in the title to count 3x toward the document's concept score and terms appearing in the abstract to count 1x. This again assumes that you're working with QVR data; if not, you'll need to change the column names.
```r
appls$dtext <- paste(appls$title, appls$abstract_text_with_public_health_rel, appls$title, appls$title)
```

Then read in the `vbic_classify()` set of functions to do the concept assignment.
```r
source("vbic_classify.r")
```

Then read in the term vocabulary you want to use. The function expects the vocabulary to have columns named "term", "weight", and "concept", but you can specify other column names in the `vbic_classify()` function call later. In this case, we read in a set of terms for mapping applications to themes from the NICHD strategic plan and then filter the term vocabulary to remove cross-cutting themes. Setting `allowEscapes` to TRUE is important here, because the term list includes regular expressions that have escape characters embedded in them.
```r
tterms <- read.csv("nichd_theme_terms2.csv", stringsAsFactors = FALSE, allowEscapes = TRUE)
tterms <- tterms[grepl("theme|ncmrr", tterms$concept),]
```

Next, run the classification algorithm to assign concepts to documents. In this case, we want to classify documents in `appls` using the term vocabulary `tterms`. The default concept score (the `concept_threshold` argument) is set at 20, but you might want to increase that to as high as 50, depending on how targeted you want the resulting classifications to be. See [my vbic repo](https://github.com/christopherBelter/vbic) for more details on how the algorithm works. Depending on how many documents you have, this might take several minutes to run. 
```r
vbic_out <- vbic_classify(appls, tterms, doc_id_column = "appl_id", doc_text_column = "dtext")
```

The resulting `vbic_out` object will be a list of three data frames. The first, `vbic_out$dcm` will be the document-concept matrix giving the concept scores for each document and each concept in the term vocabulary. The second, `vbic_out$concept_list`, contains columns for the concepts assigned to each document. The `vbic_out$concept_list$primary_concept` column gives a semicolon-delimited list of the concept(s) that had the highest concept score, while the `vbic_out$concept_list$all_concepts` column gives a semicolon-delimited list of all the concepts that met the `concept_threshold` argument. The third data frame, `vbic_out$term_vocab`, provides total appearance counts and total document counts for each term in the concept vocabulary, which can be useful for fine-tuning the vocabulary to produce better results.

Finally, merge the concept mappings from the `vbic_out$concept_list` data frame with the original set of documents
```r
appls <- merge(appls, vbic_out$concept_list[,c(1,3,4)], by.x = "appl_id", by.y = "doc_id", all.x = TRUE)
```

and then save the results for future analyses. 
```r
write.csv(appls, file = "clustered_applications.csv", row.names = FALSE)
```


## Vignette 2: Using RePORTER data

First, read in the `get_nih_reporter()` functions used to gather the data and the `vbic_classify()` functions to do the classification. 
```r
source("get_nih_reporter.r")
source("vbic_classify.r")
```

Next, create the query you want to run and retrieve data for. In this case, we'll return all grants administered by NICHD that are currently active, excluding subprojects.
```r
mq <- create_query(IC = "NICHD", include_active = TRUE, exclude_subprojects = TRUE)
```

Then run the query and save the results. This will generate a data frame with the resulting grant data.
```r
grants <- get_nih_reporter(mq, "nichd_active.txt")
```

Next, paste together the grant titles, abstracts, and public health relevance texts, allowing terms in the title to count 3x toward the resulting concept scores.
```r
grants$dtext <- paste(grants$project_title, grants$abstract_text, grants$phr_text, grants$project_title, grants$project_title)
```

Then read in the term vocabulary to map grants to NICHD's strategic themes and filter the vocabulary to remove terms for cross-cutting themes. 
```r
tterms <- read.csv("nichd_theme_terms2.csv", stringsAsFactors = FALSE, allowEscapes = TRUE)
tterms <- tterms[grepl("theme|ncmrr", tterms$concept),]
```

Next, run the classification algorithm on the resulting texts, setting the `concept_threshold` argument to 50 to assign fewer concepts to each document.
```r
vbic_out <- vbic_classify(grants, tterms, concept_threshold = 50, doc_id_column = "appl_id", doc_text_column = "dtext")
```

Then merge the resulting concept list with the original grant data
```r
grants <- merge(grants, vbic_out$concept_list[,c(1,3,4)], by.x = "appl_id", by.y = "doc_id", all.x = TRUE)
```

and save the results.
```r
write.csv(grants, file = "clustered_grants.csv", row.names = FALSE)
```
