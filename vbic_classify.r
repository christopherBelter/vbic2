## version 0.4

vbic_classify <- function(doc_csv, term_csv, concept_threshold = 20, unclassified = FALSE, doc_id_column = "", doc_text_column = "", term_name_column = "term", term_concept_column = "concept", term_weight_column = "weight") {
	the_terms <- unique(term_csv[,term_name_column])
	doc_concept_matrix <- data.frame(doc_id = doc_csv[,doc_id_column], doc_text = doc_csv[,doc_text_column])
	message("Preparing the text.")
	doc_concept_matrix$doc_text <- expand_abbreviations(doc_concept_matrix$doc_id, doc_concept_matrix$doc_text)
	doc_concept_matrix$doc_text <- tolower(doc_concept_matrix$doc_text)
	message("Classifying the documents.")
	doc_term_mtx <- t(sapply(1:nrow(doc_concept_matrix), function(x) {
		if (x %% 500 == 0) message(paste("Classifying document", x, "of", nrow(doc_concept_matrix)))
		stringr::str_count(doc_concept_matrix$doc_text[x], the_terms)
	}))
	rownames(doc_term_mtx) <- doc_concept_matrix$doc_id
	colnames(doc_term_mtx) <- the_terms
	message("Summarizing the results.")
	the_vocab <- data.frame(
		term = colnames(doc_term_mtx),
		term_count = colSums(doc_term_mtx),
		doc_count = sapply(1:ncol(doc_term_mtx), function(x) sum(doc_term_mtx[,x] > 0))
	)
	term_df <- data.frame(term = term_csv[,term_name_column], term_concept = term_csv[,term_concept_column], term_weight = term_csv[,term_weight_column])
	split_mtx <- lapply(unique(term_df$term_concept), function(x) doc_term_mtx[,term_df$term[term_df$term_concept == x]])
	#split_mtx <- split_mtx[sapply(split_mtx, is.matrix) == TRUE] ##?
	#split_mtx <- split_mtx[sapply(split_mtx, nrow) > 0] ##?
	#return(split_mtx)
	#split_mtx <- lapply(split_mtx, as.data.frame)
	names(split_mtx) <- unique(term_df$term_concept)
	split_mtx <- lapply(unique(term_df$term_concept), function(y) sapply(1:nrow(split_mtx[[y]]), function(x) split_mtx[[y]][x,] * term_df$term_weight[term_df$term_concept == y]))
	#return(split_mtx)
	names(split_mtx) <- unique(term_df$term_concept)
	split_mtx <- lapply(split_mtx, colSums)
	doc_concept_matrix <- cbind(doc_concept_matrix, as.data.frame(split_mtx))
	concepts <- sapply(1:nrow(doc_concept_matrix), function(x) names(doc_concept_matrix[which(doc_concept_matrix[x,] >= concept_threshold)])) 
	concepts <- lapply(1:length(concepts), function(x) concepts[[x]][!concepts[[x]] %in% c("doc_id", "doc_text")])
	concept_sum <- data.frame(doc_id = doc_concept_matrix$doc_id, doc_text = doc_concept_matrix$doc_text, primary_concept = sapply(1:nrow(doc_concept_matrix), function(x) paste(colnames(doc_concept_matrix)[which(doc_concept_matrix[x,3:ncol(doc_concept_matrix)] == max(doc_concept_matrix[x,3:ncol(doc_concept_matrix)])) + 2], collapse = ";")), all_concepts = sapply(concepts, paste, collapse = ";"))
	concept_sum$all_concepts[concept_sum$all_concepts == ""] <- "unclassified"
	if (unclassified == FALSE) {
		## create a final concept list for each document that includes the unique set of the primary and all secondary concepts
		## necessary because some documents don't have enough text to meet the threshold for any concept, so they end up being assigned to whichever concept score is highest
		fin_concepts <- sapply(1:nrow(concept_sum), function(x) unique(c(unlist(strsplit(concept_sum[x,3], ";")), unlist(strsplit(concept_sum[x,4], ";")))))
		fin_concepts <- lapply(fin_concepts, sort)
		## merge the results back into the concept_sum table
		concept_sum$all_concepts <- sapply(fin_concepts, paste, collapse = ";")
		concept_sum$all_concepts <- gsub(";unclassified", "", concept_sum$all_concepts)
		concept_sum$all_concepts <- gsub("unclassified;", "", concept_sum$all_concepts)
		## isolate docs that have no term matches in the term_csv table and set them to 'unclassified'
		concept_sum$primary_concept[sapply(1:nrow(doc_concept_matrix), function(x) sum(doc_concept_matrix[x,3:ncol(doc_concept_matrix)])) == 0] <- "unclassified"
		concept_sum$all_concepts[concept_sum$primary_concept == "unclassified"] <- "unclassified"
	}
	else {
		concept_sum$highest_score <- concept_sum$primary_concept
		concept_sum$primary_concept[concept_sum$all_concepts == "unclassified"] <- "unclassified"
		concept_sum$all_concepts <- gsub(";unclassified", "", concept_sum$all_concepts)
		concept_sum$all_concepts <- gsub("unclassified;", "", concept_sum$all_concepts)
	}
	the_results <- list(dcm = doc_concept_matrix, concept_list = concept_sum, term_vocab = the_vocab)
	message("Done.")
	return(the_results)
}

vbic_classify_old <- function(doc_csv, term_csv, concept_threshold = 20, unclassified = FALSE, doc_id_column = "", doc_text_column = "", term_name_column = "term", term_concept_column = "concept", term_weight_column = "weight") {

## need to specify: term DF, term DF column names, doc DF, doc ID column name, doc title column, doc text columns
## could create preset lists of document columns based on document source: reporter, qvr, pubmed, wos, scopus, etc
## *** update: preset lists do too much; doesn't allow the user to define other text columns from data sources

## use allowEscapes = TRUE in read.csv to get backslashes to read in correctly (i.e. \\ in the file stays \\ instead of being changed to \\\\
#term_csv$term <- gsub("\\\\b", "\\b", term_csv[,term_name_column])
termLists <- split(term_csv[,term_name_column], term_csv[,term_concept_column])
termWeights <- split(term_csv[,term_weight_column], term_csv[,term_concept_column])
termLists <- lapply(1:length(termLists), function(x) rep(termLists[[x]], termWeights[[x]]))
names(termLists) <- names(termWeights)

## set up the text for generating the document-concept matrix
doc_concept_matrix <- data.frame(doc_id = doc_csv[,doc_id_column], doc_text = doc_csv[,doc_text_column])
doc_concept_matrix$doc_text <- tolower(doc_concept_matrix$doc_text)

## generate concept scores for each document for each concept and rename the resulting columns
for (i in 1:length(termLists)) {
  doc_concept_matrix[,i + 2] <- sapply(1:nrow(doc_concept_matrix), function(x) sum(stringr::str_count(doc_concept_matrix$doc_text[x], termLists[[i]])))
  message(paste("Finished term list", i, "of", length(termLists)))
}
colnames(doc_concept_matrix) <- c("doc_id", "doc_text", names(termLists))

## identify the concepts in the doc_concept_matrix with concept scores above the preselected threshold
concepts <- sapply(1:nrow(doc_concept_matrix), function(x) names(doc_concept_matrix[which(doc_concept_matrix[x,] >= concept_threshold)])) 
concepts <- lapply(1:length(concepts), function(x) concepts[[x]][!concepts[[x]] %in% c("doc_id", "doc_text")])

## create a results table with the document ID, primary concept (i.e. the concept with the highest concept score), and a delimited list of the concepts above the threshold
concept_sum <- data.frame(doc_id = doc_concept_matrix$doc_id, doc_text = doc_concept_matrix$doc_text, primary_concept = sapply(1:nrow(doc_concept_matrix), function(x) paste(colnames(doc_concept_matrix)[which(doc_concept_matrix[x,3:ncol(doc_concept_matrix)] == max(doc_concept_matrix[x,3:ncol(doc_concept_matrix)])) + 2], collapse = ";")), all_concepts = sapply(concepts, paste, collapse = ";"))
concept_sum$all_concepts[concept_sum$all_concepts == ""] <- "unclassified"

if (unclassified == FALSE) {
	## create a final concept list for each document that includes the unique set of the primary and all secondary concepts
	## necessary because some documents don't have enough text to meet the threshold for any concept, so they end up being assigned to whichever concept score is highest
	fin_concepts <- sapply(1:nrow(concept_sum), function(x) unique(c(unlist(strsplit(concept_sum[x,3], ";")), unlist(strsplit(concept_sum[x,4], ";")))))
	fin_concepts <- lapply(fin_concepts, sort)
	## merge the results back into the concept_sum table
	concept_sum$all_concepts <- sapply(fin_concepts, paste, collapse = ";")
	concept_sum$all_concepts <- gsub(";unclassified", "", concept_sum$all_concepts)
	concept_sum$all_concepts <- gsub("unclassified;", "", concept_sum$all_concepts)
	## isolate docs that have no term matches in the term_csv table and set them to 'unclassified'
	concept_sum$primary_concept[sapply(1:nrow(doc_concept_matrix), function(x) sum(doc_concept_matrix[x,3:ncol(doc_concept_matrix)])) == 0] <- "unclassified"
	concept_sum$all_concepts[concept_sum$primary_concept == "unclassified"] <- "unclassified"
}
else {
	concept_sum$highest_score <- concept_sum$primary_concept
	concept_sum$primary_concept[concept_sum$all_concepts == "unclassified"] <- "unclassified"
	concept_sum$all_concepts <- gsub(";unclassified", "", concept_sum$all_concepts)
	concept_sum$all_concepts <- gsub("unclassified;", "", concept_sum$all_concepts)
}

results_list <- list(dcm = doc_concept_matrix, concept_list = concept_sum)
return(results_list)
}

change_concept_threshold <- function(doc_concept_matrix, concept_threshold, unclassified = FALSE) {

concepts <- sapply(1:nrow(doc_concept_matrix), function(x) names(doc_concept_matrix[which(doc_concept_matrix[x,] >= concept_threshold)])) 
concepts <- lapply(1:length(concepts), function(x) concepts[[x]][!concepts[[x]] %in% c("doc_id", "doc_text")])

## create a results table with the document ID, primary concept (i.e. the concept with the highest concept score), and a delimited list of the concepts above the threshold
concept_sum <- data.frame(doc_id = doc_concept_matrix$doc_id, doc_text = doc_concept_matrix$doc_text, primary_concept = sapply(1:nrow(doc_concept_matrix), function(x) paste(colnames(doc_concept_matrix)[which(doc_concept_matrix[x,3:ncol(doc_concept_matrix)] == max(doc_concept_matrix[x,3:ncol(doc_concept_matrix)])) + 2], collapse = ";")), all_concepts = sapply(concepts, paste, collapse = ";"))
concept_sum$all_concepts[concept_sum$all_concepts == ""] <- "unclassified"

if (unclassified == FALSE) {
	## create a final concept list for each document that includes the unique set of the primary and all secondary concepts
	## necessary because some documents don't have enough text to meet the threshold for any concept, so they end up being assigned to whichever concept score is highest
	fin_concepts <- sapply(1:nrow(concept_sum), function(x) unique(c(unlist(strsplit(concept_sum[x,3], ";")), unlist(strsplit(concept_sum[x,4], ";")))))
	fin_concepts <- lapply(fin_concepts, sort)
	## merge the results back into the concept_sum table
	concept_sum$all_concepts <- sapply(fin_concepts, paste, collapse = ";")
	concept_sum$all_concepts <- gsub(";unclassified", "", concept_sum$all_concepts)
	concept_sum$all_concepts <- gsub("unclassified;", "", concept_sum$all_concepts)
	## isolate docs that have no term matches in the term_csv table and set them to 'unclassified'
	concept_sum$primary_concept[sapply(1:nrow(doc_concept_matrix), function(x) sum(doc_concept_matrix[x,3:ncol(doc_concept_matrix)])) == 0] <- "unclassified"
	concept_sum$all_concepts[concept_sum$primary_concept == "unclassified"] <- "unclassified"
}
else {
	concept_sum$highest_score <- concept_sum$primary_concept
	concept_sum$primary_concept[concept_sum$all_concepts == "unclassified"] <- "unclassified"
	concept_sum$all_concepts <- gsub(";unclassified", "", concept_sum$all_concepts)
	concept_sum$all_concepts <- gsub("unclassified;", "", concept_sum$all_concepts)
}
return(concept_sum)
}

expand_abbreviations <- function(doc_id_column, doc_text_column, return_abbrs = FALSE) {
	doc_df <- data.frame(id_column = doc_id_column, text_column = doc_text_column)
	abbrs <- stringr::str_extract_all(doc_df$text_column, "\\([A-Z]{2,8}\\)|\\([a-z][A-Z]{2,8}\\)")
	abbrs <- data.frame(id_column = doc_df$id_column, abbrev = sapply(abbrs, paste, collapse = ";"))
	abbrs$abbrev <- gsub("\\(|\\)", "", abbrs$abbrev)
	abbrs <- tidyr::separate_rows(abbrs, abbrev, sep = ";")
	abbrs$start_char <- strtrim(abbrs$abbrev, 1)
	abbrs$start_char <- paste0(abbrs$start_char, tolower(abbrs$start_char))
	abbrs$abbr_len <- nchar(abbrs$abbrev)
	abbrs <- abbrs[!abbrs$abbrev == "",]
	#abbrs$meanings <- sapply(1:nrow(abbrs), function(x) stringr::str_extract(doc_df$text_column[doc_df$id_column == abbrs$id_column[x]], paste0("\\b[", abbrs$start_char[x], "].{1,50}", "\\(", abbrs$abbrev[x], "\\)")))
	abbrs$meanings <- sapply(1:nrow(abbrs), function(x) stringr::str_extract(doc_df$text_column[doc_df$id_column == abbrs$id_column[x]], paste0("\\b[", abbrs$start_char[x], "]\\S+\\s(?:\\S+\\s){0,", abbrs$abbr_len[x], "}", "\\(", abbrs$abbrev[x], "\\)")))
	abbrs$meanings <- gsub(" \\(.+", "", abbrs$meanings)
	abbrs <- abbrs[!is.na(abbrs$meanings),]
	for (i in 1:nrow(abbrs)) {
	  doc_df$text_column[doc_df$id_column == abbrs$id_column[i]] <- gsub(abbrs$abbrev[i], abbrs$meanings[i], doc_df$text_column[doc_df$id_column == abbrs$id_column[i]])
	}
	if (return_abbrs == TRUE) {
		doc_list <- list(doc_df, abbrs)
		names(doc_list) <- c("doc_df", "abbreviations")
		doc_list$abbreviations$start_char <- NULL
		return(doc_list)
	}
	else {
		return(doc_df$text_column)
	}
} ## problem acronyms: Centers for Disease Control and Prevention (CDC) ; men who have sex with men (MSM)

create_doc_matrix <- function(docs, max_ngram = 1, max_vocab_prop = 0.001) {
	docs$dtext <- expand_abbreviations(docs$doc_id, docs$dtext)
	docs$dtext <- gsub("Project Summary/Abstract", "", docs$dtext, ignore.case = TRUE)
	docs$dtext <- gsub("Project Abstract", "", docs$dtext, ignore.case = TRUE)
	docs$dtext <- gsub("DESCRIPTION \\(provided by applicant\\):", "", docs$dtext, ignore.case = TRUE)
	docs$dtext <- gsub("Project Summary", "", docs$dtext, ignore.case = TRUE)
	docs$dtext <- gsub("Abstract", "", docs$dtext, ignore.case = TRUE)
	docs$dtext <- tm::stripWhitespace(docs$dtext)
	mystopwords <- scan("R/Functions/stopwords.txt", what = "varchar", skip = 1)
	docs$dtext <- gsub("-", " ", docs$dtext)
	docs$dtext <- gsub("<.+?>", " ", iconv(docs$dtext, to = "ASCII", sub = " "))
	docs$dtext <- removePunctuation(docs$dtext)
	docs$dtext <- tolower(docs$dtext)
	docs$dtext <- removeWords(docs$dtext, stopwords("English"))
	docs$dtext <- removeWords(docs$dtext, mystopwords)
	docs$dtext <- removeNumbers(docs$dtext)
	docs$dtext <- stemDocument(docs$dtext)
	docs$dtext <- stripWhitespace(docs$dtext)
	it <- itoken(docs$dtext, ids = docs$doc_id)
	v <- create_vocabulary(it, ngram = c(1, max_ngram))
	v <- prune_vocabulary(v, doc_proportion_min = max_vocab_prop)
	vectorizer <- vocab_vectorizer(v)
	dtm <- create_dtm(it, vectorizer, type = "dgTMatrix")
	return(dtm)
}