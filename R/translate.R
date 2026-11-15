# ---------------------------------------------------------------------------
# translate.R : word-by-word translation functions
# ---------------------------------------------------------------------------

#' Split a sentence into lowercase words
#'
#' Internal helper used by the translation and detection functions.
#'
#' @param sentence A single character string.
#' @return A character vector of lowercase words (empty strings removed).
#' @keywords internal
#' @noRd
split_words <- function(sentence) {
  if (!is.character(sentence) || length(sentence) != 1 || is.na(sentence)) {
    stop("`sentence` must be a single character string.", call. = FALSE)
  }
  words <- strsplit(tolower(sentence), "[^[:alpha:]'\u2019]+")[[1]]
  words[words != ""]
}

#' Check that a lexicon has the expected columns
#'
#' Internal helper.
#'
#' @param lexicon A data frame.
#' @param columns Character vector of required column names.
#' @return Invisibly `TRUE`, or an error.
#' @keywords internal
#' @noRd
check_lexicon <- function(lexicon, columns) {
  if (!is.data.frame(lexicon)) {
    stop("`lexicon` must be a data frame.", call. = FALSE)
  }
  missing_cols <- setdiff(columns, names(lexicon))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in `lexicon`: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

#' Translate a sentence word by word (exact match)
#'
#' Each word of the sentence is looked up in the lexicon. Words that are not
#' in the lexicon (or have an empty translation) are replaced by `unknown`.
#'
#' @param sentence A single character string to translate.
#' @param lexicon A data frame with one column per language, for example
#'   `french`, `bassa` and `bafang`.
#' @param from Name of the source-language column. Default `"french"`.
#' @param to Name of the target-language column. Default `"bassa"`.
#' @param unknown Text used for words that cannot be translated.
#'   Default `"[?]"`.
#'
#' @return A single character string containing the translation.
#'
#' @examples
#' lexicon <- data.frame(
#'   french = c("bonjour", "merci"),
#'   bassa  = c("mboo", "bikak")
#' )
#' translate_text("Bonjour merci", lexicon)
#' translate_text("bonjour pluie", lexicon)
#'
#' @export
translate_text <- function(sentence, lexicon, from = "french", to = "bassa",
                           unknown = "[?]") {
  check_lexicon(lexicon, c(from, to))
  words <- split_words(sentence)
  if (length(words) == 0) return("")

  index  <- match(words, tolower(lexicon[[from]]))
  result <- as.character(lexicon[[to]][index])
  result[is.na(result) | result == ""] <- unknown
  paste(result, collapse = " ")
}

#' Translate a sentence word by word, tolerating spelling mistakes
#'
#' Same as [translate_text()], but a word is matched to the closest entry of
#' the lexicon when its edit distance is at most `max_dist`.
#'
#' @inheritParams translate_text
#' @param max_dist Maximum number of character edits allowed between a word
#'   and a lexicon entry. Default `2`.
#'
#' @return A single character string containing the translation.
#'
#' @examples
#' lexicon <- data.frame(
#'   french = c("bonjour", "merci"),
#'   bassa  = c("mboo", "bikak")
#' )
#' translate_fuzzy("bonjoure merci", lexicon)
#'
#' @export
#' @importFrom stringdist amatch
translate_fuzzy <- function(sentence, lexicon, from = "french", to = "bassa",
                            max_dist = 2, unknown = "[?]") {
  check_lexicon(lexicon, c(from, to))
  words <- split_words(sentence)
  if (length(words) == 0) return("")

  index  <- stringdist::amatch(words, tolower(lexicon[[from]]),
                               maxDist = max_dist)
  result <- as.character(lexicon[[to]][index])
  result[is.na(result) | result == ""] <- unknown
  paste(result, collapse = " ")
}
