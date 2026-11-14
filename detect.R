# ---------------------------------------------------------------------------
# detect.R : language detection based on the lexicon
# ---------------------------------------------------------------------------

#' Detect the language of a sentence
#'
#' For each language of the lexicon, computes the share of the sentence's
#' words found in that language's column. The language with the highest score
#' is returned.
#'
#' @param sentence A single character string.
#' @param lexicon A data frame with one column per language.
#' @param languages Names of the columns to compare. By default, all columns
#'   of `lexicon`.
#' @param return_scores If `TRUE`, return the named vector of scores
#'   (between 0 and 1) instead of the detected language. Default `FALSE`.
#'
#' @return The name of the detected language, `"unknown"` if no word is
#'   recognised, or a named numeric vector if `return_scores = TRUE`.
#'
#' @details The detection only knows the words of the lexicon: the larger the
#'   lexicon, the more reliable it is. Ties are resolved in favour of the
#'   first language listed.
#'
#' @examples
#' lexicon <- data.frame(
#'   french = c("bonjour", "merci"),
#'   bassa  = c("mboo", "bikak")
#' )
#' detect_language("mboo bikak", lexicon)
#' detect_language("bonjour", lexicon, return_scores = TRUE)
#'
#' @export
detect_language <- function(sentence, lexicon, languages = NULL,
                            return_scores = FALSE) {
  if (!is.data.frame(lexicon)) {
    stop("`lexicon` must be a data frame.", call. = FALSE)
  }
  if (is.null(languages)) languages <- names(lexicon)
  check_lexicon(lexicon, languages)

  words <- split_words(sentence)
  if (length(words) == 0) {
    if (return_scores) {
      return(structure(rep(0, length(languages)), names = languages))
    }
    return("unknown")
  }

  scores <- vapply(languages, function(lang) {
    vocab <- tolower(as.character(lexicon[[lang]]))
    vocab <- vocab[!is.na(vocab) & vocab != ""]
    mean(words %in% vocab)
  }, numeric(1))

  if (return_scores) return(scores)
  if (max(scores) == 0) return("unknown")
  names(scores)[which.max(scores)]
}

#' Detect the source language, then translate
#'
#' Combines [detect_language()] and [translate_text()]: the source language
#' is detected automatically before translating into `to`.
#'
#' @param sentence A single character string.
#' @param lexicon A data frame with one column per language.
#' @param to Name of the target-language column. Default `"bassa"`.
#' @param fuzzy If `TRUE`, use [translate_fuzzy()] to tolerate spelling
#'   mistakes. Default `FALSE`.
#' @param ... Further arguments passed to [translate_text()] or
#'   [translate_fuzzy()] (for example `unknown` or `max_dist`).
#'
#' @return A character string: the translation, or a message if the language
#'   is not recognised or is already the target language.
#'
#' @examples
#' lexicon <- data.frame(
#'   french = c("bonjour", "merci"),
#'   bassa  = c("mboo", "bikak")
#' )
#' smart_translate("bonjour merci", lexicon, to = "bassa")
#' smart_translate("mboo bikak", lexicon, to = "french")
#'
#' @export
smart_translate <- function(sentence, lexicon, to = "bassa", fuzzy = FALSE, ...) {
  check_lexicon(lexicon, to)
  from <- detect_language(sentence, lexicon)

  if (from == "unknown") return("Langue non reconnue")
  if (from == to)        return(paste("Le texte est d\u00e9j\u00e0 en", to))

  if (fuzzy) {
    translate_fuzzy(sentence, lexicon, from = from, to = to, ...)
  } else {
    translate_text(sentence, lexicon, from = from, to = to, ...)
  }
}
