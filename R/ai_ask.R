#' Ask a question to a chatbot
#'
#' The question is sent to the chatbot server, and the answer is returned. The
#' default chatbot server is ollama running locally, and the default model is
#' codestral:latest. It was also tested with mistral:7b-instruct-v0.2-q6_K for
#' an even smaller model.
#'
#' @param question A character string with the question to ask.
#' @param context An R object used as context (usually a data frame). This is
#'   not used yet, but it should be implemented in the future.
#' @param max_tokens The maximum number of tokens to return in the answer. By
#'   default, it is 1000.
#' @param lang The language to use for the answer. Default is "en". You can also
#'   use "fr" for instance.
#' @param url The URL of the chatbot server. Default is
#'   http://localhost:11434/api/chat
#' @param model The LLM (large language model) to use. Default is codestral.
#'   Make sure you complies to its license
#'   (see https://mistral.ai/news/mistral-ai-non-production-license-mnpl/), or
#'   switch to another model that better suits your requirements.
#' @param api_key The API key to use for connecting to the chatbot server
#'   (optional, see your server administrator).
#' @param verbose Should more information be printed? `FALSE` by default.
#'
#' @return
#' The answer is returned invisibly. The function is used for its side-effect of
#' displaying the chatbot help page with the question, answer and examples
#' @export
#'
#' @examples
#' \dontrun{
#'
#'
#' # Basic questions
#' ai_ask("Who are you?")
#' ai_ask("What is a chatbot?")
#' ai_ask("Qui es-tu ?")
#' ai_ask("Qu'est-ce que R ?")
#' ai_ask("Qu'est-ce que RStudio ?")
#' ai_ask("What is GitHub?")
#' ai_ask("Qu'est-ce que le R Markdown ?")
#' ai_ask("What is data science?")
#'
#' # Inappropriate questions
#' ai_ask("Qu'est ce qu'un Acanthurus sp ?")
#' ai_ask("Raconte-moi une bonne blague.")
#' ai_ask("Va te faire voir !")
#'
#' # Now, more complex questions
#' ai_ask("Comment filtrer un data frame en R?")
#' ai_ask("Write R code to filter a data frame.")
#' ai_ask("Que fait AIC()? Donne un exemple.")
#' ai_ask("Qu'est ce que l'hétéroscédasticite et comment la détecter dans une ANOVA à un facteur ?")
#' ai_ask("How to determine which model is better using an ANOVA for nested linear models?")
#'
#' # Explain terms
#' ai_explain_term("True positive")
#' ai_explain_term("percentile", lang = "fr")
#' ai_explain_term("git push")
#' ai_explain_term("Quarto", lang = "fr")
#' ai_explain_term("boite à moustaches") # Language mismatch
#' ai_explain_term("boites à moustaches parallèles", lang = "fr")
#'
#' # Explain R functions
#' ai_explain_function("mean")
#' ai_explain_function("fmean", lang = "fr")
#' ai_explain_function("collapse::fmean", lang = "fr")
#' ai_explain_function("glm", package = "stats", lang = "fr")
#' ai_explain_function("replace_na", "tidyr", lang = "fr") # collapse::replace_na() used instead!
#' try(ai_explain_function("nonexistingfunction")) # Error
#' try(ai_explain_function("apropos", package = "stats")) # Wrong package
#' try(ai_explain_function("apropos", package = "unknownpkg")) # Unknown package
#'
#' # Explain R code
#' ai_explain_code("y <- c(1, 5, 7, NA, -Inf, 8)")
#' ai_explain_code(r"-[
#' mtcars |>
#'   filter(cyl == 4) |>
#'   summarise(mean_hp = mean(hp), median_disp = median(disp))]-")
#' ai_explain_code(r"-[
#'   mtcars %>.%
#'     sfilter(., cyl == 4) %>.%
#'     ssummarise(., mean_hp = fmean(hp), median_disp = fmedian(disp))
#' ]-")
#' ai_explain_code(r"-[
#'   chart(data = trees, Volume ~ Girth) +
#'     geom_point() +
#'     geom_smooth()
#' ]-")
#' ai_explain_error(error = "longer object length is not a multiple of shorter object length")
#' ai_explain_error(error = "Error: object 'mydata' not found")
#' ai_explain_error(error = "Error in lenght(1:10) : could not find function \"lenght\"", lang = "fr")
#' ai_explain_error(code = r"-[y <- c(1, 5, 7, NA, -Inf, 8, )]-", lang = "fr")
#' ai_explain_error(code = r"-[trees %>.% filter(Girth > 10))]-", lang = "fr")
#' ai_explain_error(code = r"-[
#' urchin <- read("urchin", package = "data.io")
#'   ]-", error = r"-[
#' Error in read("urchin", package = "data.io") :
#'   dataset 'urchin' not found in package 'data.io'
#'   ]-", lang = "fr")
#' }
ai_ask <- function(question, context = NULL,
    max_tokens = getOption("SciViews.chatbot.max_tokens",
      Sys.getenv("SCIVIEWS_CHATBOT_MAX_TOKENS", 1000L)),
    lang = getOption("data.io_lang", "en"),
    url = getOption("SciViews.chatbot.url",
      Sys.getenv("SCIVIEWS_CHATBOT_URL",
        "http://localhost:11434/api/chat")), # Default is using Ollama locally
    model = getOption("SciViews.chatbot.model",
      Sys.getenv("SCIVIEWS_CHATBOT_MODEL",
      "codestral:latest")), # Default is using Codestral
      #"mistral:7b-instruct-v0.2-q6_K"), # For Mistral 7B
    api_key = Sys.getenv("CONNECT_API_KEY", ""),
    verbose = FALSE) {

  # We use three help pages and alternate between these pages...
  max_page <- 3L
  last_page <- getOption("SciViews.chatbot.page", max_page)
  page <- last_page + 1L
  if (page > max_page)
    page <- 1L
  chat_page <- paste0("SciViews.chatbot.page", page)

  question <- paste(question, collapse = "\n")
  # In case there are % signs, we need to escape them for a Rd file
  question2 <- gsub("%", "\\%", question, fixed = TRUE)

  # First clean up the corresponding section
  chat_data <- list(question = "", context = "", answer = "", examples = "")

  # Check if the question is the same as the previous one
  last_data <- .get_temp(paste0("SciViews.chatbot.page", last_page), chat_data)
  if (last_data$question == question2) {
    # Reuse the previous answer
    page <- last_page
  } else {
    # Update chat_data
    chat_data$question <- question2
    .assign_temp(chat_page, chat_data)

    # TODO: build context
    #chat_data$context = ...

    if (tolower(lang) == "fr")
      question <- paste(question, "R\u00e9pond en fran\u00e7ais", sep = "\n")

    # Allow connecting with API key (for Posit Connect)
    if (api_key != "") {
      request <- httr2::request(url) |>
        httr2::req_headers("Accept" = "application/json",
                           "Authorization" = paste("Key", api_key))
    } else {# No API key
      request <- httr2::request(url) |>
        httr2::req_headers("Accept" = "application/json")
    }
    request <- request |>
      httr2::req_body_json(list(
        model = model,
        messages = list(
          list(role = "system", content = "<s>[INST]I am SciViews chatbot, a large language model. I am trained to answer questions and provide information. My answers are about about the R language, statistics and data science. I answer with precision. I stop answering if I am not sure to tell the truth. The transcript uses Markdown tags. R code is between ```R and ``` tags. Outputs are between ``` and ```. I explain code. I give short examples. My code is executable and therefore, I construct a small example dataset or I use existing R examples in the datasets package. I use ggplot() preferrably to plot(). I use the base R pipe operator (|>) instead of the magrittr's pipe (%>%). The chart() function is equivalent to ggplot(), but it is more powerful because it can use variable labels automatically. Otherwise, chart() can generate exactly the same plots as ggplot() using the data= and mapping= arguments. The tabularise() function creates nice tables for various objects, for instance, tabularise(iris). It automatically uses variable labels. I avoid as much as possible tu use library(). Instead of library(car); Anova(), I prefer car::Anova(). For example datasets, I use something like data('my_data', package = 'datasets') instead of library(datasets); data(my_data) with my_data being an existing dataset in the datasets R package. For an explanation of an R function, I details its first few (usually two or three) main arguments and I give a brief, executable example. For an explanation of R functions, if the function name is \"fun\" for instance, I also indicate at the end: \"For more details, see the help page of the function fun() by typing ?fun  at the R Console\". If the question is in French, I also answer in French.[/INST]"),
          list(role = "user", content = question)
        ),
        `tool-choice` = "none",
        `response-format` = "text",
        max_tokens = max_tokens,
        stream = FALSE,
        raw = TRUE,
        stop = c("[/ANS]", "[/SOL]", "[/INST]")
      ))
    .assign_temp("SciViews.chatbot.last_request", request)
    if (isTRUE(verbose)) {
      message("Request:")
      print(request)
    }

    # Perform the request
    res <- try(httr2::req_perform(request), silent = TRUE)
    if (inherits(res, 'try-error')) {
      stop(gettextf("Error while performing the request to the chatbot server: %s. Check connexion parameters and verify the chatbot server is running.", res))
    }
    .assign_temp(".SciViews.chatbot_last_result", res)
    if (isTRUE(verbose)) {
      message("Result:")
      print(res)
    }

    # Extract message
    body <- httr2::resp_body_json(res)
    answer <- body$message$content
    if (isTRUE(verbose)) {
      message("Answer:")
      print(body)
    }
    # Use Roxygen2 to transform Markdown tags to Rd tags
    markdown_on <- getNamespace("roxygen2")$markdown_on
    old_mkdown <- markdown_on()
    markdown_on(TRUE)
    tagged <- roxygen2::roxy_tag("description", answer) |>
      roxygen2::tag_markdown()
    chat_data$answer <- tagged$val

    markdown_on(old_mkdown)

    # Extract examples
    examples <- gsub("Output ?:\n+```[Rr]\n", "Output:\n\n```\n", answer)
    examples <- strsplit(examples, "```", fixed = TRUE)[[1]]
    examples <- examples[grepl("^[Rr]\n", examples)]
    examples <- sub("^[Rr]\n", "", examples)
    examples <- paste(examples, collapse = "\n")
    chat_data$examples <- examples
    .assign_temp(chat_page, chat_data)
  }

  # Record the page number
  options(SciViews.chatbot.page = page)

  # Display the chatbot help page with question, answer and examples inserted
  print(help(paste0("chatbot", page), package = "SciViews"))

  # Return the answer invisibly
  invisible(chat_data$answer)
}

#ai_ask("How to decide which model is better for nested models using an ANOVA? Do not use the aov1 package.")

#ai_ask("Explain subsetting in R in simple terms.")

#ai_ask("Can I use R^2 to compare models of different conplexities?")

#ai_ask("Which metric is better for species x stations datasets?")

#' @export
#' @rdname ai_ask
#' @param term The term to describe.
#' @param ... Further arguments passed to [SciViews::ai_ask()].
ai_explain_term <- function(term, lang = getOption("data.io_lang", "en"), ...) {
  if (!is.character(term) || length(term) != 1)
    stop("term must be a single character string")
  # TODO: allow for more different languages
  if (lang == "fr") {
    ai_ask(paste("Explique", term, "en termes simples avec un exemple."),
      lang = lang, ...)
  } else {# Use English by default
    ai_ask(paste("Explain", term, "in simple terms with an example."),
      lang = lang, ...)
  }
}

#' @export
#' @rdname ai_ask
#' @param fun The R function to explain.
#' @param package The R package that provides the function.
ai_explain_function <- function(fun, package = NULL,
    lang = getOption("data.io_lang", "en"), ...) {
  if (!is.character(fun) || length(fun) != 1)
    stop("fun must be a single character string with the name of an R function")

  # In case we have pkg::fun, split it
  if (grepl("^[^:]+::[^:]+$", fun)) {
    package <- sub("^([^:]+)::[^:]+$", "\\1", fun)
    fun <- sub("^[^:]+::([^:]+)$", "\\1", fun)
  }

  # Check if the function exists and is available in this R installation
  if (!is.null(package)) {
    if (!is.character(package) || length(package) != 1)
      stop("package must be a single character string")
    res <- try(is.function(getFromNamespace(fun, package)), silent = TRUE)
    if (inherits(res, 'try-error') || !isTRUE(res))
      stop(gettextf("The function %s is not available in the package %s in your current R installation.", fun, package))
    # TODO: allow for more different languages
    if (lang == "fr") {
      ai_ask(paste("Explique la fonction", fun, "du package", package),
        lang = lang, ...)
    } else {# Use English by default
      ai_ask(paste("Explain the function", fun, "from the package", package),
        lang = lang, ...)
    }

  } else {# package not provided
    if (is.null(get0(fun, mode = 'function')))
      stop(gettextf("The function %s is not available in your R process. Either specify the package it comes from or load it first.", fun))
    # TODO: allow for more different languages
    if (lang == "fr") {
      ai_ask(paste("Explique la fonction", fun),
        lang = lang, ...)
    } else {# Use English by default
      ai_ask(paste("Explain the function", fun),
        lang = lang, ...)
    }
  }
}

#' @export
#' @rdname ai_ask
#' @param code A small chunk of R code to explain.
ai_explain_code <- function(code, lang = getOption("data.io_lang", "en"), ...) {
  if (!is.character(code))
    stop("code must be a character string")
  code <- paste(code, collapse = '\n')
  # TODO: allow for more different languages
  if (lang == "fr") {
    ai_ask(paste("Explique le code R suivant :\n\n\\code{", code, "}"),
      lang = lang, ...)
  } else {# Use English by default
    ai_ask(paste("Explain the following R code:\n\n\\code{", code, "}"),
      lang = lang, ...)
  }
}

#' @export
#' @rdname ai_ask
#' @param error The error message that R returns.
ai_explain_error <- function(code = NULL, error = NULL,
    lang = getOption("data.io_lang", "en"), ...) {
  if (!is.null(code)) {
    if (!is.character(code))
      stop("code must be a character string")
    code <- paste(code, collapse = '\n')
  }

  if (is.null(error)) {
    # TODO: allow for more different languages
    if (lang == "fr") {
      ai_ask(paste("Quelle est l'erreur dans ce code R ?\n\n\\code{", code, "}"),
        lang = lang, ...)
    } else {# Use English by default
      ai_ask(paste("What is the error in this R code?\n\n\\code{", code, "}"),
        lang = lang, ...)
    }
  } else {# Error message provided
    if (!is.character(error))
      stop("error must be a character string")
    error <- paste(error, collapse = '\n')
    if (is.null(code)) {
      if (lang == "fr") {
        ai_ask(paste("Explique ce message d'erreur renvoy\u00e9 par R:\n\n\\bold{",
          error, "}"), lang = lang, ...)
      } else {# Use English by default
        ai_ask(paste("Explain this error message returned by R:\n\n\\bold{",
          error, "}"), lang = lang, ...)
      }
    } else {# Both code and error provided
      # TODO: allow for more different languages
      if (lang == "fr") {
        ai_ask(paste("Explique ce message d'erreur de R :\n\n\\bold{", error,
          "} lorsque j'ex\u00e9cute ce code:\\code{", code, "}"),
          lang = lang, ...)
      } else {# Use English by default
        ai_ask(paste("Explain this error message in R:\n\n\\bold{", error,
          "} when I run this code:\\code{", code, "}"),
          lang = lang, ...)
      }
    }
  }
}


#' SciViews chatbot
#'
#' @description
#' *A chatbot does not always provide reliable results. Take this with a grain of salt!*
#'
#' Question: \Sexpr[eval=TRUE,stage=render,results=rd]{ if (exists('SciViews.chatbot.page1')) SciViews.chatbot.page1$question else "No question" }
#'
#' @section Answer:
#' \Sexpr[eval=TRUE,stage=render,results=rd]{ if (exists('SciViews.chatbot.page1')) SciViews.chatbot.page1$answer else "No answer" }
#'
#' @name chatbot1
#' @examples
#' \Sexpr[eval=TRUE,stage=render,results=text]{ if (exists('SciViews.chatbot.page1')) SciViews.chatbot.page1$examples else "# No example" }
NULL

# @section Test:
# This is a test...
# \Sexpr[eval=TRUE,stage=render,results=rd]{ "Test \\\\emph{italic}" }
# \ifelse{latex}{\out{$\alpha$}}{\ifelse{html}{\out{<a href="../html/chatbot1.html" onclick="window.alert(\"test\"); window.parent.helpNavigate(this ref); return false">link</a>}}{alpha}}

# Test of runnable links: \Sexpr[eval=TRUE,stage=render,results=rd]{ cli::ansi_has_hyperlink_support() }
#
# TODO: render differently in text of pdf
# \Sexpr[eval=TRUE,stage=render,results=verbatim]{ '<a href="../chatbot1" onclick="window.parent.helpNavigate(this.href); return false">Test link injection</a>' }

# cli::cli_text("{.run [Click here](rstudio:run:rlang::last_trace())}")
# #' \href{"../chatbot1" onclick="window.parent.helpNavigate(this ref); return false"}{Click here to see the answer in the help page}
# Show md of html file in fenêtre (lite) with curl http://127.0.0.1:23489/open\?url\=<path>

#' SciViews chatbot
#'
#' @description
#' *A chatbot does not always provide reliable results. Take this with a grain of salt!*
#'
#' Question: \Sexpr[eval=TRUE,stage=render,results=rd]{ if (exists('SciViews.chatbot.page2')) SciViews.chatbot.page2$question else "No question" }
#'
#' @section Answer:
#' \Sexpr[eval=TRUE,stage=render,results=rd]{ if (exists('SciViews.chatbot.page2')) SciViews.chatbot.page2$answer else "No answer" }
#'
#' @name chatbot2
#' @examples
#' \Sexpr[eval=TRUE,stage=render,results=text]{ if (exists('SciViews.chatbot.page2')) SciViews.chatbot.page2$examples else "# No example" }
NULL

#' SciViews chatbot
#'
#' @description
#' *A chatbot does not always provide reliable results. Take this with a grain of salt!*
#'
#' Question: \Sexpr[eval=TRUE,stage=render,results=rd]{ if (exists('SciViews.chatbot.page3')) SciViews.chatbot.page3$question else "No question" }
#'
#' @section Answer:
#' \Sexpr[eval=TRUE,stage=render,results=rd]{ if (exists('SciViews.chatbot.page3')) SciViews.chatbot.page3$answer else "No answer" }
#'
#' @name chatbot3
#' @examples
#' \Sexpr[eval=TRUE,stage=render,results=text]{ if (exists('SciViews.chatbot.page3')) SciViews.chatbot.page3$examples else "# No example" }
NULL
