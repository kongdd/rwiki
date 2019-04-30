# Sys.setenv(http_proxy="http://127.0.0.1:1080")

# Fix chinese encoding
fix_encoding <- function(x) {
    map_chr(x, function(str){
        URLdecode(str) %>% iconv("utf-8", "gbk")
    })
}

find_other_lang <- function(url, lang = "zh"){
    p <- GET(url) %>% content()
    href <- xml_find_all(p, sprintf('//a[@lang="%s"]', lang)) %>%
        xml_attr("href")
    if (length(href) == 0) href <- NA_character_
    href
}

fprintf <- function(fmt, x) {
    cat(sprintf(fmt, x))
}

#' wikibook
#'
#' Retrieve wiki information
#'
#' @param url the url of wiki book
#' @param lang language
#'
#' @examples
#' url  <- "https://en.wikipedia.org/wiki/User:Kongdd/Books/%E7%BB%9F%E8%AE%A1%E5%AD%A6%E5%9F%BA%E7%A1%80"
#' info <- wikibook(url)
#'
#' outdir <- info$origin$title[1]
#' # plyr::m_ply(info$origin[, .(url = url, outfile = title)],
#' #             write_pdf, outdir = outdir)
#' # plyr::m_ply(info$lang[, .(url = url, outfile = title)],
#' #             write_pdf, outdir = outdir)
#' @importFrom purrr map map_int map_chr
#' @import plyr
#' @export
wikibook <- function(url, lang = "en", .progress="text"){
    url_root <- str_extract(url, ".*org")
    title <- basename(url) %>% fix_encoding() %>% paste0("wikibook_", ., "")

    p <- GET(url) %>% content(encoding = "utf-8")
    dl <- xml_find_all(p, "//dl") %>% xml_children()
    chapters <- xml_text(dl) %>% fix_encoding()
    tags <- html_name(dl)

    # L1
    I_1 <- which(tags == "dt")
    xs_1 <- chapters[I_1]

    I_2  <- seq_along(tags)[-I_1]
    L1   <- c(1, diff(I_2) != 1) %>% cumsum()
    href_org <- dl[I_2] %>% xml_find_all("a") %>% xml_attr("href") %>% paste0(url_root, .)
    href_lang <- NULL
    if (!is.null(lang) && !grepl(lang, url_root)) {
        fprintf('Retrieve other language: %s\n', lang)
        href_lang <- laply(href_org, find_other_lang, .progress = "text")
    }

    get_info <- function(hrefs = NULL) {
        if (is.null(hrefs)) return(NULL)

        info <- data.table(
            L1,
            title = basename(hrefs) %>% fix_encoding %>% gsub("_", " ", .),
            url = hrefs
        )[, .(L2 = as.character(1:.N),
              title = sprintf("%d.%s %s", L1, as.character(1:.N), title),
              url), .(L1)]
        info_0 <- data.table(L1 = 0, L2 = "", title, url = NA)
        info_1 <- data.table(L1 = seq_along(xs_1), L2 = "", title = xs_1, url = NA)
        info <- rbind(info_0, info_1, info) [order(L1, L2, na.last = FALSE)]
    }

    info <- get_info(href_org)
    info_lang <- get_info(href_lang)
    list(origin = info, lang = info_lang)
}
