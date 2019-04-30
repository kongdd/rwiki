#' write_pdf
#'
#' Save to pdf for wikipedia page
#'
#' @param outdir If outdir is null, will save pdfs into the directory of book title.
#' @param overwrite Whether overwrite existing pdf?
#'
#' @examples
#' url <- "https://en.wikipedia.org/wiki/Degrees_of_freedom_%28statistics%29"
#' write_pdf(url)
#' @importFrom stringr str_extract
#' @export
write_pdf <- function(url, outfile = NULL, outdir = ".", overwrite = FALSE){
    if (is.na(url)) return()
    if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

    url_root <- str_extract(url, ".*org")
    # pdflink <- paste0(url_root, "/api/rest_v1/page/pdf/", basename(url))

    # check outfile
    if (is.null(outfile)) {
        outfile <- sprintf("%s/%s.pdf", outdir, fix_encoding(basename(url)))
    } else {
        outfile <- sprintf("%s/%s.pdf", outdir, basename(outfile))
    }

    ## check similar file
    # if file already exist, just rename it
    title <- gsub("(\\d.\\d|\\d.|\\d)[ ]|.pdf", "", basename(outfile)) %>%
        gsub("\\(", "\\\\(", .) %>%
        gsub("\\)", "\\\\)", .)
    pattern <- sprintf("((\\d\\.\\d|\\d.|\\d)[ ]*)(%s|%s).pdf",
                       title, gsub("[ ]+", "_", title))

    file_pdf <- dir(outdir, pattern, full.names = TRUE)[1]
    if (!is.na(file_pdf) && outfile != file_pdf) {
        file.rename(file_pdf, outfile)
    }

    ## main
    file_size <- file.size(outfile)
    tmpfile <- "temp.pdf"
    if (is.na(file_size) || file_size <= 1024*16 || overwrite) {
        overwrite = TRUE

        cat(sprintf("process %s | %s ... \n", url, basename(outfile)))
        p1 <- POST(sprintf("%s/wiki/Special:ElectronPdf", url_root),
                   body = list(action="redirect-to-electron",
                               page=basename(url) %>% fix_encoding()),
                   encode = "form",
                   write_disk(tmpfile, overwrite = overwrite),
                   progress())

        # download.file(url, outfile, mode="wb")
        file.rename(tmpfile, outfile)
        # p <- GET(url,
        #          write_disk(outfile, overwrite = overwrite),
        #          progress())
    } else {
        cat(sprintf("[ok] file already exist: %s ... \n", url %>% fix_encoding()))
    }

    # rm tmpfile
    on.exit({ if (file.exists(tmpfile)) file.remove(tmpfile) })
}
