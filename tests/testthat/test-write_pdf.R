context("test-write_pdf")
data("book")
# Sys.setenv(https_proxy="http://127.0.0.1:1080")

test_that("write_pdf works", {
    url <- book$origin[!is.na(url), ]$url[1]
    
    expect_true({
        url <- book$origin[!is.na(url), ]$url[1]
        write_pdf(url, overwrite = TRUE)
        TRUE
    })

    expect_true({
        url <- book$lang[!is.na(url), ]$url[1]
        write_pdf(url, overwrite = TRUE)
        TRUE
    })
})
