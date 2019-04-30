context("test-wikibook")

test_that("wikibook works", {
    expect_true({
        url  <- "https://en.wikipedia.org/wiki/User:Kongdd/Books/%E7%BB%9F%E8%AE%A1%E5%AD%A6%E5%9F%BA%E7%A1%80"
        info <- wikibook(url)
        TRUE
    })

    expect_true({
        url  <- "https://en.wikipedia.org/wiki/User:Kongdd/Books/%E7%BB%9F%E8%AE%A1%E5%AD%A6%E5%9F%BA%E7%A1%80"
        info <- wikibook(url, lang = "zh")
        TRUE
    })
})
