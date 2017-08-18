context("SummaryVars")

test_that("Size of data", {
    data(input)
    res <- SummaryVars(data=input, trt='Arm', subgroup='BEP', var=c('Age','Sex'), var.class=c("numeric","categorical"))

    expect_equal(as.numeric(res["N", "All(CTRL)"]) + as.numeric(res["N", "All(TRT)"]), nrow(input))
})
