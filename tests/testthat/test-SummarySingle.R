context("SummarySingle")

test_that("Size of data", {
    data(input)
    res <- SummarySingle(data=input, trt = "Arm", var = "OS")
    
    expect_equal(as.numeric(res["N", "ITT(CTRL)"]) + as.numeric(res["N", "ITT(TRT)"]), nrow(input))
})


