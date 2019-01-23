context("Data applications")

test_that("GDP application is robust",
          {
            expect_identical(as.numeric(round(estimate.functional(Y=GDP$observation,X=GDP$forecast,stateVariable = GDP$forecast,model=probit_linear)$gmm$coefficients,digits=2)),c(-0.11,0.11))
            expect_identical(as.numeric(round(summary(estimate.functional(Y=GDP$observation,X=GDP$forecast,stateVariable = GDP$forecast,model=probit_linear))$Jtest$test[2],digits=2)),c(0.24))
          })
