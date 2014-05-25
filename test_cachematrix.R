library(testthat)
# I don't know how to make proper packages yet, so I'll hack with `source` for
# now.
#library(cachematrix)
source("cachematrix.R")

# Pre-computed results
setup_fixtures <- function() {
    xvect <- c(2,3,0,3,0,2,2,1,1)
    xm <- matrix(xvect,3,3)
    xvect_inv <- c(2,3,-6,-1,-2,4,-3,-4,9)
    xm_inv <- matrix(xvect_inv,3,3)

    xvect2 <- xvect
    xvect2[1] <- 1
    xm2 <- matrix(xvect2,3,3)
    xvect2_inv <- c(-2,-3,6,1,1,-2,3,5,-9)
    xm2_inv <- matrix(xvect2_inv,3,3)

    list(xm=xm, xm_inv=xm_inv, xm2=xm2, xm2_inv=xm2_inv)
}

test_that("makeCacheMatrix makes a matrix-ish", {
    fix <- setup_fixtures()

    cm <- makeCacheMatrix(fix$xm)

    expect_that(cm$get(), equals(fix$xm))
})

test_that("cacheSolve produces inverted matrix", {
    fix <- setup_fixtures()

    cm <- makeCacheMatrix(fix$xm)
    cm_inv <- cacheSolve(cm)

    expect_that(cm_inv, equals(fix$xm_inv))

})

test_that("cacheSolve uses cache", {
    fix <- setup_fixtures()

    cm <- makeCacheMatrix(fix$xm)
    cm_inv <- cacheSolve(cm)

    expect_that(cacheSolve(cm), shows_message("getting cached data"))
})

test_that("set invalidates cache", {
    fix <- setup_fixtures()

    cm <- makeCacheMatrix(fix$xm)
    cm_inv <- cacheSolve(cm)

    expect_that(cm$set(fix$xm2), shows_message("invalidating cache"))
    cm_inv2 <- cacheSolve(cm)

    expect_that(cm_inv2, not(equals(cm_inv)))
    expect_that(cm_inv2, equals(fix$xm2_inv))
})
