/*
 * This file uses the Catch unit testing library, alongside
 * testthat's simple bindings, to test a C++ function.
 *
 * For your own packages, ensure that your test files are
 * placed within the `src/` folder, and that you include
 * `LinkingTo: testthat` within your DESCRIPTION file.
 */

#include <testthat.h>
#include <Rcpp.h>
#include "Tag.hpp"

context("Test Tag class") {

	test_that("We can construct a Tag object") {
		Tag t("<metabolite>", "</metabolite>");
	}

}
