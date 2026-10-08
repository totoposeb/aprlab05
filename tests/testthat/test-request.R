# request_api() and request_sprite(): wrong input and failed requests

test_that("request_api rejects a wrong path", {
  expect_error(request_api(25))
})

test_that("request_api explains a name that does not exist", {
  httr2::local_mocked_responses(
    function(req) httr2::response_json(status_code = 404)
  )

  expect_error(request_api("pokemon/nope"), "not found")
})

test_that("request_api stops on any other failed status", {
  httr2::local_mocked_responses(
    function(req) httr2::response_json(status_code = 500)
  )

  expect_error(request_api("pokemon/pikachu"), "500")
})

test_that("request_api returns the answer as a list", {
  httr2::local_mocked_responses(fake_api)
  json <- request_api("pokemon/anything")

  expect_equal(json$name, "fuecoco")
})

test_that("request_sprite stops when the picture is not found", {
  httr2::local_mocked_responses(
    function(req) httr2::response_json(status_code = 404)
  )

  expect_error(request_sprite("https://example.com/x.png"), "404")
})
