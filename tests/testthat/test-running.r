test_that("run_app launches correctly", {
  skip_on_cran()

  app <- AppDriver$new(
    app = microecoshiny::run_app,
    variant = "desktop",
    name = "app_launch",
    width = 1280,
    height = 720
  )

  # Check that the app loads
  app$expect_values(output = TRUE, timeout = 30000)

  # Verify sidebar menu is present
  app$expect_text(selector = ".sidebar-menu", pattern = "Import|Preprocess|Composition")

  app$stop()
})
