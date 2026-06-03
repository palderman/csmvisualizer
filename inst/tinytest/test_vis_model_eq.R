library(tinytest)

model <-
  list(state_variables = list(
    X = structure("Lorenz X",
                  name = "X",
                  units = "unitless",
                  class = c("character",
                            "csm_variable", "csm_transform", "csm_state"),
                  equation = ~a * X + Y * Z),
    Y = structure("Lorenz Y",
                  name = "Y",
                  units = "unitless",
                  class = c("character",
                            "csm_variable", "csm_transform", "csm_state"),
                  equation = ~b * Y - Z),
    Z = structure("Lorenz Z",
                  name = "Z",
                  units = "unitless",
                  class = c("character",
                            "csm_variable", "csm_transform", "csm_state"),
                  equation = ~-X * Y + c * Y - Z)),
    parameters = list(
      a = structure("Lorenz a",
                    name = "a",
                    units = "unitless",
                    class = c("character", "csm_variable", "csm_parameter"),
                    lower_bound = -Inf, upper_bound = Inf),
      b = structure("Lorenz b",
                    name = "b",
                    units = "unitless",
                    class = c("character", "csm_variable", "csm_parameter"),
                    lower_bound = -Inf, upper_bound = Inf),
      c = structure("Lorenz c",
                    name = "c",
                    units = "unitless",
                    class = c("character", "csm_variable", "csm_parameter"),
                    lower_bound = -Inf, upper_bound = Inf)))

expect_error({
  csmvisualizer::vis_model_eq(model, "a")
})

expect_error({
  csmvisualizer::vis_model_eq(model, "test", X = 1, Y = 1, Z = 1, a = -8/3)
})

expect_equal_to_reference({
  csmvisualizer::vis_model_eq(model, "X", X = 1, Y = 1, Z = 1, a = -8/3)
}, file = "vis_model_eq_reference.rds")
