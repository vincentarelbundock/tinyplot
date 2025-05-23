source("helpers.R")
using("tinysnapshot")

## Avoid test fails on older R versions (pre 4.4.0) due to slight change in
## density grid value calculations.
## https://bugs.r-project.org/show_bug.cgi?id=18337
if (getRversion() < "4.4.0") exit_file("R < 4.4.0")

mtcars$am = as.factor(mtcars$am)

f = function() with(mtcars, tinyplot(density(mpg)))
expect_snapshot_plot(f, label = "density_nogroups")
# 
# f = function() with(mtcars, tinyplot(density(mpg), by = am))
# expect_snapshot_plot(f, label = "density_numeric")
# 
# f = function() with(iris, tinyplot(density(Sepal.Width), by = Species))
# expect_snapshot_plot(f, label = "density_factor")
# 
# f = function() with(iris, tinyplot(density(Sepal.Width), by = Species, bg = "by"))
# expect_snapshot_plot(f, label = "density_fill")
# 
# f = function() with(iris, tinyplot(density(Sepal.Width), by = Species, fill = "by"))
# expect_snapshot_plot(f, label = "density_fill")
# 
# f = function() with(iris, tinyplot(density(Sepal.Width), by = Species, type = "area"))
# expect_snapshot_plot(f, label = "density_fill")

## Now test `type = "density"` versions (both atomic and formula)
## Should be the same as above, modulo missing titles

f1 = function() with(mtcars, tinyplot(mpg, type = "density"))
f2 = function() tinyplot(~ mpg, mtcars, type = "density")
expect_snapshot_plot(f1, label = "density_type_nogroups")
expect_snapshot_plot(f2, label = "density_type_nogroups")

f1 = function() with(mtcars, tinyplot(mpg, by = am, type = "density"))
f2 = function() tinyplot(~ mpg | am, mtcars, type = "density")
expect_snapshot_plot(f1, label = "density_type_numeric")
expect_snapshot_plot(f2, label = "density_type_numeric")

f1 = function() with(iris, tinyplot(Sepal.Width, by = Species, type = "density"))
f2 = function() tinyplot(~ Sepal.Width | Species, iris, type = "density")
expect_snapshot_plot(f1, label = "density_type_factor")
expect_snapshot_plot(f2, label = "density_type_factor")

f1a = function() with(iris, tinyplot(Sepal.Width, by = Species, type = "density", bg = "by"))
f1b = function() with(iris, tinyplot(Sepal.Width, by = Species, type = "density", fill = "by"))
f2a = function() tinyplot(~ Sepal.Width | Species, iris, type = "density", bg = "by")
f2b = function() tinyplot(~ Sepal.Width | Species, iris, type = "density", fill = "by")
expect_snapshot_plot(f1a, label = "density_type_fill")
expect_snapshot_plot(f1b, label = "density_type_fill")
expect_snapshot_plot(f2a, label = "density_type_fill")
expect_snapshot_plot(f2b, label = "density_type_fill")

# test arg passing
f1 = function() tinyplot(
  ~ Sepal.Width | Species, iris,
  type = type_density(bw = "SJ"),
  main = 'bw = "SJ"'
)
expect_snapshot_plot(f1, label = "density_type_bw_sj")

f1 = function() {
  tinyplot(~ Sepal.Width | Species, iris, type = type_density(joint.bw = "mean"))
  tinyplot_add(type = type_density(joint.bw = "full"), lty = 2)
  tinyplot_add(type = type_density(joint.bw = "none"), lty = 3)
  legend("topright", c("Mean", "Full", "None"), lty = 1:3, title = "Joint BW")
}
expect_snapshot_plot(f1, label = "density_type_joint_bw")

# logical version (TRUE -> "mean" and FALSE -> "none")
f1 = function() {
  tinyplot(~ Sepal.Width | Species, iris, type = type_density(joint.bw = TRUE))
  tinyplot_add(type = type_density(joint.bw = FALSE), lty = 3)
  legend("topright", c("TRUE (mean)", "FALSE (none)"), lty = 1:3, title = "Joint BW")
}
expect_snapshot_plot(f1, label = "density_type_joint_bw_logical")


# Some extra tests for bespoke legend placement
f1 = function() with(mtcars, tinyplot(mpg, by = am, type = "density", legend = "bottom!"))

f2 = function() with(mtcars, tinyplot(mpg, by = am, type = "density", legend = list(x = "bottom!")))
expect_snapshot_plot(f1, label = "density_legend_bottom")
expect_snapshot_plot(f2, label = "density_legend_bottom")

