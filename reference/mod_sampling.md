# Sampling Module for nemetonApp

Field sampling plan module. Lets the user generate sampling plots over
the current project's study area and download a QField project (.qgz)
for on-field data capture. The .qgz is produced by
[`nemeton::create_qfield_project()`](https://pobsteta.github.io/nemeton/reference/create_qfield_project.html)
and bundles placettes + empty arbres layers with pre-configured forms.

First iteration uses a spatial random draw (sf::st_sample). A full
stratified GRTS pipeline lives in the 09-sampling tutorial and will be
lifted to nemeton as create_sampling_plan() in a follow-up iteration.
