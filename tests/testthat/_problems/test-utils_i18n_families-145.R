# Extracted from test-utils_i18n_families.R:145

# test -------------------------------------------------------------------------
skip_if_not_installed("nemeton")
fams <- tryCatch(nemeton::indicator_families(), error = function(e) NULL)
skip_if(
    !is.data.frame(fams) || nrow(fams) == 0,
    "nemeton::indicator_families() unavailable"
  )
for (i in seq_len(nrow(fams))) {
    key <- fams$family_column[i]
    static_name <- nemetonshiny:::TRANSLATIONS[[key]]
    static_desc <- nemetonshiny:::TRANSLATIONS[[paste0(key, "_desc")]]

    expect_false(is.null(static_name), info = paste("Missing fallback key:", key))
    expect_false(
      is.null(static_desc),
      info = paste("Missing fallback key:", paste0(key, "_desc"))
    )

    expect_equal(static_name$fr, fams$name_fr[i], info = paste(key, "fr"))
    expect_equal(static_name$en, fams$name_en[i], info = paste(key, "en"))
    expect_equal(static_desc$fr, fams$description_fr[i], info = paste(key, "desc fr"))
    expect_equal(static_desc$en, fams$description_en[i], info = paste(key, "desc en"))
  }
