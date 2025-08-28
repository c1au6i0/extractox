#!/usr/bin/env just
# shellcheck shell=bash

package_name := 'extractox'

github_org := 'c1aud6io'

@default:
  just --choose

@pcommit:
   #!/usr/bin/env -vS bash -i
   \builtin set -uxo pipefail;
   R -q -e 'devtools::load_all(); styler::style_pkg();';
   R -q -e 'devtools::load_all(); usethis::use_tidy_description();';
   R -q -e 'devtools::load_all(); devtools::document();';
   R -q -e 'devtools::load_all(); devtools::run_examples();';
   R -q -e 'devtools::load_all(); rmarkdown::render("README.Rmd", output_format = "md_document");';
   R -q -e 'devtools::load_all(); devtools::check(manual = TRUE, remote = TRUE);';
   R -q -e 'devtools::load_all(); pkgdown::build_site()'


