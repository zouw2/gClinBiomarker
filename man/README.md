# Documentation Files

This directory will contain documentation generated using roxygen. Because these files change dynamically and are redundant to the code used to generate them during build, they are not maintained in the development branch.

## Pulling to Master

When pulling `dev-master` changes into `master` you can remove the exclusion for `*/man/*.Rd` from `.git/info/exclude` and re-build the documentation with roxygen for access to these documentation files in the main github branch, ensuring that they're available. 

*TODO:* create a Makefile that is run from a git hook during merges to master that will automatically rebuild documentation.
