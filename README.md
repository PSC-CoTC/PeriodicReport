# PeriodicReport

The periodic report is still in draft form; the report is the `index.html` file, which can be downloaded from this folder.

`index.html` is generated from `index.qmd`, a quarto document. `index.qmd` relies in part on sensitive data that is stored external to this repository; as such, the `.qmd` document will not compile on your computer without additional file access.

If compiling `index.qmd`, note that a weird bug arises from older versions of the package `{scales}`, which looks like

> Error in train_continuous(x, self$range, call = call) : 
  unused argument (call = call)

Updating the scales package will resolve this issue.

For questions or suggestions, contact Diego Holgren or John Brady from the Coho Technical Committee.