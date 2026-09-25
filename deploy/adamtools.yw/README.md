# adamtools.yw sourceable deployment bundle

Use this bundle where the corporate R environment cannot install `adamtools.yw`
as a package but can source R functions.

```r
source("C:/approved-location/adamtools.yw/load_adamtools_yw.R")
```

This loads the same functions as the package source into the current R session.
Required dependency packages still need to be available in the corporate R
environment; this bundle only removes the need to install **adamtools.yw** itself.

## Maintenance

`R/` is generated. Do not edit it directly.

1. Make and test changes in the canonical package source.
2. From the package root, run:

   ```r
   source("tools/sync_source_bundle.R")
   source("tools/validate_source_bundle.R")
   ```

3. Deploy the complete `deploy/adamtools.yw/` folder, including `MANIFEST.csv`.

The validation script fails when the deployed function files or manifest differ
from canonical `R/` sources, preventing silent drift between the package and
sourceable copies.
