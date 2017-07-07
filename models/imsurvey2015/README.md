## 2015 .impact Survey Analysis

Run the model and analysis:

```
cd ~/path/to/ea-data
R
```

```R
run("imsurvey2015")
```

-

Run the full data pipeline, assuming you possess the confidential un-anonymous CSVs in your `data` directory:

```R
source("models/dev/imsurvey2015/ea_id.R")
source("models/dev/imsurvey2015/convert_money.R")
run("imsurvey")
```
