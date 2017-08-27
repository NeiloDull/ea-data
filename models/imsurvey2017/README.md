## 2017 .impact Survey Analysis

Run the model and analysis:

```
cd ~/path/to/ea-data
R
```

```R
run("imsurvey2017")
```

-

Run the full data pipeline, assuming you possess the confidential un-anonymous CSVs in your `data` directory:

```R
source("models/imsurvey2017/ea_id.R")
source("models/imsurvey2017/convert_money.R")
run("imsurvey2017")
source("models/imsurvey2017/write_comments.R")
```
