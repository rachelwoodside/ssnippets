library(RSocrata)

# Annapolis County
annapolis_wq_api_url <- "https://data.novascotia.ca/resource/knwz-4bap.csv"
annapolis_wq_data <- read.socrata(annapolis_wq_api_url)

# Antigonish County
antigonish_wq_api_url <- "https://data.novascotia.ca/resource/kgdu-nqdp.csv"
antigonish_wq_data <- read.socrata(antigonish_wq_api_url)

# Cape Breton County
cape_breton_wq_api_url <- "https://data.novascotia.ca/resource/5daj-5icy.csv"
cape_breton_wq_data <- read.socrata(cape_breton_wq_api_url)

# Colchester County
colchester_wq_api_url <- "https://data.novascotia.ca/resource/gfri-gzxa.csv"
colchester_wq_data <- read.socrata(colchester_wq_api_url)

# Digby County
digby_wq_api_url <- "https://data.novascotia.ca/resource/wpsu-7fer.csv"
digby_wq_data <- read.socrata(digby_wq_api_url)

# Halifax county
halifax_wq_api_url <- "https://data.novascotia.ca/resource/x9dy-aai9.csv"
halifax_wq_data <- read.socrata(halifax_wq_api_url)

# Inverness County
inverness_wq_api_url <- "https://data.novascotia.ca/resource/a9za-3t63.csv"
inverness_wq_data <- read.socrata(inverness_wq_api_url)

# Pictou County
pictou_wq_api_url <- "https://data.novascotia.ca/resource/adpu-nyt8.csv"
pictou_wq_data <- read.socrata(pictou_wq_api_url)

# Queens County
queens_wq_api_url <- "https://data.novascotia.ca/resource/qspp-qhb6.csv"
queens_wq_data <- read.socrata(queens_wq_api_url)

# Victoria County
victoria_wq_api_url <- "https://data.novascotia.ca/resource/t2ms-7jgj.csv"
victoria_wq_data <- read.socrata(victoria_wq_api_url)

# Yarmouth County
yarmouth_wq_api_url <- "https://data.novascotia.ca/resource/9qw2-yb2f.csv"
yarmouth_wq_data <- read.socrata(yarmouth_wq_api_url)
