library(chromote)
library(rvest)
library(stringr)

# Specify a website
url <- "https://data.novascotia.ca/Fishing-and-Aquaculture/Pictou-County-Current-Data/e7d2-4ybd/about_data"

# Specify a CSS selector to retrieve
selector <- ".metadata-pair"

# Start a new Chrome session
chrome_session <- ChromoteSession$new()

# Open a browser remotely controlled from R
#chrome_session$view()

# Navigate to a website
chrome_session$Page$navigate(url)
#chrome_session$Page$loadEventFired()

# Find the root document node
doc <- chrome_session$DOM$getDocument()

# Find the element using DOM methods
node <- chrome_session$DOM$querySelectorAll(
  nodeId = doc$root$nodeId,
  selector = selector
)

html_elements <- c()
# Get the HTML content of the node
for (i in 1:length(node$nodeIds)) {
  html <- chrome_session$DOM$getOuterHTML(
    nodeId = node$nodeIds[[i]]
  )
  if (str_detect(html, "Views")|str_detect(html, "Downloads"))
  html_elements <- c(html_elements, html)
}


# Close the browser session
chrome_session$close()

