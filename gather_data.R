# Download data

library(fs)

# Download primary and general election poll data from FiveThirtyEight
dir_create("538-polls-data")
download.file("https://projects.fivethirtyeight.com/polls-page/president_primary_polls.csv", 
              "538-polls-data/primaries.csv",
              mode = "wb")
download.file("https://projects.fivethirtyeight.com/polls-page/president_polls.csv", 
              "538-polls-data/general.csv",
              mode = "wb")

# Download political ad data from Google
download.file("https://storage.googleapis.com/transparencyreport/google-political-ads-transparency-bundle.zip",
              "ads.zip")
unzip("ads.zip")
file_delete("ads.zip")
file.rename("google-political-ads-transparency-bundle/", "google-ads-data")



