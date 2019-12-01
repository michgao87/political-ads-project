# Download data

library(fs)

# Download primary election poll data from FiveThirtyEight

dir_create("538-polls-data")
download.file("https://projects.fivethirtyeight.com/polls-page/president_primary_polls.csv", 
              "538-polls-data/primaries.csv",
              mode = "wb")

# Download political ad data from Google
# The data is in the form of csv files contained within one zip file 

download.file("https://storage.googleapis.com/transparencyreport/google-political-ads-transparency-bundle.zip",
              "ads.zip")
unzip("ads.zip")
file_delete("ads.zip")
file.rename("google-political-ads-transparency-bundle/", "google-ads-data")




