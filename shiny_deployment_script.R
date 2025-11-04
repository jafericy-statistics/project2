install.packages('rsconnect')
install.packages('skimr')
install.packages('shinycssloaders')


NAME = ""
TOKEN = ""
SECRET = ""

rsconnect::setAccountInfo(name=NAME,
                          token=TOKEN,
                          secret=SECRET)

library(rsconnect)
rsconnect::deployApp('path/to/your/app')