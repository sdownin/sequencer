.libPaths("./R-Portable/App/R-Portable/library")
# this message is printed on several lines (one per path) to make multiple paths
# easier to spot
message('library paths:\n', paste('... ', .libPaths(), sep='', collapse='\n'))
# the path to portable chrome
#browser.path = sprintf('%s/%s',getwd(),"GoogleChromePortable/GoogleChromePortable.exe")
#options(browser = browser.path)

#shiny::runApp("./shiny/",port=8888,launch.browser=TRUE)

# both chromes work!
#chrome.sys = 'C:/Program Files (x86)/Google/Chrome/Application/chrome.exe'
chrome.portable = sprintf('"%s"', file.path(getwd(),
                            'GoogleChromePortable/App/Chrome-bin/chrome.exe'))

launch.browser = function(appUrl, browser.path=chrome.portable) {
  message('Browser path: ', browser.path)
  #print(sprintf('testprint: %s --app=%s', browser.path, appUrl))
  shell(sprintf('%s --app=%s', browser.path, appUrl))
}

 shiny::runApp('./shiny/', launch.browser=launch.browser)
