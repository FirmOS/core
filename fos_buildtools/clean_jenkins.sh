#!/bin/sh
#!/bin/sh
echo "--> Cleaning Full Build Logs on Jenkins"
find ../.. | grep full_build_log.txt | xargs -r rm

