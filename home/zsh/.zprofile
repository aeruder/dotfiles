# Super annoying to do this again but this
# gets around /etc/zprofile on mac blowing
# away the path
refresh-path

if [ -e ~/.zprofile.local ]; then
    source ~/.zprofile.local
fi
