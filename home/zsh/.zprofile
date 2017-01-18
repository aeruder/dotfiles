# Super annoying to do this again but this
# gets around /etc/zprofile on mac blowing
# away the path
prepaths=( ~/.dotfiles/bin )
prepaths+=( ${(f)"$(<~/.zsh-pre-paths)"} )

postpaths=( )
postpaths+=( ${(f)"$(<~/.zsh-post-paths)"} )

for a in "${(Oa)prepaths[@]}"; do
    path=( "$a" "${path[@]}" )
done
for a in "${postpaths[@]}"; do
    path+=( "$a" )
done
path=( ${(u)path[@]} )
