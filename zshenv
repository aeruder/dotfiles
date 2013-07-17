
# Environment variables for non-interactive shells
export EDITOR=vim
export VISUAL=vim
export LESS=-FmqXR
export PAGER=less

# Deal with konsole's broken TERM setting
if [ "$COLORTERM" = "gnome-terminal" ] || [ -n "$KONSOLE_PROFILE_NAME" ]; then
    export TERM=xterm-256color
fi

# Read in from .zsh-pre-manpaths and .zsh-post-manpaths
if [ -z "$MANPATH" ]; then
    unset MANPATH
    if ( which manpath >/dev/null 2>&1 ); then
        export MANPATH="`manpath`"
    else
        export MANPATH="`man -w`"
    fi
fi

manpath=( "${(s%:%)MANPATH}" )
[ -e ~/.zsh-pre-manpaths ] || touch ~/.zsh-pre-manpaths
[ -e ~/.zsh-post-manpaths ] || touch ~/.zsh-post-manpaths

prepaths=( ~/.dotfiles/man )
prepaths+=( ${(f)"$(<~/.zsh-pre-manpaths)"} )

postpaths=( )
postpaths+=( ${(f)"$(<~/.zsh-post-manpaths)"} )

for a in "${(Oa)prepaths[@]}"; do
    manpath=( "$a" "${manpath[@]}" )
done
for a in "${postpaths[@]}"; do
    manpath+=( "$a" )
done
manpath=( ${(u)manpath[@]} )
export MANPATH="${(j%:%)manpath}"

# Read in from .zsh-pre-paths and .zsh-post-paths
[ -e ~/.zsh-pre-paths ] || touch ~/.zsh-pre-paths
[ -e ~/.zsh-post-paths ] || touch ~/.zsh-post-paths

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

# Read in from .zsh-pre-ldpaths and .zsh-post-ldpaths
[ -e ~/.zsh-pre-ldpaths ] || touch ~/.zsh-pre-ldpaths
[ -e ~/.zsh-post-ldpaths ] || touch ~/.zsh-post-ldpaths

ldpath=( "${(s%:%)LD_LIBRARY_PATH}" )
prepaths=( )
prepaths+=( ${(f)"$(<~/.zsh-pre-ldpaths)"} )

postpaths=( )
postpaths+=( ${(f)"$(<~/.zsh-post-ldpaths)"} )

for a in "${(Oa)prepaths[@]}"; do
    ldpath=( "$a" "${ldpath[@]}" )
done
for a in "${postpaths[@]}"; do
    ldpath+=( "$a" )
done
ldpath=( ${(u)ldpath[@]} )
export LD_LIBRARY_PATH="${(j%:%)ldpath}"

