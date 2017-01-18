
export PATH="$HOME/.dotfiles/bin":"$PATH"
if [ -e ~/.zsh-pre-paths ]; then
    while read line ; do
        export PATH="$line":"$PATH"
    done < ~/.zsh-pre-paths
fi

if [ -e ~/.zsh-post-paths ]; then
    while read line ; do
        export PATH="$line":"$PATH"
    done < ~/.zsh-post-paths
fi

if [ -d "$HOME/.perl5" ]; then
    export PATH="$HOME/.perl5/bin${PATH:+:${PATH}}"
    export PERL5LIB="$HOME/.perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"
    export PERL_LOCAL_LIB_ROOT="$HOME/.perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"
    export PERL_MB_OPT="--install_base \"$HOME/.perl5\""
    export PERL_MM_OPT="INSTALL_BASE=$HOME/.perl5"
fi
