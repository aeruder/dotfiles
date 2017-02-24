
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

export PERLBREW_ROOT="$HOME"/.perl5
if [ -e "$PERLBREW_ROOT"/etc/bashrc ]; then
  source "$PERLBREW_ROOT"/etc/bashrc
fi
