export PATH=$HOME/.emacs.d/bin:$PATH
eval "$(starship init zsh)"
alias config='/usr/bin/git --git-dir=$HOME/.git --work-tree=$HOME'
export PATH="$HOME/.jenv/bin:$PATH"
eval "$(jenv init -)"
