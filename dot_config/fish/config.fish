function set_alias
    alias nv="nvim"
    alias k="kubectl"
    alias k="kubectl"
    alias tf="terraform"
    alias l="exa"
    alias ll="exa -l"
    alias la="exa -la"
    alias ld="exa -lD"
    alias lt="exa -lT --icons"
    alias llg="exa -l --git-ignore --icons"
    alias cm="chezmoi"

end
function set_path
    fish_add_path /usr/local/bin
    fish_add_path /Users/vrodriguez/.cargo/bin/
    fish_add_path /Users/vrodriguez/.local/bin
    fish_add_path /Users/vrodriguez/bin
    fish_add_path /Users/vrodriguez/.deno/bin
    fish_add_path /usr/bin
    fish_add_path bin
    fish_add_path usr/sbin
    fish_add_path sbin
    # fish_add_path $(go env GOPATH)/bin
    # fish_add_path $(rtx where golang)/packages
end

function fzf_conf
    set -g -x FZF_CTRL_T_COMMAND 'fd -L --full-path $HOME --exclude "Library*" --exclude "Google*" --ignore-vcs'
    set -g -x FZF_CTRL_T_OPTS "--preview '(bat -f {} || erd --icons --layout inverted --sort name --color force {}) 2> /dev/null | head -200'"
    set -g -x FZF_ALT_C_COMMAND 'fd -L --full-path $HOME --exclude "Library*" --exclude "Google*" --ignore-vcs --type d'
    set -g -x FZF_ALT_C_OPTS "--preview 'erd --icons --layout inverted --sort name --color force {} | head -200'"
    set -g -x FZF_CTRL_R_OPTS "--preview 'echo {}' --preview-window down:3:hidden:wrap --bind '?:toggle-preview'"
end

~/.local/bin/rtx activate fish | source

if status is-interactive
    # Commands to run in interactive sessions can go here
    set_alias
    set_path
    fzf_conf
    #Configure auto-attach/exit to your likings (default is off).
    set ZELLIJ_AUTO_ATTACH true
    set ZELLIJ_AUTO_EXIT false
    set -g -x EDITOR nvim
    set -g -x ZK_NOTEBOOK_DIR /Users/vrodriguez/dev/kb/braindump
    set -g -x XDG_CONFIG_HOME /Users/vrodriguez/.config
    eval (zellij setup --generate-auto-start fish | string collect)
    # bind \cl true
    #neofetch
end

if not set -q ZELLIJ
    if test "$ZELLIJ_AUTO_ATTACH" = true
        zellij attach -c
    else
        zellij
    end

    if test "$ZELLIJ_AUTO_EXIT" = true
        kill $fish_pid
    end
end

if command -q nix-your-shell
  nix-your-shell fish | source
end

starship init fish | source
