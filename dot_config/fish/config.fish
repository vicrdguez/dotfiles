function set_alias
    alias nv="nvim"
    alias k="kubectl"
    alias k="kubectl"
    alias l="exa"
    alias ll="exa -l"
    alias la="exa -la"
    alias ld="exa -lD"
    alias lt="exa -lT --icons"
    alias llg="exa -l --git-ignore --icons"
    alias cm="chezmoi"

end

fish_add_path /usr/local/bin
fish_add_path /Users/vrodriguez/.cargo/bin/
fish_add_path /Users/vrodriguez/.local/bin
fish_add_path /Users/vrodriguez/.jenv/shims:/bin
fish_add_path /Users/vrodriguez/.goenv/bin
fish_add_path /Users/vrodriguez/bin
fish_add_path /Users/vrodriguez/.jenv/bin
fish_add_path /usr/bin
fish_add_path bin
fish_add_path usr/sbin
fish_add_path sbin

source ~/.asdf/asdf.fish
source ~/.asdf/plugins/java/set-java-home.fish

if status is-interactive
    # Commands to run in interactive sessions can go here
    set_alias
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

asdf_update_java_home
starship init fish | source
