# Add executable locations to PATH
export PATH="$HOME/.local/bin:$PATH"
export PATH="$PATH:/usr/local/bin:/home/andy/go/bin"
export PATH="$PATH:/usr/local/go/bin"
export PATH="$PATH:/home/andy/.tfenv/bin"
export PATH="$PATH:$HOME/node-v16.13.1-linux-x64/bin"
export PATH="$PATH:/opt/homebrew/lib/ruby/gems/3.0.0/bin"
export PATH="$PATH:/opt/homebrew/bin"
export PATH="$PATH:$HOME/.cargo/bin"
export PATH="$PATH:$HOME/go/bin"

# Oh my zsh configuration.
export ZSH="$HOME/.oh-my-zsh"

# Use default zsh theme.
ZSH_THEME="robbyrussell"

# Mac OS and Linux specific configuration.
if [[ "$(uname)" == "Darwin" ]]; then
    # Use gnu sed.
    export PATH="/usr/local/opt/gnu-sed/libexec/gnubin:$PATH"
fi

# Zsh plugins.
plugins=(
    colored-man-pages   # Colourised man pages. 
    fzf-tab             # Fuzzy file finding with tab.
)

# Include oh my zsh, but disable auto-update checking
export DISABLE_AUTO_UPDATE=true
source $ZSH/oh-my-zsh.sh

# Also disable the colorful szh prompt.
export PS1='$ '

# Disable venv prompt.
export VIRTUAL_ENV_DISABLE_PROMPT=

# Aliases.
alias ip=ipython
alias cdr='cd ~/repos/andykuszyk'
alias x='exit'
alias e="emacsclient -n"

# Use Emacs as default editor
export EDITOR=emacsclient

# AWS CLI command completion
if $(which aws > /dev/null); then
    autoload bashcompinit && bashcompinit
    autoload -Uz compinit && compinit
    complete -C $(which aws_completer) aws
fi

# Initialise sdkman
source "$HOME/.sdkman/bin/sdkman-init.sh"

# set zsh auto suggestion colour to something compatible with emacs
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=241"

# Initialise gvm
if [[ -a $HOME/.gvm/scripts/gvm ]]; then
    source $HOME/.gvm/scripts/gvm
fi

# Use private Go imports for my current company
export GOPRIVATE='github.com/Typeform/*'

# Disable auto-update on every brew command
export HOMEBREW_NO_AUTO_UPDATE=1

# Initialise nvm
if [[ -d "$HOME/.nvm" ]]; then
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
fi

# Initialise pyvm
if [[ -d "$HOME/.pyenv" ]]; then
    export PYENV_ROOT="$HOME/.pyenv"
    [[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init -)"
fi

# kubectl command completion
if $(which kubectl > /dev/null); then
    alias k='kubectl'
    source <(kubectl completion zsh)
    complete -F __start_kubectl k
fi

# Function to read stdin, and open it in an Emacs buffer.
function ep() {
    file=$(mktemp)
    cat > $file
    emacsclient -n $file
}
