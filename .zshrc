# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi


# Path to your oh-my-zsh installation.
ZSH=/usr/share/oh-my-zsh/
source /usr/share/zsh-theme-powerlevel10k/powerlevel10k.zsh-theme

# $1 is the name of the plugin
# $2 is the github url
install_plugin() {
    if [ ! -e $ZSH"custom/plugins/$1" ]
    then
      echo "cloning $2 into $ZSH""custom/plugins/$1"
      sudo git clone $2 $ZSH"custom/plugins/$1"
    fi
}

install_plugin "zsh-autosuggestions" "https://github.com/zsh-users/zsh-autosuggestions"
install_plugin "zsh-completions" "https://github.com/zsh-users/zsh-completions"
install_plugin "zsh-syntax-highlighting" "https://github.com/zsh-users/zsh-syntax-highlighting"

export plugins=(
  zsh-autosuggestions
  zsh-completions
  zsh-syntax-highlighting
  git
  catimg
  command-not-found
  extract
  python
)

ZSH_CACHE_DIR=$HOME/.cache/oh-my-zsh
if [[ ! -d $ZSH_CACHE_DIR ]]; then
  mkdir $ZSH_CACHE_DIR
fi

export BROWSER=/usr/bin/firefox
export EDITOR=/usr/bin/emacs
export ANDROID_HOME=/mnt/storage/android
export ANDROID_SDK_ROOT=$ANDROID_HOME
export PATH=/home/ebeem/workspace/git/flutter/bin:/home/ebeem/.dotnet/tools:$PATH
export PATH=/home/ebeem/.dotnet/tools:$PATH
export PATH=~/.emacs.d/bin:~/.bin:$PATH
export STEAM_COMPAT_CLIENT_INSTALL_PATH=/home/ebeem/.steam
export XDG_CONFIG_HOME=~/.config
source $ZSH/oh-my-zsh.sh

alias em="/usr/bin/emacsclient -nw"
# archey3
neofetch

source $HOME/.aliases
# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
