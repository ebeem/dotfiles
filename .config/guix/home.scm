(use-modules (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu packages)
             (gnu services)
             (gnu packages admin)
             (guix gexp))


(home-environment
 (packages (map specification->package '(
            "font-jetbrains-mono"
            "font-iosevka"
            "font-iosevka-aile"
            "font-iosevka-comfy"
            "font-iosevka-etoile"
            "font-fira-mono"
            "font-fira-sans"

            "isync"
            "mu"

            "gdb"
            "radare2"
            "cutter"
            "ghex"

            "plymouth"
            "stumpwm"
            "sbcl-stumpwm-swm-gaps"
            "sbcl-stumpwm-globalwindows"
            "sbcl-stumpwm-ttf-fonts"

            "rofi"
            "rofi-pass"

            "polybar"
            "dunst"
            "picom"
            "feh"
            "pamixer"
            "brightnessctl"
            "slock"
            "upower"
            "playerctl"
            "xsettingsd"
            "qtile"

            ;; helper gui applications
            "thunar"
            "thunar-volman"
            "thunar-archive-plugin"
            "flameshot"
            "ristretto"
            "xclip"

            "retroarch"

            "gimp"
            "inkscape"
            "krita"
            "darktable"

            "ckb-next"
            "openrgb"

            "mpd"
            "ncmpcpp"
            "vlc"
            "kodi"

            "hostapd"
            "deluge"
            "icecat"
            "qutebrowser"
            "ungoogled-chromium"
            "nyxt"
            "openvpn"
            "yt-dlp"

            "nix"

            "hydra"
            "nmap"
            "tor"

            "python"
            "python-cython"
            "python"

            ;; go
            "go"

            ;; java
            "maven"
            "openjdk"

            ;; databases
            "postgresql"
            "mariadb"
            "sqlite"

            ;; php
            "php"

            ;; octave/matlab
            "octave"

            ;; lisp
            "guile"

            ;; lua
            "lua"

            ;; javascript
            "node"

            ;; rest-client
            ;; "insomnia"

            ;; ftp
            "filezilla"

            ;; game development
            "godot"

            "audacity"
            "lmms"

            "alacritty"
            "kitty"
            "zsh"
            "zsh-autosuggestions"
            "zsh-syntax-highlighting"
            "zsh-completions"

            "libreoffice"
            "stow"
            "aspell"
            "aspell-dict-ar"
            "aspell-dict-en"
            "ncurses"
            "git"
            "stow"
            "vim"
            "emacs"
            "make"
            "cmake"

            ;; "gvfs"
            "neofetch"
            "password-store"

            "obs"
            "openshot"
            "blender"

            "qemu"
            "docker"
            "docker-registry"
            "docker-compose")))

 (services
  (list
   (service home-bash-service-type
            (home-bash-configuration
             (guix-defaults? #t)
             (bash-profile (list (plain-file "bash-profile" "\
export HISTFILE=$XDG_CACHE_HOME/.bash_history"))))))))
