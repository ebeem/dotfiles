(add-to-load-path
 (string-append (getenv "HOME") "/.config/guix"))

(use-modules (gnu)
             (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu services)
             (guix gexp)
             (ebeem packages fonts)
             (ebeem packages dotnet)
             (ebeem packages game-development)
             (ebeem packages gnome-xyz)
             (nongnu packages messaging)
             (nongnu packages mozilla)
             (nongnu packages k8s)
			 (nongnu packages game-client)
             (nonguix multiarch-container))

(use-package-modules admin linux aspell bittorrent fonts ftp mail video image debug
                     gnuzilla kodi pulseaudio audio music graphics shells guile-xyz
                     shellutils terminals rust-apps emacs inkscape gimp photo gnucash
                     disk sqlite gdb mpd libreoffice chromium package-management hardware
					 kde-multimedia vim
                     task-management gnome librewolf lua node php xfce engineering maths
                     tor vpn wm xdisorg password-utils web-browsers golang
                     game-development python jupyter python-xyz machine-learning
                     java virtualization containers emulators wine glib base)

(home-environment
 (packages (list
            ;; utils
            hostapd
			gparted
            brightnessctl
            htop
            btop
            deluge
			ispell
            aspell
            aspell-dict-en
            aspell-dict-ar
            fd
            password-store
            neofetch
            pamixer
            tlp
			qalculate-gtk
			dbus
			sqlite
			
            ;; hardware
            ;; openrgb
            ;; ckb-next

            ;; file manager
            filezilla
            thunar
            thunar-volman
            thunar-vcs-plugin
            thunar-shares-plugin
            thunar-media-tags-plugin
            thunar-archive-plugin
            ristretto

            ;; office suite
            libreoffice

            ;; ui
            colloid-catppuccin-purple-gtk-theme
			colloid-catppuccin-purple-icon-theme
            gsettings-desktop-schemas
            (list glib "bin")

            ;; fonts
            font-iosevka
            font-iosevka-etoile
            font-iosevka-ss09
            font-iosevka-ss08
            font-iosevka-ss14
            font-iosevka-aile
            font-jetbrains-mono
            font-fira-mono
            font-fira-sans
            font-google-noto
            font-awesome

            ;; programming
            ;; make
            go
            python
            ;; python-keras
            ;; python-matplotlib
            ;; python-scikit-learn
            ;; python-seaborn
            ;; tensorflow
            ;; python-jupyterlab-server
            python-cython
            lua
            octave
            php
            node
            openjdk

            ;; game development
            godot
            ;; godot-mono
			tiled

            ;; communication
            isync
            mu

            ;; reverse engineering
            gdb
            radare2
            cutter
            ghex
			scanmem

            ;; hacking
            nmap
            hydra
            tor
            openvpn

            ;; shells
            zsh
            zsh-syntax-highlighting
            zsh-completions
            zsh-autopair
            zsh-autosuggestions
			fish

            ;; terminals
            alacritty
            kitty
            foot
			ripgrep

            ;; video editing
            obs
            openshot
			kdenlive

            ;; graphics editing
            ;; blender
            darktable
            inkscape
            gimp
            ;;krita
			libresprite

            ;; sound editing
            audacity
			ardour
            lmms

            ;; media
            mpd
            blanket
            ncmpcpp
            vlc
            kodi
            yt-dlp
			picard

            ;; window manager
            sway
            dunst
            waybar
            rofi
			fuzzel
            wofi
			flameshot
			guile-swayer

            ;; text editors
            emacs
			neovim
            ;; vscodium

            ;; browser
			librewolf
			;; ungoogled-chromium
            ;; nyxt
            qutebrowser

            ;; virtualization
            qemu
			kubectl
            podman
            flatpak

			;; finance
			gnucash

            ;; gaming
            retroarch
            dolphin-emu
            ;;wine
            ;;wine64

            ;; nongnu
            ;; element-desktop
            ;; firefox

            ;; dotnet
            ;; dotnet-8
            ;; dotnet-7
            ;; dotnet-6

            ;; nonfree
            steam
            ))

 (services
  (list
   (service home-bash-service-type
            (home-bash-configuration
             (guix-defaults? #t)
             (bash-profile (list (plain-file "bash-profile" "\
export HISTFILE=$XDG_CACHE_HOME/.bash_history"))))))))

