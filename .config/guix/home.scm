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
                     kde-multimedia vim autotools finance ssh ncurses networking freedesktop
                     task-management gnome librewolf lua node php xfce engineering maths
                     tor vpn wm xdisorg password-utils web-browsers golang
                     game-development python jupyter python-xyz machine-learning
                     java virtualization containers emulators wine glib base)

(define %packages-utils
  (list alsa-utils aspell aspell-dict-en aspell-dict-ar ispell bluez
        fastfetch findutils gparted brightnessctl ripgrep grep hostapd
        btop inotify-tools openssh sed ncurses stow sudo wget rtorrent
        dbus password-store fd tlp nix
		;; TODO bandwhich (custom package)
		))

(define %packages-file-managers
  (list thunar thunar-volman thunar-vcs-plugin thunar-shares-plugin
        thunar-media-tags-plugin thunar-archive-plugin nemo tumbler yazi))

(define %packages-mail
  (list goimapnotify notmuch))

(define %packages-media
  (list ardour blender darktable inkscape gimp krita libresprite
        ffmpegthumbnailer kdenlive obs libresprite lmms mpd mpd-mpc
        tenacity vlc yt-dlp))

(define %packages-fonts
  (list font-iosevka font-google-noto font-fira-mono font-fira-sans
        ;; TODO: font-nerd-iosevka (custom package)
        ))

(define %packages-coding
  (list emacs neovim tokei autoconf automake binutils
        ;; TODO: android, agy, dotnet
        go python python-cython lua octave php node openjdk zig
        ;; TODO: dart, flutter (nix)
        git rkdeveloptool))

(define %packages-reverse-engineering
  (list ghex radare2 scanmem))

(define %packages-terminal
  (list bat
        ;; TODO: binsider (nix)
        foot zsh zsh-syntax-highlighting zsh-completions zsh-autopair
        zsh-autosuggestions))

(define %packages-window-management
  (list awww bibata-cursor-theme colloid-catppuccin-purple-gtk-theme
        colloid-catppuccin-purple-icon-theme flameshot fuzzel river
        sway swaybg swayidle swayimg swaynotificationcenter guile-swayer
        wofi waybar xdg-utils mako))

(define %packages-game-development
  (list godot
        ;; TODO: steamcmd (nix), butler (nix)
        ))

(define %packages-databases
  (list sqlite postgresql
        ;; TODO: dbeaver (nix)
        ))

(define %packages-communication
  (list 
        ;; TODO: discord (nix)
        ))

(define %packages-virtualization
  (list podman flatpak))

(define %packages-websites
  (list 
        ;; TODO: hugo (wishlist)
        ))

(define %packages-office-suite
  (list libreoffice))

(define %packages-finance
  (list ledger gnucash))

(define %packages-browser
  (list librewolf))

(define %packages-networking
  (list wireshark))

(define %packages-gaming
  (list wine64 wine retroarch steam))

(home-environment
 (packages (append %packages-utils
                   %packages-file-managers
                   %packages-mail
                   %packages-media
                   %packages-fonts
                   %packages-coding
                   %packages-reverse-engineering
                   %packages-terminal
                   %packages-window-management
                   %packages-game-development
                   %packages-databases
                   %packages-communication
                   %packages-virtualization
                   %packages-websites
                   %packages-office-suite
                   %packages-finance
                   %packages-browser
                   %packages-networking
                   %packages-gaming))
 (services
  (list
   (service home-bash-service-type
            (home-bash-configuration
             (guix-defaults? #t)
             (bash-profile (list (plain-file "bash-profile" "\
export HISTFILE=$XDG_CACHE_HOME/.bash_history"))))))))
