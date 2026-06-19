;; This is an operating system configuration generated
;; by the graphical installer.
;;
;; Once installation is complete, you can learn and modify
;; this file to tweak the system configuration, and pass it
;; to the 'guix system reconfigure' command to effect your
;; changes.


;; Indicate which modules to import to access the variables
;; used in this configuration.
(use-modules (gnu))
(use-modules (gnu system setuid)
             (nongnu packages linux)
             (nongnu system linux-initrd))

(use-service-modules guix admin sysctl pm nix avahi dbus cups desktop linux
                     mcron networking xorg ssh docker audio virtualization)

(use-package-modules audio video nfs certs shells ssh linux bash emacs gnome
                     networking wm fonts libusb cups freedesktop file-systems
                     version-control package-management vim curl sqlite databases
                     disk docker)

(operating-system
  (kernel linux)
  (initrd microcode-initrd)
  (firmware (list linux-firmware))

  (locale "en_US.utf8")
  (timezone "Asia/Riyadh")
  (keyboard-layout (keyboard-layout "us" "altgr-intl"))
  (host-name "mark3")

  ;; The list of user accounts ('root' is implicit).
  (users (cons* (user-account
                  (name "ebeem")
                  (comment "ebeem")
                  (group "users")
                  (home-directory "/home/ebeem")
                  (supplementary-groups '("wheel" "netdev"
                                          "audio" "video" "cdrom")))
                %base-user-accounts))

  ;; Packages installed system-wide.  Users can also install packages
  ;; under their own account: use 'guix search KEYWORD' to search
  ;; for packages and 'guix install PACKAGE' to install a package.
  (packages (append (list curl
                          git
                          vim
                          pipewire)
                    %base-packages))

   ;; Configure only the services necessary to run the system
  (services (append
             (modify-services %base-services
                              (delete mingetty-service-type)
                              (delete console-font-service-type))
             (list
              ;; Seat management (can't use seatd because Wireplumber depends on elogind)
              (service elogind-service-type)

              ;; Configure TTYs and graphical greeter
              (service console-font-service-type
                       (map (lambda (tty)
                              ;; Use a larger font for HIDPI screens
                              (cons tty (file-append
                                         font-terminus
                                         "/share/consolefonts/ter-132n")))
                            '("tty1" "tty2" "tty3" "tty4" "tty5" "tty6" "tty7")))

              (service greetd-service-type
                       (greetd-configuration
                        (greeter-supplementary-groups (list "video" "input"))
                        (terminals
                         (list
                          (greetd-terminal-configuration
                           (terminal-vt "1")
                           (terminal-switch #t))
                          (greetd-terminal-configuration (terminal-vt "2"))
                          (greetd-terminal-configuration (terminal-vt "3"))
                          (greetd-terminal-configuration (terminal-vt "4"))
                          (greetd-terminal-configuration (terminal-vt "5"))
                          (greetd-terminal-configuration (terminal-vt "6"))
                          (greetd-terminal-configuration (terminal-vt "7"))))))

              ;; Configure swaylock as a setuid program
              (service screen-locker-service-type
                       (screen-locker-configuration
                        (name "swaylock")
                        (program (file-append swaylock "/bin/swaylock"))
                        (using-pam? #t)
                        (using-setuid? #f)))
              
              ;; Configure the Guix service and ensure we use Nonguix substitutes
              (simple-service 'add-nonguix-substitutes
                              guix-service-type
                              (guix-extension
                               (substitute-urls
                                (append (list "https://substitutes.nonguix.org")
                                        %default-substitute-urls))
                               (authorized-keys
                                (append (list (plain-file "nonguix.pub"
                                                          "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))
                                        %default-authorized-guix-keys))))
              
              ;; Set up Polkit to allow `wheel' users to run admin tasks
              polkit-wheel-service
              
              ;; Give certain programs super-user access
              (simple-service 'mount-setuid-helpers
                              privileged-program-service-type
                              (map (lambda (program)
                                     (setuid-program
                                      (program program)))
                                   (list (file-append nfs-utils "/sbin/mount.nfs")
                                         (file-append ntfs-3g "/sbin/mount.ntfs-3g"))))
              
              ;; Networking services
              (service network-manager-service-type
                       (network-manager-configuration
                        (vpn-plugins
                         (list network-manager-openvpn))))
              (service wpa-supplicant-service-type) ;; Needed by NetworkManager
              (service modem-manager-service-type)  ;; For cellular modems
              (service bluetooth-service-type
                       (bluetooth-configuration
                        (auto-enable? #t)))
              (service usb-modeswitch-service-type)
              
              ;; Basic desktop system services (copied from %desktop-services)
              (service avahi-service-type)
              (service udisks-service-type)
              (service upower-service-type)
              (service cups-pk-helper-service-type)
              (service geoclue-service-type)
              (service polkit-service-type)
              (service dbus-root-service-type)
              fontconfig-file-system-service ;; Manage the fontconfig cache
              
              ;; Power and thermal management services
              (service thermald-service-type)
              (service tlp-service-type
                       (tlp-configuration
                        (cpu-boost-on-ac? #t)
                        (wifi-pwr-on-bat? #t)))
              
              ;; Enable JACK to enter realtime mode
              (service pam-limits-service-type
                       (list
                        (pam-limits-entry "@realtime" 'both 'rtprio 99)
                        (pam-limits-entry "@realtime" 'both 'nice -19)
                        (pam-limits-entry "@realtime" 'both 'memlock 'unlimited)))
              
              ;; Enable Docker containers and virtual machines
              (service containerd-service-type)
              (service docker-service-type)
              (service libvirt-service-type
                       (libvirt-configuration
                        (unix-sock-group "libvirt")
                        (tls-port "16555")))
              
              ;; Enable SSH access
              (service openssh-service-type)
              
              ;; Enable printing and scanning
              (service sane-service-type)
              (service cups-service-type
                       (cups-configuration
                        (web-interface? #t)
                        (extensions
                         (list cups-filters))))
              
              ;; Set up the X11 socket directory for XWayland
              (service x11-socket-directory-service-type)
              
              ;; Sync system clock with time servers
              (service ntp-service-type)
              
              ;; Add udev rules for MTP (mobile) devices for non-root user access
              (simple-service 'mtp udev-service-type (list libmtp))
              
              ;; Add udev rules for a few packages
              (udev-rules-service 'pipewire-add-udev-rules pipewire)
              (udev-rules-service 'brightnessctl-udev-rules brightnessctl)
              
              ;; Enable the build service for Nix package manager
              (service nix-service-type)
              
              ;; Schedule cron jobs for system tasks
              (simple-service 'system-cron-jobs
                              mcron-service-type
                              (list
                               ;; Run `guix gc' 5 minutes after midnight every day.
                               ;; Clean up generations older than 2 months and free
                               ;; at least 10G of space.
                               #~(job "5 0 * * *" "guix gc -d 2m -F 10G"))))))

 
  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)))
  (swap-devices (list (swap-space
                        (target (uuid
                                 "97f9e316-c6a1-487c-8bec-df3303637694")))))

  ;; The list of file systems that get "mounted".  The unique
  ;; file system identifiers there ("UUIDs") can be obtained
  ;; by running 'blkid' in a terminal.
  (file-systems (cons* (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "92C7-2ADC"
                                       'fat32))
                         (type "vfat"))
                       (file-system
                         (mount-point "/")
                         (device (uuid
                                  "a8987e51-ee6b-40db-93b5-50a63b941fab"
                                  'ext4))
                         (type "ext4")) %base-file-systems)))
