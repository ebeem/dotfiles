;; Indicate which modules to import to access the variables
;; used in this configuration.
(use-modules (gnu))
(use-modules (gnu packages lisp))
(use-modules (gnu packages vim))
(use-service-modules cups desktop networking ssh xorg)
(use-package-modules wm fonts)

(operating-system
  (locale "en_US.utf8")
  (timezone "Asia/Riyadh")
  (keyboard-layout (keyboard-layout "us"))
  (host-name "mark3")

  ;; The list of user accounts ('root' is implicit).
  (users (cons* (user-account
                  (name "ebeem")
                  (comment "ebeem")
                  (group "users")
                  (home-directory "/home/ebeem")
                  (supplementary-groups '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))

  ;; Packages installed system-wide.  Users can also install packages
  ;; under their own account: use 'guix search KEYWORD' to search
  ;; for packages and 'guix install PACKAGE' to install a package.
  (packages (append (list (map specification->package
                               '("nss-certs" "vim"
                                "openbox"
                                "stumpwm" "sbcl" "sbcl-stumpwm-globalwindows"
                                "sbcl-stumpwm-ttf-fonts" "plymouth")))
                    %base-packages))

  ;; Below is the list of system services.  To search for available
  ;; services, run 'guix system search KEYWORD' in a terminal.
  (services
   (append (list

                 ;; To configure OpenSSH, pass an 'openssh-configuration'
                 ;; record as a second argument to 'service' below.
                 (service openssh-service-type)
                 (service cups-service-type)
                 (set-xorg-configuration
                  (xorg-configuration (keyboard-layout keyboard-layout))))

           ;; This is the default list of services we
           ;; are appending to.
           %desktop-services))
  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (targets (list "/dev/sda"))
                (keyboard-layout keyboard-layout)))
  (swap-devices (list (swap-space
                        (target (uuid
                                 "99df73eb-a055-4fe2-bf81-bc9386f9a5ef")))))

  ;; The list of file systems that get "mounted".  The unique
  ;; file system identifiers there ("UUIDs") can be obtained
  ;; by running 'blkid' in a terminal.
  (file-systems (cons* (file-system
                         (mount-point "/")
                         (device (uuid
                                  "073607d6-64cf-4b23-8e09-8660d363a382"
                                  'ext4))
                         (type "ext4")) %base-file-systems)))
