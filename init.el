(require 'cl)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar my-packages 
 '(magit)
 "Libraries that should be installed by default")

(defun install-my-packages ()
  "Install all packages listed in my-packages that aren't installed."
  (interactive)
  (dolist (package my-packages)
    (unless (or (member package package-activated-list)
                (functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package))))

(defun is-online? ()
  "See if we're online.  

On Windows, which doesn't have network-interface-list, assume we're online."
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (some (lambda (iface) (unless (equal "lo" (car iface))
                              (member 'up (first (last (network-interface-info
                                                        (car iface)))))))
            (network-interface-list))
      t))

(when (is-online?)
  (unless package-archive-contents (package-refresh-contents))
  (install-my-packages))


