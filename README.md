
Auto Complete for TypeScript
============================

ac-typescript is typescript completion tool on auto-complete for Emacs.
This contain emacs lisp and language service server for completion.

Requirement
===========
You have to install node.js before using this, but not have to install typescript on npm,
because ac-typescript is already included them.
But, we recommend to install TypeScript.el that is major mode for Typescript editing before install this.

Usega
=====

1. You download ac-typescript from this page.
2. Extract it and move to as you like.
3. Put follow script into your init.el .
`
(add-to-list 'load-path "path/to/ac-typescript")
(require 'ac-typescript)
(setq ac-typescript-server/isense-location "path/to/bin/isense.js")
;; uncoment this If you want to change server port
;; (setq ac-typescript-server/server-port 9999)
(ac-typescript/init-server)
(add-hook 'typescript-mode-hook 'ac-typescript/ac-enable)
`
