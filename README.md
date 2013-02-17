
Auto Complete for TypeScript
============================

ac-typescript is typescript completion tool on auto-complete for Emacs.
This contain emacs lisp and language service server for completion.

Require
=======
You have to install node.js before using this.

Usega
=====

> (require 'ac-typescript)
> (ac-typescript/init-server)
> (add-hook 'typescript-mode-hook 'ac-typescript/ac-enable)
