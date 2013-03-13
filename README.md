
Auto Complete for TypeScript
============================

ac-typescript is typescript completion tool on auto-complete for Emacs.
This contain emacs lisp and language service server is written by TypeScript for completion.

Requirement
===========
You have to install Node and be able to use npm before, but not have to install typescript by npm,
because ac-typescript is already included them.
Therefore, we recommend to install TypeScript.el that is major mode to edit Typescript.

Setup and Usage
===============

1. You download or clone ac-typescript.
2. Extract it and move to as you like if you download zipped archive. Skip this step when you choice clone this.
3. You have to type `npm install` in the extracted directory to install dependencies.
4. Put follow script into your init.el .
```
(add-to-list 'load-path "path/to/ac-typescript")
(require 'ac-typescript)
(setq ac-typescript-server/isense-location "path/to/bin/isense.js")
;; uncomment this If you want to change server port
;; (setq ac-typescript-server/server-port 9999)
(ac-typescript/start-server)
(add-hook 'typescript-mode-hook 'ac-typescript/ac-enable)
```
