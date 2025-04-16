<!-- PROJECT SHIELDS -->

<div align="center">
  
[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
  
</div>

<!-- PROJECT LOGO -->
<br />
<p align="center">
  <h3 align="center">Bible Verse of The Day in Emacs</h3>

  <p align="center">
    <b>votd</b>: A simple Emacs package that fetches the <b>v</b>erse
    <b>o</b>f <b>t</b>he <b>d</b>ay and any requested verse, passage,
    or chapter from [BibleGateway](https://www.biblegateway.com/).
    <br />
  </p>
</p>

### Using as an emacs-dashboard footer

<img src="https://github.com/kristjoc/votd/blob/main/screenshots/dashboard-dark.png?raw=true">

<img src="https://github.com/kristjoc/votd/blob/main/screenshots/dashboard-light.png?raw=true">

### Using as a `*scratch*` buffer message

<img src="https://github.com/kristjoc/votd/blob/main/screenshots/scratch-dark.png?raw=true">

### Inserting the Bible passage at point

<img src="https://github.com/kristjoc/votd/blob/main/screenshots/votd-get-passage.gif?raw=true">

<!-- TABLE OF CONTENTS -->
<details open="open">
  <summary><h2 style="display: inline-block">Table of Contents</h2></summary>
  <ul>
  <li><a href="#introduction">Introduction</a></li>
  <li><a href="#installation">Installation</a></li>
  <li><a href="#configuration--usage">Configuration & Usage</a></li>
  <li><a href="#contributing">Contributing</a></li>
  <li><a href="#license">License</a></li>
  <li><a href="#contact">Contact</a></li>
  <li><a href="#acknowledgements">Acknowledgements</a></li>
  </ul>
</details>


<!-- INTRODUCTION -->
## Introduction

votd is a simple Emacs package that fetches the Bible verse of the day
from the [BibleGateway](https://www.biblegateway.com/). What it
basically does is retrieve the content from the
[BibleGateway](https://www.biblegateway.com/votd/get/?format=json&version=KJV)
API in JSON format and then format the text and reference accordingly.
It can also insert any requested Bible verse, passage, or chapter at the
current point in the buffer.  

I'm currently using it to display the verse of the day as a footer in
the Emacs [dashboard](https://github.com/emacs-dashboard/emacs-dashboard), as
well as in the `*scratch*` buffer message.

<!-- INSTALLATION -->
## Installation

### From MELPA (Recommended)

Starting from April 2025, [`votd`](https://melpa.org/#/votd) is available on MELPA. To install it using the package manager:

1. Ensure MELPA is added to your `package-archives` (if not already):
   ```commonlisp
   (require 'package)
   (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
   ```

2. Refresh the package list and install `votd`:
   ```commonlisp
   M-x package-refresh-contents RET
   M-x package-install RET votd RET
   ```

### From GitHub

To fetch the package directly from source you can use
`package-vc-install`, available in Emacs >= 29. Switch to the
`*scratch*` buffer and `yank` the following line:

``` commonlisp
(package-vc-install '(votd :vc-backend Git :url  "https://github.com/kristjoc/votd"))
```

Hit `C-x C-e` once you move the cursor to the last parenthesis. For Emacs 30, you can use the
new `:vc` keyword of `use-package` as follows:

``` commonlisp
(use-package votd
  :vc (:url "https://github.com/kristjoc/votd")) ; For Emacs>=30
```

Alternatively, clone the repository from GitHub and install `votd.el` with `M-x package-install-file`.



<!-- CONFIGURATION -->
## Configuration & Usage

### `*scratch*` buffer message

If you would like to use the verse of the day as your `*scratch*`
buffer message*`, use the following configuration in your `init.el`:

``` commonlisp
(use-package votd
  :config
  (setq initial-scratch-message
	(concat ";;; *scratch* ;;;\n\n"
		(string-join
		 (mapcar (lambda (line) (concat ";;; " line))
			 (split-string (votd-get-verse) "\n"))
		 "\n")
		"\n;;;\n")))
```
By setting `(setq inhibit-splash-screen t)`, the `*scratch*` buffer will be displayed immediately upon starting Emacs, allowing you to see the verse of the day right away.

### `emacs-dashboard` footer

Additionally, here is a minimal `init.el` to add the verse of the day to your `emacs-dashboard` footer:

``` commonlisp
;;; init.el --- minimal init.el for emacs-dashboard & votd -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; melpa needed for installing emacs-dashboard
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; place any customize-based settings in custom.el.
(setq custom-file "~/.emacs.d/custom.el")

;; emacs-dashboard config
(use-package dashboard
  :ensure t
  :init
  (dashboard-setup-startup-hook)
  :config 
  (setq dashboard-center-content t))

;; install votd from MELPA
(use-package votd
  :ensure t
  :after dashboard
  :config
  (setq dashboard-footer-messages (list (votd-get-verse)))) ; votd as emacs-dashboard footer

(provide 'init)
;;; init.el ends here
```

### `emacs-dashboard` footer + `*scratch*` buffer message

To use the verse of the day both as a `*scratch*` message and as a footer in the `emacs-dashboard`, change the `use-package` configuration as follows:

``` commonlisp
(use-package votd
  :ensure t
  :after dashboard
  :config
  (let ((verse (votd-get-verse)))
    (setq dashboard-footer-messages (list verse))
    (setq initial-scratch-message
	  (concat ";;; *scratch* ;;;\n\n"
		  (string-join
		   (mapcar (lambda (line) (concat ";;; " line))
                           (split-string verse "\n"))
		    "\n")
                   "\n\n"))))
```

### `doom-dashboard`

If you're using `doom-dashboard`, the following snippet from a Reddit comment should do the trick.

``` commonlisp
(use-package votd
  :config
  (defun doom-dashboard-widget-votd ()
    (insert "\n" (+doom-dashboard--center +doom-dashboard--width (votd-get-verse))))
  (add-hook! '+doom-dashboard-functions :append #'doom-dashboard-widget-votd))
```

### Insert passage at point

To insert a Bible passage in the current buffer, at point, invoke `M-x
votd-get-passage`, enter the reference, e.g., John 3:16, or John
3:15-15, or John 3, and hit `RET`. Set the user option `votd-include-ref` to `t` to include the reference, or to `nil` to exclude it.


And that's it! God bless you! Have a great day! :-)


<!-- CONTRIBUTING -->
## Contributing

Contributions are what make the open source community a valuable place
to learn and create. Any contributions you make are **appreciated**.

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/cool`)
3. Commit your Changes (`git commit -m 'Added cool feature'`)
4. Push to the Branch (`git push origin feature/cool`)
5. Open a Pull Request


<!-- LICENSE -->
## License

Distributed under the GPL-3.0 license. See
[`LICENSE`](https://github.com/kristjoc/votd/blob/main/LICENSE) for more information.


<!-- CONTACT -->
## Contact

[Signal Me](https://signal.me/#eu/7axcnRBeqe3T1fJ3aDXFqFUOU68-DiBzkLbU3U5kogZ1UR7N5YlH665PzEOJSxdD)


<!-- ACKNOWLEDGEMENTS -->
## Acknowledgements

* [All Glory to GOD](https://www.biblegateway.com/passage/?search=John%203%3A16&version=KJV)
* [BibleGateway Ministry](https://www.biblegateway.com/)
* [Emacs Dashboard](https://github.com/emacs-dashboard/emacs-dashboard)
* [gptel: A simple LLM client for Emacs](https://github.com/karthink/gptel)
* [GitHub Copilot](https://github.com/copilot)


<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[contributors-shield]: https://img.shields.io/github/contributors/kristjoc/votd.svg?style=for-the-badge
[contributors-url]: https://github.com/kristoc/votd/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/kristjoc/votd.svg?style=for-the-badge
[forks-url]: https://github.com/kristjoc/votd/network/members
[stars-shield]: https://img.shields.io/github/stars/kristjoc/votd.svg?style=for-the-badge
[stars-url]: https://github.com/kristjoc/votd/stargazers
[issues-shield]: https://img.shields.io/github/issues/kristjoc/votd.svg?style=for-the-badge
[issues-url]: https://github.com/kristjoc/votd/issues
