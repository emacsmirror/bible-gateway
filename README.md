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
  <h3 align="center">Bible Verse of the Day in Emacs</h3>

  <p align="center">
    <b>votd</b>: A simple Emacs package that fetches the <b>v</b>erse <b>o</b>f <b>t</b>he <b>d</b>ay from BibleGateway
    <br />
  </p>
</p>

<img src="https://github.com/kristjoc/votd/blob/main/screenshots/dark.png?raw=true">

<img src="https://github.com/kristjoc/votd/blob/main/screenshots/light.png?raw=true">

<!-- TABLE OF CONTENTS -->
<details open="open">
  <summary><h2 style="display: inline-block">Table of Contents</h2></summary>
  <ul>
  <li><a href="#introduction">Introduction</a></li>
  <li><a href="#installation">Installation</a></li>
  <li><a href="#configuration">Configuration</a></li>
  <li><a href="#contributing">Contributing</a></li>
  <li><a href="#license">License</a></li>
  <li><a href="#contact">Contact</a></li>
  <li><a href="#acknowledgements">Acknowledgements</a></li>
  </ul>
</details>


<!-- INTRODUCTION -->
## Introduction

votd is a simple Emacs package that fetches the Bible verse of the day
from the [BibleGateway](https://www.biblegateway.com/).
What it basically does is retrieve the content from the [BibleGateway](https://www.biblegateway.com/votd/get/?format=json&version=KJV)
API in JSON format and then format the text and reference accordingly.  

I'm currently using it to display the verse of the day as a footer in the Emacs [dashboard](https://github.com/emacs-dashboard/emacs-dashboard), but
one can use it as a message in the `*scratch*` buffer, etc.

<!-- INSTALLATION -->
## Installation

votd is not yet in a package archive. For Emacs 29, you can use
`package-vc-install`. Switch to the `*scratch*` buffer and `yank` the
following line:

``` commonlisp
(package-vc-install '(votd :vc-backend Git :url  "https://github.com/kristjoc/votd"))
```

Hit `C-x C-e` once you move the cursor to the last parenthesis. For Emacs 30, you can use the
new `:vc` keyword of `use-package` as follows:

``` commonlisp
(use-package votd
  :vc (:url "https://github.com/kristjoc/votd")) ; For Emacs>=30
```


<!-- CONFIGURATION -->
## Configuration

If you would like to use the verse of the day as your `*scratch message*`, use the following configuration in your `init.el`:

``` commonlisp
(use-package votd
  :vc (:url "https://github.com/kristjoc/votd") ; For Emacs>=30
  :config
  (setq initial-scratch-message
	(concat ";;; *scratch* ;;;\n\n"
		(string-join
		 (mapcar (lambda (line) (concat ";;; " line))
			 (split-string (get-votd) "\n"))
		 "\n")
		"\n;;;\n")))
```
By setting `(setq inhibit-splash-screen t)`, the `*scratch*` buffer will be displayed immediately upon starting Emacs, allowing you to see the verse of the day right away.

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

;; install votd with use-package using :vc
(use-package votd
  :vc (:url "https://github.com/kristjoc/votd") ; For Emacs>=30
  :after dashboard
  :config
  (setq dashboard-footer-messages (list (get-votd)))) ; votd as emacs-dashboard footer

(provide 'init)
;;; init.el ends here
```

To use the verse of the day both as a `*scratch*` message and as a footer in the `emacs-dashboard`, change the `use-package` configuration as follows:

``` commonlisp
(use-package votd
  :vc (:url "https://github.com/kristjoc/votd") ; For Emacs>=30
  :after dashboard
  :config
  (let ((verse (get-votd)))
    (setq dashboard-footer-messages (list verse))
    (setq initial-scratch-message
	        (concat ";;; *scratch* ;;;\n\n"
		              (string-join
		               (mapcar (lambda (line) (concat ";;; " line))
			                     (split-string verse "\n"))
		               "\n")
		               "\n;;;\n"))))
```

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

Distributed under the Unlicensed License. See
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
