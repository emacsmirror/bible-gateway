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

I'm currently using it to display the verse of the day as a footer in the Emacs
[dashboard](https://github.com/emacs-dashboard/emacs-dashboard), but
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

Here is a minimal [init.el](https://github.com/kristjoc/votd/blob/main/config/init.el) configuration to add the verse of the day to your dashboard footer:

``` commonlisp
;;; init.el --- Load minimal init.el configuration -*- lexical-binding: t -*-

;;; Commentary:

;; This file bootstraps a minimal configuration for votd as footer in Emacs dashboard.

;;; Code:


;; Melpa needed to install Emacs dashboard
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; Place any customize-based settings in custom.el.
(setq custom-file "~/.emacs.d/custom.el")


;; Install votd using :vc in use-package
(use-package votd
  :vc (:url "https://github.com/kristjoc/votd")) ; For Emacs>=30


;; Emacs dashboard configuration
(use-package dashboard
  :ensure t
  :init
  (dashboard-setup-startup-hook)
  :config 
  (setq dashboard-banner-logo-title "")
  (setq dashboard-footer-messages '(""))
  (setq dashboard-navigation-cycle t)
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-items '((recents   . 3)
                          (projects  . 3)
                          (bookmarks . 3)))
  (setq dashboard-footer-icon "")
  ;; Set up the dashboard footer using votd
  (require 'votd)
  (setq dashboard-footer-messages (list (get-votd))))


(provide 'init)
;;; init.el ends here
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
