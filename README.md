# ox-qmd.el

[![MELPA](https://melpa.org/packages/ox-qmd-badge.svg)](https://melpa.org/#/ox-qmd)

Markdown ([qiita flavor](http://qiita.com/Qiita/items/c686397e4a0f4f11683d)) export back-end for GNU Emacs Org-mode.
This back-end is based on Markdown back-end (part of GNU Emacs)
and [Github Flavored Markdown](http://github.com/larstvei/ox-gfm) back-end.

## Installation

### package.el

ox-qmd is included in [MELPA](https://melpa.org/#/ox-qmd).

### Manual install

Copy ox-qmd.el to the directory which is listed in your `load-path`.

## Setup

Add the following line to your init file like `init.el` or `.emacs`.

```emacs-lisp
(require 'ox-qmd)
```

## Usage

You can call export commands from export dispatcher ( `C-c C-e` ).

## Configuration

### Remove newlines from paragraphs or not

You can choose whether you remove newlines from paragraphs or not.
When `ox-qmd-unfill-paragraph` is non- `nil` (default),
ox-qmd.el remove newlines and replace line-break strings with newlines.
If you want to prevent it, set `ox-qmd-unfill-paragraph` to `nil` .

```emacs-lisp
(setq ox-qmd-unfill-paragraph nil)
```

### Set keyword for syntax highlight

Some language keywords used in code block of Org-mode
are not recognized by Qiita.
For instance, keyword `sh` is required for shell-script
syntax highlight (keyword `shell-script` is not recognized).
In this case, you can set keyword association
by adding following code to your `.emacs` file.

```emacs-lisp
(add-to-list 'ox-qmd-language-keyword-alist '("shell-script" . "sh"))
```

## Experimental feature

`ox-qmd` offers experimental feature to upload inline images.
Please note that this feature may cease to work without notice because it
depends on non-public API of Qiita.
To use this feature, load the library,

```emacs-lisp
(require 'ox-qmd-upload-inline-image)
```

then the uploading command is added to org-export menu.
