# ox-qmd.el

Markdown ([qiita flavor](http://qiita.com/Qiita/items/c686397e4a0f4f11683d)) export back-end for GNU Emacs Org-mode.
This back-end is based on Markdown back-end (part of GNU Emacs)
and [Github Flavored Markdown](http://github.com/larstvei/ox-gfm) back-end.

## Installation

### package.el

ox-qmd is included in [MELPA](https://melpa.org/).

### Manual install

1.  Copy ox-qmd.el to the directory which is listed in your `load-path`.
2.  Add the following code to your `.emacs` file.

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
