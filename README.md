
# Table of Contents

1.  [next-cfg](#org23712f8):next_browser:
    1.  [Author: Taylor Viti](#orgd46d006)
    2.  [Note about versions](#org80a895e)
    3.  [Features](#org6582930)
        1.  [Automatically determine the `dbus` socket's location on macOS](#orgbaf830c)
        2.  [Make buffer deletion prompt more consistent w/ Emacs](#org12731e2)
        3.  [`delete-all-buffers`](#org0351b7d)
        4.  [`open-home-dir`](#orgda40ef4)
        5.  [Vim `ex` style command abbreviations](#orgbf8fddf)
        6.  [Use `C-[` like `ESCAPE`](#orga8060ee)
        7.  ["Hot-swapping" and version controlling `bookmark-db` files](#org7850003):bookmarks:
    4.  [`README.org` TODO-list](#orgbac2ced)
        1.  [Literate style init file?](#orge59c8a5)


<a id="org23712f8"></a>

# next-cfg     :next_browser:


<a id="orgd46d006"></a>

## Author: Taylor Viti

A repo for version controlling my `next-browser` init/config file(s).

For more information on `next-browser`, see:

-   <https://github.com/next-browser/next>
-   <https://github.com/atlas-engineer/next/blob/master/documents/MANUAL.org>


<a id="org80a895e"></a>

## Note about versions

This config works with latest `next-browser` **release** v1.3.2, but seems to
be broken when run with the latest **dev version** (i.e. the master branch of
the `next-browser` repo). From what I can tell, this is due to the switch to
*functional* style configurations (see [this issue ticket](https://github.com/atlas-engineer/next/issues/419)), but I honestly
haven't spent a whole lot of time debugging so far.


<a id="org6582930"></a>

## Features


<a id="orgbaf830c"></a>

### Automatically determine the `dbus` socket's location on macOS

macOS doesn't define the env var `DBUS_SESSION_BUS_ADDRESS` on it's own, and
I have also noticed that often times `DBUS_LAUNCHD_SESSION_BUS_SOCKET` will
be pointing to the wrong location, so I query the value of the latter and
then use it to set the former when `next` starts


<a id="org12731e2"></a>

### Make buffer deletion prompt more consistent w/ Emacs

The default behavior of `C-x k` in Emacs (at least with `evil-mode` active)
is to query the user for a buffer to delete, with the default being the
active buffer, while in `next`, the completion function for the
`delete-buffer` command explicitly selects a non-active buffer as the default
for deletion. Here, I use a modified completion function that retains the
Emacs behavior (the command implementing this is un-creatively termed
`my-delete-buffer`).


<a id="org0351b7d"></a>

### `delete-all-buffers`

The command `delete-all-buffers` will delete ALL buffers except for the
currently active one.


<a id="orgda40ef4"></a>

### `open-home-dir`

The current file manager implementation felt a little un-intuitive and clunky
to me, so when I need to open a local `html` file, I often just start by
calling `open-home-dir`, and then link-hint my way to where I need to be.


<a id="orgbf8fddf"></a>

### Vim `ex` style command abbreviations

In Emacs, I rely heavily on the `b` and `e` `ex` commands for swapping
buffers and opening files. Here, they are aliases for `switch-buffer` and
`set-url-new-buffer`. Of course, unlike in Vim and Emacs, they don't
actually take args in `next`.

1.  TODO Better implementation of `def-cmd-alias`

    I'm guessing that my method of implementing the aliases is probably a
    dirty hack, so I should redo that at some point.


<a id="orga8060ee"></a>

### Use `C-[` like `ESCAPE`

This one isn't an *actual* alias for `ESCAPE`, but will do the same thing in
`vi-normal-mode`, `vi-insert-mode`, and `minibuffer-mode`.


<a id="org7850003"></a>

### "Hot-swapping" and version controlling `bookmark-db` files     :bookmarks:

The command `select-bookmark-db` allows you to change `bookmark-db-path`
(i.e. the "active" bookmark database file) on the fly, via a minibuffer
prompt implementing `file-manager-mode`. The selected file will be created if
it doesn't already exist. If a `.git` directory is found in the directory
housing the selected file, the command `git add <bookmark-db>` is called.

The command `bookmark-db-push` will call the following commands in sequence:

1.  `git ... add --update`
2.  `git ... commit -m "bookmark-db-push"`
3.  `git ... push origin master`

where `...` denotes the flags `--git-dir=<db-dir>.git --work-tree=<db-dir>`.
The command `bookmark-db-pull` will perform the same sequence, but with the
commit message of (2) as `bookmark-db-pull`, and (3) as a `pull` instead.
Both commands will print a warning to the repl (**not** the minibuffer) if no
`.git` folder is detected in the bookmark-db folder.

These commands will all assume that `origin` is setup to be accessed over
ssh, and that the requisite ssh-key has already been added to the ssh-agent
(i.e. they are very primitive, and rely on the assumption that you will not
be prompted for a username/password at any point when calling `git`).

1.  TODO Allow user to specify remote and branch

2.  TODO Display git command output in minibuffer

3.  TODO Password prompts

4.  TODO Select-bookmark-db should glob for .db files

5.  TODO Add a cmd to move/copy bookmarks between .db files


<a id="orgbac2ced"></a>

## `README.org` TODO-list


<a id="orge59c8a5"></a>

### TODO Literate style init file?

Vindarel's *literate style* init file using `erudite` is really damned
slick. Should we do the same thing?

