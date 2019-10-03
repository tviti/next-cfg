
# Table of Contents

1.  [Features](#orgc0604f6)
    1.  [Automatically determine the `dbus` socket's location on macOS](#orgb87044f)
    2.  [Make buffer deletion prompt more consistent w/ Emacs](#org6f29e07)
    3.  [`delete-all-buffers`](#org7fe4d1b)
    4.  [`open-home-dir`](#orgdda5eed)
    5.  [Vim `ex` style command abbreviations](#org971dad0)
        1.  [Update ex-command filter to use multi-select](#org9a5789b)
    6.  [Use `C-[` like `ESCAPE`](#orgd8bd1c8)
    7.  ["Hot-swapping" and version controlling `bookmark-db` files](#org464b23b):bookmarks:
        1.  [Update bookmark-db-mv and bookmark-db-cp to use multi-select](#org0b9837d)
        2.  [Better system for git interaction](#org4000525)
        3.  [Should we use command hooks for git interaction?](#orge77d787)
        4.  [Allow user to specify remote and branch](#org692fc96)
        5.  [Display git command output in minibuffer](#orge206b0c)
        6.  [Password prompts](#org09e6e9d)
        7.  [Select-bookmark-db should glob for database files](#orgf16b79c)
2.  [`README.org` TODO-list](#org0c41776)
    1.  [Literate style init file?](#orgcb5c3ee)

A repo for version controlling my `next-browser` init/config file(s).

For more information on `next-browser`, see:

-   <https://github.com/next-browser/next>
-   <https://github.com/atlas-engineer/next/blob/master/documents/MANUAL.org>


<a id="orgc0604f6"></a>

# Features


<a id="orgb87044f"></a>

## Automatically determine the `dbus` socket's location on macOS

macOS doesn't define the env var `DBUS_SESSION_BUS_ADDRESS` on it's own, and
I have also noticed that often times `DBUS_LAUNCHD_SESSION_BUS_SOCKET` will
be pointing to the wrong location, so I query the value of the latter and
then use it to set the former when `next` starts


<a id="org6f29e07"></a>

## Make buffer deletion prompt more consistent w/ Emacs

The default behavior of `C-x k` in Emacs (at least with `evil-mode` active)
is to query the user for a buffer to delete, with the default being the
active buffer, while in `next`, the completion function for the
`delete-buffer` command explicitly selects a non-active buffer as the default
for deletion. Here, I use a modified completion function that retains the
Emacs behavior (the command implementing this is un-creatively termed
`my-delete-buffer`).


<a id="org7fe4d1b"></a>

## `delete-all-buffers`

The command `delete-all-buffers` will delete ALL buffers except for the
currently active one.


<a id="orgdda5eed"></a>

## `open-home-dir`

The current file manager implementation felt a little un-intuitive and clunky
to me, so when I need to open a local `html` file, I often just start by
calling `open-home-dir`, and then link-hint my way to where I need to be.


<a id="org971dad0"></a>

## Vim `ex` style command abbreviations

In Emacs, I rely heavily on the `b` and `e` `ex` commands for swapping
buffers and opening files. Here, they are aliases for `switch-buffer` and
`set-url-new-buffer`. The `ex-style` aliases live in their own command list
though, and are thus not callable from `execute-command` prompt (i.e. they
are invisible to the `M-x` prompt). To access them, call
`execute-command-or-ex` (accessible via `:`, like in Vi or `evi-mode`).

The `execute-command-or-ex` prompt should allow you to access all the
*non*-ex-style commands as well (just like `execute-command`). It also
implements a very primitive/hackish/experimental attempt at passing
tab-completable arguments to the ex-style commands. For example, after
entering `b SPACE`, the prompt should bring up the same completion list as
`switch-buffer`.


<a id="org9a5789b"></a>

### TODO Update ex-command filter to use multi-select


<a id="orgd8bd1c8"></a>

## Use `C-[` like `ESCAPE`

The chord `C-[` is set to spoof an `ESCAPE` keypress to the browser core, so
you can use it for all the same things (e.g. going back to `vi-normal` mode,
or closing the minibuffer prompt).


<a id="org464b23b"></a>

## "Hot-swapping" and version controlling `bookmark-db` files     :bookmarks:

The command `select-bookmark-db` allows you to change `bookmark-db-path`
(i.e. the "active" bookmark database file) on the fly, via a minibuffer
prompt implementing `file-manager-mode`. The selected file will be created if
it doesn't already exist. If a `.git` directory is found in the directory
housing the selected file, the command `git add <bookmark-db>` is called,
followed by steps (1) and (2) of `bookmark-db-push`.

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

The commands `bookmark-db-cp` and `bookmark-db-mv` can be used to duplicate
and move bookmarks (resp.) between your different database files. They will
first prompt the user for an entry in the currently active bookmark
database, then for a path to the destination database. The state of the
bookmark repo is committed at the start of the command (before any changes
have occurred), and then upon command completion. Like `select-bookmark-db`,
the destination database will be created and added to the repo if it does
not already exist.


<a id="org0b9837d"></a>

### TODO Update bookmark-db-mv and bookmark-db-cp to use multi-select


<a id="org4000525"></a>

### TODO Better system for git interaction

-   I can't help but feel that the current system is a little excessive with
    commit frequency.


<a id="orge77d787"></a>

### TODO Should we use command hooks for git interaction?

-   It may be elegant to call the start/end repo updates in the entry/exit
    command hooks (e.g. for `bookmark-db-mv` and `bookmark-db-cp`). One
    possible downside though, is that since the git interaction is not coded
    explicitly in the function body, it may become more challenging to
    understand what is going on if these things get more complicated (and I
    tend to be stupid so&#x2026;)


<a id="org692fc96"></a>

### TODO Allow user to specify remote and branch


<a id="orge206b0c"></a>

### TODO Display git command output in minibuffer


<a id="org09e6e9d"></a>

### TODO Password prompts


<a id="orgf16b79c"></a>

### TODO Select-bookmark-db should glob for database files


<a id="org0c41776"></a>

# `README.org` TODO-list


<a id="orgcb5c3ee"></a>

## TODO Literate style init file?

Vindarel's *literate style* init file using `erudite` is really damned
slick. Should we do the same thing?

