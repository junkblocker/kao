# Selections and saved recipes

Kao and Sam use the same selections. Use Kao to select text that you can
see. Use Sam to describe how to find text and what to do with it.

A buffer holds text. A window displays a buffer. You can display the same
buffer in two windows. A Sam command buffer holds instructions for another
buffer; its header names that target.

## Start with one command

`M-x` means hold Alt, press x, then enter a command name and press Return.
Run `M-x sam` to enter one Sam command. Regexes use Emacs syntax.

- `,x/foo/` selects each `foo` in the accessible buffer.
- `a/!/` adds `!` after each current selection.
- `,x/foo/ a/!/` finds each `foo` and adds `!` after it.

The comma means the whole accessible buffer. If the buffer is narrowed,
hidden text stays outside that range. A command with no address starts from
each current selection. An explicit address that does not depend on a
selection runs once at the top level. Nested loops keep their repetitions.

Sam returns its result ranges as Kao selections. With Kao disabled, Sam
uses the Emacs region or point instead.

## Practise recipe replay

Use a disposable text buffer that contains `foo bar foo`. Enable Kao, then
run `M-x my/sam-workbench`. The menu shows the target, accessible range,
selection count, and current recipe.

1. Press `e` and enter `,x/foo/ a/!/`.
2. Press `r`. The text becomes `foo! bar foo!`.
3. Press `r` again. The text becomes `foo!! bar foo!!`.
4. Press `q` to leave the menu. Press `u` to undo the last run, or `U` to redo it.

Recipe replay finds targets again. Kao's `.` repeats Kao's recorded editing
action. These commands keep their separate meanings. Save a named recipe
with `s`; load it with `l`. Saving and loading do not execute it. Named
recipes use Emacs savehist storage.

## Review before applying

In the workbench, press `p` to preview text changes. Read the diff, then
press `a` to apply it once. If the target context changes, create a new
preview. Buffer loops (`X` and `Y`) enter preview before they edit buffers.

Preview rejects shell commands and undo. Direct `M-x sam` and
`M-x sam-scratch` still accept them. Shell side effects cannot be undone.
Editing hooks and protected text can reject or change an actual edit.
Earlier completed commands can remain applied if a later command fails.

## Use a command buffer

`M-x sam-scratch` opens instructions for the current target. Return runs the
current line. `C-c C-c` runs a selected multiline program. Exclude comment
lines that explain the lesson. `#0` is an address, not a comment.
`C-c C-t` chooses a different target.

In the workbench, `t` chooses a target and `?` opens help. `q`, Escape, or
`C-g` leaves the menu. Here `C-g` means hold Ctrl and press g. If the target
is gone or its mode changes, reopen the menu on the intended buffer.

## Connect your config

The package does not replace your leader bindings. To use `SPC s` in Kao
normal state, add this to your config:

```elisp
(with-eval-after-load 'kao
  (autoload 'my/sam-workbench "my-sam-workbench" nil t)
  (define-key kao-user-map "s"
    '("Sam recipe workbench" . my/sam-workbench)))
```

The workbench uses Transient. Core Kao and direct Sam commands do not load
it. Keep your existing help and practice bindings when you change the
package source.
