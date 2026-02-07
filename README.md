# Helheim Emacs

> This is the realm of Hel

Helheim is an Emacs configuration built around [Hel](https://github.com/anuvyklack/hel) — [Helix](https://helix-editor.com/) emulation layer for Emacs, that aims to provide tight integration between Hel and other packages available for Emacs.

I wasted an unreasonable amount of time and effort trying to tune other editors to myself. It started with Sublime Text, then Atom (which I really liked), then VS Code (which I never liked), then Neovim, Emacs + Evil, VS Code again, then Doom Emacs. I also tried Helix and Zed. I liked one thing in one editor and something else in another.

I want keyboard-driven modal editor, multiple cursors, smooth scrolling, Lisp (I would prever Common Lisp, but Emacs Lisp is better then nothing).

Eventually, I decided that enough was enough — its easier to implementing all the things I want by myself. After all, if Linus Torvalds can maintain his own version of [MicroEmacs](https://github.com/torvalds/uemacs), why can’t I?

That’s how Hel and Helheim were born.

Helheim gains three goals:
1. Be my editor of the dream;
2. Be useful to others to attract more users to Emacs;
3. Celebrate the power of open source software as an act of pure selfless creation in an era of late capitalism.

Someone might say that I’m continuing to tune yet another editor — Emacs this time — but I would disagree. Emacs is not a text editor; it’s a Lisp machine with a terminal emulator (which is unfortunate, since [I would prefer a full-fledged GUI](https://andreyor.st/posts/2023-07-11-emacs-gui-library/)).

---

## Key Features
### [Hel](https://github.com/anuvyklack/hel) – Helix Emulation Layer

- Selection → action modal editor with multiple cursors.
- Smooth scrolling commands out of the box.

### Outliner and personal knowledge management system

Helheim comes with one of the best outliners and personal knowledge management systems, fully preconfigured.

- **Offline-first** — your data belongs to you.
- **Unmatched editing capabilities** —no other note-taking app offers fully capable Vim emulation not to mention Kakoune of Helix.
- **Org-mode**
  - Rich markup — more powerful than Markdown with LaTeX as a first-class
    citizen (like HTML for Markdown).
  - The best code snippet support of any note taking app.
  - Task management.
  - Tables that allow multiline cells and Excel-like formulas.
  - Attach files to outline nodes (I personally use it as my file system).
- **Bidirectional links** via [Org-node](https://github.com/meedstrom/org-node)
- **Daily notes**—your main scratchpad and better "Inbox".

There is a lot of praise for Org mode that I can only agree with. For me, Org mode with bidirectional links (provided by [Org-roam](https://www.orgroam.com/) or [Org-node](https://github.com/meedstrom/org-node)), daily notes, and the ability to [attach files](https://orgmode.org/manual/Attachments.html) to outline nodes divides my life into "before" and "after". For the first time in my life, I have built a reliable and retrievable system for accumulating and processing knowledge, and it has already served me for more than two years — while all previous ones were abandoned within weeks.

### Ibuffer Done Right

Buffers are grouped intelligently:

- Buffers are grouped by project.
- Within each project, buffers are grouped by file tree depth.
- File paths are relative to the project root.
- Special buffers are grouped after buffers visiting files.
- Buffers visiting files outside of any project are grouped separately from special buffers.
- Denote IDs are stripped from buffer names.

<img width="540" alt="Image" src="https://github.com/user-attachments/assets/9a74b442-c1ae-4e64-b1fa-3fc3b323998f" />

### Xref with fallback behavior

Xref is hacked to try all registered backends in sequence until one succeeds, and [Dumb Jump](https://github.com/jacktasia/dumb-jump) as an universal fallback backend by default.

---

## Installation

> [!WARNING]  
> GNU Emacs 29.1 or later is required

1. Download and install **Symbols Nerd Font** from [www.nerdfonts.com](https://www.nerdfonts.com/). This font contains only nerd font icons. Emacs can assign fonts for individual Unicode code points, which means you can use any unpatched font and still get Nerd Icons.

2. Clone or fork this repository.

   Clone into `~/.config/emacs` so Emacs loads it automatically on startup:
   ```sh
   git clone https://github.com/anuvyklack/helheim-emacs.git ~/.config/emacs
   ```

   Or clone to any other location and pass the path to Emacs explicitly:
   ```sh
   emacs --init-directory <path/to/helheim-emacs>
   ```

3. Rename `init.example.el` file to `init.el`.

> [!NOTE]  
> In `init.example.el`, the default font is set to [Cascadia Code](https://github.com/microsoft/cascadia-code) . Either install it or replace it with a font you prefer.

---

## Configuration

> [!TIP]  
> Bind `Caps Lock` to `Esc`, and configure `Space` to tap+hold behavior: `Space` on tap and `Ctrl` on hold. You can use any of the following tools for this:
> - [kanata](https://github.com/jtroo/kanata)
> - [kmonad](https://github.com/kmonad/kmomod)
> - [keyd](https://github.com/rvaiya/keyd) (Linux only)
> - [Karabiner-Elements](https://github.com/pqrs-org/Karabiner-Elements) (Mac only)

### Hel

Read the [Hel documentation](https://github.com/anuvyklack/hel) for how to use and configure it.

> [!WARNING]
> Hel uses the `U+2000` (EN QUAD) character for secondary cursors by default. It should be bound to a variable-pitch font.

### Modular Architecture

> [!IMPORTANT]
> `universal-argument` is rebound to `M-u` since `C-u` is used for scrolling.

All folders in `user-lisp/` directory are automatically added to the `load-path` and all Emacs Lisp files are byte-compiled and scraped for autoload cookies on startup.

You can also invoke `prepare-user-lisp` command manually to rescan `user-lisp/` for content added after startup. With `universal-argument` — `M-u : prepare-user-lisp` — files will be recompiled and `.user-lisp-autoloads.el` file with autoload cookies will be rebuild.

`require` only the modules you need in your `init.el`.

> [!NOTE]  
> This is an Emacs 31 feature backported to Helheim.

### Color shemes workflow that actually works

Forget the nightmare of Emacs color themes management:

- Which function should you use to properly activate a color theme: `load-theme` or `enable-theme`?
- How customize faces for a specific theme? By default you can only use the `customize` interface, and all face overrides are global — if you switch themes, the overrides persist.
- How do you switch themes on the fly? It’s not easy. Loading a new theme doesn’t disable the previous one, leaving multiple themes enabled simultaneously.

Helheim takes care of all of these problems. Just use `load-theme`, either interactively or from Elisp, to activate the theme you want. That’s it.

If you want to customize a specific theme, use `helheim-theme-set-faces`. You can even apply your customizations immediately without restarting Emacs: place the cursor after the closing parenthesis of the form and evaluate it with `Space e e` (native Emacs keybinding is `C-x C-e`).

Example:

``` emacs-lisp
(helheim-theme-set-faces 'modus-operandi
  '(region :background "#d9eaff")
  '(help-key-binding :foreground "#0000b0" :background "grey96"
                     :box (:line-width (-1 . -1) :color "grey80")
                     :inherit fixed-pitch))
```

---

## Contributing
- **Share It**

  A quick post about Helheim on your blog or social media could bring new users to Emacs — which would be great!

- **Documentation**

  I struggle with deciding what to document: some things feel obvious, others seem too insignificant. If something is unclear, please open an issue and I'll add documentation.

- **Support Development**

  Hel and Helheim were developed on an old laptop with a cracked screen. I worked on them instead of grinding LeetCode, and I have run out of money. If you like them and want to support their development, you can donate via:

  - [PayPal](https://www.paypal.me/anuvyklack)

  Every contribution is greatly appreciated. Thank you!
