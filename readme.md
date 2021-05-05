# Swayfire
*Sway/I3 inspired tiling window manager for Wayfire.*

This project aims to emulate and improve upon sway/i3wm features as a
plugin for the [Wayfire](https://github.com/WayfireWM/wayfire)
compositor.

Currently, Swayfire implements most basic tiling features such as splits
and window movement and navigation keys. Swayfire also supports mouse
resizing and moving of windows/tiled parents.

*NOTE:* Swayfire is currently still in early development and not meant
to be used yet.

Notable planned features:
- Sway/i3-like Window decorations (borders, titles, and tabbed and
    stacked titles as in Sway/i3)
- Sway/i3 ipc (wherever it makes sense)
- Option for rounded corners for floating windows and window groups
- Scratchpad

## Installation

To build and install from source:
```sh
# Swayfire is developped using clang, but should compile with gcc as
# well at least. The following exports are optional:
export CC=clang
export CXX=clang++

# Generate the build directory:
meson setup --prefix /usr build

# To compile Swayfire:
meson compile -C build

# To install Swayfire:
meson install -C build
```

Alternatively, Swayfire has been packaged for the following distros:

###### Arch Linux

[swayfire-git] is available in the AUR.

```sh
yay -S swayfire-git
```

[swayfire-git]: https://aur.archlinux.org/packages/swayfire-git/

## Contributing

Contributions are welcome.

Swayfire uses the `c++17` standard and a modified `llvm` coding style
defined in `.clang_format`. Please run `meson compile format -C build`
to run the formatter before every commit.
