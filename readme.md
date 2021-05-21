# Swayfire
*Sway/I3 inspired tiling window manager for Wayfire.*

This project aims to improve upon sway/i3wm features as a plugin for the
[Wayfire](https://github.com/WayfireWM/wayfire) compositor.

Currently, Swayfire implements most tiling features, optional decorations, and
mouse resizing and moving of windows/tiled parents. See
[here](https://github.com/Javyre/swayfire/projects/2) for a roadmap towards the
first stable release.

*NOTE:* Swayfire is currently still in early development and not meant
to be used yet.

Notable planned features:
- Sway/i3-like Window decorations (borders, titles, and tabbed and
    stacked titles as in Sway/i3)
- Sway/i3-inspired ipc (wherever it makes sense)
- Option for rounded corners for floating windows and window groups
- Scratchpad
- Public API for writing Swayfire-specific Wayfire plugins.

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
