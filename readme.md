# Swayfire

[![IRC: #swayfire on libera](https://img.shields.io/badge/irc-%23swayfire-informational)](https://web.libera.chat/#swayfire)

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
- Sway/i3-inspired ipc
- Option for rounded corners ([wherever it makes sense](https://github.com/Javyre/swayfire/issues/19#issuecomment-847123760))
- Scratchpad
- Custom tiling styles achievable through lua callbacks (binary
  split, master-stack layout, ...).
- Public API for writing Swayfire-specific Wayfire plugins.

## Installation

To build and install from source:
```sh
# Generate the build directory:
meson --prefix /usr --buildtype=release build

# Build and install swayfire:
sudo ninja -C build install
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
defined in `.clang_format`. Please run `nina -C build clang-format`
to run the formatter before every commit.
