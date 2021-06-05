plugin_src = files([
    'deco.cpp',
    'subsurf.cpp',
])

all_src += plugin_src
all_src += files([
    'deco.hpp',
    'subsurf.hpp',
])

swayfire_deco = shared_module('swayfire-deco', plugin_src,
    cpp_pch: ['../pch/prefix.hpp'],
    dependencies: [wayfire, wlroots, pixman],
    link_with: swayfire_core,
    install_rpath: get_option('prefix') / get_option('libdir') / 'wayfire',
    install: true,
    install_dir: get_option('libdir') / 'wayfire')
