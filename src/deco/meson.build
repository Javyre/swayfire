plugin_src = files([
    'deco.cpp',
    'subsurf.cpp',
])

all_src += plugin_src
all_src += files([
    'deco.hpp',
    'subsurf.hpp',
])

pms = shared_module('swayfire-deco', plugin_src,
    cpp_pch: ['../pch/prefix.hpp'],
    dependencies: [wayfire, wlroots, pixman],
    install: true, install_dir: join_paths(get_option('libdir'), 'wayfire'))
