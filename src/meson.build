plugin_src = files([
    'binding.cpp',
    'grab.cpp',
    'resize.cpp',
    'swayfire.cpp',
])

all_src += plugin_src
all_src += files([
    'grab.hpp',
    'swayfire.hpp',
    'pch/prefix.hpp',
])

pms = shared_module('swayfire', plugin_src,
    cpp_pch: ['pch/prefix.hpp'],
    dependencies: [wayfire, wlroots],
    install: true, install_dir: join_paths(get_option('libdir'), 'wayfire'))
