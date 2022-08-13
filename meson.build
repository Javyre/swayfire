project(
	'swayfire',
    'c',
	'cpp',
	version: '0.1',
	license: 'GPLv3',
	meson_version: '>=0.51.0',
	default_options: [
		'cpp_std=c++17',
        'c_std=c11',
		'warning_level=2',
		'werror=false',
	],
)

wayfire = dependency('wayfire')
wlroots = dependency('wlroots')
wfconfig = dependency('wf-config', version: '>=0.8.0')
cairo = dependency('cairo')
pixman = dependency('pixman-1')

add_project_arguments(['-DWLR_USE_UNSTABLE'], language: ['cpp', 'c'])
add_project_arguments(['-DWAYFIRE_PLUGIN'], language: ['cpp', 'c'])
add_project_link_arguments(['-rdynamic'], language:'cpp')

resource_dir = join_paths(get_option('prefix'), 'share', 'wayfire')
metadata_dir = join_paths(resource_dir, 'metadata')

all_src = []

subdir('src')
subdir('metadata')

summary = [
	'',
	'----------------',
	'swayfire @0@'.format(meson.project_version()),
	'----------------',
	''
]
message('\n'.join(summary))
