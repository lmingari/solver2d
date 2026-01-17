core_lib = library(
  'core_lib',
  sources: [
    'core/mod_KindType.f90'
    ],
  install: false
  )

io_lib = library(
  'io_lib', 
  sources: [
    'io/mod_YAML_value.f90',
    'io/mod_YAML_node.f90',
    'io/mod_YAML_parser.f90',
    'io/mod_YAML.F90',
    ],
  link_with: core_lib,
  include_directories: inc,
  install: false
)
