test_value = executable(
  'test_yaml_value',
  'test_yaml_value.f90',
  link_with: io_lib
  )

test_node = executable(
  'test_yaml_node',
  'test_yaml_node.f90',
  link_with: io_lib
  )

test_parser = executable(
  'test_yaml_parser',
  'test_yaml_parser.f90',
  link_with: io_lib
  )

test_yaml = executable(
  'test_yaml',
  'test_yaml.f90',
  link_with: io_lib
  )

test('test_value',test_value)
test('test_node',test_node)
test('test_parser',test_parser)
test('test_yaml',test_yaml)
