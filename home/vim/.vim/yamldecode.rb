require 'yaml'
require 'json'

print JSON.generate YAML.load $stdin.read
