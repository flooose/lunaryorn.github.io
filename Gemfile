require 'json'
require 'open-uri'

source 'https://rubygems.org'

versions = open('https://pages.github.com/versions.json') do |source|
  JSON.parse(source.read)
end

# Github Pages environment
gem 'github-pages', versions['github-pages']

# Tools
gem 'rake'
gem 'travis'
gem 'html-proofer'
gem 'scss_lint', require: false
gem 'rubocop'
