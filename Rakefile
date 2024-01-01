
require 'logger'
require 'fileutils'

if Gem.win_platform?
  # Needed to be able to create symlinks on Windows.
  require 'win32/file'
end

$logger = Logger.new($stdout)

release_flag =
  if ENV['GDLISP_RELEASE']
    ['--release']
  else
    []
  end

task default: %w[run]

task :clippy do |t, args|
  sh 'cargo', 'clippy', *args
end

task :doc do |t, args|
  ENV['RUSTDOCFLAGS'] ||= ''
  ENV['RUSTDOCFLAGS'] += ' -D warnings'
  sh 'cargo', 'doc', *args
end

task :build do |t, args|
  sh 'cargo', 'build', *release_flag
end

task :rtest do |t, args|
  sh 'cargo', 'test', *release_flag, *args
end

task itest: [:build] do |t, args|
  # Run gd-rehearse tests
  Dir.glob('tests/*').each do |package|
    puts "Running #{package} gd-rehearse tests ..."
    # NOTE: We currently don't run benchmarks here. We have no
    # benchmark tests, and gd-rehearse considers a lack of benchmarks
    # a failure, causing Rake to think we failed.
    sh 'godot4', '--headless', '--path', "#{package}/godot/", '--', '--rust-test'
  end
end

task test: %i[rtest itest]

task :clean do |t, args|
  sh 'cargo', 'clean', *args
end
