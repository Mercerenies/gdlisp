
require 'logger'
require 'fileutils'

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

task :build_rs do |t, args|
  sh 'cargo', 'build', *release_flag
end

task build: :build_rs do |t, args|
  sh 'cargo', 'run', *release_flag, '--', '--compile-stdlib'
  cp 'GDLisp.gd', 'MacroServer/GDLisp.gd'
  cp_r 'MacroServer', 'target/debug'
  cp 'GDLisp.msgpack', 'target/debug'
  cp_r 'MacroServer', 'target/debug/deps'
  cp 'GDLisp.msgpack', 'target/debug/deps'
  cp_r 'MacroServer', 'target/release'
  cp 'GDLisp.msgpack', 'target/release'
  cp_r 'MacroServer', 'target/release/deps'
  cp 'GDLisp.msgpack', 'target/release/deps'
end

task run: :build do |t, args|
  sh 'cargo', 'run', *release_flag, *args
end

task test: :build do |t, args|
  sh 'cargo', 'test', *release_flag, *args
end

task :clean do |t, args|
  sh 'cargo', 'clean', *args
end
