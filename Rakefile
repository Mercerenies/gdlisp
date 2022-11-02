
require 'logger'
require 'fileutils'

$logger = Logger.new($stdout)

task default: %w[run]

task :clippy do |t, args|
  sh 'cargo', 'clippy', *args
end

task :build_rs do |t, args|
  sh 'cargo', 'build'
end

task build: :build_rs do |t, args|
  sh 'cargo', 'run', '--', '--compile-stdlib'
  cp 'GDLisp.gd', 'MacroServer/GDLisp.gd'
  cp_r 'MacroServer', 'target/debug'
  cp 'GDLisp.msgpack', 'target/debug'
  cp_r 'MacroServer', 'target/debug/deps'
  cp 'GDLisp.msgpack', 'target/debug/deps'
end

task run: :build do |t, args|
  sh 'cargo', 'run', *args
end

task test: :build do |t, args|
  sh 'cargo', 'test', *args
end

task :clean do |t, args|
  sh 'cargo', 'clean', *args
end
