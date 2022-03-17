
require 'logger'
require 'fileutils'

$logger = Logger.new($stdout)

task default: %w[run]

task :clippy do |t, args|
  sh 'cargo', 'clippy', *args
end

task :build do |t, args|
  sh 'cargo', 'build', *args
  sh 'cargo', 'run', '--', '--compile-stdlib'
end

task run: :build do |t, args|
  sh 'cargo', 'run', *args
end

task :test do |t, args|
  case args.to_a[0]
  when nil
    sh 'cargo', 'test'
  when "--ignored"
    sh 'cargo', 'test', '--', '--ignored'
  when "--all"
    sh 'cargo', 'test'
    sh 'cargo', 'test', '--', '--ignored'
  end
end

task :clean do |t, args|
  sh 'cargo', 'clean', *args
end
