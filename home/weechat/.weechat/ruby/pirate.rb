require 'rubygems'
require 'booty'
require 'weechat'
include Weechat
include Script::Skeleton
include Weechat::Helper

@script = {
   :name => "pirate",
   :author => "Andrew Ruder <andy@aeruder.net>",
   :version => "0.0.1",
   :license => "GPL3",
   :description => "turns text into pirate talk"
}

def setup
  Modifier.new("irc_out_privmsg") do |server, msg|
    before, after = msg.message.split(":", 2)
    name, message = after.split(":", 2)
    if message and name =~ /^[^ ]+$/
      before = "#{before}:#{name}"
      after = message
    end
    after = Booty.call(after)
    msg.message = "#{before}:#{after}"
    nil
  end
end
