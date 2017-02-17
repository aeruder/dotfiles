-- This file is largely based on needing to map ESC to F18 with karabiner-elements.
-- then pressing ESC should work as ESC or you can use it as a modifier key for all this stuff.

MAPS = {}

get_preferred_browser = function()
  safari_id = 'com.apple.Safari'
  chrome_id = 'com.google.Chrome'
  browser = hs.application.get(safari_id)
  if browser ~= nil then
    return safari_id
  end

  browser = hs.application.get(chrome_id)
  if browser ~= nil then
    return chrome_id
  end

  return chrome_id
end

switch_browser = function()
  browser = get_preferred_browser()
  hs.application.launchOrFocusByBundleID(browser)
end

switch_app = function(id, all)
  app = hs.application.get(id)
  x = app and app:activate(all)
end

bind_key = function(k, func)
  if type(k) == "string" then
    k = hs.keycodes.map[k]
  end
  MAPS[k] = func
end

do_app_maps = function()
  -- Use list_apps() from the Hammerspoon console to get the identifiers
  quick_switches = {
    w = "org.gnu.Emacs",
    c = "com.github.communi.Communi",
    q = "com.googlecode.iterm2",
    f = "com.apple.finder",
  }
  for k, v in pairs(quick_switches) do
    bind_key(k, function() switch_app(v, true) end)
  end
end

list_apps = function()
  apps = hs.application.runningApplications()
  for k, v in pairs(apps) do
    print(k, v:name(), v:bundleID())
  end
end

bind_key('b', switch_browser)
do_app_maps()

hotkeyKeyPress = function(e)
  keycode = e:getKeyCode()
  if keycode == 79 then -- ESCAPE
    return true
  end
  shortcut = MAPS[keycode]
  K.triggered = true
  if shortcut then
    shortcut(keycode, e)
  else
    print(hs.inspect(e:getRawEventData()))
  end
  return true
end

hotkeyEnter = function()
  K.triggered = false
  ET:start()
end

hotkeyExit = function()
  ET:stop()
  if not K.triggered then
    hs.eventtap.keyStroke({}, 'ESCAPE')
  end
end
K = hs.hotkey.bind({}, 'F18', hotkeyEnter, hotkeyExit)
ET = hs.eventtap.new({hs.eventtap.event.types["keyDown"]}, hotkeyKeyPress)
ET:stop()
