hs.notify.new({title = 'Hammerspoon config reloaded.'}):send()


-- Basic settings

hs.window.animationDuration = 0


-- Reload config automatically

function reloadConfig()
  configFileWatcher:stop()
  configFileWatcher = nil
  hs.reload()
end

configFileWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig)
configFileWatcher:start()


-- This method can be used to place a window to a position and size on the screen by using
-- four floats instead of pixel sizes. Returns the window instance. Examples:
--     windowToGrid( someWindow, {0, 0, 0.25, 0.5} );     -- top-left, width: 25%, height: 50%
--     windowToGrid( someWindow, {0.3, 0.2, 0.5, 0.35} ); -- top: 30%, left: 20%, width: 50%, height: 35%
-- Taken from https://github.com/gwww/dotfiles/blob/master/_hammerspoon/init.lua
function windowToGrid( window, rect )
  -- TODO: change rect to use named indices rather than integer
  if not window then
    return window
  end

  local screen = hs.screen.mainScreen():fullFrame()
  window:setFrame( {
    x = math.floor( rect[1] * screen.w + .5 ) + screen.x,
    y = math.floor( rect[2] * screen.h + .5 ) + screen.y,
    w = math.floor( rect[3] * screen.w + .5 ),
    h = math.floor( rect[4] * screen.h + .5 )
  } )
  return window
end

function focusedWindowToGrid(rect)
  windowToGrid( hs.window.focusedWindow(), rect );
end


-- Window management modal bindings

local winMode = hs.hotkey.modal.new({'cmd','shift'}, 'space')
winMode:bind({}, 'escape',             function() winMode:exit() end)
winMode:bind({'cmd','shift'}, 'space', function() winMode:exit() end)

-- Show a center-screen message while in winMode
function winMode:entered()
  hs.alert.show('Hammerspoon mode on', 9999)
end
function winMode.exited()
  hs.alert.closeAll()
end

-- The winMode bindings
winMode:bind({'cmd'}, 'r', function()
  winMode:exit()
  reloadConfig()
end)

winMode:bind({}, 'm', function()
  focusedWindowToGrid {0, 0, 1, 1}
end)

local nextWinMoveLeft = hs.fnutils.cycle({
 {0, 0, .6, 1},
 {0, 0, .5, 1},
 {0, 0, .4, 1}
})
winMode:bind({}, 'h', function()
  focusedWindowToGrid(nextWinMoveLeft())
end)

local nextWinMoveRight = hs.fnutils.cycle({
 {.4, 0, .6, 1},
 {.5, 0, .5, 1},
 {.6, 0, .4, 1}
})
winMode:bind({}, 'l', function()
  focusedWindowToGrid(nextWinMoveRight())
end)

local nextWinMoveUp = hs.fnutils.cycle({
 {0, 0, 1, .7},
 {0, 0, 1, .5},
 {0, 0, 1, .3}
})
winMode:bind({}, 'k', function()
  focusedWindowToGrid(nextWinMoveUp())
end)

local nextWinMoveDown = hs.fnutils.cycle({
 {.3, 0, 1, .7},
 {.5, 0, 1, .5},
 {.7, 0, 1, .3}
})
winMode:bind({}, 'j', function()
  focusedWindowToGrid(nextWinMoveDown())
end)
