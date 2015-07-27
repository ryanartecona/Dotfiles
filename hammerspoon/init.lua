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


----------------------------------------
--- Window management modal bindings ---
----------------------------------------

local winMode = hs.hotkey.modal.new({'cmd','shift'}, 'space')
winMode:bind({}, 'escape',             function() winMode:exit() end)
winMode:bind({'cmd','shift'}, 'space', function() winMode:exit() end)

-- Show a center-screen message while in winMode
function winMode:entered()
  hs.alert.show('Hammerspoon mode on', 999999)
end
function winMode.exited()
  hs.alert.closeAll()
end

-- Misc. winMode bindings

winMode:bind({'cmd'}, 'r', function()
  winMode:exit()
  reloadConfig()
end)

winMode:bind({'cmd'}, 'c', hs.toggleConsole)


-----------------------------
--- Grid window movements ---
-----------------------------

-- Grid config
hs.grid.setGrid {w=16, h=12}
hs.grid.setMargins {w=0, h=0}


--- Shove a window all the way to an edge ---

function hs.grid.shoveWindowLeft(window)
  window = hs.window.focusedWindow()
  function shoveLeft(gridCell)
    gridCell.x = 0
  end
  hs.grid.adjustWindow(shoveLeft, window)
end

function hs.grid.shoveWindowUp(window)
  window = hs.window.focusedWindow()
  function shoveUp(gridCell)
    gridCell.y = 0
  end
  hs.grid.adjustWindow(shoveUp, window)
end

function hs.grid.shoveWindowRight(window)
  window = hs.window.focusedWindow()
  local w, h = hs.grid.getGrid(window:screen())
  function shoveRight(gridCell)
    gridCell.x = w - gridCell.w
  end
  hs.grid.adjustWindow(shoveRight, window)
end

function hs.grid.shoveWindowDown(window)
  window = window or hs.window.focusedWindow()
  local w, h = hs.grid.getGrid(window:screen())
  function shoveDown(gridCell)
    gridCell.y = h - gridCell.h
  end
  hs.grid.adjustWindow(shoveDown, window)
end


winMode:bind({'cmd'},         'm',     hs.grid.maximizeWindow)

winMode:bind({'cmd'},         'left',  hs.grid.pushWindowLeft)
winMode:bind({'cmd'},         'right', hs.grid.pushWindowRight)
winMode:bind({'cmd'},         'up',    hs.grid.pushWindowUp)
winMode:bind({'cmd'},         'down',  hs.grid.pushWindowDown)

winMode:bind({'shift'},       'left',  hs.grid.resizeWindowThinner)
winMode:bind({'shift'},       'right', hs.grid.resizeWindowWider)
winMode:bind({'shift'},       'up',    hs.grid.resizeWindowShorter)
winMode:bind({'shift'},       'down',  hs.grid.resizeWindowTaller)

winMode:bind({'cmd','shift'}, 'left',  hs.grid.shoveWindowLeft)
winMode:bind({'cmd','shift'}, 'right', hs.grid.shoveWindowRight)
winMode:bind({'cmd','shift'}, 'up',    hs.grid.shoveWindowUp)
winMode:bind({'cmd','shift'}, 'down',  hs.grid.shoveWindowDown)
