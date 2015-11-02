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
  hs.alert.show('(ESC or ⇧⌘SPC to quit)', 999999)
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


--- Partition window into _n_ slices ---

-- `partialWindowHorizontal` slices a window's screen into `nSlices`
-- full-height slices, and sets the window to fill the `sliceIdx`
-- of those slices, 1-indexed.
-- E.g. `hs.grid.partialWindowHorizontal(2,1)(someWindow)`
-- would make `someWindow` fill the whole left half of its screen.
function hs.grid.partialWindowHorizontal(nSlices, sliceIdx)
  return function(window)
    window = window or hs.window.focusedWindow()
    local w, h = hs.grid.getGrid(window:screen())
    function slice(gridCell)
      gridCell.x = math.floor((w / nSlices) * (sliceIdx - 1))
      gridCell.y = 0
      gridCell.w = math.floor((w / nSlices) * (sliceIdx))
      gridCell.h = h
    end
    hs.grid.adjustWindow(slice, window)
  end
end

-- `partialWindowVertical` works exactly analogously to
-- `partialWindowHorizontal`.
function hs.grid.partialWindowVertical(nSlices, sliceIdx)
  return function(window)
    window = window or hs.window.focusedWindow()
    local w, h = hs.grid.getGrid(window:screen())
    function slice(gridCell)
      gridCell.x = 0
      gridCell.y = math.floor((h / nSlices) * (sliceIdx - 1))
      gridCell.w = w
      gridCell.h = math.floor((h / nSlices) * (sliceIdx))
    end
    hs.grid.adjustWindow(slice, window)
  end
end


----------------------------------
--- Movement/resizing bindings ---
----------------------------------

winMode:bind({'cmd'},         'm',     hs.grid.maximizeWindow)
winMode:bind({'cmd','shift'}, 'm',     hs.grid.maximizeWindow)

winMode:bind({},              'left',  hs.grid.pushWindowLeft)
winMode:bind({},              'right', hs.grid.pushWindowRight)
winMode:bind({},              'up',    hs.grid.pushWindowUp)
winMode:bind({},              'down',  hs.grid.pushWindowDown)

winMode:bind({'shift'},       'left',  hs.grid.resizeWindowThinner)
winMode:bind({'shift'},       'right', hs.grid.resizeWindowWider)
winMode:bind({'shift'},       'up',    hs.grid.resizeWindowShorter)
winMode:bind({'shift'},       'down',  hs.grid.resizeWindowTaller)

winMode:bind({'cmd'},         'left',  hs.grid.shoveWindowLeft)
winMode:bind({'cmd'},         'right', hs.grid.shoveWindowRight)
winMode:bind({'cmd'},         'up',    hs.grid.shoveWindowUp)
winMode:bind({'cmd'},         'down',  hs.grid.shoveWindowDown)

winMode:bind({'cmd','shift'}, 'left',  hs.grid.partialWindowHorizontal(2, 1))
winMode:bind({'cmd','shift'}, 'right', hs.grid.partialWindowHorizontal(2, 2))
winMode:bind({'cmd','shift'}, 'up',    hs.grid.partialWindowVertical(2, 1))
winMode:bind({'cmd','shift'}, 'down',  hs.grid.partialWindowVertical(2, 2))

winMode:bind({'ctrl'},        'left',  hs.grid.pushWindowPrevScreen)
winMode:bind({'ctrl'},        'right', hs.grid.pushWindowNextScreen)
winMode:bind({'ctrl'},        'up',    hs.grid.pushWindowPrevScreen)
winMode:bind({'ctrl'},        'down',  hs.grid.pushWindowNextScreen)
