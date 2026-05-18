{ primaryUser ? "ryanartecona", ... }:

{
  system.defaults = {
    NSGlobalDomain = {
      AppleShowAllExtensions = true;
      NSAutomaticCapitalizationEnabled = false;
      NSAutomaticSpellingCorrectionEnabled = false;
      "com.apple.mouse.tapBehavior" = 1;
      "com.apple.trackpad.enableSecondaryClick" = true;
      # "com.apple.keyboard.fnState" = true;
      # disable press-and-hold diacritics
      ApplePressAndHoldEnabled = false;
      # cmd+ctrl drag to drag from anywhere in a window
      NSWindowShouldDragOnGesture = true;
    };
    dock = {
      autohide = true;
      autohide-delay = 0.2;
      autohide-time-modifier = 0.1;
      tilesize = 60;
      magnification = true;
      largesize = 100;
      mineffect = "scale";
      # minimize-to-application = true;
      orientation = "bottom";
      showhidden = false;
      show-recents = false;
    };
    finder = {
      ShowPathbar = true;
      ShowStatusBar = true;
    };
    trackpad = {
      Clicking = true;
      TrackpadRightClick = true;
    };
    magicmouse = {
      MouseButtonMode = "TwoButton";
    };

    CustomUserPreferences = {
      # Settings of plist in /Users/${vars.user}/Library/Preferences/
      "com.apple.finder" = {
        # Set home directory as startup window
        NewWindowTargetPath = "file:///Users/${primaryUser}/";
        NewWindowTarget = "PfHm";
        # Set search scope to directory
        FXDefaultSearchScope = "SCcf";
        # Multi-file tab view
        FinderSpawnTab = true;
        # show full POSIX path in window title
        _FXShowPosixPathInTitle = true;
        # show hidden files
        AppleShowAllFiles = true;
        # enable text selection in QuickLook
        QLEnableTextSelection = true;
      };
      "com.apple.desktopservices" = {
        # Disable creating .DS_Store files in network an USB volumes
        DSDontWriteNetworkStores = true;
        DSDontWriteUSBStores = true;
      };
      # Show battery percentage
      "/Users/${primaryUser}/Library/Preferences/ByHost/com.apple.controlcenter".BatteryShowPercentage =
        true;
      # Privacy
      "com.apple.AdLib".allowApplePersonalizedAdvertising = false;
      # play nicer with AeroSpace
      "com.apple.dock".expose-group-apps = true;
      "com.apple.dock".spans-displays = true;
    };
    CustomSystemPreferences = {
      # /Users/${vars.user}/Library/Preferences/
    };
  };
}
