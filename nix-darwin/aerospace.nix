{ ... }:

{
  services.aerospace.enable = true;
  services.aerospace.settings =
    let
      widthInc = 120;
      heightInc = 70;
    in
    {
      accordion-padding = 80;
      gaps = {
        outer.top = 10;
        outer.right = 10;
        outer.bottom = 10;
        outer.left = 10;
        inner.horizontal = 10;
        inner.vertical = 10;
      };
      # https://nikitabobko.github.io/AeroSpace/guide#default-config
      mode.main.binding = {
        # un-float and tile this window, toggling horizontal/vertical
        ctrl-slash = [
          "layout tiling"
          "layout tiles horizontal vertical"
        ];
        ctrl-cmd-slash = [
          "layout tiling"
          "layout tiles horizontal vertical"
        ];
        # un-float and accordion this window, toggling horizontal/vertical
        ctrl-comma = [
          "layout tiling"
          "layout accordion horizontal vertical"
        ];
        ctrl-cmd-comma = [
          "layout tiling"
          "layout accordion horizontal vertical"
        ];

        ctrl-left = "focus left";
        ctrl-down = "focus down";
        ctrl-up = "focus up";
        ctrl-right = "focus right";

        # deprecate?
        ctrl-h = "focus left";
        ctrl-j = "focus down";
        ctrl-k = "focus up";
        ctrl-l = "focus right";

        ctrl-cmd-left = "move left";
        ctrl-cmd-down = "move down";
        ctrl-cmd-up = "move up";
        ctrl-cmd-right = "move right";

        # deprecate?
        ctrl-cmd-h = "move left";
        ctrl-cmd-j = "move down";
        ctrl-cmd-k = "move up";
        ctrl-cmd-l = "move right";

        ctrl-minus = "resize smart -100";
        ctrl-equal = "resize smart +100";
        ctrl-cmd-minus = [
          "resize width -${toString widthInc}"
          "resize height -${toString heightInc}"
        ];
        ctrl-cmd-equal = [
          "resize width +${toString widthInc}"
          "resize height +${toString heightInc}"
        ];

        ctrl-1 = "workspace 1";
        ctrl-2 = "workspace 2";
        ctrl-3 = "workspace 3";
        ctrl-4 = "workspace 4";
        ctrl-5 = "workspace 5";
        ctrl-6 = "workspace 6";
        ctrl-7 = "workspace 7";
        ctrl-8 = "workspace 8";
        ctrl-9 = "workspace 9";

        ctrl-cmd-1 = [
          "move-node-to-workspace 1"
          "workspace 1"
        ];
        ctrl-cmd-2 = [
          "move-node-to-workspace 2"
          "workspace 2"
        ];
        ctrl-cmd-3 = [
          "move-node-to-workspace 3"
          "workspace 3"
        ];
        ctrl-cmd-4 = [
          "move-node-to-workspace 4"
          "workspace 4"
        ];
        ctrl-cmd-5 = [
          "move-node-to-workspace 5"
          "workspace 5"
        ];
        ctrl-cmd-6 = [
          "move-node-to-workspace 6"
          "workspace 6"
        ];
        ctrl-cmd-7 = [
          "move-node-to-workspace 7"
          "workspace 7"
        ];
        ctrl-cmd-8 = [
          "move-node-to-workspace 8"
          "workspace 8"
        ];
        ctrl-cmd-9 = [
          "move-node-to-workspace 9"
          "workspace 9"
        ];

        ctrl-tab = "workspace-back-and-forth";
        ctrl-cmd-tab = "move-workspace-to-monitor --wrap-around next";

        ctrl-cmd-space = "mode service";
      };
      mode.service.binding = {
        esc = [
          "reload-config"
          "mode main"
        ];
        ctrl-cmd-space = [
          "balance-sizes"
          "resize width +${toString (widthInc * 2)}"
          "resize height +${toString (heightInc * 2)}"
          "mode main"
        ];
        ctrl-cmd-0 = [
          "balance-sizes"
          "mode main"
        ];
        # reset layout
        r = [
          "flatten-workspace-tree"
          "balance-sizes"
          "mode main"
        ];
        # Toggle between floating and tiling layout
        f = [
          "layout floating tiling"
          "mode main"
        ];
        backspace = [
          "close-all-windows-but-current"
          "mode main"
        ];

        # sticky is not yet supported https://github.com/nikitabobko/AeroSpace/issues/2
        #s = ["layout sticky tiling" "mode main"]

        ctrl-cmd-h = [
          "join-with left"
          "mode main"
        ];
        ctrl-cmd-j = [
          "join-with down"
          "mode main"
        ];
        ctrl-cmd-k = [
          "join-with up"
          "mode main"
        ];
        ctrl-cmd-l = [
          "join-with right"
          "mode main"
        ];

        ctrl-cmd-1 = [
          "move-node-to-workspace 1"
          "mode main"
        ];
        ctrl-cmd-2 = [
          "move-node-to-workspace 2"
          "mode main"
        ];
        ctrl-cmd-3 = [
          "move-node-to-workspace 3"
          "mode main"
        ];
        ctrl-cmd-4 = [
          "move-node-to-workspace 4"
          "mode main"
        ];
        ctrl-cmd-5 = [
          "move-node-to-workspace 5"
          "mode main"
        ];
        ctrl-cmd-6 = [
          "move-node-to-workspace 6"
          "mode main"
        ];
        ctrl-cmd-7 = [
          "move-node-to-workspace 7"
          "mode main"
        ];
        ctrl-cmd-8 = [
          "move-node-to-workspace 8"
          "mode main"
        ];
        ctrl-cmd-9 = [
          "move-node-to-workspace 9"
          "mode main"
        ];

        # make new iterm window
        ctrl-cmd-t = [
          # https://gist.github.com/reyjrar/1769355?permalink_comment_id=1498007#gistcomment-1498007
          ''
            exec-and-forget osascript -e 'if application "iTerm" is running then
                tell application "iTerm"
                    create window with default profile
                end tell
            else
                activate application "iTerm"
            end if'
          ''
          "mode main"
        ];

        down = "volume down";
        up = "volume up";
        cmd-down = [
          "volume set 0"
          "mode main"
        ];
      };
    };

  services.jankyborders = {
    enable = true;
    # active_color = "0xffe1e3e4";
    active_color = "glow(0xffd1aef1)";
    inactive_color = "0xff494d64";
    width = 4.0;
    blur_radius = 10.0;
    order = "above";
    hidpi = true;
  };

}
