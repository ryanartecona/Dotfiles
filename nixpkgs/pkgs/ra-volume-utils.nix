{ stdenv, alsaUtils, writeScriptBin, buildEnv }:

let
  volume-up = writeScriptBin "volume-up" ''
    #!${stdenv.shell}
    ${alsaUtils}/bin/amixer set Master 5%+
  '';
  volume-down = writeScriptBin "volume-down" ''
    #!${stdenv.shell}
    ${alsaUtils}/bin/amixer set Master 5%-
  '';

in
  buildEnv {
    name = "ra-volume-utils";
    paths = [ volume-up volume-down ];
  }
