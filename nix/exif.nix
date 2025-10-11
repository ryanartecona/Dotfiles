{ writeShellScriptBin, symlinkJoin }:

let
  # cameras
  exif-olympus-xa = writeShellScriptBin "exif-olympus-xa" ''
    exiftool -Make="Olympus XA" -LensMake="F. Zuiko" -LensModel="35mm f/2.8" -FocalLength=35 -FocalLengthIn35mmFormat=35 "$@"
  '';
  exif-minolta-xd = writeShellScriptBin "exif-minolta-xd" ''
    exiftool -Make="Minolta XD" "$@"
  '';

  # lenses
  exif-minolta-md-50mm-f1-4 = writeShellScriptBin "exif-minolta-md-50mm-f1.4" ''
    exiftool -LensMake="Minolta MD" -LensModel="50mm f/1.4" -FocalLength=50 -FocalLengthIn35mmFormat=50 "$@"
  '';
  exif-minolta-md-35-70mm-f3-5 = writeShellScriptBin "exif-minolta-md-35-70mm-f3.5" ''
    exiftool -LensMake="Minolta MD" -LensModel="35-70mm f/3.5" -FocalLength=50 -FocalLengthIn35mmFormat=50 "$@"
  '';

  # film stocks
  exif-fujicolor-200 = writeShellScriptBin "exif-fujicolor-200" ''
    exiftool -Model="Fujifilm Fujicolor 200" -ISO=200 "$@"
  '';
  exif-kodak-gold-200 = writeShellScriptBin "exif-kodak-gold-200" ''
    exiftool -Model="Kodak Gold 200" -ISO=200 "$@"
  '';
  exif-ilford-hp5-plus = writeShellScriptBin "exif-ilford-hp5-plus" ''
    exiftool -Model="Ilford HP5 Plus" -ISO=400 "$@"
  '';

in
symlinkJoin {
  name = "exiftool presets";
  paths = [
    exif-olympus-xa
    exif-minolta-xd
    exif-minolta-md-50mm-f1-4
    exif-minolta-md-35-70mm-f3-5
    exif-fujicolor-200
    exif-kodak-gold-200
    exif-ilford-hp5-plus
  ];
}
