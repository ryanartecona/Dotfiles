self: super:
{
  ra-e = super.writeScriptBin "e" ''
    #!${super.bash}/bin/bash

    if [[ -z "$EDITOR" ]]; then
      1>&2 echo '$EDITOR is not set'
      exit 1
    fi

    exec $EDITOR "$@"
  '';
}
