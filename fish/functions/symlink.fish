function symlink --argument from to
  ln -s (pwd)"/$from" "$to"
end
