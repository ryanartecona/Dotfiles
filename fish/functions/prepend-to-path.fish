function prepend-to-path \
  --description "Prepend an element to $PATH" \
  --argument element

  set --local real_element (realpath $element)
  set --local pretty_element (string replace $HOME "~" $real_element)

  if test -d $real_element
    if status is-interactive
      echo "Adding to \$PATH: $pretty_element" >&2
    end
    set PATH $real_element $PATH
  else
    if status is-interactive
      echo "Error: $pretty_element does not exist" >&2
      return 1
    end
  end
end
