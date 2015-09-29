function ssht --argument session host rest
  ssh $host -t "tmux a -t $session || tmux new -s $session" $rest
end
