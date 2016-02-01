function ssht --argument session host rest
  ssh $host -t "sudo tmux a -t $session || sudo tmux new -s $session" $rest
end
