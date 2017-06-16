alias foo='ls -l'
foo .
unalias foo
foo . || echo 'failed as expected'
